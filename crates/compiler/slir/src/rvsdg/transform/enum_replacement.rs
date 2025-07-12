use crate::rvsdg::{
    Connectivity, Node, NodeKind, Region, Rvsdg, SimpleNode, ValueInput, ValueOrigin, ValueUser,
};
use crate::ty::{TypeKind, TY_PTR_U32, TY_U32};
use crate::{Enum, Function, Module};

pub struct EnumAllocaReplacer {
    variant_node_buffer: Vec<Node>,
}

impl EnumAllocaReplacer {
    pub fn new() -> Self {
        EnumAllocaReplacer {
            variant_node_buffer: vec![],
        }
    }

    pub fn replace_in_fn(&mut self, module: &mut Module, rvsdg: &mut Rvsdg, function: Function) {
        let node = rvsdg
            .get_function_node(function)
            .expect("function should have RVSDG body");
        let body_region = rvsdg[node].expect_function().body_region();

        self.visit_region(module, rvsdg, body_region);
    }

    fn visit_region(&mut self, module: &mut Module, rvsdg: &mut Rvsdg, region: Region) {
        use NodeKind::*;
        use SimpleNode::*;

        let node_count = rvsdg[region].nodes().len();

        for i in 0..node_count {
            let node = rvsdg[region].nodes()[i];

            match rvsdg[node].kind() {
                Simple(OpAlloca(_)) => self.visit_alloca_node(module, rvsdg, node),
                Switch(_) => self.visit_switch_node(module, rvsdg, node),
                Loop(_) => self.visit_switch_node(module, rvsdg, node),
                _ => {}
            }
        }
    }

    fn visit_alloca_node(&mut self, module: &mut Module, rvsdg: &mut Rvsdg, alloca_node: Node) {
        let ty = rvsdg[alloca_node].expect_op_alloca().ty();

        if let TypeKind::Enum(enum_handle) = module.ty[ty] {
            let mut replacer = AllocaReplacer {
                module,
                rvsdg,
                enum_handle,
            };

            replacer.replace_alloca(alloca_node);
        }
    }

    fn visit_switch_node(&mut self, module: &mut Module, rvsdg: &mut Rvsdg, node: Node) {
        let branch_count = rvsdg[node].expect_switch().branches().len();

        for i in 0..branch_count {
            let branch = rvsdg[node].expect_switch().branches()[i];

            self.visit_region(module, rvsdg, branch);
        }
    }

    fn visit_loop_node(&mut self, module: &mut Module, rvsdg: &mut Rvsdg, node: Node) {
        let loop_region = rvsdg[node].expect_loop().loop_region();

        self.visit_region(module, rvsdg, loop_region);
    }
}

struct AllocaReplacer<'a, 'b> {
    module: &'a mut Module,
    rvsdg: &'b mut Rvsdg,
    enum_handle: Enum,
}

impl<'a, 'b> AllocaReplacer<'a, 'b> {
    fn replace_alloca(&mut self, node: Node) {
        let node_data = &self.rvsdg[node];
        let region = node_data.region();
        let discriminant_node = self
            .rvsdg
            .add_op_alloca(&mut self.module.ty, region, TY_U32);

        let mut replacements = Vec::new();

        replacements.push(ValueInput {
            ty: TY_PTR_U32,
            origin: ValueOrigin::Output {
                producer: discriminant_node,
                output: 0,
            },
        });

        replacements.extend(
            self.module.enums[self.enum_handle]
                .variants
                .iter()
                .copied()
                .map(|variant| {
                    let ty = self.module.ty.register(TypeKind::Struct(variant));
                    let ptr_ty = self.module.ty.register(TypeKind::Ptr(ty));
                    let variant_node = self.rvsdg.add_op_alloca(&mut self.module.ty, region, ty);

                    ValueInput {
                        ty: ptr_ty,
                        origin: ValueOrigin::Output {
                            producer: variant_node,
                            output: 0,
                        },
                    }
                }),
        );

        self.visit_users(node, 0, &replacements);

        // The OpAlloca node now should not have any users left, so we can remove it
        self.rvsdg.remove_node(node);
    }

    fn visit_users(&mut self, node: Node, output: u32, split_inputs: &[ValueInput]) {
        let region = self.rvsdg[node].region();
        let user_count = self.rvsdg[node].value_outputs()[output as usize]
            .users
            .len();

        // We iterate over users in reverse order, so that users may more themselves from the user
        // set, without disrupting iteration
        for i in (0..user_count).rev() {
            let user = self.rvsdg[node].value_outputs()[output as usize].users[i];

            self.visit_user(region, user, split_inputs);
        }
    }

    fn visit_user(&mut self, region: Region, user: ValueUser, split_input: &[ValueInput]) {
        match user {
            ValueUser::Result(result) => self.split_region_result(region, result, split_input),
            ValueUser::Input { consumer, input } => {
                self.visit_node_input(consumer, input, split_input)
            }
        }
    }

    fn split_region_result(&mut self, region: Region, result: u32, split_input: &[ValueInput]) {
        let owner = self.rvsdg[region].owner();

        match self.rvsdg[owner].kind() {
            NodeKind::Switch(_) => self.split_switch_result(region, result, split_input),
            NodeKind::Loop(_) => self.split_loop_result(region, result, split_input),
            NodeKind::Function(_) => panic!(
                "cannot split function result; \
            non-local-use analyses should have rejected the alloca"
            ),
            _ => unreachable!("node kind cannot by a region owner"),
        }
    }

    fn visit_node_input(&mut self, node: Node, input: u32, split_input: &[ValueInput]) {
        use NodeKind::*;
        use SimpleNode::*;

        match self.rvsdg[node].kind() {
            Switch(_) => self.split_switch_input(node, input, split_input),
            Loop(_) => self.split_loop_input(node, input, split_input),
            Simple(OpGetDiscriminant(_)) => self.replace_op_get_discriminant(node, split_input),
            Simple(OpSetDiscriminant(_)) => self.replace_op_set_discriminant(node, split_input),
            Simple(OpPtrDiscriminantPtr(_)) => {
                self.elide_op_ptr_discriminant_ptr(node, split_input)
            }
            Simple(OpPtrVariantPtr(_)) => self.elide_op_ptr_variant_ptr(node, split_input),
            Simple(ValueProxy(_)) => self.visit_value_proxy(node, split_input),
            _ => unreachable!("node kind cannot take a pointer to an enum as input"),
        }
    }

    fn elide_op_ptr_discriminant_ptr(&mut self, node: Node, split_input: &[ValueInput]) {
        let node_data = &self.rvsdg[node];
        let region = node_data.region();
        let node_data = node_data.expect_op_ptr_discriminant_ptr();
        let new_user_origin = split_input[0].origin;
        let user_count = node_data.output().users.len();

        for i in (0..user_count).rev() {
            let user = self.rvsdg[node]
                .expect_op_ptr_discriminant_ptr()
                .output()
                .users[i];

            self.rvsdg
                .reconnect_value_user(region, user, new_user_origin);
        }

        // We've reconnected all the node's users now. Consequently, it's now dead and can
        // be removed.
        self.rvsdg.remove_node(node);
    }

    fn elide_op_ptr_variant_ptr(&mut self, node: Node, split_input: &[ValueInput]) {
        let node_data = &self.rvsdg[node];
        let region = node_data.region();
        let node_data = node_data.expect_op_ptr_variant_ptr();
        let variant_index = node_data.variant_index();
        let new_user_origin = split_input[variant_index as usize + 1].origin;
        let user_count = node_data.output().users.len();

        for i in (0..user_count).rev() {
            let user = self.rvsdg[node].expect_op_ptr_variant_ptr().output().users[i];

            self.rvsdg
                .reconnect_value_user(region, user, new_user_origin);
        }

        // We've reconnected all the node's users now. Consequently, it's now dead and can
        // be removed.
        self.rvsdg.remove_node(node);
    }

    fn replace_op_get_discriminant(&mut self, node: Node, split_input: &[ValueInput]) {
        let region = self.rvsdg[node].region();
        let state_origin = self.rvsdg[node]
            .state()
            .expect("get-discriminant operation should part of state chain")
            .origin;
        let user_count = self.rvsdg[node]
            .expect_op_get_discriminant()
            .output()
            .users
            .len();

        let load_node = self
            .rvsdg
            .add_op_load(region, split_input[0], TY_U32, state_origin);

        for i in (0..user_count).rev() {
            let user = self.rvsdg[node].expect_op_get_discriminant().output().users[i];

            self.rvsdg.reconnect_value_user(
                region,
                user,
                ValueOrigin::Output {
                    producer: load_node,
                    output: 0,
                },
            );
        }

        // After visiting the users of the original node's output, all users should have been
        // reconnected to the new load node and the original node should have no users left; we
        // should be able to remove the node now.
        self.rvsdg.remove_node(node);
    }

    fn replace_op_set_discriminant(&mut self, node: Node, split_input: &[ValueInput]) {
        let region = self.rvsdg[node].region();
        let state_origin = self.rvsdg[node]
            .state()
            .expect("set-discriminant operation should part of state chain")
            .origin;
        let node_data = self.rvsdg[node].expect_op_set_discriminant();
        let variant_index = node_data.variant_index();

        let variant_node = self.rvsdg.add_const_u32(region, variant_index);

        self.rvsdg.add_op_store(
            region,
            split_input[0],
            ValueInput::output(TY_U32, variant_node, 0),
            state_origin,
        );
        self.rvsdg.remove_node(node);
    }

    fn split_switch_input(&mut self, node: Node, input: u32, split_input: &[ValueInput]) {
        assert_ne!(input, 0, "the branch selector input is never an aggregate");

        let arg_index = input as usize - 1;
        let node_data = self.rvsdg[node].expect_switch();
        let branch_count = node_data.branches().len();
        let split_args_base = node_data.value_inputs().len() - 1;

        let split_args = split_input
            .iter()
            .copied()
            .enumerate()
            .map(|(i, input)| {
                self.rvsdg.add_switch_input(node, input);

                ValueInput {
                    ty: input.ty,
                    origin: ValueOrigin::Argument((split_args_base + i) as u32),
                }
            })
            .collect::<Vec<_>>();

        for branch_index in 0..branch_count {
            let branch = self.rvsdg[node].expect_switch().branches()[branch_index];

            self.redirect_region_argument(branch, arg_index as u32, &split_args);
        }

        self.rvsdg.remove_switch_input(node, input);
    }

    fn split_switch_result(&mut self, branch: Region, result: u32, split_input: &[ValueInput]) {
        let node = self.rvsdg[branch].owner();
        let node_data = self.rvsdg[node].expect_switch();
        let branch_count = node_data.branches().len();
        let base_index = node_data.value_outputs().len();

        let mut split_output = Vec::with_capacity(split_input.len());

        // First split the output/results and connect the new results for the provoking branch. Also
        // record a mapping for the split output
        for (i, input) in split_input.iter().enumerate() {
            let index = base_index + i;

            self.rvsdg.add_switch_output(node, input.ty);
            self.rvsdg
                .reconnect_region_result(branch, index as u32, input.origin);

            split_output.push(ValueInput {
                ty: input.ty,
                origin: ValueOrigin::Output {
                    producer: node,
                    output: index as u32,
                },
            })
        }

        // Now reconnect the results for the other branches
        for i in 0..branch_count {
            let current_branch = self.rvsdg[node].expect_switch().branches()[i];

            if current_branch != branch {
                self.redirect_region_result(current_branch, result as usize, base_index);
            }
        }

        // Finally, split the output
        self.visit_users(node, result, &split_output);
    }

    fn split_loop_input(&mut self, node: Node, input: u32, split_input: &[ValueInput]) {
        let node_data = self.rvsdg[node].expect_loop();
        let loop_region = node_data.loop_region();
        let prior_input_count = node_data.value_inputs().len();
        let prior_result_count = prior_input_count + 1;

        let mut split_args = Vec::with_capacity(split_input.len());
        let mut split_outputs = Vec::with_capacity(split_input.len());

        // Add inputs/outputs/arguments/results for each element of the original aggregate input
        // and record mappings for both the arguments and the outputs.
        for (i, input) in split_input.iter().enumerate() {
            self.rvsdg.add_loop_input(node, *input);

            split_args.push(ValueInput {
                ty: input.ty,
                origin: ValueOrigin::Argument((prior_input_count + i) as u32),
            });
            split_outputs.push(ValueInput {
                ty: input.ty,
                origin: ValueOrigin::Output {
                    producer: node,
                    output: (prior_input_count + i) as u32,
                },
            });
        }

        // First connect all region results that we've created to the unsplit input that the
        // original result connects to, via [OpGetDiscriminant] nodes or [OpPtrVariantPtr] nodes.
        // This also disconnects the original result.
        self.redirect_region_result(loop_region, input as usize + 1, prior_result_count);

        // Now redirect the argument using the argument mapping. We do this after redirecting the
        // results, because otherwise this might try to split the original result again; by doing
        // this after result redirection, all the argument's user tree should terminate at the
        // [OpGetDiscriminant]/[OpPtrVariantPtr] nodes that were inserted, since nothing should
        // be connected to the original result anymore.
        self.redirect_region_argument(loop_region, input, &split_args);

        // Finally, redirect the value output using the output mapping
        self.visit_users(node, input, &split_outputs);

        // Now neither the argument nor the output should have any remaining users, so we can remove
        // the original input/output/argument/result and disconnect the input from its origin.
        self.rvsdg.remove_loop_input(node, input);
    }

    fn split_loop_result(&mut self, region: Region, result: u32, split_input: &[ValueInput]) {
        assert_ne!(
            result, 0,
            "the reentry decider result is never a pointer to an enum"
        );

        let owner = self.rvsdg[region].owner();
        let outer_region = self.rvsdg[owner].region();
        let loop_data = self.rvsdg[owner].expect_loop();
        let prior_input_count = loop_data.value_inputs().len();
        let prior_result_count = prior_input_count + 1;

        let input_index = result - 1;
        let value_input = loop_data.value_inputs()[input_index as usize];

        // A loop node's input and outputs must match, so splitting a result also means splitting
        // the corresponding input. However, propagating splits "upwards" runs the risk of
        // concurrently modifying a user set that is currently being traversed. To sidestep this,
        // we insert a proxy between the input and its origin, so that our modification will modify
        // the proxy's user set, which we know is not currently being traversed.
        let proxy = self.rvsdg.proxy_origin_user(
            outer_region,
            value_input.ty,
            value_input.origin,
            ValueUser::Input {
                consumer: owner,
                input: input_index,
            },
        );
        let proxy_input = ValueInput {
            ty: value_input.ty,
            origin: ValueOrigin::Output {
                producer: proxy,
                output: 0,
            },
        };

        let mut split_args = Vec::with_capacity(split_input.len());
        let mut split_outputs = Vec::with_capacity(split_input.len());

        // Add an input for the discriminant and for each variant and connect them to the proxy via
        // an OpPtrDiscriminantPtr node for the discriminant input or an OpPtrVariantPtr node for
        // the variant inputs. Also record an argument mapping in `split_args` and an output mapping
        // in `split_outputs`.

        let discriminant_node = self
            .rvsdg
            .add_op_ptr_discriminant_ptr(outer_region, proxy_input);

        self.rvsdg
            .add_loop_input(owner, ValueInput::output(TY_PTR_U32, discriminant_node, 0));

        split_args.push(ValueInput::argument(TY_PTR_U32, prior_input_count as u32));
        split_outputs.push(ValueInput::output(
            TY_PTR_U32,
            owner,
            prior_input_count as u32,
        ));

        let variant_count = self.module.enums[self.enum_handle].variants.len();

        for i in 0..variant_count {
            let input_index = (prior_input_count + 1 + i) as u32;
            let variant_node = self.rvsdg.add_op_ptr_variant_ptr(
                &mut self.module.ty,
                &mut self.module.enums,
                outer_region,
                proxy_input,
                i as u32,
            );
            let variant_ptr_ty = self.rvsdg[variant_node]
                .expect_op_ptr_variant_ptr()
                .output()
                .ty;

            self.rvsdg
                .add_loop_input(owner, ValueInput::output(variant_ptr_ty, variant_node, 0));

            split_args.push(ValueInput::argument(variant_ptr_ty, input_index));
            split_outputs.push(ValueInput::output(variant_ptr_ty, owner, input_index));
        }

        // Reconnect the results we just created to the `split_input`.
        for (i, input) in split_input.iter().enumerate() {
            let result_index = prior_result_count + i;

            self.rvsdg
                .reconnect_region_result(region, result_index as u32, input.origin);
        }

        // Redirect the argument using the argument mapping
        self.redirect_region_argument(region, input_index, &split_args);

        // Redirect the value output using the output mapping
        self.visit_users(owner, input_index, &split_outputs);

        // Now neither the argument nor the output should have any remaining users, so we can remove
        // the original input/output/argument/result and disconnect the input from its origin. We
        // don't have to worry about the input's removal affecting the user traversal of an upstream
        // node, because we proxied the input earlier.
        self.rvsdg.remove_loop_input(owner, input_index);
    }

    fn visit_value_proxy(&mut self, node: Node, split_input: &[ValueInput]) {
        self.visit_users(node, 0, split_input);
        self.rvsdg.remove_node(node);
    }

    /// Redirects all users of the `region`'s given `argument` to the `split_input` nodes.
    ///
    /// Leaves the `argument` without any users.
    fn redirect_region_argument(
        &mut self,
        region: Region,
        argument: u32,
        split_input: &[ValueInput],
    ) {
        let arg_index = argument as usize;
        let user_count = self.rvsdg[region].value_arguments()[arg_index].users.len();

        // We iterate over users in reverse order, so that users may more themselves from the user
        // set, without disrupting iteration
        for user_index in (0..user_count).rev() {
            let user = self.rvsdg[region].value_arguments()[arg_index].users[user_index];

            self.visit_user(region, user, split_input)
        }
    }

    /// Redirects the origin for the `region`'s given `result` to a set of "split" results that
    /// start at `split_results_start`, via either [OpPtrDiscriminant] for the discriminant or
    /// [OpPtrVariantPtr] nodes for the variants.
    ///
    /// Leaves the original result connected to the "placeholder" origin.
    fn redirect_region_result(
        &mut self,
        region: Region,
        original: usize,
        split_results_start: usize,
    ) {
        let original_input = self.rvsdg[region].value_results()[original];
        let discriminant_node = self
            .rvsdg
            .add_op_ptr_discriminant_ptr(region, original_input);

        self.rvsdg.reconnect_region_result(
            region,
            split_results_start as u32,
            ValueOrigin::Output {
                producer: discriminant_node,
                output: 0,
            },
        );

        let variant_count = self.module.enums[self.enum_handle].variants.len();

        for i in 0..variant_count {
            let result_index = (split_results_start + 1 + i) as u32;
            let variant_node = self.rvsdg.add_op_ptr_variant_ptr(
                &mut self.module.ty,
                &mut self.module.enums,
                region,
                original_input,
                i as u32,
            );

            self.rvsdg.reconnect_region_result(
                region,
                result_index,
                ValueOrigin::Output {
                    producer: variant_node,
                    output: 0,
                },
            );
        }

        self.rvsdg.disconnect_region_result(region, original as u32);
    }
}

pub fn entry_points_enum_replacement(module: &mut Module, rvsdg: &mut Rvsdg) {
    let mut replacer = EnumAllocaReplacer::new();

    let entry_points = module
        .entry_points
        .iter()
        .map(|(f, _)| f)
        .collect::<Vec<_>>();

    for entry_point in entry_points {
        replacer.replace_in_fn(module, rvsdg, entry_point);
    }
}
