use rustc_hash::FxHashMap;

use crate::rvsdg::{
    Connectivity, Node, NodeKind, Region, Rvsdg, SimpleNode, StateOrigin, ValueInput, ValueOrigin,
    ValueOutput, ValueUser,
};
use crate::Module;

struct RegionReplicator<'a, 'b> {
    module: &'a mut Module,
    rvsdg: &'b mut Rvsdg,
    src_region: Region,
    dst_region: Region,

    /// A mapping from the inlined function's arguments to the new value origins at the apply site.
    ///
    /// Since arguments are identified by a contiguous range of indices, we can use a vec rather
    /// than a hash map to record this mapping.
    value_argument_mapping: Vec<ValueOrigin>,

    state_argument_mapping: Option<StateOrigin>,
    node_mapping: FxHashMap<Node, Node>,
}

impl<'a, 'b> RegionReplicator<'a, 'b> {
    fn new(
        module: &'a mut Module,
        rvsdg: &'b mut Rvsdg,
        src_region: Region,
        dst_region: Region,
        value_argument_mapping: Vec<ValueOrigin>,
        state_argument_mapping: Option<StateOrigin>,
    ) -> Self {
        Self {
            module,
            rvsdg,
            src_region,
            dst_region,
            value_argument_mapping,
            state_argument_mapping,
            node_mapping: Default::default(),
        }
    }

    fn replicate_region(&mut self) -> Vec<ValueOrigin> {
        let result_count = self.rvsdg[self.src_region].value_results().len();
        let mut result_mapping = Vec::with_capacity(result_count);

        for i in 0..result_count {
            let origin = self.rvsdg[self.src_region].value_results()[i].origin;

            if origin.is_placeholder() {
                panic!("cannot inline a function that still contains placeholder result origins");
            }

            match origin {
                ValueOrigin::Argument(i) => {
                    result_mapping.push(self.value_argument_mapping[i as usize]);
                }
                ValueOrigin::Output { producer, output } => {
                    let replicate_node = self.visit_node(producer);

                    result_mapping.push(ValueOrigin::Output {
                        producer: replicate_node,
                        output,
                    });
                }
            }
        }

        if let StateOrigin::Node(node) = *self.rvsdg[self.src_region].state_result() {
            self.visit_node(node);
        }

        result_mapping
    }

    fn visit_node(&mut self, node: Node) -> Node {
        // We want to replicate the function body in a bottom-up post-order; that is, we want to
        // start from the results, and then do a depth-first search up the inputs, until we reach
        // nodes that are either input-less, or for which all inputs are region arguments. If we
        // create nodes in this order at the apply site, then we guarantee that for nodes that
        // depend on other nodes, all such dependencies will have been created before the dependent
        // node. We maintain a node mapping so that we can map function body origins to apply site
        // origins. Because outputs can have multiple users, this can result in us visiting the same
        // node multiple times. However, we can get dual use out of a node mapping by using it as a
        // "visited" set.

        if let Some(replicate_node) = self.node_mapping.get(&node) {
            // We've already visited this node
            return *replicate_node;
        }

        // Make sure all dependencies are replicated first
        for i in 0..self.rvsdg[node].value_inputs().len() {
            let origin = self.rvsdg[node].value_inputs()[i].origin;

            if !origin.is_placeholder()
                && let ValueOrigin::Output { producer, .. } = origin
            {
                self.visit_node(producer);
            }
        }

        if let Some(state) = self.rvsdg[node].state()
            && let StateOrigin::Node(dependency) = state.origin
        {
            self.visit_node(dependency);
        }

        let replicate_node = self.replicate_node(node);

        self.node_mapping.insert(node, replicate_node);

        replicate_node
    }

    fn replicate_node(&mut self, node: Node) -> Node {
        match self.rvsdg[node].kind() {
            NodeKind::Switch(_) => self.replicate_switch_node(node),
            NodeKind::Loop(_) => self.replicate_loop_node(node),
            NodeKind::Simple(SimpleNode::ConstU32(_)) => self.replicate_const_u32_node(node),
            NodeKind::Simple(SimpleNode::ConstI32(_)) => self.replicate_const_i32_node(node),
            NodeKind::Simple(SimpleNode::ConstF32(_)) => self.replicate_const_f32_node(node),
            NodeKind::Simple(SimpleNode::ConstBool(_)) => self.replicate_const_bool_node(node),
            NodeKind::Simple(SimpleNode::ConstPtr(_)) => self.replicate_const_ptr_node(node),
            NodeKind::Simple(SimpleNode::OpAlloca(_)) => self.replicate_op_alloca_node(node),
            NodeKind::Simple(SimpleNode::OpLoad(_)) => self.replicate_op_load_node(node),
            NodeKind::Simple(SimpleNode::OpStore(_)) => self.replicate_op_store_node(node),
            NodeKind::Simple(SimpleNode::OpPtrElementPtr(_)) => {
                self.replicate_op_ptr_element_ptr_node(node)
            }
            NodeKind::Simple(SimpleNode::OpApply(_)) => self.replicate_op_apply_node(node),
            NodeKind::Simple(SimpleNode::OpUnary(_)) => self.replicate_op_unary_node(node),
            NodeKind::Simple(SimpleNode::OpBinary(_)) => self.replicate_op_binary_node(node),
            NodeKind::Function(_)
            | NodeKind::UniformBinding(_)
            | NodeKind::StorageBinding(_)
            | NodeKind::WorkgroupBinding(_) => {
                panic!("node kind should not appear inside a region")
            }
        }
    }

    fn replicate_switch_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_switch();
        let value_inputs = data
            .value_inputs()
            .iter()
            .map(|input| self.mapped_value_input(input))
            .collect();
        let value_outputs = data
            .value_outputs()
            .iter()
            .map(|output| ValueOutput::new(output.ty))
            .collect();
        let state_origin = data
            .state()
            .map(|state| self.mapped_state_origin(&state.origin));

        let replicate_node =
            self.rvsdg
                .add_switch(self.dst_region, value_inputs, value_outputs, state_origin);

        // Replicate each of the node's branch regions
        let branch_count = self.rvsdg[node].expect_switch().branches().len();
        for i in 0..branch_count {
            let src_region = self.rvsdg[node].expect_switch().branches()[i];
            let replicate_region = self.rvsdg.add_switch_branch(replicate_node);
            let value_argument_mapping = (0..self.rvsdg[src_region].value_arguments().len())
                .map(|i| ValueOrigin::Argument(i as u32))
                .collect();

            let mut region_replicator = RegionReplicator::new(
                self.module,
                self.rvsdg,
                src_region,
                replicate_region,
                value_argument_mapping,
                Some(StateOrigin::Argument),
            );

            region_replicator.replicate_region();

            // Connect the replicate region's results
            let result_count = self.rvsdg[src_region].value_results().len();
            for i in 0..result_count {
                let input = &self.rvsdg[src_region].value_results()[i];
                let mapped_input = self.mapped_value_input(input);

                self.rvsdg
                    .reconnect_region_result(replicate_region, i as u32, mapped_input.origin);
            }
        }

        replicate_node
    }

    fn replicate_loop_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_loop();
        let value_inputs = data
            .value_inputs()
            .iter()
            .map(|input| self.mapped_value_input(input))
            .collect();
        let state_origin = data
            .state()
            .map(|state| self.mapped_state_origin(&state.origin));
        let src_region = *data.region();

        let (replicate_node, replicate_region) =
            self.rvsdg
                .add_loop(self.dst_region, value_inputs, state_origin);

        let value_argument_mapping = (0..self.rvsdg[src_region].value_arguments().len())
            .map(|i| ValueOrigin::Argument(i as u32))
            .collect();

        let mut region_replicator = RegionReplicator::new(
            self.module,
            self.rvsdg,
            src_region,
            replicate_region,
            value_argument_mapping,
            Some(StateOrigin::Argument),
        );

        region_replicator.replicate_region();

        // Connect the replicate region's results
        let result_count = self.rvsdg[src_region].value_results().len();
        for i in 0..result_count {
            let input = &self.rvsdg[src_region].value_results()[i];
            let mapped_input = self.mapped_value_input(input);

            self.rvsdg
                .reconnect_region_result(replicate_region, i as u32, mapped_input.origin);
        }

        replicate_node
    }

    fn replicate_const_u32_node(&mut self, node: Node) -> Node {
        let value = self.rvsdg[node].expect_const_u32().value();

        self.rvsdg.add_const_u32(self.dst_region, value)
    }

    fn replicate_const_i32_node(&mut self, node: Node) -> Node {
        let value = self.rvsdg[node].expect_const_i32().value();

        self.rvsdg.add_const_i32(self.dst_region, value)
    }

    fn replicate_const_f32_node(&mut self, node: Node) -> Node {
        let value = self.rvsdg[node].expect_const_f32().value();

        self.rvsdg.add_const_f32(self.dst_region, value)
    }

    fn replicate_const_bool_node(&mut self, node: Node) -> Node {
        let value = self.rvsdg[node].expect_const_bool().value();

        self.rvsdg.add_const_bool(self.dst_region, value)
    }

    fn replicate_const_ptr_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_const_ptr();
        let pointee_ty = data.pointee_ty();
        let base = self.mapped_value_input(data.base());

        self.rvsdg
            .add_const_ptr(&mut self.module.ty, self.dst_region, pointee_ty, base)
    }

    fn replicate_op_alloca_node(&mut self, node: Node) -> Node {
        let ty = self.rvsdg[node].expect_op_alloca().ty();

        self.rvsdg
            .add_op_alloca(&mut self.module.ty, self.dst_region, ty)
    }

    fn replicate_op_load_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_op_load();
        let ptr_input = self.mapped_value_input(data.ptr_input());
        let output_ty = data.value_output().ty;
        let state_origin = self.mapped_state_origin(&data.state().unwrap().origin);

        self.rvsdg
            .add_op_load(self.dst_region, ptr_input, output_ty, state_origin)
    }

    fn replicate_op_store_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_op_store();
        let ptr_input = self.mapped_value_input(data.ptr_input());
        let value_input = self.mapped_value_input(data.value_input());
        let state_origin = self.mapped_state_origin(&data.state().unwrap().origin);

        self.rvsdg
            .add_op_store(self.dst_region, ptr_input, value_input, state_origin)
    }

    fn replicate_op_ptr_element_ptr_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_op_ptr_element_ptr();
        let element_ty = data.element_ty();
        let ptr_input = self.mapped_value_input(data.ptr());
        let index_inputs = data
            .indices()
            .iter()
            .map(|input| self.mapped_value_input(input))
            .collect::<Vec<_>>();

        self.rvsdg.add_op_ptr_element_ptr(
            &mut self.module.ty,
            self.dst_region,
            element_ty,
            ptr_input,
            index_inputs,
        )
    }

    fn replicate_op_apply_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_op_apply();
        let fn_input = self.mapped_value_input(data.fn_input());
        let argument_inputs = data
            .argument_inputs()
            .iter()
            .map(|input| self.mapped_value_input(input))
            .collect::<Vec<_>>();
        let state_origin = self.mapped_state_origin(&data.state().unwrap().origin);

        self.rvsdg.add_op_apply(
            self.module,
            self.dst_region,
            fn_input,
            argument_inputs,
            state_origin,
        )
    }

    fn replicate_op_unary_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_op_unary();
        let operator = data.operator();
        let input = self.mapped_value_input(data.input());

        self.rvsdg.add_op_unary(self.dst_region, operator, input)
    }

    fn replicate_op_binary_node(&mut self, node: Node) -> Node {
        let data = self.rvsdg[node].expect_op_binary();
        let operator = data.operator();
        let lhs_input = self.mapped_value_input(data.lhs_input());
        let rhs_input = self.mapped_value_input(data.rhs_input());

        self.rvsdg
            .add_op_binary(self.dst_region, operator, lhs_input, rhs_input)
    }

    fn mapped_value_input(&self, input: &ValueInput) -> ValueInput {
        dbg!(input);
        dbg!(&self.value_argument_mapping);
        let origin = match input.origin {
            ValueOrigin::Argument(i) => self.value_argument_mapping[i as usize],
            ValueOrigin::Output { producer, output } => ValueOrigin::Output {
                producer: self
                    .node_mapping
                    .get(&producer)
                    .copied()
                    .expect("producer should have been visited earlier"),
                output,
            },
        };

        ValueInput {
            ty: input.ty,
            origin,
        }
    }

    fn mapped_state_origin(&self, origin: &StateOrigin) -> StateOrigin {
        match *origin {
            StateOrigin::Argument => self
                .state_argument_mapping
                .expect("a state argument should have been provided for a region that uses state"),
            StateOrigin::Node(node) => StateOrigin::Node(
                self.node_mapping
                    .get(&node)
                    .copied()
                    .expect("dependency should have been visited earlier"),
            ),
        }
    }
}

pub fn replicate_region(
    module: &mut Module,
    rvsdg: &mut Rvsdg,
    src_region: Region,
    dst_region: Region,
    value_argument_mapping: Vec<ValueOrigin>,
    state_argument_mapping: Option<StateOrigin>,
) -> Vec<ValueOrigin> {
    let mut region_replicator = RegionReplicator::new(
        module,
        rvsdg,
        src_region,
        dst_region,
        value_argument_mapping,
        state_argument_mapping,
    );

    region_replicator.replicate_region()
}

pub fn inline_function(module: &mut Module, rvsdg: &mut Rvsdg, apply_node: Node) {
    let node_data = &rvsdg[apply_node];
    let region = node_data.region();
    let apply_site = node_data.expect_op_apply();

    dbg!(apply_site);

    let argument_mapping = apply_site
        .argument_inputs()
        .iter()
        .map(|input| input.origin)
        .collect::<Vec<_>>();
    let state_argument_mapping = apply_site.state().map(|state| state.origin);
    let value_output_count = node_data.value_outputs().len();

    let function = apply_site.resolve_fn(module);
    let function_region = rvsdg
        .get_function_region(function)
        .expect("cannot apply a function without a body region");

    let result_mapping = replicate_region(
        module,
        rvsdg,
        function_region,
        region,
        argument_mapping,
        state_argument_mapping,
    );

    // Region replication does not connect the outputs used by the results of the original function
    // body, so we do that now using the mapping returned by region replication.
    for i in 0..value_output_count {
        let mapped_origin = result_mapping[i];
        let user_count = rvsdg[apply_node].value_outputs()[i].users.len();

        // Iterate over the user indices in reverse order, as during this loop we'll be removing
        // the respective user connection at each index; by going in reverse order, the indices will
        // remain correct despite this concurrent modification of the user list during iteration.
        // This is effectively equivalent to repeatedly popping the last entry until the list is
        // empty.
        for j in (0..user_count).rev() {
            match rvsdg[apply_node].value_outputs()[i].users[j] {
                ValueUser::Result(res) => {
                    rvsdg.reconnect_region_result(region, res, mapped_origin);
                }
                ValueUser::Input { consumer, input } => {
                    rvsdg.reconnect_value_input(consumer, input, mapped_origin);
                }
            }
        }
    }

    // Now that we've disconnected the users of all of the apply node's value outputs, the apply
    // node should be "dead", so we can remove it from the graph.
    rvsdg.remove_node(apply_node);
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;
    use crate::rvsdg::StateUser;
    use crate::ty::{TypeKind, TY_DUMMY, TY_U32};
    use crate::{BinaryOperator, FnArg, FnSig, Function, Symbol};

    #[test]
    fn test_inline_function() {
        let mut module = Module::new(Symbol::from_ref(""));

        let inline_target = Function {
            name: Symbol::from_ref("inline_target"),
            module: Symbol::from_ref(""),
        };
        let inline_target_ty = module.ty.register(TypeKind::Function(inline_target));

        module.fn_sigs.register(
            inline_target,
            FnSig {
                name: Default::default(),
                ty: inline_target_ty,
                args: vec![
                    FnArg {
                        ty: TY_U32,
                        shader_io_binding: None,
                    },
                    FnArg {
                        ty: TY_U32,
                        shader_io_binding: None,
                    },
                ],
                ret_ty: Some(TY_U32),
            },
        );

        let inline_dst = Function {
            name: Symbol::from_ref("inline_dst"),
            module: Symbol::from_ref(""),
        };

        module.fn_sigs.register(
            inline_dst,
            FnSig {
                name: Default::default(),
                ty: TY_DUMMY,
                args: vec![
                    FnArg {
                        ty: TY_U32,
                        shader_io_binding: None,
                    },
                    FnArg {
                        ty: TY_U32,
                        shader_io_binding: None,
                    },
                ],
                ret_ty: Some(TY_U32),
            },
        );

        let mut rvsdg = Rvsdg::new();

        // Build inline_target
        let (inline_target_node, src_region) =
            rvsdg.register_function(&module, inline_target, iter::empty());

        let inline_target_node_0 = rvsdg.add_op_binary(
            src_region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::argument(TY_U32, 1),
        );

        rvsdg.reconnect_region_result(
            src_region,
            0,
            ValueOrigin::Output {
                producer: inline_target_node_0,
                output: 0,
            },
        );

        // Build inline_dst
        let (_, dst_region) = rvsdg.register_function(&module, inline_dst, [inline_target_node]);

        let inline_dst_node_0 = rvsdg.add_const_u32(dst_region, 5);
        let inline_dst_node_1 = rvsdg.add_op_apply(
            &module,
            dst_region,
            ValueInput::argument(inline_target_ty, 0),
            [
                ValueInput::argument(TY_U32, 1),
                ValueInput::output(TY_U32, inline_dst_node_0, 0),
            ],
            StateOrigin::Argument,
        );
        let inline_dst_node_2 = rvsdg.add_op_binary(
            dst_region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 2),
            ValueInput::output(TY_U32, inline_dst_node_1, 0),
        );

        rvsdg.reconnect_region_result(
            dst_region,
            0,
            ValueOrigin::Output {
                producer: inline_dst_node_2,
                output: 0,
            },
        );

        inline_function(&mut module, &mut rvsdg, inline_dst_node_1);

        // The first argument to the region is the function dependency, which should no longer be
        // in use after inlining
        assert!(rvsdg[dst_region].value_arguments()[0].users.is_empty());

        // The second argument should now connect to input `0` of the inlined "add" node.
        let arg_1_users = &rvsdg[dst_region].value_arguments()[1].users;

        assert_eq!(arg_1_users.len(), 1);

        let arg_1_user = arg_1_users[0];

        let ValueUser::Input {
            consumer: inlined_node,
            input,
        } = arg_1_user
        else {
            panic!("expected user to be a node input");
        };

        assert_eq!(
            rvsdg[inlined_node].expect_op_binary().operator(),
            BinaryOperator::Add
        );
        assert_eq!(input, 0);

        // Node `0` in the destination region should now connect to input `1` of the inlined "add"
        // node.
        let inline_dst_node_0_users = &rvsdg[inline_dst_node_0].expect_const_u32().output().users;

        assert_eq!(inline_dst_node_0_users.len(), 1);
        assert_eq!(
            inline_dst_node_0_users[0],
            ValueUser::Input {
                consumer: inlined_node,
                input: 1
            }
        );

        // The second input of node `2` is the destination region should now connect to the output
        // of the inlined "add" node.
        assert_eq!(
            rvsdg[inline_dst_node_2].value_inputs()[1].origin,
            ValueOrigin::Output {
                producer: inlined_node,
                output: 0
            }
        );

        // The first input of the inlined node should now connect to the second argument of the
        // destination region.
        assert_eq!(
            rvsdg[inlined_node].value_inputs()[0].origin,
            ValueOrigin::Argument(1)
        );

        // The second input of the inlined node should now connect to the output of node `0` in the
        // destination region.
        assert_eq!(
            rvsdg[inlined_node].value_inputs()[1].origin,
            ValueOrigin::Output {
                producer: inline_dst_node_0,
                output: 0,
            }
        );

        // The output of the inlined node should now connect to the second input of node `2` in the
        // destination region.
        let inlined_node_users = &rvsdg[inlined_node].value_outputs()[0].users;

        assert_eq!(inlined_node_users.len(), 1);
        assert_eq!(
            inlined_node_users[0],
            ValueUser::Input {
                consumer: inline_dst_node_2,
                input: 1
            }
        );

        // After inlining the only "apply" node in the region, the destination region should no
        // longer contain any "apply" nodes.
        assert!(!rvsdg[dst_region]
            .nodes()
            .into_iter()
            .any(|n| rvsdg[n].is_op_apply()));
    }

    #[test]
    fn test_inline_function_stateful() {
        let mut module = Module::new(Symbol::from_ref(""));

        let inline_target = Function {
            name: Symbol::from_ref("inline_target"),
            module: Symbol::from_ref(""),
        };
        let inline_target_ty = module.ty.register(TypeKind::Function(inline_target));
        let ty_ptr_u32 = module.ty.register(TypeKind::Ptr(TY_U32));

        module.fn_sigs.register(
            inline_target,
            FnSig {
                name: Default::default(),
                ty: inline_target_ty,
                args: vec![FnArg {
                    ty: ty_ptr_u32,
                    shader_io_binding: None,
                }],
                ret_ty: None,
            },
        );

        let inline_dst = Function {
            name: Symbol::from_ref("inline_dst"),
            module: Symbol::from_ref(""),
        };

        module.fn_sigs.register(
            inline_dst,
            FnSig {
                name: Default::default(),
                ty: TY_DUMMY,
                args: vec![],
                ret_ty: Some(TY_U32),
            },
        );

        let mut rvsdg = Rvsdg::new();

        // Build inline_target
        let (inline_target_node, src_region) =
            rvsdg.register_function(&module, inline_target, iter::empty());

        let inline_target_node_0 = rvsdg.add_const_u32(src_region, 5);

        rvsdg.add_op_store(
            src_region,
            ValueInput::argument(ty_ptr_u32, 0),
            ValueInput::output(TY_U32, inline_target_node_0, 0),
            StateOrigin::Argument,
        );

        // Build inline_dst
        let (_, dst_region) = rvsdg.register_function(&module, inline_dst, [inline_target_node]);

        let inline_dst_node_0 = rvsdg.add_op_alloca(&mut module.ty, dst_region, TY_U32);
        let inline_dst_node_1 = rvsdg.add_op_apply(
            &module,
            dst_region,
            ValueInput::argument(inline_target_ty, 0),
            [ValueInput::output(ty_ptr_u32, inline_dst_node_0, 0)],
            StateOrigin::Argument,
        );

        inline_function(&mut module, &mut rvsdg, inline_dst_node_1);

        // The destination region's state argument should now be connected to an inlined "store"
        // node.
        let StateUser::Node(inlined_node) = *rvsdg[dst_region].state_argument() else {
            panic!("expected the user of the state argument to be a node");
        };

        rvsdg[inlined_node].expect_op_store();

        // The destination region's state result should now be connected to the inlined node.
        assert_eq!(
            *rvsdg[dst_region].state_result(),
            StateOrigin::Node(inlined_node)
        );
    }
}
