use std::collections::VecDeque;
use std::ops::Range;

use rustc_hash::FxHashSet;

use crate::rvsdg::{
    Connectivity, LoopNode, Node, NodeKind, OpAlloca, OpLoad, Region, Rvsdg, SimpleNode,
    StateOrigin, SwitchNode, ValueInput, ValueOrigin, ValueOutput, ValueUser,
};
use crate::ty::{Type, TypeKind, TY_U32};
use crate::{Function, Module};

enum ElementIndex {
    Static(u32),
    Dynamic(ValueOrigin),
}

impl ElementIndex {
    fn from_origin(rvsdg: &Rvsdg, origin: ValueOrigin) -> Self {
        match origin {
            ValueOrigin::Argument(_) => ElementIndex::Dynamic(origin),
            ValueOrigin::Output { producer, .. } => {
                if let NodeKind::Simple(SimpleNode::ConstU32(n)) = rvsdg[producer].kind() {
                    ElementIndex::Static(n.value())
                } else {
                    ElementIndex::Dynamic(origin)
                }
            }
        }
    }
}

/// Collects all [OpAlloca] nodes of aggregate types in a region and all sub-regions (e.g. a switch
/// node branch region) into a queue of candidates for scalar replacement.
///
/// Note that this does not yet make any decisions about whether we should perform a scalar
/// replacement transform on a given [OpAlloca] node, this requires further analysis.
struct CandidateAllocaCollector<'a, 'b> {
    rvsdg: &'a Rvsdg,
    candidate_queue: &'b mut VecDeque<Node>,
}

impl<'a, 'b> CandidateAllocaCollector<'a, 'b> {
    fn new(rvsdg: &'a Rvsdg, candidate_queue: &'b mut VecDeque<Node>) -> Self {
        CandidateAllocaCollector {
            rvsdg,
            candidate_queue,
        }
    }

    fn visit_region(&mut self, region: Region) {
        for node in self.rvsdg[region].nodes() {
            self.visit_node(node);
        }
    }

    fn visit_node(&mut self, node: Node) {
        match self.rvsdg[node].kind() {
            NodeKind::Simple(SimpleNode::OpAlloca(_)) => self.candidate_queue.push_back(node),
            NodeKind::Switch(n) => {
                for branch in n.branches() {
                    self.visit_region(*branch)
                }
            }
            NodeKind::Loop(n) => self.visit_region(*n.loop_region()),
            _ => {}
        }
    }
}

/// Analyzes whether an [OpAlloca] of an aggregate should be replaced with multiple [OpAlloca]s, one
/// for each of the aggregate's elements.
///
/// We generally want to replace as many [OpAlloca]s of aggregates as possible: scalar values are
/// much easier to reason about, and as such most other transforms (including legalizing
/// transforms), only operate on scalar values. Our primary compilation target (WGSL) also does not
/// permit pointer type elements in aggregate; in these cases, scalar replacement is itself a
/// legalizing transform. As such, scalar replacement is treated as the default outcome, and this
/// analysis is instead focussed on identifying cases in which we should not or cannot perform a
/// scalar replacement.
struct Analyzer {
    visited: FxHashSet<Node>,
}

impl Analyzer {
    fn should_replace(&mut self, rvsdg: &Rvsdg, node: Node, function_body_region: Region) -> bool {
        let mut non_local_use_analyzer = NonlocalUseAnalyzer {
            rvsdg,
            visited: &mut self.visited,
            node,
            function_body_region,
        };

        !non_local_use_analyzer.has_nonlocal_use()
    }
}

/// Analyzes whether an [OpAlloca] escapes its local function context.
///
/// An [OpAlloca] of aggregate (a struct or array) escapes if the output pointer, or a load of the
/// output pointer, is passed whole as a call argument input to an [OpApply] node, or is returned as
/// a result from the local function region.
///
/// For the [OpAlloca] to be found to escape, it must the whole pointer to the full aggregate, or
/// the whole unsplit loaded value, that is passed to an [OpApply] node or returned  as a result.
/// Passing or returning sub-elements of the aggregate, obtained via an [OpPtrElementPtr] or an
/// [OpExtractElement], does not constitute an escape, as in these cases scalar replacement will
/// only require local modifications (the [OpPtrElementPtr] or [OpExtractElement] can be adjusted
/// such that any [OpApply] user or result user can remain unchanged).
struct NonlocalUseAnalyzer<'a, 'b> {
    rvsdg: &'a Rvsdg,
    visited: &'b mut FxHashSet<Node>,
    node: Node,
    function_body_region: Region,
}

impl<'a, 'b> NonlocalUseAnalyzer<'a, 'b> {
    fn reset(&mut self) {
        self.visited.clear();
    }

    fn has_nonlocal_use(&mut self) -> bool {
        self.reset();

        let node_data = &self.rvsdg[self.node];
        let region = node_data.region();
        let node_data = node_data.expect_op_alloca();

        self.check_value_output(region, node_data.value_output())
    }

    fn check_value_output(&mut self, region: Region, output: &ValueOutput) -> bool {
        for user in output.users.iter().copied() {
            if self.check_value_user(region, user) {
                return true;
            }
        }

        false
    }

    fn check_value_user(&mut self, region: Region, user: ValueUser) -> bool {
        match user {
            ValueUser::Result(_) => region == self.function_body_region,
            ValueUser::Input { consumer, input } => self.check_node_input(region, consumer, input),
        }
    }

    fn check_node_input(&mut self, region: Region, node: Node, input: u32) -> bool {
        if self.rvsdg[node].is_op_apply() {
            return true;
        }

        if self.visited.insert(node) {
            match self.rvsdg[node].kind() {
                NodeKind::Switch(n) => {
                    for branch in n.branches() {
                        if self.check_region_argument(*branch, input - 1) {
                            return true;
                        }
                    }
                }
                NodeKind::Loop(n) => {
                    if self.check_region_argument(*n.loop_region(), input) {
                        return true;
                    }
                }
                NodeKind::Simple(SimpleNode::OpLoad(op)) => {
                    if self.check_value_output(region, op.value_output()) {
                        return true;
                    }
                }
                _ => {}
            }
        }

        false
    }

    fn check_region_argument(&mut self, region: Region, argument: u32) -> bool {
        for user in self.rvsdg[region].value_arguments()[argument as usize]
            .users
            .iter()
            .copied()
        {
            if self.check_value_user(region, user) {
                return true;
            }
        }

        false
    }
}

struct AllocaReplacer<'a, 'b> {
    module: &'a mut Module,
    rvsdg: &'b mut Rvsdg,
}

impl<'a, 'b> AllocaReplacer<'a, 'b> {
    fn replace_alloca(&mut self, node: Node) {
        let node_data = &self.rvsdg[node];
        let region = node_data.region();
        let node_data = node_data.expect_op_alloca();
        let ty = node_data.ty();

        let mut scalar_replacements = Vec::new();

        match self.module.ty[ty] {
            TypeKind::Array { base, count, .. } => {
                let element_ptr_ty = self.module.ty.register(TypeKind::Ptr(base));

                for _ in 0..count {
                    let scalar_node = self.rvsdg.add_op_alloca(&mut self.module.ty, region, base);

                    scalar_replacements.push(ValueInput {
                        ty: element_ptr_ty,
                        origin: ValueOrigin::Output {
                            producer: scalar_node,
                            output: 0,
                        },
                    });
                }
            }
            TypeKind::Struct(struct_handle) => {
                let field_count = self.module.structs[struct_handle].fields.len();

                for i in 0..field_count {
                    let field_ty = self.module.structs[struct_handle].fields[i].ty;
                    let field_ptr_ty = self.module.ty.register(TypeKind::Ptr(field_ty));
                    let scalar_node =
                        self.rvsdg
                            .add_op_alloca(&mut self.module.ty, region, field_ty);

                    scalar_replacements.push(ValueInput {
                        ty: field_ptr_ty,
                        origin: ValueOrigin::Output {
                            producer: scalar_node,
                            output: 0,
                        },
                    });
                }
            }
            _ => unreachable!("type is not an aggregate, node should not have been a candidate"),
        }

        self.visit_users(node, 0, &scalar_replacements);
    }

    fn visit_users(&mut self, node: Node, output: u32, split_inputs: &[ValueInput]) {
        let region = self.rvsdg[node].region();
        let user_count = self.rvsdg[node].value_outputs()[output as usize]
            .users
            .len();

        for i in 0..user_count {
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
            Simple(OpLoad(_)) => self.split_op_load(node, split_input),
            Simple(OpStore(_)) => self.split_op_store(node, input, split_input),
            Simple(OpPtrElementPtr(_)) => self.visit_op_ptr_element_ptr(node, split_input),
            Simple(OpExtractElement(_)) => self.visit_op_extract_element(node, split_input),
            _ => unreachable!("node king cannot take a pointer or aggregate value as input"),
        }
    }

    fn visit_op_ptr_element_ptr(&mut self, node: Node, split_input: &[ValueInput]) {
        let node_data = &self.rvsdg[node];
        let region = node_data.region();
        let node_data = node_data.expect_op_ptr_element_ptr();
        let output_ty = node_data.output().ty;
        let user_count = node_data.output().users.len();

        let first_index = ElementIndex::from_origin(self.rvsdg, node_data.indices()[0].origin);

        let new_user_origin = match first_index {
            ElementIndex::Static(index) => {
                let split_input = split_input[index as usize];

                self.adapt_op_ptr_element_ptr(node, split_input, region)
            }
            ElementIndex::Dynamic(selector) => {
                // The element index is not statically known. We'll have to dynamically select an
                // input at runtime with a switch node.

                let mut switch_inputs = Vec::with_capacity(split_input.len() + 1);

                switch_inputs.push(ValueInput {
                    ty: TY_U32,
                    origin: selector,
                });
                switch_inputs.extend(split_input.iter().copied());

                let switch = self.rvsdg.add_switch(
                    region,
                    switch_inputs,
                    vec![ValueOutput::new(output_ty)],
                    None,
                );

                for (i, input) in split_input.iter().enumerate() {
                    let branch = self.rvsdg.add_switch_branch(switch);
                    let origin = self.adapt_op_ptr_element_ptr(
                        node,
                        ValueInput {
                            ty: input.ty,
                            origin: ValueOrigin::Argument(i as u32),
                        },
                        region,
                    );

                    self.rvsdg.reconnect_region_result(branch, 0, origin);
                }

                ValueOrigin::Output {
                    producer: switch,
                    output: 0,
                }
            }
        };

        for i in (0..user_count).rev() {
            let user = self.rvsdg[node].expect_op_ptr_element_ptr().output().users[i];

            self.rvsdg
                .reconnect_value_user(region, user, new_user_origin);
        }

        // We've reconnected all the node's users now. Consequently, it's now dead and can
        // be removed.
        self.rvsdg.remove_node(node);
    }

    fn adapt_op_ptr_element_ptr(
        &mut self,
        original: Node,
        input: ValueInput,
        target_region: Region,
    ) -> ValueOrigin {
        let node_data = self.rvsdg[original].expect_op_ptr_element_ptr();
        let is_multi_layer_access = node_data.indices().len() > 1;

        if is_multi_layer_access {
            let element_ty = node_data.element_ty();
            let indices = node_data
                .indices()
                .iter()
                .copied()
                .skip(1)
                .collect::<Vec<_>>();

            let node = self.rvsdg.add_op_ptr_element_ptr(
                &mut self.module.ty,
                target_region,
                element_ty,
                input,
                indices,
            );

            ValueOrigin::Output {
                producer: node,
                output: 0,
            }
        } else {
            input.origin
        }
    }

    fn visit_op_extract_element(&mut self, node: Node, split_input: &[ValueInput]) {
        let node_data = &self.rvsdg[node];
        let region = node_data.region();
        let node_data = node_data.expect_op_extract_element();
        let output_ty = node_data.output().ty;
        let user_count = node_data.output().users.len();

        let first_index = ElementIndex::from_origin(self.rvsdg, node_data.indices()[0].origin);

        let new_user_origin = match first_index {
            ElementIndex::Static(index) => {
                let split_input = split_input[index as usize];

                self.adapt_op_extract_element(node, split_input, region)
            }
            ElementIndex::Dynamic(selector) => {
                // The element index is not statically known. We'll have to dynamically select an
                // input at runtime with a switch node.

                let mut switch_inputs = Vec::with_capacity(split_input.len() + 1);

                switch_inputs.push(ValueInput {
                    ty: TY_U32,
                    origin: selector,
                });
                switch_inputs.extend(split_input.iter().copied());

                let switch = self.rvsdg.add_switch(
                    region,
                    switch_inputs,
                    vec![ValueOutput::new(output_ty)],
                    None,
                );

                for (i, input) in split_input.iter().enumerate() {
                    let branch = self.rvsdg.add_switch_branch(switch);
                    let origin = self.adapt_op_extract_element(
                        node,
                        ValueInput {
                            ty: input.ty,
                            origin: ValueOrigin::Argument(i as u32),
                        },
                        region,
                    );

                    self.rvsdg.reconnect_region_result(branch, 0, origin);
                }

                ValueOrigin::Output {
                    producer: switch,
                    output: 0,
                }
            }
        };

        for i in (0..user_count).rev() {
            let user = self.rvsdg[node].expect_op_extract_element().output().users[i];

            self.rvsdg
                .reconnect_value_user(region, user, new_user_origin);
        }

        // We've reconnected all the node's users now. Consequently, it's now dead and can
        // be removed.
        self.rvsdg.remove_node(node);
    }

    fn adapt_op_extract_element(
        &mut self,
        original: Node,
        input: ValueInput,
        target_region: Region,
    ) -> ValueOrigin {
        let node_data = self.rvsdg[original].expect_op_extract_element();
        let is_multi_layer_access = node_data.indices().len() > 1;

        if is_multi_layer_access {
            let element_ty = node_data.element_ty();
            let indices = node_data
                .indices()
                .iter()
                .copied()
                .skip(1)
                .collect::<Vec<_>>();

            let node = self
                .rvsdg
                .add_op_extract_element(target_region, element_ty, input, indices);

            ValueOrigin::Output {
                producer: node,
                output: 0,
            }
        } else {
            input.origin
        }
    }

    fn split_op_load(&mut self, node: Node, split_input: &[ValueInput]) {
        let region = self.rvsdg[node].region();
        let state_origin = self.rvsdg[node]
            .state()
            .expect("load operation should part of state chain")
            .origin;

        // The OpLoad nodes we produce will have to be linked into the state chain. Though the order
        // in which this happens does not semantically to the program, for inspection of the RVSDG
        // it is more intuitive when this matches the input order. Because of the way successively
        // inserting a node with the same state origin behaves, we reverse the iteration order to
        // achieve this.
        let mut split_input = split_input
            .iter()
            .rev()
            .map(|input| {
                let TypeKind::Ptr(output_ty) = self.module.ty[input.ty] else {
                    panic!("expected input to load operation to be a pointer");
                };

                let split_node = self
                    .rvsdg
                    .add_op_load(region, *input, output_ty, state_origin);

                ValueInput {
                    ty: output_ty,
                    origin: ValueOrigin::Output {
                        producer: split_node,
                        output: 0,
                    },
                }
            })
            .collect::<Vec<_>>();

        // We reversed the iteration order when we produced this list. Though again the order of
        // these does not matter semantically to the program, for inspection of the RVSDG it is more
        // intuitive to maintain a consistent order, so we unreverse the order here.
        split_input.reverse();

        self.visit_users(node, 0, &split_input);

        // After visiting the users of the original node's output, all users should have been
        // reconnected to the split nodes and the original node should have no users left; we should
        // be able to remove the node now.
        self.rvsdg.remove_node(node);
    }

    fn split_op_store(&mut self, node: Node, input: u32, split_input: &[ValueInput]) {
        // OpStore presents a more complex case than most of the other cases. This is due to OpStore
        // having two inputs, both of which has to split at the same time (as opposed to e.g. a
        // Switch input, where we can split inputs individually). For whichever input is visited
        // first, we generate final connectivity immediately; for the other, we introduce an
        // intermediate set of [OpElementPtrElement] nodes (for case of the "pointer" input) or an
        // intermediate set of [OpExtractElement] nodes (for the case of the "value" input).
        //
        // However, it is possible that both inputs originate from a common output somewhere up
        // the DAG. Adding [OpElementPtrElement]/[OpExtractElement] nodes to upstream user sets
        // runs the risk of modifying a user set concurrently with its iteration. To sidestep this
        // problem, instead of directly connecting the [OpElementPtrElement]/[OpExtractElement]
        // nodes to the value origin, we first introduce a [ValueProxy] node with
        // [Rvsdg::proxy_origin_user] and then connect the [OpElementPtrElement]/[OpExtractElement]
        // to the output of this [ValueProxy] node instead.
        //
        // The [ValueProxy] and [OpElementPtrElement]/[OpExtractElement] nodes introduced by this
        // strategy are in most cases temporary. When later the DAG path that would have arrived at
        // the other input is traversed, the [ValueProxy] and [OpElementPtrElement]/
        // [OpExtractElement] nodes will typically be eliminated. For cases where the input does
        // not originate from an OpAlloca, [ValueProxy] nodes will have to be cleaned up by a later
        // pass (in these cases the [OpElementPtrElement]/[OpExtractElement] nodes typically remain
        // necessary).

        assert!(input < 2, "OpStore only has 2 value inputs");

        let node_data = &self.rvsdg[node];
        let region = node_data.region();
        let state_origin = node_data
            .state()
            .expect("store op should have state")
            .origin;
        let node_data = node_data.expect_op_store();
        let mut ptr_input = *node_data.ptr_input();
        let mut value_input = *node_data.value_input();

        if input == 0 {
            // If the provoking input is the pointer input, then proxy the value input
            let proxy = self.rvsdg.proxy_origin_user(
                region,
                value_input.ty,
                value_input.origin,
                ValueUser::Input {
                    consumer: node,
                    input: 1,
                },
            );

            value_input.origin = ValueOrigin::Output {
                producer: proxy,
                output: 0,
            };
        }

        if input == 1 {
            // If the provoking input is the value input, then proxy the pointer input
            let proxy = self.rvsdg.proxy_origin_user(
                region,
                ptr_input.ty,
                ptr_input.origin,
                ValueUser::Input {
                    consumer: node,
                    input: 0,
                },
            );

            ptr_input.origin = ValueOrigin::Output {
                producer: proxy,
                output: 0,
            };
        }

        match self.module.ty[value_input.ty] {
            TypeKind::Array { base, count, .. } => {
                // We iterate the element indices in reverse due to the way we link the load and
                // store nodes we create into the state chain: we repeatedly reuse the state_origin
                // from the original unsplit store node. This means that adding the lowest index
                // last, will make it end up the earliest in the state chain. Though the actual
                // order should not matter for the validity of the program, lowest-to-highest index
                // order is the more natural order for human review of the compiler's output.
                for i in (0..count).rev() {
                    self.add_store_element_nodes(
                        region,
                        input,
                        i as u32,
                        base,
                        ptr_input,
                        value_input,
                        state_origin,
                        split_input,
                    );
                }
            }
            TypeKind::Struct(struct_handle) => {
                let field_count = self.module.structs[struct_handle].fields.len();

                // We iterate in reverse for the same reason as for the array case, see the comment
                // above.
                for i in (0..field_count).rev() {
                    let element_ty = self.module.structs[struct_handle].fields[i].ty;

                    self.add_store_element_nodes(
                        region,
                        input,
                        i as u32,
                        element_ty,
                        ptr_input,
                        value_input,
                        state_origin,
                        split_input,
                    );
                }
            }
            _ => unreachable!("type is not an aggregate"),
        }

        self.rvsdg.remove_node(node);
    }

    fn add_store_element_nodes(
        &mut self,
        region: Region,
        provoking_input: u32,
        element_index: u32,
        element_ty: Type,
        ptr_input: ValueInput,
        value_input: ValueInput,
        state_origin: StateOrigin,
        split_input: &[ValueInput],
    ) {
        let ptr_ty = self.module.ty.register(TypeKind::Ptr(element_ty));
        let index_input = self.rvsdg.add_const_u32(region, element_index);

        let element_ptr_input = if provoking_input == 0 {
            split_input[element_index as usize]
        } else {
            let element_ptr = self.rvsdg.add_op_ptr_element_ptr(
                &mut self.module.ty,
                region,
                element_ty,
                ptr_input,
                [ValueInput::output(TY_U32, index_input, 0)],
            );

            ValueInput::output(ptr_ty, element_ptr, 0)
        };

        let element_value_input = if provoking_input == 1 {
            split_input[element_index as usize]
        } else {
            let element_value = self.rvsdg.add_op_extract_element(
                region,
                element_ty,
                value_input,
                [ValueInput::output(TY_U32, index_input, 0)],
            );

            ValueInput::output(element_ty, element_value, 0)
        };

        self.rvsdg
            .add_op_store(region, element_ptr_input, element_value_input, state_origin);
    }

    fn split_switch_input(&mut self, node: Node, input: u32, split_input: &[ValueInput]) {
        assert_ne!(input, 0, "the branch selector input is never an aggregate");

        let arg_index = input as usize - 1;
        let node_data = self.rvsdg[node].expect_switch();
        let branch_count = node_data.branches().len();
        let split_args_base = node_data.value_inputs().len() - 1;

        let split_args = split_input
            .iter()
            .enumerate()
            .map(|(i, input)| {
                self.rvsdg.add_switch_input(node, *input);

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

        // First split the output/results in connect the new results for the provoking branch. Also
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
        let loop_region = *node_data.loop_region();
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
        // original result connects to, via [OpElementPtrElement] nodes or [OpExtractElement] nodes,
        // depending on whether the result type is a pointer or an immediate value. This also
        // disconnects the original result.
        self.redirect_region_result(loop_region, input as usize + 1, prior_result_count);

        // Now redirect the argument using the argument mapping. We do this after redirecting the
        // results, because otherwise this might try to split the original result again; by doing
        // this after result redirection, all the argument's user tree should terminate at the
        // [OpElementPtrElement]/[OpExtractElement] nodes that were inserted, since nothing should
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
            "the reentry decider result is never an aggregate"
        );

        let owner = self.rvsdg[region].owner();
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
            region,
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

        // Add inputs for each element of the aggregate, and connect them to the proxy via either
        // an OpElementPtrElement node if the input is of a pointer type, or via an OpExtractElement
        // node otherwise. Also record an argument mapping in `split_args` and an output mapping in
        // `split_outputs`.
        match self.module.ty[value_input.ty] {
            TypeKind::Ptr(pointee_ty) => match self.module.ty[pointee_ty] {
                TypeKind::Array { base, count, .. } => {
                    for i in 0..count {
                        let ptr_ty = self.module.ty.register(TypeKind::Ptr(base));
                        let index = self.rvsdg.add_const_u32(region, i as u32);
                        let element = self.rvsdg.add_op_ptr_element_ptr(
                            &mut self.module.ty,
                            region,
                            base,
                            proxy_input,
                            [ValueInput::output(TY_U32, index, 0)],
                        );

                        self.rvsdg
                            .add_loop_input(owner, ValueInput::output(ptr_ty, element, 0));

                        split_args.push(ValueInput::argument(
                            ptr_ty,
                            prior_input_count as u32 + i as u32,
                        ));
                        split_outputs.push(ValueInput::output(
                            ptr_ty,
                            owner,
                            prior_input_count as u32 + i as u32,
                        ));
                    }
                }
                TypeKind::Struct(struct_handle) => {
                    let field_count = self.module.structs[struct_handle].fields.len();

                    for i in 0..field_count {
                        let element_ty = self.module.structs[struct_handle].fields[i].ty;
                        let ptr_ty = self.module.ty.register(TypeKind::Ptr(element_ty));

                        let index = self.rvsdg.add_const_u32(region, i as u32);
                        let element = self.rvsdg.add_op_ptr_element_ptr(
                            &mut self.module.ty,
                            region,
                            element_ty,
                            proxy_input,
                            [ValueInput::output(TY_U32, index, 0)],
                        );

                        self.rvsdg
                            .add_loop_input(owner, ValueInput::output(ptr_ty, element, 0));

                        split_args.push(ValueInput::argument(
                            ptr_ty,
                            prior_input_count as u32 + i as u32,
                        ));
                        split_outputs.push(ValueInput::output(
                            ptr_ty,
                            owner,
                            prior_input_count as u32 + i as u32,
                        ));
                    }
                }
                _ => panic!("pointee type is not an aggregate"),
            },
            TypeKind::Array { base, count, .. } => {
                for i in 0..count {
                    let index = self.rvsdg.add_const_u32(region, i as u32);
                    let element = self.rvsdg.add_op_extract_element(
                        region,
                        base,
                        proxy_input,
                        [ValueInput::output(TY_U32, index, 0)],
                    );

                    self.rvsdg
                        .add_loop_input(owner, ValueInput::output(base, element, 0));

                    split_args.push(ValueInput::argument(
                        base,
                        prior_input_count as u32 + i as u32,
                    ));
                    split_outputs.push(ValueInput::output(
                        base,
                        owner,
                        prior_input_count as u32 + i as u32,
                    ));
                }
            }
            TypeKind::Struct(struct_handle) => {
                let field_count = self.module.structs[struct_handle].fields.len();

                for i in 0..field_count {
                    let element_ty = self.module.structs[struct_handle].fields[i].ty;

                    let index = self.rvsdg.add_const_u32(region, i as u32);
                    let element = self.rvsdg.add_op_extract_element(
                        region,
                        element_ty,
                        proxy_input,
                        [ValueInput::output(TY_U32, index, 0)],
                    );

                    self.rvsdg
                        .add_loop_input(owner, ValueInput::output(element_ty, element, 0));

                    split_args.push(ValueInput::argument(
                        element_ty,
                        prior_input_count as u32 + i as u32,
                    ));
                    split_outputs.push(ValueInput::output(
                        element_ty,
                        owner,
                        prior_input_count as u32 + i as u32,
                    ));
                }
            }
            _ => unreachable!("type is not an aggregate"),
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

        for user_index in 0..user_count {
            let user = self.rvsdg[region].value_arguments()[arg_index].users[user_index];

            self.visit_user(region, user, split_input)
        }
    }

    /// Redirects the origin for the `region`'s given `result` to a set of "split" results that
    /// start at `split_results_start`, via either [OpElementPtrElement] or [OpExtractElement] nodes
    /// depending on whether to original input type is a pointer or immediate value.
    ///
    /// Leaves the original result connected to the "placeholder" origin.
    fn redirect_region_result(
        &mut self,
        region: Region,
        original: usize,
        split_results_start: usize,
    ) {
        let original_input = self.rvsdg[region].value_results()[original];

        match self.module.ty[original_input.ty] {
            TypeKind::Ptr(pointee_ty) => match self.module.ty[pointee_ty] {
                TypeKind::Array { base, count, .. } => {
                    for i in 0..count {
                        let index_node = self.rvsdg.add_const_u32(region, i as u32);
                        let split_node = self.rvsdg.add_op_ptr_element_ptr(
                            &mut self.module.ty,
                            region,
                            base,
                            original_input,
                            [ValueInput::output(TY_U32, index_node, 0)],
                        );
                        let result_index = split_results_start + i as usize;

                        self.rvsdg.reconnect_region_result(
                            region,
                            result_index as u32,
                            ValueOrigin::Output {
                                producer: split_node,
                                output: 0,
                            },
                        );
                    }
                }
                TypeKind::Struct(struct_handle) => {
                    let field_count = self.module.structs[struct_handle].fields.len();

                    for i in 0..field_count {
                        let field_ty = self.module.structs[struct_handle].fields[i].ty;
                        let index_node = self.rvsdg.add_const_u32(region, i as u32);
                        let split_node = self.rvsdg.add_op_ptr_element_ptr(
                            &mut self.module.ty,
                            region,
                            field_ty,
                            original_input,
                            [ValueInput::output(TY_U32, index_node, 0)],
                        );
                        let result_index = split_results_start + i as usize;

                        self.rvsdg.reconnect_region_result(
                            region,
                            result_index as u32,
                            ValueOrigin::Output {
                                producer: split_node,
                                output: 0,
                            },
                        );
                    }
                }
                _ => unreachable!("pointee type is not an aggregate"),
            },
            TypeKind::Array { base, count, .. } => {
                for i in 0..count {
                    let index_node = self.rvsdg.add_const_u32(region, i as u32);
                    let split_node = self.rvsdg.add_op_extract_element(
                        region,
                        base,
                        original_input,
                        [ValueInput::output(TY_U32, index_node, 0)],
                    );
                    let result_index = split_results_start + i as usize;

                    self.rvsdg.reconnect_region_result(
                        region,
                        result_index as u32,
                        ValueOrigin::Output {
                            producer: split_node,
                            output: 0,
                        },
                    );
                }
            }
            TypeKind::Struct(struct_handle) => {
                let field_count = self.module.structs[struct_handle].fields.len();

                for i in 0..field_count {
                    let field_ty = self.module.structs[struct_handle].fields[i].ty;
                    let index_node = self.rvsdg.add_const_u32(region, i as u32);
                    let split_node = self.rvsdg.add_op_extract_element(
                        region,
                        field_ty,
                        original_input,
                        [ValueInput::output(TY_U32, index_node, 0)],
                    );
                    let result_index = split_results_start + i;

                    self.rvsdg.reconnect_region_result(
                        region,
                        result_index as u32,
                        ValueOrigin::Output {
                            producer: split_node,
                            output: 0,
                        },
                    );
                }
            }
            _ => unreachable!("type is not an aggregate or a pointer to an aggregate"),
        }

        self.rvsdg.disconnect_region_result(region, original as u32);
    }
}

pub struct ScalarReplacementTransform {
    candidate_queue: VecDeque<Node>,
    analyzer: Analyzer,
}

impl ScalarReplacementTransform {
    pub fn new() -> Self {
        ScalarReplacementTransform {
            candidate_queue: Default::default(),
            analyzer: Analyzer {
                visited: Default::default(),
            },
        }
    }

    pub fn replace_in_fn(&mut self, module: &mut Module, rvsdg: &mut Rvsdg, function: Function) {
        let node = rvsdg
            .get_function_node(function)
            .expect("function should have RVSDG body");
        let body_region = rvsdg[node].expect_function().body_region();

        let mut candidate_collector =
            CandidateAllocaCollector::new(rvsdg, &mut self.candidate_queue);

        candidate_collector.visit_region(body_region);

        while let Some(candidate) = self.candidate_queue.pop_front() {
            if self.analyzer.should_replace(rvsdg, candidate, body_region) {
                let mut replacer = AllocaReplacer { module, rvsdg };

                replacer.replace_alloca(candidate);
            }
        }
    }

    pub fn replace_in_entry_points(&mut self, module: &mut Module, rvsdg: &mut Rvsdg) {
        let entry_points = module
            .entry_points
            .iter()
            .map(|(f, _)| f)
            .collect::<Vec<_>>();

        for entry_point in entry_points {
            self.replace_in_fn(module, rvsdg, entry_point);
        }
    }
}
