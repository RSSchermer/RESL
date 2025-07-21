use std::ops::Index;

use index_vec::IndexVec;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::cfg::{
    BasicBlock, Body, Cfg, InlineConst, LocalValue, OpAlloca, OpAssign, OpBinary,
    OpBoolToBranchPredicate, OpCall, OpCaseToBranchPredicate, OpGetDiscriminant, OpLoad,
    OpPtrElementPtr, OpPtrVariantPtr, OpSetDiscriminant, OpStore, OpUnary, RootIdentifier,
    Statement, Terminator, Value,
};
use crate::cfg_to_rvsdg::control_flow_restructuring::{
    restructure_branches, restructure_loops, Graph,
};
use crate::cfg_to_rvsdg::control_tree::{
    annotate_demand, annotate_item_dependencies, annotate_read_write, annotate_state_use,
    BranchingNode, ControlTree, ControlTreeNode, ControlTreeNodeKind, LinearNode, LoopNode,
    SliceAnnotation,
};
use crate::cfg_to_rvsdg::item_dependencies::{item_dependencies, Item, ItemDependencies};
use crate::rvsdg::{Node, Region, Rvsdg, StateOrigin, ValueInput, ValueOrigin, ValueOutput};
use crate::ty::{Type, TypeKind, TY_BOOL, TY_F32, TY_I32, TY_U32};
use crate::Module;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum InputState {
    Value(LocalValue),
    Item(Item),
}

/// Keeps track of the most recent source of a [Value] or [Item] data dependency in a region's
/// RVSDG during construction.
///
/// When translating CFG construct to RVSDG constructs we need to map the [Value] and [Item]
/// operands of the CFG construct, to region arguments or sibling node outputs that represent the
/// corresponding data origin in the RVSDG. Whenever we begin a new region, we insert such mappings
/// into this tracker for each of the regions arguments. Subsequently, whenever a node is added to
/// the region that outputs a value, we insert another mapping or - if a mapping was already present
/// for the value/item (this can happen; we don't require the CFG be in SSA form) - we update the
/// existing mapping.
///
/// Whenever we add a node that requires inputs, we resolve them from the tracker. Walking the
/// [ControlTree] (that we use to construct the RVSDG) in post-order, should ensure that any input
/// requirements that a node may have, should have already been added to the tracker when visiting
/// prior nodes, thus such lookups should never fail.
#[derive(Clone, Debug)]
struct InputStateTracker<'a> {
    body: &'a Body,
    state: FxHashMap<InputState, ValueInput>,
    current_arg_index: u32,
}

impl<'a> InputStateTracker<'a> {
    fn new(body: &'a Body) -> Self {
        InputStateTracker {
            body,
            state: Default::default(),
            current_arg_index: 0,
        }
    }

    fn insert_value(&mut self, value: LocalValue, input: ValueInput) {
        self.state.insert(InputState::Value(value), input);
    }

    fn insert_value_arg(&mut self, value: LocalValue) {
        let ty = self.body[value]
            .ty
            .expect("region argument should by typed");
        let input = ValueInput {
            ty,
            origin: ValueOrigin::Argument(self.current_arg_index),
        };

        self.state.insert(InputState::Value(value), input);
        self.current_arg_index += 1;
    }

    fn insert_item_arg(&mut self, item: Item, ty: Type) {
        let input = ValueInput {
            ty,
            origin: ValueOrigin::Argument(self.current_arg_index),
        };

        self.state.insert(InputState::Item(item), input);
        self.current_arg_index += 1;
    }

    fn insert_value_node(&mut self, value: LocalValue, producer: Node, output: u32) {
        let ty = self.body[value].ty.expect("value should by typed");
        let input = ValueInput {
            ty,
            origin: ValueOrigin::Output { producer, output },
        };

        self.state.insert(InputState::Value(value), input);
    }
}

impl Index<LocalValue> for InputStateTracker<'_> {
    type Output = ValueInput;

    fn index(&self, value: LocalValue) -> &Self::Output {
        self.state
            .get(&InputState::Value(value))
            .expect("no input found for value")
    }
}

impl Index<Item> for InputStateTracker<'_> {
    type Output = ValueInput;

    fn index(&self, item: Item) -> &Self::Output {
        self.state
            .get(&InputState::Item(item))
            .expect("no input found for item")
    }
}

struct RegionBuilder<'a> {
    region: Region,
    module: &'a mut Module,
    control_tree: &'a ControlTree,
    body: &'a Body,
    item_dependencies: &'a SliceAnnotation<Item>,
    demand: &'a SliceAnnotation<LocalValue>,
    state_use: &'a IndexVec<ControlTreeNode, bool>,
    rvsdg: &'a mut Rvsdg,
    input_state_tracker: InputStateTracker<'a>,
    state_origin: StateOrigin,
}

impl<'a> RegionBuilder<'a> {
    fn visit_node_expect_linear(&mut self, node: ControlTreeNode) {
        let data = self.control_tree[node].expect_linear();

        self.visit_linear_node(data);
    }

    fn visit_linear_node(&mut self, data: &LinearNode) {
        for i in 0..data.children.len() {
            let child = data.children[i];

            match &self.control_tree[child] {
                ControlTreeNodeKind::BasicBlock(bb) => self.visit_basic_block(*bb),
                ControlTreeNodeKind::Linear(child_data) => self.visit_linear_node(child_data),
                ControlTreeNodeKind::Branching(child_data) => {
                    let next_sibling = data.children.get(i + 1).copied();

                    self.visit_branching_node((child, child_data), next_sibling);
                }
                ControlTreeNodeKind::Loop(child_data) => self.visit_loop_node((child, child_data)),
            }
        }
    }

    fn visit_branching_node(
        &mut self,
        (node, data): (ControlTreeNode, &BranchingNode),
        next_sibling: Option<ControlTreeNode>,
    ) {
        let item_deps = self.item_dependencies.get(node);
        let demand = self.demand.get(node);
        let uses_state = self.state_use[node];

        let mut value_inputs = Vec::with_capacity(item_deps.len() + demand.len() + 1);

        value_inputs.push(self.input_state_tracker[data.selector]);

        // We need to construct the input state for the branch regions, based on the inputs to the
        // switch node. Each region builder for a branch's sub-region will start with a copy of this
        // tracker.
        let mut branch_input_state = InputStateTracker::new(self.body);

        for value in demand {
            value_inputs.push(self.input_state_tracker[*value]);
            branch_input_state.insert_value_arg(*value);
        }

        for dep in item_deps {
            value_inputs.push(self.input_state_tracker[*dep]);
            branch_input_state.insert_item_arg(*dep, dep.ty(self.module));
        }

        // The branching node needs to output the values that its next sibling demands.
        //
        // Note that we don't have a node handle for the switch node yet, so we'll have to update
        // the input state tracker later.
        let value_outputs = if let Some(next_sibling) = next_sibling {
            let next_sibling_demand = self.demand.get(next_sibling);

            next_sibling_demand
                .iter()
                .map(|value| {
                    let ty = self.body[*value]
                        .ty
                        .expect("used local values should be typed");

                    ValueOutput::new(ty)
                })
                .collect()
        } else {
            Vec::new()
        };

        let state_origin = uses_state.then(|| self.state_origin);

        // Add the switch node itself
        let node = self
            .rvsdg
            .add_switch(self.region, value_inputs, value_outputs, state_origin);

        // Add each of the branches
        for branch in &data.branches {
            let region = self.rvsdg.add_switch_branch(node);
            let mut branch_builder = self.subregion_builder(region, branch_input_state.clone());

            branch_builder.visit_node_expect_linear(*branch);

            // If the switch node has output demand, then connect the results of the branch region
            if let Some(next_sibling) = next_sibling {
                let next_sibling_demand = branch_builder.demand.get(next_sibling);

                for (i, value) in next_sibling_demand.iter().enumerate() {
                    branch_builder.connect_result(i as u32, value.into());
                }
            }
        }

        // Now that we have a node handle, update the input state tracker with the switch node's
        // outputs
        if let Some(next_sibling) = next_sibling {
            let next_sibling_demand = self.demand.get(next_sibling);

            for (i, value) in next_sibling_demand.iter().enumerate() {
                self.input_state_tracker
                    .insert_value_node(*value, node, i as u32);
            }
        }

        // Keep track of the state tail
        if uses_state {
            self.state_origin = StateOrigin::Node(node);
        }
    }

    fn visit_loop_node(&mut self, (node, data): (ControlTreeNode, &LoopNode)) {
        let item_deps = self.item_dependencies.get(node);
        let demand = self.demand.get(node);
        let uses_state = self.state_use[node];

        let mut value_inputs = Vec::with_capacity(item_deps.len() + demand.len());
        let mut inner_input_state = InputStateTracker::new(self.body);

        for value in demand {
            value_inputs.push(self.input_state_tracker[*value]);
            inner_input_state.insert_value_arg(*value);
        }

        for dep in item_deps {
            value_inputs.push(self.input_state_tracker[*dep]);
            inner_input_state.insert_item_arg(*dep, dep.ty(self.module));
        }

        let state_origin = uses_state.then(|| self.state_origin);

        let (node, region) = self.rvsdg.add_loop(self.region, value_inputs, state_origin);

        let mut inner_builder = self.subregion_builder(region, inner_input_state);

        inner_builder.visit_node_expect_linear(data.inner);

        // Connect the re-entry predicate result
        inner_builder.connect_result(0, data.reentry_predicate.into());

        // Connect the other results based on the demand.
        for (i, value) in demand.iter().enumerate() {
            // The first result is the re-entry predicate, the demand-based results follow, so shift
            // the index by 1.
            let result_index = i as u32 + 1;

            inner_builder.connect_result(result_index, value.into());
        }

        // Now that we have a node handle, update the input state tracker with the switch node's
        // outputs
        for (i, value) in demand.iter().enumerate() {
            self.input_state_tracker
                .insert_value_node(*value, node, i as u32);
        }

        // Keep track of the state tail
        if uses_state {
            self.state_origin = StateOrigin::Node(node);
        }
    }

    fn visit_basic_block(&mut self, bb: BasicBlock) {
        let data = &self.body.basic_blocks[bb];

        for statement in &data.statements {
            self.visit_statement(statement)
        }

        if let Terminator::Return(Some(value)) = &data.terminator {
            // Restructuring should have left only a single return terminator (if any), and it
            // should belong to the last child of the control tree's root linear node, so we know we
            // should currently be in a function's top-level region. We can therefor simply connect
            // the return value to result `0` of the current region.
            self.connect_result(0, *value);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::OpAlloca(op) => self.visit_op_alloca(op),
            Statement::OpAssign(op) => self.visit_op_assign(op),
            Statement::OpLoad(op) => self.visit_op_load(op),
            Statement::OpStore(op) => self.visit_op_store(op),
            Statement::OpPtrElementPtr(op) => self.visit_op_ptr_element_ptr(op),
            Statement::OpPtrVariantPtr(op) => self.visit_op_ptr_variant_ptr(op),
            Statement::OpGetDiscriminant(op) => self.visit_op_get_discriminant(op),
            Statement::OpSetDiscriminant(op) => self.visit_op_set_discriminant(op),
            Statement::OpUnary(op) => self.visit_op_unary(op),
            Statement::OpBinary(op) => self.visit_op_binary(op),
            Statement::OpCall(op) => self.visit_op_call(op),
            Statement::OpCaseToBranchPredicate(op) => self.visit_op_case_to_branch_predicate(op),
            Statement::OpBoolToBranchPredicate(op) => self.visit_op_bool_to_branch_predicate(op),
        }
    }

    fn visit_op_alloca(&mut self, op: &OpAlloca) {
        let node = self.rvsdg.add_op_alloca(self.region, op.ty);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
    }

    fn visit_op_assign(&mut self, op: &OpAssign) {
        // Assignment operations are not represented in the RVSDG, they are implicit in the data
        // flow. We instead redirect the tracker to the origin of the data that is being assigned.

        let input = self.resolve_value(op.value);

        self.input_state_tracker.insert_value(op.result, input);
    }

    fn visit_op_load(&mut self, op: &OpLoad) {
        let ptr_input = self.resolve_value(op.ptr);
        let output_ty = self.body[op.result]
            .ty
            .expect("load result should be typed");
        let node = self
            .rvsdg
            .add_op_load(self.region, ptr_input, output_ty, self.state_origin);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
        self.state_origin = StateOrigin::Node(node);
    }

    fn visit_op_store(&mut self, op: &OpStore) {
        let ptr_input = self.resolve_value(op.ptr);
        let value_input = self.resolve_value(op.value);
        let node = self
            .rvsdg
            .add_op_store(self.region, ptr_input, value_input, self.state_origin);

        self.state_origin = StateOrigin::Node(node);
    }

    fn visit_op_ptr_element_ptr(&mut self, op: &OpPtrElementPtr) {
        let ptr_input = self.resolve_value(op.ptr);
        let index_inputs = op
            .indices
            .iter()
            .copied()
            .map(|v| self.resolve_value(v))
            .collect::<Vec<_>>();
        let node =
            self.rvsdg
                .add_op_ptr_element_ptr(self.region, op.element_ty, ptr_input, index_inputs);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
    }

    fn visit_op_ptr_variant_ptr(&mut self, op: &OpPtrVariantPtr) {
        let input = self.resolve_value(op.ptr);

        let node = self
            .rvsdg
            .add_op_ptr_variant_ptr(self.region, input, op.variant_index);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
    }

    fn visit_op_get_discriminant(&mut self, op: &OpGetDiscriminant) {
        let ptr_input = self.resolve_value(op.ptr);

        let node = self
            .rvsdg
            .add_op_get_discriminant(self.region, ptr_input, self.state_origin);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
        self.state_origin = StateOrigin::Node(node);
    }

    fn visit_op_set_discriminant(&mut self, op: &OpSetDiscriminant) {
        let ptr_input = self.resolve_value(op.ptr);
        let node = self.rvsdg.add_op_set_discriminant(
            self.region,
            ptr_input,
            op.variant_index,
            self.state_origin,
        );

        self.state_origin = StateOrigin::Node(node);
    }

    fn visit_op_unary(&mut self, op: &OpUnary) {
        let input = self.resolve_value(op.value);
        let node = self.rvsdg.add_op_unary(self.region, op.operator, input);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
    }

    fn visit_op_binary(&mut self, op: &OpBinary) {
        let lhs_input = self.resolve_value(op.lhs);
        let rhs_input = self.resolve_value(op.rhs);
        let node = self
            .rvsdg
            .add_op_binary(self.region, op.operator, lhs_input, rhs_input);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
    }

    fn visit_op_call(&mut self, op: &OpCall) {
        let fn_input = self.input_state_tracker[Item::Function(op.function)];
        let arg_inputs = op
            .args
            .iter()
            .copied()
            .map(|v| self.resolve_value(v))
            .collect::<Vec<_>>();
        let node = self.rvsdg.add_op_apply(
            self.module,
            self.region,
            fn_input,
            arg_inputs,
            self.state_origin,
        );

        if let Some(result) = op.result {
            self.input_state_tracker.insert_value_node(result, node, 0);
        }

        self.state_origin = StateOrigin::Node(node);
    }

    fn visit_op_case_to_branch_predicate(&mut self, op: &OpCaseToBranchPredicate) {
        let input = self.resolve_value(op.value);
        let cases = op.cases.clone();
        let node = self
            .rvsdg
            .add_op_case_to_switch_predicate(self.region, input, cases);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
    }

    fn visit_op_bool_to_branch_predicate(&mut self, op: &OpBoolToBranchPredicate) {
        let input = self.resolve_value(op.value);
        let node = self
            .rvsdg
            .add_op_bool_to_switch_predicate(self.region, input);

        self.input_state_tracker
            .insert_value_node(op.result, node, 0);
    }

    fn connect_result(&mut self, result: u32, value: Value) {
        let input = self.resolve_value(value);

        self.rvsdg
            .reconnect_region_result(self.region, result, input.origin);
    }

    fn resolve_value(&mut self, value: Value) -> ValueInput {
        match value {
            Value::Local(v) => self.input_state_tracker[v],
            Value::InlineConst(c) => self.resolve_inline_const(c),
            Value::Const => todo!(),
        }
    }

    fn resolve_inline_const(&mut self, c: InlineConst) -> ValueInput {
        let (ty, producer) = match c {
            InlineConst::U32(v) => (TY_U32, self.rvsdg.add_const_u32(self.region, v)),
            InlineConst::I32(v) => (TY_I32, self.rvsdg.add_const_i32(self.region, v)),
            InlineConst::F32(v) => (
                TY_F32,
                self.rvsdg.add_const_f32(self.region, v.into_inner()),
            ),
            InlineConst::Bool(v) => (TY_BOOL, self.rvsdg.add_const_bool(self.region, v)),
            InlineConst::Ptr(ptr) => {
                let base = self.resolve_root_identifier(ptr.base);
                let node = self.rvsdg.add_const_ptr(self.region, ptr.pointee_ty, base);

                (self.module.ty.register(TypeKind::Ptr(ptr.pointee_ty)), node)
            }
        };

        ValueInput {
            ty,
            origin: ValueOrigin::Output {
                producer,
                output: 0,
            },
        }
    }

    fn resolve_root_identifier(&self, root_identifier: RootIdentifier) -> ValueInput {
        match root_identifier {
            RootIdentifier::Local(v) => self.input_state_tracker[v],
            RootIdentifier::Uniform(b) => self.input_state_tracker[Item::UniformBinding(b)],
            RootIdentifier::Storage(b) => self.input_state_tracker[Item::StorageBinding(b)],
            RootIdentifier::Workgroup(b) => self.input_state_tracker[Item::WorkgroupBinding(b)],
        }
    }

    fn subregion_builder(
        &mut self,
        region: Region,
        input_state_tracker: InputStateTracker<'a>,
    ) -> RegionBuilder {
        RegionBuilder {
            region,
            module: self.module,
            control_tree: self.control_tree,
            body: self.body,
            item_dependencies: self.item_dependencies,
            demand: self.demand,
            state_use: self.state_use,
            rvsdg: &mut self.rvsdg,
            input_state_tracker,
            state_origin: StateOrigin::Argument,
        }
    }
}

fn build_body(
    into: Region,
    from: &Body,
    module: &mut Module,
    rvsdg: &mut Rvsdg,
    input_state_tracker: InputStateTracker,
) {
    let mut graph = Graph::init(from.clone());

    let reentry_edges = restructure_loops(&mut graph);
    let branch_info = restructure_branches(&mut graph, &reentry_edges);

    let control_tree = ControlTree::generate(&graph, &reentry_edges, &branch_info);
    let body = graph.into_inner();

    let item_dependencies = annotate_item_dependencies(&control_tree, &body);
    let (read, write) = annotate_read_write(&control_tree, &body);
    let demand = annotate_demand(&control_tree, &read, &write);
    let state_use = annotate_state_use(&control_tree, &body);

    let mut region_builder = RegionBuilder {
        region: into,
        module,
        control_tree: &control_tree,
        body: &body,
        item_dependencies: &item_dependencies,
        demand: &demand,
        state_use: &state_use,
        rvsdg,
        input_state_tracker,
        state_origin: StateOrigin::Argument,
    };

    region_builder.visit_node_expect_linear(control_tree.root());
}

fn add_item(
    item: Item,
    module: &mut Module,
    cfg: &Cfg,
    item_dependencies: &ItemDependencies,
    rvsdg: &mut Rvsdg,
    visited: &mut FxHashSet<Item>,
    item_node: &mut FxHashMap<Item, Node>,
) {
    if visited.insert(item) {
        let node = match item {
            Item::UniformBinding(binding) => rvsdg.register_uniform_binding(module, binding),
            Item::StorageBinding(binding) => rvsdg.register_storage_binding(module, binding),
            Item::WorkgroupBinding(binding) => rvsdg.register_workgroup_binding(module, binding),
            Item::Function(function) => {
                let body = &cfg.function_body[function];

                let mut input_state_tracker = InputStateTracker::new(body);

                let (node, region) = if let Some(deps) = item_dependencies.get(&item) {
                    for dep in deps {
                        add_item(
                            *dep,
                            module,
                            cfg,
                            item_dependencies,
                            rvsdg,
                            visited,
                            item_node,
                        );

                        input_state_tracker.insert_item_arg(*dep, dep.ty(module));
                    }

                    let deps = deps.iter().map(|dep| item_node.get(dep).unwrap()).copied();

                    rvsdg.register_function(module, function, deps)
                } else {
                    rvsdg.register_function(module, function, [])
                };

                for param in &body.params {
                    input_state_tracker.insert_value_arg(*param);
                }

                build_body(region, body, module, rvsdg, input_state_tracker);

                node
            }
        };

        item_node.insert(item, node);
    }
}

pub fn cfg_to_rvsdg(module: &mut Module, cfg: &Cfg) -> Rvsdg {
    let mut rvsdg = Rvsdg::new(module.ty.clone());
    let mut visited = FxHashSet::default();
    let mut item_node = FxHashMap::default();

    let item_dependencies = item_dependencies(cfg);

    for item in item_dependencies.keys() {
        add_item(
            *item,
            module,
            cfg,
            &item_dependencies,
            &mut rvsdg,
            &mut visited,
            &mut item_node,
        );
    }

    rvsdg
}

#[cfg(test)]
mod tests {
    use std::iter;

    use smallvec::smallvec;

    use super::*;
    use crate::cfg::{Branch, LocalValueData};
    use crate::ty::{TY_DUMMY, TY_PREDICATE};
    use crate::{BinaryOperator, FnArg, FnSig, Function, Symbol};

    #[test]
    fn test_single_bb() {
        let mut module = Module::new(Symbol::from_ref(""));
        let function = Function {
            name: Symbol::from_ref(""),
            module: Symbol::from_ref(""),
        };

        module.fn_sigs.register(
            function,
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

        let mut body = Body::init(&module.fn_sigs[function]);

        let bb = body.append_block();

        let a0 = body.params[0];
        let a1 = body.params[1];
        let l0 = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });
        let l1 = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });

        body.basic_blocks[bb]
            .statements
            .push(Statement::OpBinary(OpBinary {
                operator: BinaryOperator::Add,
                lhs: a0.into(),
                rhs: Value::InlineConst(InlineConst::U32(5)),
                result: l0,
            }));
        body.basic_blocks[bb]
            .statements
            .push(Statement::OpBinary(OpBinary {
                operator: BinaryOperator::Add,
                lhs: l0.into(),
                rhs: a1.into(),
                result: l1,
            }));
        body.basic_blocks[bb].terminator = Terminator::Return(Some(l1.into()));

        let mut cfg = Cfg::default();

        cfg.function_body.insert(function, body);

        let actual = cfg_to_rvsdg(&mut module, &cfg);

        let mut expected = Rvsdg::new(module.ty.clone());

        let (_, region) = expected.register_function(&module, function, iter::empty());

        let node_0 = expected.add_const_u32(region, 5);

        let node_1 = expected.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::output(TY_U32, node_0, 0),
        );

        let node_2 = expected.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::output(TY_U32, node_1, 0),
            ValueInput::argument(TY_U32, 1),
        );

        expected.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: node_2,
                output: 0,
            },
        );

        dbg!(&actual);
        dbg!(&expected);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_simple_branch() {
        let mut module = Module::new(Symbol::from_ref(""));
        let function = Function {
            name: Symbol::from_ref(""),
            module: Symbol::from_ref(""),
        };

        module.fn_sigs.register(
            function,
            FnSig {
                name: Default::default(),
                ty: TY_DUMMY,
                args: vec![
                    FnArg {
                        ty: TY_PREDICATE,
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

        let mut body = Body::init(&module.fn_sigs[function]);

        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();
        let bb3 = body.append_block();

        let a0 = body.params[0];
        let a1 = body.params[1];
        let l0 = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });
        let l1 = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });

        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch {
            selector: Some(a0),
            branches: smallvec![bb1, bb2],
        });

        body.basic_blocks[bb1]
            .statements
            .push(Statement::OpBinary(OpBinary {
                operator: BinaryOperator::Add,
                lhs: a1.into(),
                rhs: Value::InlineConst(InlineConst::U32(1)),
                result: l0,
            }));
        body.basic_blocks[bb1]
            .statements
            .push(Statement::OpAssign(OpAssign {
                value: l0.into(),
                result: l1,
            }));
        body.basic_blocks[bb1].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb3],
        });

        body.basic_blocks[bb2]
            .statements
            .push(Statement::OpAssign(OpAssign {
                value: Value::InlineConst(InlineConst::U32(0)),
                result: l1,
            }));
        body.basic_blocks[bb2].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb3],
        });

        body.basic_blocks[bb3].terminator = Terminator::Return(Some(l1.into()));

        let mut cfg = Cfg::default();

        cfg.function_body.insert(function, body);

        let actual = cfg_to_rvsdg(&mut module, &cfg);

        let mut expected = Rvsdg::new(module.ty.clone());

        let (_, region) = expected.register_function(&module, function, iter::empty());

        let switch_node = expected.add_switch(
            region,
            vec![
                ValueInput::argument(TY_PREDICATE, 0),
                ValueInput::argument(TY_U32, 1),
            ],
            vec![ValueOutput::new(TY_U32)],
            None,
        );

        expected.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: switch_node,
                output: 0,
            },
        );

        let branch_0 = expected.add_switch_branch(switch_node);

        let branch_0_node_0 = expected.add_const_u32(branch_0, 1);
        let branch_0_node_1 = expected.add_op_binary(
            branch_0,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::output(TY_U32, branch_0_node_0, 0),
        );

        expected.reconnect_region_result(
            branch_0,
            0,
            ValueOrigin::Output {
                producer: branch_0_node_1,
                output: 0,
            },
        );

        let branch_1 = expected.add_switch_branch(switch_node);

        let branch_1_node_0 = expected.add_const_u32(branch_1, 0);

        expected.reconnect_region_result(
            branch_1,
            0,
            ValueOrigin::Output {
                producer: branch_1_node_0,
                output: 0,
            },
        );

        dbg!(&actual);
        dbg!(&expected);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_simple_loop() {
        let mut module = Module::new(Symbol::from_ref(""));
        let function = Function {
            name: Symbol::from_ref(""),
            module: Symbol::from_ref(""),
        };

        module.fn_sigs.register(
            function,
            FnSig {
                name: Default::default(),
                ty: TY_DUMMY,
                args: vec![FnArg {
                    ty: TY_U32,
                    shader_io_binding: None,
                }],
                ret_ty: Some(TY_U32),
            },
        );

        let mut body = Body::init(&module.fn_sigs[function]);

        let bb0 = body.append_block();
        let bb1 = body.append_block();

        let a0 = body.params[0];
        let l0 = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });

        body.basic_blocks[bb0]
            .statements
            .push(Statement::OpBinary(OpBinary {
                operator: BinaryOperator::Add,
                lhs: a0.into(),
                rhs: Value::InlineConst(InlineConst::U32(1)),
                result: a0,
            }));
        body.basic_blocks[bb0]
            .statements
            .push(Statement::OpBinary(OpBinary {
                operator: BinaryOperator::Gt,
                lhs: a0.into(),
                rhs: Value::InlineConst(InlineConst::U32(10)),
                result: l0,
            }));
        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch {
            selector: Some(l0),
            branches: smallvec![bb1, bb0],
        });

        body.basic_blocks[bb1].terminator = Terminator::Return(Some(a0.into()));

        let mut cfg = Cfg::default();

        cfg.function_body.insert(function, body);

        let actual = cfg_to_rvsdg(&mut module, &cfg);

        let mut expected = Rvsdg::new(module.ty.clone());

        let (_, region) = expected.register_function(&module, function, iter::empty());

        let (loop_node, loop_region) =
            expected.add_loop(region, vec![ValueInput::argument(TY_U32, 0)], None);

        expected.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: loop_node,
                output: 0,
            },
        );

        let loop_node_0 = expected.add_const_u32(loop_region, 1);
        let loop_node_1 = expected.add_op_binary(
            loop_region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::output(TY_U32, loop_node_0, 0),
        );
        let loop_node_2 = expected.add_const_u32(loop_region, 10);
        let loop_node_3 = expected.add_op_binary(
            loop_region,
            BinaryOperator::Gt,
            ValueInput::output(TY_U32, loop_node_1, 0),
            ValueInput::output(TY_U32, loop_node_2, 0),
        );

        expected.reconnect_region_result(
            loop_region,
            0,
            ValueOrigin::Output {
                producer: loop_node_3,
                output: 0,
            },
        );
        expected.reconnect_region_result(
            loop_region,
            1,
            ValueOrigin::Output {
                producer: loop_node_1,
                output: 0,
            },
        );

        dbg!(&actual);
        dbg!(&expected);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_stateful() {
        let mut module = Module::new(Symbol::from_ref(""));
        let function = Function {
            name: Symbol::from_ref(""),
            module: Symbol::from_ref(""),
        };

        module.fn_sigs.register(
            function,
            FnSig {
                name: Default::default(),
                ty: TY_DUMMY,
                args: vec![
                    FnArg {
                        ty: module.ty.register(TypeKind::Ptr(TY_U32)),
                        shader_io_binding: None,
                    },
                    FnArg {
                        ty: TY_U32,
                        shader_io_binding: None,
                    },
                ],
                ret_ty: None,
            },
        );

        let mut body = Body::init(&module.fn_sigs[function]);

        let bb = body.append_block();

        let a0 = body.params[0];
        let a1 = body.params[1];
        let l0 = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });
        let l1 = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });

        body.basic_blocks[bb]
            .statements
            .push(Statement::OpLoad(OpLoad {
                ptr: a0.into(),
                result: l0,
            }));
        body.basic_blocks[bb]
            .statements
            .push(Statement::OpBinary(OpBinary {
                operator: BinaryOperator::Add,
                lhs: l0.into(),
                rhs: Value::InlineConst(InlineConst::U32(1)),
                result: l1,
            }));
        body.basic_blocks[bb]
            .statements
            .push(Statement::OpStore(OpStore {
                ptr: a0.into(),
                value: l1.into(),
            }));
        body.basic_blocks[bb].terminator = Terminator::Return(None);

        let mut cfg = Cfg::default();

        cfg.function_body.insert(function, body);

        let actual = cfg_to_rvsdg(&mut module, &cfg);

        let mut expected = Rvsdg::new(module.ty.clone());

        let (_, region) = expected.register_function(&module, function, iter::empty());

        let node_0 = expected.add_op_load(
            region,
            ValueInput::argument(module.ty.register(TypeKind::Ptr(TY_U32)), 0),
            TY_U32,
            StateOrigin::Argument,
        );
        let node_1 = expected.add_const_u32(region, 1);
        let node_2 = expected.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::output(TY_U32, node_0, 0),
            ValueInput::output(TY_U32, node_1, 0),
        );
        let node_3 = expected.add_op_store(
            region,
            ValueInput::argument(module.ty.register(TypeKind::Ptr(TY_U32)), 0),
            ValueInput::output(TY_U32, node_2, 0),
            StateOrigin::Node(node_0),
        );

        dbg!(&actual);
        dbg!(&expected);

        assert_eq!(actual, expected);
    }
}
