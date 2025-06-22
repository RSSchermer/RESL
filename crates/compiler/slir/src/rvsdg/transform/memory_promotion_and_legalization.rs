use indexmap::IndexSet;
use rustc_hash::FxHashMap;

use crate::cfg::OpAlloca;
use crate::rvsdg::transform::variable_pointer_emulation::EmulationContext;
use crate::rvsdg::{
    Connectivity, Node, NodeKind, Region, Rvsdg, SimpleNode, StateOrigin, StateUser, ValueInput,
    ValueOrigin, ValueUser,
};
use crate::ty::TypeRegistry;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum PointerAction {
    /// A memory operation that uses the pointer can be promoted to value-flow.
    ///
    /// The pointer has a determinate access chain that resolves back to an originating [OpAlloca].
    /// Contains the [Node] that represents the originating [OpAlloca].
    Promotion(Node),

    /// Memory operations that use this pointer need variable-pointer-emulation to be legal on a
    /// target that does not support variable pointers.
    VariablePointerEmulation,

    /// Memory operations that use the pointer should be left as they are.
    Nothing,
}

struct PointerAnalyzer {
    cache: FxHashMap<(Region, ValueOrigin), PointerAction>,
}

impl PointerAnalyzer {
    fn new() -> Self {
        PointerAnalyzer {
            cache: Default::default(),
        }
    }

    fn resolve_action(
        &mut self,
        rvsdg: &Rvsdg,
        region: Region,
        pointer_origin: ValueOrigin,
    ) -> PointerAction {
        *self
            .cache
            .entry((region, pointer_origin))
            .or_insert_with(|| Self::analyze_origin(rvsdg, region, pointer_origin, false))
    }

    fn analyze_origin(
        rvsdg: &Rvsdg,
        region: Region,
        pointer_origin: ValueOrigin,
        element_access: bool,
    ) -> PointerAction {
        match pointer_origin {
            ValueOrigin::Argument(argument) => {
                Self::analyze_argument(rvsdg, region, argument, element_access)
            }
            ValueOrigin::Output { producer, .. } => {
                Self::analyze_output(rvsdg, producer, element_access)
            }
        }
    }

    fn analyze_argument(
        rvsdg: &Rvsdg,
        region: Region,
        argument: u32,
        element_access: bool,
    ) -> PointerAction {
        let owner = rvsdg[region].owner();

        match rvsdg[owner].kind() {
            NodeKind::Switch(_) => Self::analyze_input(rvsdg, owner, argument + 1, element_access),
            NodeKind::Loop(_) => Self::analyze_input(rvsdg, owner, argument, element_access),
            NodeKind::Function(_) => PointerAction::Nothing,
            _ => unreachable!("node kind cannot be a region owner"),
        }
    }

    fn analyze_input(rvsdg: &Rvsdg, node: Node, input: u32, element_access: bool) -> PointerAction {
        let node_data = &rvsdg[node];
        let region = node_data.region();
        let origin = node_data.value_inputs()[input as usize].origin;

        Self::analyze_origin(rvsdg, region, origin, element_access)
    }

    fn analyze_output(rvsdg: &Rvsdg, node: Node, element_access: bool) -> PointerAction {
        use NodeKind::*;
        use SimpleNode::*;

        match rvsdg[node].kind() {
            Switch(_) | Loop(_) => PointerAction::VariablePointerEmulation,
            Simple(OpAlloca(_)) if !element_access => PointerAction::Promotion(node),
            Simple(OpPtrElementPtr(op)) => Self::analyze_input(rvsdg, node, 0, true),
            Simple(OpAlloca(_) | ConstPtr(_) | OpLoad(_)) => PointerAction::Nothing,
            _ => unreachable!("node kind cannot output a pointer"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum PointerValue {
    Stored(ValueOrigin),
    Fallback,
}

struct TouchedAllocaStack {
    stack: Vec<IndexSet<Node>>,
}

impl TouchedAllocaStack {
    fn new() -> Self {
        TouchedAllocaStack { stack: vec![] }
    }

    fn touch(&mut self, op_alloca: Node) {
        if let Some(touched_allocas) = self.stack.last_mut() {
            touched_allocas.insert(op_alloca);
        }
    }

    fn push(&mut self) {
        self.stack.push(IndexSet::new());
    }

    fn pop(&mut self) -> Option<IndexSet<Node>> {
        let touched_allocas = self.stack.pop();

        // When an alloca is touched in a region, it should also be considered touched in all of
        // that region's ancestor regions. Instead of moving up the entire stack every time
        // TouchedAllocaStack::touch is called, we instead add them all at once here when popping.
        if let Some(touched_allocas) = &touched_allocas
            && let Some(parent) = self.stack.last_mut()
        {
            for alloca in touched_allocas.iter().copied() {
                parent.insert(alloca);
            }
        }

        touched_allocas
    }
}

pub struct MemoryPromoterLegalizer {
    emulation_context: EmulationContext,
    pointer_analyzer: PointerAnalyzer,
    state_origin: (Region, StateOrigin),
    value_availability: FxHashMap<(Node, Region), PointerValue>,
    owner_stack: Vec<Node>,
    touched_alloca_stack: TouchedAllocaStack,
}

impl MemoryPromoterLegalizer {
    pub fn new(function_body: Region) -> Self {
        MemoryPromoterLegalizer {
            emulation_context: EmulationContext::new(),
            pointer_analyzer: PointerAnalyzer::new(),
            state_origin: (function_body, StateOrigin::Argument),
            value_availability: Default::default(),
            owner_stack: vec![],
            touched_alloca_stack: TouchedAllocaStack::new(),
        }
    }

    fn promote_and_legalize(&mut self, ty: &mut TypeRegistry, rvsdg: &mut Rvsdg) {
        while self.visit_state_user(ty, rvsdg) {}
    }

    fn visit_state_user(&mut self, ty: &mut TypeRegistry, rvsdg: &mut Rvsdg) -> bool {
        let (current_region, current_origin) = self.state_origin;

        let current_user = match current_origin {
            StateOrigin::Argument => *rvsdg[current_region].state_argument(),
            StateOrigin::Node(node) => {
                rvsdg[node]
                    .state()
                    .expect("state origin should be part of state chain")
                    .user
            }
        };

        match current_user {
            StateUser::Result => {
                // We've reached the end of the region's state chain: return `false` to indicate
                // that we're done visiting state users.
                false
            }
            StateUser::Node(node) => {
                use NodeKind::*;
                use SimpleNode::*;

                match rvsdg[node].kind() {
                    Switch(_) => self.visit_switch(ty, rvsdg, node),
                    Loop(_) => self.visit_loop(ty, rvsdg, node),
                    Simple(OpLoad(_)) => self.visit_op_load(ty, rvsdg, node),
                    Simple(OpStore(_)) => self.visit_op_store(ty, rvsdg, node),
                    _ => unreachable!("node kind cannot be part of the state chain"),
                }

                true
            }
        }
    }

    fn visit_switch(&mut self, ty: &mut TypeRegistry, rvsdg: &mut Rvsdg, switch_node: Node) {
        let region = rvsdg[switch_node].region();
        let switch_data = rvsdg[switch_node].expect_switch();
        let branch_count = switch_data.branches().len();

        self.touched_alloca_stack.push();

        for i in 0..branch_count {
            let branch = rvsdg[switch_node].expect_switch().branches()[i];

            self.state_origin = (branch, StateOrigin::Argument);

            while self.visit_state_user(ty, rvsdg) {}
        }

        let touched_allocas = self
            .touched_alloca_stack
            .pop()
            .expect("we should be able to pop the set we pushed earlier");

        // Create an output for every alloca that was touched (stored to) inside the switch node to
        // make the pointer value available in the outer region.
        for op_alloca in touched_allocas.iter().copied() {
            let output_ty = rvsdg[op_alloca].expect_op_alloca().ty();
            let output = rvsdg.add_switch_output(switch_node, output_ty);

            for i in 0..branch_count {
                let branch = rvsdg[switch_node].expect_switch().branches()[i];

                // Make sure the value is available in the branch. If the branch actually touched
                // the alloca (did a store), then the value will already be available. If the branch
                // did not tough the alloca, then this will find the latest available value in the
                // outer region and make it available as an argument.
                let value = self.resolve_alloca_value(rvsdg, op_alloca, branch);

                match value {
                    PointerValue::Stored(origin) => {
                        rvsdg.reconnect_region_result(branch, output, origin)
                    }
                    PointerValue::Fallback => todo!("can this case actually happen?"),
                }
            }

            // Mark the value as available in the outer region. Note that we must do this after
            // looping over the branches above, since for some branches we may need to make the
            // latest pointer value available as an argument; if we update this prematurely, then
            // we would incorrectly find the switch node's own new output to be the latest pointer
            // value available in the outer region.
            self.value_availability.insert(
                (op_alloca, region),
                PointerValue::Stored(ValueOrigin::Output {
                    producer: switch_node,
                    output,
                }),
            );
        }

        self.state_origin = (region, StateOrigin::Node(switch_node));
    }

    fn visit_loop(&mut self, ty: &mut TypeRegistry, rvsdg: &mut Rvsdg, loop_node: Node) {
        let region = rvsdg[loop_node].region();
        let loop_region = rvsdg[loop_node].expect_loop().loop_region();

        self.touched_alloca_stack.push();

        self.state_origin = (loop_region, StateOrigin::Argument);

        while self.visit_state_user(ty, rvsdg) {}

        let touched_allocas = self
            .touched_alloca_stack
            .pop()
            .expect("we should be able to pop the set we pushed earlier");

        // Create an output for every alloca that was touched (stored to) inside the switch node to
        // make the pointer value available in the outer region.
        for op_alloca in touched_allocas.iter().copied() {
            let value = self.resolve_alloca_value(rvsdg, op_alloca, loop_region);

            let origin = match value {
                PointerValue::Stored(origin) => origin,
                PointerValue::Fallback => todo!("can this case actually happen?"),
            };

            let users = match origin {
                ValueOrigin::Argument(argument) => {
                    &rvsdg[loop_region].value_arguments()[argument as usize].users
                }
                ValueOrigin::Output { producer, output } => {
                    &rvsdg[producer].value_outputs()[output as usize].users
                }
            };

            let mut preexisting_output = None;

            for user in users {
                if let ValueUser::Result(result) = user {
                    preexisting_output = Some(*result - 1);
                }
            }

            let output = if let Some(preexisting_output) = preexisting_output {
                preexisting_output
            } else {
                let initial_value = self.resolve_alloca_value(rvsdg, op_alloca, region);
                let ty = rvsdg[op_alloca].expect_op_alloca().ty();
                let origin = match initial_value {
                    PointerValue::Stored(origin) => origin,
                    PointerValue::Fallback => todo!("can this case actually happen?"),
                };

                let input = rvsdg.add_loop_input(loop_node, ValueInput { ty, origin });
                let result = input + 1;

                rvsdg.reconnect_region_result(loop_region, result, origin);

                input
            };

            self.value_availability.insert(
                (op_alloca, region),
                PointerValue::Stored(ValueOrigin::Output {
                    producer: loop_node,
                    output,
                }),
            );
        }

        self.state_origin = (region, StateOrigin::Node(loop_node));
    }

    fn visit_op_store(&mut self, ty: &mut TypeRegistry, rvsdg: &mut Rvsdg, op_store: Node) {
        let region = rvsdg[op_store].region();
        let store_data = rvsdg[op_store].expect_op_store();
        let pointer_origin = store_data.ptr_input().origin;
        let value_origin = store_data.value_input().origin;
        let action = self
            .pointer_analyzer
            .resolve_action(rvsdg, region, pointer_origin);

        match action {
            PointerAction::Promotion(op_alloca) => {
                self.value_availability
                    .insert((op_alloca, region), PointerValue::Stored(value_origin));
                self.touched_alloca_stack.touch(op_alloca);

                rvsdg.remove_node(op_store);

                // Note that removing the node will connect the state chain to connect the OpStore's
                // state origin to the OpStore's state user, so we don't need to update
                // `self.state_origin`.
            }
            PointerAction::VariablePointerEmulation => {
                self.emulation_context.emulate_op_store(ty, rvsdg, op_store);

                // Note that emulation will replace the OpStore in the state chain with the
                // emulation node, so the "visit loop" will automatically visit the emulation node
                // on the next iteration.
            }
            PointerAction::Nothing => {
                // Do nothing...
            }
        }
    }

    fn visit_op_load(&mut self, ty: &mut TypeRegistry, rvsdg: &mut Rvsdg, op_load: Node) {
        let region = rvsdg[op_load].region();
        let origin = rvsdg[op_load].expect_op_load().ptr_input().origin;
        let action = self.pointer_analyzer.resolve_action(rvsdg, region, origin);

        match action {
            PointerAction::Promotion(op_alloca) => {
                let value = self.resolve_alloca_value(rvsdg, op_alloca, region);

                match value {
                    PointerValue::Stored(origin) => {
                        let user_count = rvsdg[op_load].expect_op_load().value_output().users.len();

                        for i in (0..user_count).rev() {
                            let user = rvsdg[op_load].expect_op_load().value_output().users[i];

                            rvsdg.reconnect_value_user(region, user, origin);
                        }

                        rvsdg.remove_node(op_load);

                        // Note that removing the node will connect the state chain to connect the
                        // OpLoad's state origin to the OpLoad's state user, so we don't need to
                        // update `self.state_origin`.
                    }
                    PointerValue::Fallback => todo!("can this case actually happen?"),
                }
            }
            PointerAction::VariablePointerEmulation => {
                self.emulation_context.emulate_op_load(ty, rvsdg, op_load);

                // Note that emulation will replace the OpLoad in the state chain with the emulation
                // node, so the "visit loop" will automatically visit the emulation node on the
                // next iteration.
            }
            PointerAction::Nothing => {
                // Do nothing...
            }
        }
    }

    fn resolve_alloca_value(
        &mut self,
        rvsdg: &mut Rvsdg,
        op_alloca: Node,
        mut region: Region,
    ) -> PointerValue {
        // Search outwards through parent regions until we find a region in which the value is
        // available. We record an "owner stack" of region owner-nodes in which the value will have
        // to be made available recursively.
        let pointer_value = loop {
            if let Some(pointer_value) = self.value_availability.get(&(op_alloca, region)) {
                // The value was previously recorded as being available in the region.
                break *pointer_value;
            }

            if rvsdg[op_alloca].region() == region {
                // No value is available yet, but we're in the region that contains the OpAlloca for
                // the value: use the fallback value.
                self.value_availability
                    .insert((op_alloca, region), PointerValue::Fallback);

                break PointerValue::Fallback;
            }

            let owner = rvsdg[region].owner();

            self.owner_stack.push(owner);
            region = rvsdg[owner].region();

            if region == rvsdg.global_region() {
                // We've arrived at the function node itself (whose owner region is the global
                // region) without encountering the region that contains the originating OpAlloca:
                // we're trying to make a value available that is not "in scope", which implies
                // something must have gone wrong.
                panic!("did not encounter the OpAlloca node on the region stack")
            }
        };

        if let PointerValue::Stored(mut origin) = pointer_value {
            // Iterate over the region owners on the stack from the outside in, recursively making
            // the value available inside the owned regions until the stack is empty.
            while let Some(owner) = self.owner_stack.pop() {
                // Ensure the value is available to the region(s) inside the `owner`.

                let outer_region = rvsdg[owner].region();
                let ty = rvsdg.value_origin_ty(outer_region, origin);

                origin = match rvsdg[owner].kind() {
                    NodeKind::Switch(_) => {
                        let input =
                            rvsdg[owner]
                                .value_input_for_origin(origin)
                                .unwrap_or_else(|| {
                                    rvsdg.add_switch_input(owner, ValueInput { ty, origin })
                                });

                        let argument = input - 1;
                        let inner_origin = ValueOrigin::Argument(argument);

                        for branch in rvsdg[owner].expect_switch().branches() {
                            // Make the value available in the branch, but only if it was not
                            // available already, as if a store operation already made a value
                            // available inside the branch, that value will represent the more
                            // recent value.
                            if !self.value_availability.contains_key(&(op_alloca, *branch)) {
                                self.value_availability.insert(
                                    (op_alloca, *branch),
                                    PointerValue::Stored(inner_origin),
                                );
                            }
                        }

                        inner_origin
                    }
                    NodeKind::Loop(loop_data) => {
                        let loop_region = loop_data.loop_region();

                        let argument =
                            rvsdg[owner]
                                .value_input_for_origin(origin)
                                .unwrap_or_else(|| {
                                    let input =
                                        rvsdg.add_loop_input(owner, ValueInput { ty, origin });
                                    let result = input + 1;

                                    // Connect the new input to its corresponding result to make
                                    // the value available to subsequent loop iterations.
                                    rvsdg.reconnect_region_result(
                                        loop_region,
                                        result,
                                        ValueOrigin::Argument(input),
                                    );

                                    input
                                });

                        let inner_origin = ValueOrigin::Argument(argument);

                        self.value_availability
                            .insert((op_alloca, loop_region), PointerValue::Stored(inner_origin));

                        inner_origin
                    }
                    _ => panic!("not a valid region owner"),
                }
            }

            PointerValue::Stored(origin)
        } else {
            PointerValue::Fallback
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;
    use crate::rvsdg::ValueOutput;
    use crate::ty::{TypeKind, TY_DUMMY, TY_U32};
    use crate::{thin_set, BinaryOperator, FnArg, FnSig, Function, Module, Symbol};

    #[test]
    fn test_promote_store_then_load_then_store_then_load() {
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

        let mut rvsdg = Rvsdg::new();

        let (_, region) = rvsdg.register_function(&module, function, iter::empty());

        let ptr_ty = module.ty.register(TypeKind::Ptr(TY_U32));

        let alloca_node = rvsdg.add_op_alloca(&mut module.ty, region, TY_U32);
        let store_0_node = rvsdg.add_op_store(
            region,
            ValueInput::output(ptr_ty, alloca_node, 0),
            ValueInput::argument(TY_U32, 0),
            StateOrigin::Argument,
        );
        let load_0_node = rvsdg.add_op_load(
            region,
            ValueInput::output(ptr_ty, alloca_node, 0),
            TY_U32,
            StateOrigin::Node(store_0_node),
        );
        let store_1_node = rvsdg.add_op_store(
            region,
            ValueInput::output(ptr_ty, alloca_node, 0),
            ValueInput::argument(TY_U32, 1),
            StateOrigin::Node(load_0_node),
        );
        let load_1_node = rvsdg.add_op_load(
            region,
            ValueInput::output(ptr_ty, alloca_node, 0),
            TY_U32,
            StateOrigin::Node(store_1_node),
        );
        let add_node = rvsdg.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::output(TY_U32, load_0_node, 0),
            ValueInput::output(TY_U32, load_1_node, 0),
        );

        rvsdg.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: add_node,
                output: 0,
            },
        );

        let mut promoter_legalizer = MemoryPromoterLegalizer::new(region);

        promoter_legalizer.promote_and_legalize(&mut module.ty, &mut rvsdg);

        assert_eq!(
            &rvsdg[region].value_arguments()[0].users,
            &thin_set![ValueUser::Input {
                consumer: add_node,
                input: 0,
            }]
        );
        assert_eq!(
            &rvsdg[region].value_arguments()[1].users,
            &thin_set![ValueUser::Input {
                consumer: add_node,
                input: 1,
            }]
        );
        assert!(rvsdg[alloca_node]
            .expect_op_alloca()
            .value_output()
            .users
            .is_empty());
        assert!(!rvsdg.is_live_node(store_0_node));
        assert!(!rvsdg.is_live_node(load_0_node));
        assert!(!rvsdg.is_live_node(store_1_node));
        assert!(!rvsdg.is_live_node(load_1_node));
    }

    #[test]
    fn test_promote_store_then_load_inside_switch() {
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

        let mut rvsdg = Rvsdg::new();

        let (_, region) = rvsdg.register_function(&module, function, iter::empty());

        let ptr_ty = module.ty.register(TypeKind::Ptr(TY_U32));

        let alloca_node = rvsdg.add_op_alloca(&mut module.ty, region, TY_U32);
        let store_node = rvsdg.add_op_store(
            region,
            ValueInput::output(ptr_ty, alloca_node, 0),
            ValueInput::argument(TY_U32, 0),
            StateOrigin::Argument,
        );
        let switch_node = rvsdg.add_switch(
            region,
            vec![
                ValueInput::argument(TY_U32, 1),
                ValueInput::output(ptr_ty, alloca_node, 0),
            ],
            vec![ValueOutput::new(TY_U32)],
            Some(StateOrigin::Node(store_node)),
        );

        let branch_0 = rvsdg.add_switch_branch(switch_node);

        let load_node = rvsdg.add_op_load(
            branch_0,
            ValueInput::argument(ptr_ty, 0),
            TY_U32,
            StateOrigin::Argument,
        );

        rvsdg.reconnect_region_result(
            branch_0,
            0,
            ValueOrigin::Output {
                producer: load_node,
                output: 0,
            },
        );

        let branch_1 = rvsdg.add_switch_branch(switch_node);

        let other_value_node = rvsdg.add_const_u32(branch_1, 0);

        rvsdg.reconnect_region_result(
            branch_1,
            0,
            ValueOrigin::Output {
                producer: other_value_node,
                output: 0,
            },
        );

        rvsdg.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: switch_node,
                output: 0,
            },
        );

        let mut promoter_legalizer = MemoryPromoterLegalizer::new(region);

        promoter_legalizer.promote_and_legalize(&mut module.ty, &mut rvsdg);

        assert_eq!(
            &rvsdg[region].value_arguments()[0].users,
            &thin_set![ValueUser::Input {
                consumer: switch_node,
                input: 2,
            }]
        );
        assert_eq!(
            rvsdg[switch_node].expect_switch().value_inputs(),
            &[
                ValueInput::argument(TY_U32, 1),
                ValueInput::output(ptr_ty, alloca_node, 0),
                ValueInput::argument(TY_U32, 0),
            ]
        );
        assert!(rvsdg[branch_0].value_arguments()[0].users.is_empty());
        assert_eq!(
            &rvsdg[branch_0].value_arguments()[1].users,
            &thin_set![ValueUser::Result(0)]
        );
        assert_eq!(
            rvsdg[branch_0].value_results()[0].origin,
            ValueOrigin::Argument(1)
        );
        assert!(!rvsdg.is_live_node(store_node));
        assert!(!rvsdg.is_live_node(load_node));
    }

    #[test]
    fn test_store_then_store_argument_inside_switch_then_load_outside_switch() {
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
                    FnArg {
                        ty: TY_U32,
                        shader_io_binding: None,
                    },
                ],
                ret_ty: Some(TY_U32),
            },
        );

        let mut rvsdg = Rvsdg::new();

        let (_, region) = rvsdg.register_function(&module, function, iter::empty());

        let ptr_ty = module.ty.register(TypeKind::Ptr(TY_U32));

        let alloca_node = rvsdg.add_op_alloca(&mut module.ty, region, TY_U32);
        let store_node = rvsdg.add_op_store(
            region,
            ValueInput::output(ptr_ty, alloca_node, 0),
            ValueInput::argument(TY_U32, 1),
            StateOrigin::Argument,
        );
        let switch_node = rvsdg.add_switch(
            region,
            vec![
                ValueInput::argument(TY_U32, 0),
                ValueInput::output(ptr_ty, alloca_node, 0),
                ValueInput::argument(TY_U32, 2),
            ],
            vec![],
            Some(StateOrigin::Node(store_node)),
        );

        let branch_0 = rvsdg.add_switch_branch(switch_node);

        let switch_store_node = rvsdg.add_op_store(
            branch_0,
            ValueInput::argument(ptr_ty, 0),
            ValueInput::argument(TY_U32, 1),
            StateOrigin::Argument,
        );

        // The second branch does nothing
        let branch_1 = rvsdg.add_switch_branch(switch_node);

        let load_node = rvsdg.add_op_load(
            region,
            ValueInput::output(ptr_ty, alloca_node, 0),
            TY_U32,
            StateOrigin::Node(switch_node),
        );

        rvsdg.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: load_node,
                output: 0,
            },
        );

        let mut promoter_legalizer = MemoryPromoterLegalizer::new(region);

        promoter_legalizer.promote_and_legalize(&mut module.ty, &mut rvsdg);

        assert_eq!(
            &rvsdg[region].value_arguments()[1].users,
            &thin_set![ValueUser::Input {
                consumer: switch_node,
                input: 3,
            }],
            "the second function argument should now be used as the fourth input to the switch node"
        );
        assert_eq!(
            &rvsdg[region].value_arguments()[2].users,
            &thin_set![ValueUser::Input {
                consumer: switch_node,
                input: 2,
            }],
            "the third function argument should now be used as the third input to the switch node"
        );
        assert_eq!(
            rvsdg[switch_node].expect_switch().value_inputs(),
            &[
                ValueInput::argument(TY_U32, 0),
                ValueInput::output(ptr_ty, alloca_node, 0),
                ValueInput::argument(TY_U32, 2),
                ValueInput::argument(TY_U32, 1),
            ],
            "in addition to the branch selector and the original pointer, the switch should now \
            use the third and second function arguments as inputs"
        );

        assert!(
            rvsdg[branch_0].value_arguments()[0].users.is_empty(),
            "the original pointer argument should no longer have any users"
        );
        assert_eq!(
            &rvsdg[branch_0].value_arguments()[1].users,
            &thin_set![ValueUser::Result(0)],
            "the second region argument of the first branch should now connect to the first result \
            of the branch region"
        );
        assert!(
            rvsdg[branch_0].value_arguments()[2].users.is_empty(),
            "the first branch should not use the third region argument"
        );
        assert_eq!(
            rvsdg[branch_0].value_results()[0].origin,
            ValueOrigin::Argument(1),
            "the result of the first branch should connect to the second region argument"
        );

        assert!(
            rvsdg[branch_1].value_arguments()[0].users.is_empty(),
            "the original pointer argument should still not have any users"
        );
        assert!(
            rvsdg[branch_1].value_arguments()[1].users.is_empty(),
            "the second branch should not use the second region argument"
        );
        assert_eq!(
            &rvsdg[branch_1].value_arguments()[2].users,
            &thin_set![ValueUser::Result(0)],
            "the third region argument of the second branch should now connect to the first result \
            of the branch region"
        );
        assert_eq!(
            rvsdg[branch_1].value_results()[0].origin,
            ValueOrigin::Argument(2),
            "the result of the second branch should connect to the third region argument"
        );

        assert_eq!(
            rvsdg[switch_node].value_outputs(),
            &[ValueOutput {
                ty: TY_U32,
                users: thin_set![ValueUser::Result(0)],
            }],
            "the switch node's output should be used by the function region's result"
        );
        assert_eq!(
            rvsdg[region].value_results()[0].origin,
            ValueOrigin::Output {
                producer: switch_node,
                output: 0,
            },
            "the function region's result should connect to the switch node's output"
        );

        assert!(!rvsdg.is_live_node(store_node));
        assert!(!rvsdg.is_live_node(switch_store_node));
        assert!(!rvsdg.is_live_node(load_node));
    }
}
