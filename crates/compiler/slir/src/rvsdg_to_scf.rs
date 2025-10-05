use std::collections::VecDeque;

use rustc_hash::FxHashMap;

use crate::rvsdg::analyse::region_stratification::RegionStratifier;
use crate::rvsdg::{Connectivity, Rvsdg, ValueOrigin};
use crate::scf::{BlockPosition, Expression, LoopControl, Scf};
use crate::{rvsdg, scf, Function, Module};

#[derive(Clone, Debug)]
struct ValueMapping {
    value_mapping: FxHashMap<rvsdg::ValueOrigin, scf::Expression>,
}

impl ValueMapping {
    fn new() -> Self {
        Self {
            value_mapping: Default::default(),
        }
    }

    fn map_argument(&mut self, argument: u32, expr: Expression) {
        self.value_mapping
            .insert(ValueOrigin::Argument(argument), expr);
    }

    fn map_output(&mut self, node: rvsdg::Node, output: u32, expr: Expression) {
        self.value_mapping.insert(
            ValueOrigin::Output {
                producer: node,
                output,
            },
            expr,
        );
    }

    fn mapping(&self, origin: ValueOrigin) -> Expression {
        *self
            .value_mapping
            .get(&origin)
            .expect("input values should have been recorded before visiting this node")
    }
}

struct RegionVisitor<'a, 'b, 'c> {
    rvsdg: &'a Rvsdg,
    region_stratifier: &'b mut RegionStratifier,
    scf: &'c mut Scf,
    region: rvsdg::Region,
    dst_block: scf::Block,
    value_mapping: ValueMapping,
    node_queue: VecDeque<rvsdg::Node>,
}

impl<'a, 'b, 'c> RegionVisitor<'a, 'b, 'c> {
    fn visit_region(&mut self) {
        // First organize the region's nodes into a queue sorted by stratum. This means that if we
        // process the queue front-to-back and record a mapping for each of the node's outputs,
        // every node is guaranteed to have a value mapping for each of its inputs.
        self.region_stratifier
            .stratify(&self.rvsdg, self.region, |node, _| {
                self.node_queue.push_back(node);
            });

        while let Some(node) = self.node_queue.pop_front() {
            self.visit_node(node)
        }
    }

    fn visit_node(&mut self, node: rvsdg::Node) {
        use crate::rvsdg::NodeKind::*;

        match self.rvsdg[node].kind() {
            Switch(_) => self.visit_switch_node(node),
            Loop(_) => self.visit_loop_node(node),
            Simple(_) => self.visit_simple_node(node),
            _ => unreachable!("node kind cannot be part of a function (sub)region"),
        }
    }

    fn visit_switch_node(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_switch();
        let on = self.value_mapping.mapping(data.predicate().origin);
        let switch_stmt = self
            .scf
            .add_stmt_switch(self.dst_block, BlockPosition::Append, on);
        let default_block = self.scf[switch_stmt].kind().expect_switch().default();

        // Add out variables to the switch statement and record them in the value mapping.
        for (i, output) in data.value_outputs().iter().enumerate() {
            let binding = self.scf.add_switch_out_var(switch_stmt, output.ty);
            let expr = self.scf.make_expr_local_value(binding);

            self.value_mapping.map_output(node, i as u32, expr);
        }

        // Create an argument mapping. Since each branch receives the same arguments, we construct
        // it once, then pass a clone to each branch region visitor.
        let mut argument_mapping = ValueMapping::new();
        for (arg, input) in data.value_inputs()[1..].iter().enumerate() {
            let expr = self.value_mapping.mapping(input.origin);

            argument_mapping.map_argument(arg as u32, expr);
        }

        for (i, branch) in data.branches().iter().copied().enumerate() {
            let is_last = i == data.branches().len() - 1;

            // We use the switch's default block for the last case.
            let branch_block = if is_last {
                default_block
            } else {
                self.scf.add_switch_case(switch_stmt, i as u32)
            };

            let sub_region_mapping =
                self.visit_sub_region(branch, branch_block, argument_mapping.clone());

            for (res, input) in self.rvsdg[branch].value_results().iter().enumerate() {
                let expr = sub_region_mapping.mapping(input.origin);

                self.scf.set_control_flow_var(branch_block, res, expr);
            }
        }
    }

    fn visit_loop_node(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_loop();
        let loop_region = data.loop_region();
        let (loop_stmt, loop_block) = self
            .scf
            .add_stmt_loop(self.dst_block, BlockPosition::Append);
        let mut argument_mapping = ValueMapping::new();

        for (i, input) in data.value_inputs().iter().enumerate() {
            let initializer = self.value_mapping.mapping(input.origin);
            let loop_var = self.scf.add_loop_var(loop_stmt, initializer);
            let expr = self.scf.make_expr_local_value(loop_var);

            // Add a mapping for the loop value's corresponding argument.
            argument_mapping.map_argument(i as u32, expr);

            // Add a mapping for the loop value's corresponding output.
            self.value_mapping.map_output(node, i as u32, expr);
        }

        // Process the sub-region and use the value mapping it produces to set the loop block's
        // control expression and control-flow variables.
        let sub_region_mapping = self.visit_sub_region(loop_region, loop_block, argument_mapping);

        let control_origin = self.rvsdg[loop_region].value_results()[0].origin;
        let control_expr = sub_region_mapping.mapping(control_origin);

        self.scf
            .set_loop_control(loop_stmt, LoopControl::Tail(control_expr));

        for (i, result) in self.rvsdg[loop_region].value_results()[1..]
            .iter()
            .enumerate()
        {
            let expr = sub_region_mapping.mapping(result.origin);

            self.scf.set_control_flow_var(loop_block, i, expr);
        }
    }

    fn visit_simple_node(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_simple();

        use crate::rvsdg::SimpleNode::*;

        match data {
            ConstU32(_) => self.visit_const_u32(node),
            ConstI32(_) => self.visit_const_i32(node),
            ConstF32(_) => self.visit_const_f32(node),
            ConstBool(_) => self.visit_const_bool(node),
            ConstPtr(_) => self.visit_const_ptr(node),
            ConstFallback(_) => self.visit_const_fallback(node),
            OpAlloca(_) => self.visit_op_alloca(node),
            OpLoad(_) => self.visit_op_load(node),
            OpStore(_) => self.visit_op_store(node),
            OpPtrElementPtr(_) => self.visit_op_ptr_element_ptr(node),
            OpExtractElement(_) => self.visit_op_extract_element(node),
            OpUnary(_) => self.visit_op_unary(node),
            OpBinary(_) => self.visit_op_binary(node),
            OpCaseToSwitchPredicate(_) => self.visit_op_case_to_switch_predicate(node),
            OpBoolToSwitchPredicate(_) => self.visit_op_bool_to_switch_predicate(node),
            OpU32ToSwitchPredicate(_) => self.visit_op_u32_to_switch_predicate(node),
            OpCallBuiltin(_) => self.visit_op_call_builtin(node),
            _ => {
                panic!("node kind not currently supported by SLIR's structured control-flow format")
            }
        }
    }

    fn visit_const_u32(&mut self, node: rvsdg::Node) {
        let value = self.rvsdg[node].expect_const_u32().value();
        let expr = self.scf.make_expr_const_u32(value);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_const_i32(&mut self, node: rvsdg::Node) {
        let value = self.rvsdg[node].expect_const_i32().value();
        let expr = self.scf.make_expr_const_i32(value);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_const_f32(&mut self, node: rvsdg::Node) {
        let value = self.rvsdg[node].expect_const_f32().value();
        let expr = self.scf.make_expr_const_f32(value);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_const_bool(&mut self, node: rvsdg::Node) {
        let value = self.rvsdg[node].expect_const_bool().value();
        let expr = self.scf.make_expr_const_bool(value);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_const_ptr(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_const_ptr();
        let base_expr = self.value_mapping.mapping(data.base().origin);
        let expr = self.scf.make_expr_const_ptr(base_expr);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_const_fallback(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_const_fallback();
        let expr = self.scf.make_expr_fallback_value(data.ty());

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_alloca(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_alloca();
        let (_, binding) =
            self.scf
                .add_stmt_alloca(self.dst_block, BlockPosition::Append, data.ty());
        let expr = self.scf.make_expr_local_value(binding);

        self.value_mapping.map_output(node, 0, expr);
    }

    fn visit_op_load(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_load();
        let ptr_expr = self.value_mapping.mapping(data.ptr_input().origin);
        let expr = self.scf.make_expr_op_load(ptr_expr);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_store(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_store();
        let ptr_expr = self.value_mapping.mapping(data.ptr_input().origin);
        let value_expr = self.value_mapping.mapping(data.value_input().origin);

        self.scf
            .add_stmt_store(self.dst_block, BlockPosition::Append, ptr_expr, value_expr);
    }

    fn visit_op_ptr_element_ptr(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_ptr_element_ptr();
        let element_ty = data.element_ty();
        let ptr_expr = self.value_mapping.mapping(data.ptr_input().origin);
        let indices = data
            .index_inputs()
            .iter()
            .map(|i| self.value_mapping.mapping(i.origin));
        let expr = self
            .scf
            .make_expr_op_ptr_element_ptr(ptr_expr, element_ty, indices);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_extract_element(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_extract_element();
        let element_ty = data.element_ty();
        let ptr_expr = self.value_mapping.mapping(data.aggregate().origin);
        let indices = data
            .indices()
            .iter()
            .map(|i| self.value_mapping.mapping(i.origin));
        let expr = self
            .scf
            .make_expr_op_extract_element(ptr_expr, element_ty, indices);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_unary(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_unary();
        let value_expr = self.value_mapping.mapping(data.input().origin);
        let expr = self.scf.make_expr_op_unary(data.operator(), value_expr);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_binary(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_binary();
        let lhs_expr = self.value_mapping.mapping(data.lhs_input().origin);
        let rhs_expr = self.value_mapping.mapping(data.rhs_input().origin);
        let expr = self
            .scf
            .make_expr_op_binary(data.operator(), lhs_expr, rhs_expr);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_bool_to_switch_predicate(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_bool_to_switch_predicate();
        let value_expr = self.value_mapping.mapping(data.input().origin);
        let expr = self.scf.make_expr_op_bool_to_switch_predicate(value_expr);

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_u32_to_switch_predicate(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_u32_to_switch_predicate();
        let value_expr = self.value_mapping.mapping(data.input().origin);

        self.value_mapping.map_output(node, 0, value_expr);
    }

    fn visit_op_case_to_switch_predicate(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_case_to_switch_predicate();
        let case_expr = self.value_mapping.mapping(data.input().origin);
        let expr = self
            .scf
            .make_expr_op_case_to_switch_predicate(case_expr, data.cases().iter().copied());

        self.bind_and_map_expr(node, 0, expr);
    }

    fn visit_op_call_builtin(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_call_builtin();
        let callee = data.callee().clone();
        let arguments = data
            .argument_inputs()
            .iter()
            .map(|input| self.value_mapping.mapping(input.origin));

        if data.value_output().is_some() {
            let expr = self.scf.make_expr_op_call_builtin(callee, arguments);

            self.bind_and_map_expr(node, 0, expr);
        } else {
            self.scf.add_stmt_call_builtin(
                self.dst_block,
                BlockPosition::Append,
                callee,
                arguments,
            );
        }
    }

    fn bind_and_map_expr(&mut self, node: rvsdg::Node, output: u32, expr: Expression) {
        let (_, binding) =
            self.scf
                .add_stmt_expr_binding(self.dst_block, BlockPosition::Append, expr);
        let expr = self.scf.make_expr_local_value(binding);

        self.value_mapping.map_output(node, output, expr);
    }

    fn visit_sub_region(
        &mut self,
        region: rvsdg::Region,
        dst_block: scf::Block,
        argument_mapping: ValueMapping,
    ) -> ValueMapping {
        let mut sub_region_visitor = RegionVisitor {
            rvsdg: self.rvsdg,
            region_stratifier: &mut self.region_stratifier,
            scf: &mut self.scf,
            region,
            dst_block,
            value_mapping: argument_mapping,
            node_queue: Default::default(),
        };

        sub_region_visitor.visit_region();

        sub_region_visitor.value_mapping
    }
}

fn visit_region(
    rvsdg: &Rvsdg,
    region_stratifier: &mut RegionStratifier,
    scf: &mut Scf,
    region: rvsdg::Region,
    dst_block: scf::Block,
    argument_mapping: ValueMapping,
) -> ValueMapping {
    let mut region_visitor = RegionVisitor {
        rvsdg,
        region_stratifier,
        scf,
        region,
        dst_block,
        value_mapping: argument_mapping,
        node_queue: Default::default(),
    };

    region_visitor.visit_region();

    region_visitor.value_mapping
}

pub struct ScfBuilder<'a, 'b> {
    module: &'a Module,
    rvsdg: &'b Rvsdg,
    region_stratifier: RegionStratifier,
    scf: Scf,
}

impl<'a, 'b> ScfBuilder<'a, 'b> {
    pub fn new(module: &'a Module, rvsdg: &'b Rvsdg) -> Self {
        Self {
            module,
            rvsdg,
            region_stratifier: RegionStratifier::new(),
            scf: Scf::new(rvsdg.ty().clone()),
        }
    }

    pub fn build_function_body(&mut self, function: Function) {
        let body = self.scf.register_function(&self.module, function);
        let body_block = body.block();
        let arg_bindings = body.argument_bindings().to_vec();

        let fn_node = self
            .rvsdg
            .get_function_node(function)
            .expect("function not registered with RVSDG");
        let fn_data = self.rvsdg[fn_node].expect_function();
        let body_region = fn_data.body_region();

        let mut argument_mapping = ValueMapping::new();

        // Set up value mappings for the function arguments
        let arg_start = fn_data.dependencies().len();
        for (i, binding) in arg_bindings.into_iter().enumerate() {
            let arg = arg_start + i;
            let expr = self.scf.make_expr_local_value(binding);

            argument_mapping.map_argument(arg as u32, expr);
        }

        // Set up value mappings for the global (uniform/storage/workgroup) value dependencies
        for (arg, dep) in fn_data.dependencies().iter().enumerate() {
            let ValueOrigin::Output {
                producer: dep_node,
                output: 0,
            } = dep.origin
            else {
                panic!("dependencies must connect to single-output nodes")
            };

            use crate::rvsdg::NodeKind::*;

            match self.rvsdg[dep_node].kind() {
                UniformBinding(n) => {
                    let expr = self
                        .scf
                        .make_expr_uniform_value(&self.module.uniform_bindings, n.binding());

                    argument_mapping.map_argument(arg as u32, expr);
                }
                StorageBinding(n) => {
                    let expr = self
                        .scf
                        .make_expr_storage_value(&self.module.storage_bindings, n.binding());

                    argument_mapping.map_argument(arg as u32, expr);
                }
                WorkgroupBinding(n) => {
                    let expr = self
                        .scf
                        .make_expr_workgroup_value(&self.module.workgroup_bindings, n.binding());

                    argument_mapping.map_argument(arg as u32, expr);
                }
                Constant(n) => {
                    let expr = self
                        .scf
                        .make_expr_constant_value(&self.module.constants, n.constant());

                    argument_mapping.map_argument(arg as u32, expr);
                }
                _ => (),
            }
        }

        let value_mapping = visit_region(
            self.rvsdg,
            &mut self.region_stratifier,
            &mut self.scf,
            body_region,
            body_block,
            argument_mapping,
        );

        assert!(
            self.rvsdg[body_region].value_results().len() <= 1,
            "SLIR does not support multiple return values"
        );

        if let Some(result) = self.rvsdg[body_region].value_results().first() {
            let expr = value_mapping.mapping(result.origin);

            self.scf
                .add_stmt_return(body_block, BlockPosition::Append, Some(expr));
        }
    }

    pub fn into_result(self) -> Scf {
        self.scf
    }
}

pub fn rvsdg_entry_points_to_scf(module: &Module, rvsdg: &Rvsdg) -> Scf {
    let mut builder = ScfBuilder::new(module, rvsdg);

    for (entry_point, _) in module.entry_points.iter() {
        builder.build_function_body(entry_point);
    }

    builder.into_result()
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;
    use crate::rvsdg::{ValueInput, ValueOutput};
    use crate::ty::{TY_DUMMY, TY_PREDICATE, TY_U32};
    use crate::{BinaryOperator, FnArg, FnSig, Symbol};

    #[test]
    fn test_single_region() {
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

        let mut rvsdg = Rvsdg::new(module.ty.clone());

        let (_, region) = rvsdg.register_function(&module, function, iter::empty());

        let node = rvsdg.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::argument(TY_U32, 1),
        );

        rvsdg.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: node,
                output: 0,
            },
        );

        let mut builder = ScfBuilder::new(&module, &rvsdg);

        builder.build_function_body(function);

        let scf = builder.into_result();

        let body = scf
            .get_function_body(function)
            .expect("should have registered a function body");

        assert_eq!(body.argument_bindings().len(), 2);
        assert_eq!(body.argument_bindings()[0].ty(), TY_U32);
        assert_eq!(body.argument_bindings()[1].ty(), TY_U32);

        let block_data = &scf[body.block()];

        assert_eq!(block_data.statements().len(), 2);

        let statement_0 = block_data.statements()[0];
        let statement_1 = block_data.statements()[1];

        let statement_0_data = scf[statement_0].kind().expect_expr_binding();
        let statement_0_binding = statement_0_data.binding();
        let op_binary = scf[statement_0_data.expression()].kind().expect_op_binary();

        assert_eq!(op_binary.operator(), BinaryOperator::Add);

        let lhs = scf[op_binary.lhs()].kind().expect_local_value();

        assert_eq!(lhs, body.argument_bindings()[0]);

        let rhs = scf[op_binary.rhs()].kind().expect_local_value();

        assert_eq!(rhs, body.argument_bindings()[1]);

        let statement_1_data = scf[statement_1].kind().expect_return();

        let return_value = statement_1_data
            .value()
            .expect("should have a return value");
        let return_value_data = scf[return_value].kind().expect_local_value();

        assert_eq!(return_value_data, statement_0_binding);
    }

    #[test]
    fn test_switch_node() {
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

        let mut rvsdg = Rvsdg::new(module.ty.clone());

        let (_, region) = rvsdg.register_function(&module, function, iter::empty());

        let switch_node = rvsdg.add_switch(
            region,
            vec![
                ValueInput::argument(TY_PREDICATE, 0),
                ValueInput::argument(TY_U32, 1),
            ],
            vec![ValueOutput::new(TY_U32)],
            None,
        );

        let branch_0 = rvsdg.add_switch_branch(switch_node);

        let branch_0_const_node = rvsdg.add_const_u32(branch_0, 1);
        let branch_0_add_node = rvsdg.add_op_binary(
            branch_0,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::output(TY_U32, branch_0_const_node, 0),
        );

        rvsdg.reconnect_region_result(
            branch_0,
            0,
            ValueOrigin::Output {
                producer: branch_0_add_node,
                output: 0,
            },
        );

        let branch_1 = rvsdg.add_switch_branch(switch_node);

        let branch_1_const_node = rvsdg.add_const_u32(branch_1, 0);

        rvsdg.reconnect_region_result(
            branch_1,
            0,
            ValueOrigin::Output {
                producer: branch_1_const_node,
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

        let mut builder = ScfBuilder::new(&module, &rvsdg);

        builder.build_function_body(function);

        let scf = builder.into_result();

        let body = scf
            .get_function_body(function)
            .expect("should have registered a function body");

        assert_eq!(body.argument_bindings().len(), 2);
        assert_eq!(body.argument_bindings()[0].ty(), TY_PREDICATE);
        assert_eq!(body.argument_bindings()[1].ty(), TY_U32);

        let block_data = &scf[body.block()];

        assert_eq!(block_data.statements().len(), 2);

        let stmt_switch = block_data.statements()[0];
        let stmt_return = block_data.statements()[1];

        let stmt_switch_data = scf[stmt_switch].kind().expect_switch();

        assert_eq!(stmt_switch_data.out_vars().len(), 1);

        let stmt_switch_out_var = stmt_switch_data.out_vars()[0];

        assert_eq!(stmt_switch_data.cases().len(), 1);

        let case_0_block = stmt_switch_data.cases()[0].block();
        let case_0_block_data = &scf[case_0_block];

        assert_eq!(case_0_block_data.statements().len(), 2);

        let case_0_stmt_const = case_0_block_data.statements()[0];
        let case_0_stmt_add = case_0_block_data.statements()[1];

        let case_0_stmt_const_data = scf[case_0_stmt_const].kind().expect_expr_binding();
        let case_0_stmt_const_binding = case_0_stmt_const_data.binding();
        let case_0_stmt_const_expr = case_0_stmt_const_data.expression();

        assert_eq!(scf[case_0_stmt_const_expr].kind().expect_const_u32(), 1);

        let case_0_stmt_add_data = scf[case_0_stmt_add].kind().expect_expr_binding();
        let case_0_stmt_add_binding = case_0_stmt_add_data.binding();
        let case_0_stmt_add_expr = case_0_stmt_add_data.expression();

        let case_0_add_data = scf[case_0_stmt_add_expr].kind().expect_op_binary();

        assert_eq!(case_0_add_data.operator(), BinaryOperator::Add);

        let lhs_data = scf[case_0_add_data.lhs()].kind().expect_local_value();
        let rhs_data = scf[case_0_add_data.rhs()].kind().expect_local_value();

        assert_eq!(lhs_data, body.argument_bindings()[1]);
        assert_eq!(rhs_data, case_0_stmt_const_binding);

        let case_0_var_0_expr = case_0_block_data.control_flow_var(stmt_switch_out_var);
        let case_0_var_0_value = scf[case_0_var_0_expr].kind().expect_local_value();

        assert_eq!(case_0_var_0_value, case_0_stmt_add_binding);

        let default_block_data = &scf[stmt_switch_data.default()];

        assert_eq!(default_block_data.statements().len(), 1);

        let default_stmt_const = default_block_data.statements()[0];

        let default_stmt_const_data = scf[default_stmt_const].kind().expect_expr_binding();
        let default_stmt_const_binding = default_stmt_const_data.binding();
        let default_stmt_const_expr = default_stmt_const_data.expression();

        assert_eq!(scf[default_stmt_const_expr].kind().expect_const_u32(), 0);

        let default_var_0_expr = default_block_data.control_flow_var(stmt_switch_out_var);
        let default_var_0_value = scf[default_var_0_expr].kind().expect_local_value();

        assert_eq!(default_var_0_value, default_stmt_const_binding);

        let stmt_return_data = scf[stmt_return].kind().expect_return();

        let return_expr = stmt_return_data
            .value()
            .expect("should have a return value");
        let return_value = scf[return_expr].kind().expect_local_value();

        assert_eq!(return_value, stmt_switch_out_var);
    }

    #[test]
    fn test_loop_node() {
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

        let mut rvsdg = Rvsdg::new(module.ty.clone());

        let (_, region) = rvsdg.register_function(&module, function, iter::empty());

        let (loop_node, loop_region) =
            rvsdg.add_loop(region, vec![ValueInput::argument(TY_U32, 0)], None);

        let added_value_node = rvsdg.add_const_u32(loop_region, 1);
        let add_node = rvsdg.add_op_binary(
            loop_region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::output(TY_U32, added_value_node, 0),
        );
        let compare_value_node = rvsdg.add_const_u32(loop_region, 10);
        let compare_node = rvsdg.add_op_binary(
            loop_region,
            BinaryOperator::Lt,
            ValueInput::output(TY_U32, add_node, 0),
            ValueInput::output(TY_U32, compare_value_node, 0),
        );

        rvsdg.reconnect_region_result(
            loop_region,
            0,
            ValueOrigin::Output {
                producer: compare_node,
                output: 0,
            },
        );
        rvsdg.reconnect_region_result(
            loop_region,
            1,
            ValueOrigin::Output {
                producer: add_node,
                output: 0,
            },
        );

        rvsdg.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: loop_node,
                output: 0,
            },
        );

        let mut builder = ScfBuilder::new(&module, &rvsdg);

        builder.build_function_body(function);

        let scf = builder.into_result();

        let body = scf
            .get_function_body(function)
            .expect("should have registered a function body");

        assert_eq!(body.argument_bindings().len(), 1);
        assert_eq!(body.argument_bindings()[0].ty(), TY_U32);

        let block_data = &scf[body.block()];

        assert_eq!(block_data.statements().len(), 2);

        let stmt_loop = block_data.statements()[0];
        let stmt_return = block_data.statements()[1];

        let stmt_loop_data = scf[stmt_loop].kind().expect_loop();

        assert_eq!(stmt_loop_data.loop_vars().len(), 1);

        let loop_var = stmt_loop_data.loop_vars()[0];
        let loop_var_init_value = scf[loop_var.initial_value()].kind().expect_local_value();

        assert_eq!(loop_var_init_value, body.argument_bindings()[0]);

        let loop_block = stmt_loop_data.block();
        let loop_block_data = &scf[loop_block];

        assert_eq!(loop_block_data.statements().len(), 4);

        let loop_block_stmt_0 = loop_block_data.statements()[0];
        let loop_block_stmt_1 = loop_block_data.statements()[1];
        let loop_block_stmt_2 = loop_block_data.statements()[2];
        let loop_block_stmt_3 = loop_block_data.statements()[3];

        let added_value_data = scf[loop_block_stmt_0].kind().expect_expr_binding();
        let added_value_binding = added_value_data.binding();
        let added_value_expr = added_value_data.expression();

        assert_eq!(scf[added_value_expr].kind().expect_const_u32(), 1);

        let compare_value_data = scf[loop_block_stmt_1].kind().expect_expr_binding();
        let compare_value_binding = compare_value_data.binding();
        let compare_value_expr = compare_value_data.expression();

        assert_eq!(scf[compare_value_expr].kind().expect_const_u32(), 10);

        let add_data = scf[loop_block_stmt_2].kind().expect_expr_binding();
        let add_binding = add_data.binding();
        let add_expr = add_data.expression();

        let add_expr_data = scf[add_expr].kind().expect_op_binary();

        assert_eq!(add_expr_data.operator(), BinaryOperator::Add);

        let lhs = scf[add_expr_data.lhs()].kind().expect_local_value();

        assert_eq!(lhs, loop_var.binding());

        let rhs = scf[add_expr_data.rhs()].kind().expect_local_value();

        assert_eq!(rhs, added_value_binding);

        let compare_data = scf[loop_block_stmt_3].kind().expect_expr_binding();
        let compare_binding = compare_data.binding();
        let compare_expr = compare_data.expression();

        let compare_expr_data = scf[compare_expr].kind().expect_op_binary();

        assert_eq!(compare_expr_data.operator(), BinaryOperator::Lt);

        let lhs = scf[compare_expr_data.lhs()].kind().expect_local_value();

        assert_eq!(lhs, add_binding);

        let rhs = scf[compare_expr_data.rhs()].kind().expect_local_value();

        assert_eq!(rhs, compare_value_binding);

        let loop_var_0_expr = loop_block_data.control_flow_var(loop_var.binding());
        let loop_var_0_value = scf[loop_var_0_expr].kind().expect_local_value();

        assert_eq!(loop_var_0_value, add_binding);

        let LoopControl::Tail(reentry_control_expr) = stmt_loop_data.control() else {
            panic!("should be tail-controlled loop")
        };

        let reentry_control_binding = scf[reentry_control_expr].kind().expect_local_value();

        assert_eq!(reentry_control_binding, compare_binding);

        let stmt_return_data = scf[stmt_return].kind().expect_return();

        let return_expr = stmt_return_data
            .value()
            .expect("should have a return value");
        let return_value = scf[return_expr].kind().expect_local_value();

        assert_eq!(return_value, loop_var.binding());
    }
}
