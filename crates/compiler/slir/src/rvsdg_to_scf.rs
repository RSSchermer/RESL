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
            let branch_block = self.scf.add_switch_case(switch_stmt, i as u32);
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

    fn visit_op_case_to_switch_predicate(&mut self, node: rvsdg::Node) {
        let data = self.rvsdg[node].expect_op_case_to_switch_predicate();
        let case_expr = self.value_mapping.mapping(data.input().origin);
        let expr = self
            .scf
            .make_expr_op_case_to_switch_predicate(case_expr, data.cases().iter().copied());

        self.bind_and_map_expr(node, 0, expr);
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
