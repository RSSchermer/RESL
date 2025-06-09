use std::collections::VecDeque;

use crate::cfg::{OpAlloca, OpLoad};
use crate::rvsdg::{
    Connectivity, Node, NodeKind, Region, Rvsdg, SimpleNode, StateOrigin, ValueOrigin,
};

struct DeterminateValueAnalyzer<'a> {
    rvsdg: &'a Rvsdg,
}

impl<'a> DeterminateValueAnalyzer<'a> {
    fn new(rvsdg: &'a Rvsdg) -> Self {
        Self { rvsdg }
    }

    fn has_determinate_value(&self, op_load: Node) -> bool {
        let region = self.rvsdg[op_load].region();
        let ptr_origin = self.rvsdg[op_load].expect_op_load().ptr_input().origin;

        self.check_origin(region, ptr_origin)
    }

    fn check_origin(&self, region: Region, origin: ValueOrigin) -> bool {
        match origin {
            ValueOrigin::Argument(arg) => self.check_argument(region, arg),
            ValueOrigin::Output { producer, output } => self.check_output(producer, output),
        }
    }

    fn check_argument(&self, region: Region, argument: u32) -> bool {
        let region_owner = self.rvsdg[region].owner();
        let outer_region = self.rvsdg[region_owner].region();

        match self.rvsdg[region_owner].kind() {
            NodeKind::Switch(switch_node) => {
                let input = argument + 1;
                let origin = switch_node.value_inputs()[input as usize].origin;

                self.check_origin(outer_region, origin)
            }
            NodeKind::Loop(loop_node) => {
                let origin = loop_node.value_inputs()[argument as usize].origin;

                self.check_origin(outer_region, origin)
            }
            NodeKind::Function(_) => false,
            _ => unreachable!("node kind cannot own a region"),
        }
    }

    fn check_output(&self, node: Node, output: u32) -> bool {
        use SimpleNode::*;

        match self.rvsdg[node].kind() {
            NodeKind::Switch(switch_node) => switch_node.branches().iter().copied().all(|branch| {
                let origin = self.rvsdg[branch].value_results()[output as usize].origin;

                self.check_origin(branch, origin)
            }),
            NodeKind::Loop(loop_node) => {
                let result = output + 1;
                let loop_region = loop_node.loop_region();
                let origin = self.rvsdg[loop_region].value_results()[result as usize].origin;

                self.check_origin(loop_region, origin)
            }
            NodeKind::Simple(ConstPtr(_)) => false,
            NodeKind::Simple(OpLoad(_)) => false,
            NodeKind::Simple(OpAlloca(_)) => true,
            _ => unreachable!("node kind cannot output a pointer"),
        }
    }
}

struct Promoter<'a> {
    rvsdg: &'a mut Rvsdg,
}

impl<'a> Promoter<'a> {
    fn new(rvsdg: &'a mut Rvsdg) -> Self {
        Promoter { rvsdg }
    }

    fn try_promote_op_load(&mut self, op_load: Node) {
        let analyzer = DeterminateValueAnalyzer::new(&self.rvsdg);

        if analyzer.has_determinate_value(op_load) {}
    }
}
