use crate::rvsdg::visit::region_nodes::RegionNodesVisitor;
use crate::rvsdg::{visit, Node, NodeKind, Rvsdg, SimpleNode, ValueOrigin, ValueUser};
use crate::{Function, Module};

struct NodeCollector<'a> {
    candidates: &'a mut Vec<Node>,
}

impl RegionNodesVisitor for NodeCollector<'_> {
    fn visit_node(&mut self, rvsdg: &Rvsdg, node: Node) {
        use NodeKind::*;
        use SimpleNode::*;

        match rvsdg[node].kind() {
            Simple(OpSwitchPredicateToCase(_)) => self.candidates.push(node),
            _ => (),
        }

        visit::region_nodes::visit_node(self, rvsdg, node);
    }
}

fn try_merge_pred_to_case_to_pred(rvsdg: &mut Rvsdg, pred_to_case: Node) {
    use NodeKind::*;
    use SimpleNode::*;

    let region = rvsdg[pred_to_case].region();
    let data = rvsdg[pred_to_case].expect_op_switch_predicate_to_case();
    let pred_origin = data.input().origin;
    let user_count = data.output().users.len();

    let mut merged_user_count = 0;

    for i in (0..user_count).rev() {
        let data = rvsdg[pred_to_case].expect_op_switch_predicate_to_case();
        let user = data.output().users[i];

        // TODO: if we find a use-case, we could potentially allow "set equality" on the cases,
        // then reorder the branches of any client switch nodes.

        if let ValueUser::Input {
            consumer: case_to_pred,
            input: 0,
        } = user
            && let Simple(OpCaseToSwitchPredicate(n)) = rvsdg[case_to_pred].kind()
            && data.cases() == n.cases()
        {
            rvsdg.reconnect_value_users(
                region,
                ValueOrigin::Output {
                    producer: case_to_pred,
                    output: 0,
                },
                pred_origin,
            );

            rvsdg.remove_node(case_to_pred);

            merged_user_count += 1;
        }
    }

    if merged_user_count == user_count {
        rvsdg.remove_node(pred_to_case);
    }
}

pub struct PredToCaseToPredMerger {
    candidates: Vec<Node>,
}

impl PredToCaseToPredMerger {
    pub fn new() -> Self {
        Self {
            candidates: Vec::new(),
        }
    }

    pub fn merge_in_fn(&mut self, rvsdg: &mut Rvsdg, function: Function) {
        let fn_node = rvsdg
            .get_function_node(function)
            .expect("function not registered");
        let mut collector = NodeCollector {
            candidates: &mut self.candidates,
        };

        collector.visit_node(rvsdg, fn_node);

        for node in self.candidates.drain(..) {
            try_merge_pred_to_case_to_pred(rvsdg, node);
        }
    }
}

pub fn transform_entry_points(module: &Module, rvsdg: &mut Rvsdg) {
    let mut merger = PredToCaseToPredMerger::new();

    for (function, _) in module.entry_points.iter() {
        merger.merge_in_fn(rvsdg, function);
    }
}
