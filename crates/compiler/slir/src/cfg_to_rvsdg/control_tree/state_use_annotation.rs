use index_vec::{index_vec, IndexVec};

use crate::cfg::{BasicBlock, Cfg, StatementData};
use crate::cfg_to_rvsdg::control_tree::{
    BranchingNode, ControlTree, ControlTreeNode, ControlTreeNodeKind, LinearNode, LoopNode,
};

fn stmt_uses_state(stmt: &StatementData) -> bool {
    match stmt {
        StatementData::OpLoad(_)
        | StatementData::OpStore(_)
        | StatementData::OpGetDiscriminant(_)
        | StatementData::OpSetDiscriminant(_)
        | StatementData::OpCall(_) => true,
        _ => false,
    }
}

struct StateUseAnnotationVisitor<'a> {
    control_tree: &'a ControlTree,
    cfg: &'a Cfg,
    annotation_state: IndexVec<ControlTreeNode, bool>,
}

impl<'a> StateUseAnnotationVisitor<'a> {
    fn new(control_tree: &'a ControlTree, cfg: &'a Cfg) -> Self {
        StateUseAnnotationVisitor {
            control_tree,
            cfg,
            annotation_state: index_vec![false; control_tree.node_count()],
        }
    }

    fn visit(&mut self, node: ControlTreeNode) {
        match &self.control_tree[node] {
            ControlTreeNodeKind::BasicBlock(bb) => self.visit_basic_block((node, *bb)),
            ControlTreeNodeKind::Linear(data) => self.visit_linear_node((node, data)),
            ControlTreeNodeKind::Branching(data) => self.visit_branching_node((node, data)),
            ControlTreeNodeKind::Loop(data) => self.visit_loop_node((node, data)),
        }
    }

    fn visit_basic_block(&mut self, (node, bb): (ControlTreeNode, BasicBlock)) {
        self.annotation_state[node] = self.cfg[bb]
            .statements()
            .iter()
            .map(|s| &self.cfg[*s])
            .any(stmt_uses_state);
    }

    fn visit_linear_node(&mut self, (node, data): (ControlTreeNode, &LinearNode)) {
        for child in &data.children {
            self.visit(*child);
        }

        self.annotation_state[node] = data.children.iter().any(|c| self.annotation_state[*c]);
    }

    fn visit_branching_node(&mut self, (node, data): (ControlTreeNode, &BranchingNode)) {
        for branch in &data.branches {
            self.visit(*branch);
        }

        self.annotation_state[node] = data.branches.iter().any(|b| self.annotation_state[*b]);
    }

    fn visit_loop_node(&mut self, (node, data): (ControlTreeNode, &LoopNode)) {
        self.visit(data.inner);

        self.annotation_state[node] = self.annotation_state[data.inner];
    }

    fn into_annotation_state(self) -> IndexVec<ControlTreeNode, bool> {
        self.annotation_state
    }
}

pub fn annotate_state_use(
    control_tree: &ControlTree,
    cfg: &Cfg,
) -> IndexVec<ControlTreeNode, bool> {
    let mut visitor = StateUseAnnotationVisitor::new(control_tree, cfg);

    visitor.visit(control_tree.root());

    visitor.into_annotation_state()
}
