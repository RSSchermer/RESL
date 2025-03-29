use index_vec::{index_vec, IndexVec};

use crate::cfg::{BasicBlock, Body, Statement};
use crate::cfg_to_rvsdg::control_tree::{
    BranchingNode, ControlTree, ControlTreeNode, ControlTreeNodeKind, LinearNode, LoopNode,
};

impl Statement {
    fn uses_state(&self) -> bool {
        match self {
            Statement::OpLoad(_) | Statement::OpStore(_) | Statement::OpCall(_) => true,
            _ => false,
        }
    }
}

struct StateUseAnnotationVisitor<'a> {
    control_tree: &'a ControlTree,
    body: &'a Body,
    annotation_state: IndexVec<ControlTreeNode, bool>,
}

impl<'a> StateUseAnnotationVisitor<'a> {
    fn new(control_tree: &'a ControlTree, body: &'a Body) -> Self {
        StateUseAnnotationVisitor {
            control_tree,
            body,
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
        self.annotation_state[node] = self.body.basic_blocks[bb]
            .statements
            .iter()
            .any(|s| s.uses_state());
    }

    fn visit_linear_node(&mut self, (node, data): (ControlTreeNode, &LinearNode)) {
        self.annotation_state[node] = data.children.iter().any(|c| self.annotation_state[*c]);
    }

    fn visit_branching_node(&mut self, (node, data): (ControlTreeNode, &BranchingNode)) {
        self.annotation_state[node] = data.branches.iter().any(|b| self.annotation_state[*b]);
    }

    fn visit_loop_node(&mut self, (node, data): (ControlTreeNode, &LoopNode)) {
        self.annotation_state[node] = self.annotation_state[data.inner];
    }

    fn into_annotation_state(self) -> IndexVec<ControlTreeNode, bool> {
        self.annotation_state
    }
}

pub fn annotate_state_use(
    control_tree: &ControlTree,
    body: &Body,
) -> IndexVec<ControlTreeNode, bool> {
    let mut visitor = StateUseAnnotationVisitor::new(control_tree, body);

    visitor.visit(control_tree.root());

    visitor.into_annotation_state()
}
