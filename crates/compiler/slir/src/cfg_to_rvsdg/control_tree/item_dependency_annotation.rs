use indexmap::IndexSet;

use crate::cfg::analyze::item_dependencies::{Item, WithItemDependencies};
use crate::cfg::{BasicBlock, Cfg};
use crate::cfg_to_rvsdg::control_tree::control_tree::{
    BranchingNode, ControlTree, ControlTreeNode, ControlTreeNodeKind, LinearNode, LoopNode,
};
use crate::cfg_to_rvsdg::control_tree::slice_annotation::SliceAnnotation;

struct ItemDependencyAnnotationVisitor<'a> {
    control_tree: &'a ControlTree,
    cfg: &'a Cfg,
    annotations: SliceAnnotation<Item>,
    accum: IndexSet<Item>,
}

impl<'a> ItemDependencyAnnotationVisitor<'a> {
    fn new(control_tree: &'a ControlTree, cfg: &'a Cfg) -> Self {
        ItemDependencyAnnotationVisitor {
            control_tree,
            cfg,
            annotations: SliceAnnotation::new(control_tree.node_count()),
            accum: Default::default(),
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
        self.accum.clear();

        for statement in self.cfg[bb].statements() {
            self.cfg[*statement].with_item_dependencies(|item| {
                self.accum.insert(item);
            })
        }

        self.annotations.set(node, self.accum.iter().copied());
    }

    fn visit_linear_node(&mut self, (node, data): (ControlTreeNode, &LinearNode)) {
        for child in &data.children {
            self.visit(*child);
        }

        self.accum.clear();

        for child in &data.children {
            self.accum.extend(self.annotations.get(*child));
        }

        self.annotations.set(node, self.accum.iter().copied());
    }

    fn visit_branching_node(&mut self, (node, data): (ControlTreeNode, &BranchingNode)) {
        for branch in &data.branches {
            self.visit(*branch);
        }

        self.accum.clear();

        for branch in &data.branches {
            self.accum.extend(self.annotations.get(*branch));
        }

        self.annotations.set(node, self.accum.iter().copied());
    }

    fn visit_loop_node(&mut self, (current, data): (ControlTreeNode, &LoopNode)) {
        self.visit(data.inner);
        self.annotations.copy(data.inner, current);
    }

    fn into_annotation_state(self) -> SliceAnnotation<Item> {
        self.annotations
    }
}

pub fn annotate_item_dependencies(control_tree: &ControlTree, cfg: &Cfg) -> SliceAnnotation<Item> {
    let mut visitor = ItemDependencyAnnotationVisitor::new(control_tree, cfg);

    visitor.visit(control_tree.root());
    visitor.into_annotation_state()
}
