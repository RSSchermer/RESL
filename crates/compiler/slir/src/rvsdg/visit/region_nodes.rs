use crate::rvsdg::{Node, NodeKind, Region, Rvsdg};

pub trait RegionNodesVisitor: Sized {
    fn visit_region(&mut self, rvsdg: &Rvsdg, region: Region) {
        visit_region(self, rvsdg, region);
    }
    fn visit_node(&mut self, rvsdg: &Rvsdg, node: Node) {
        visit_node(self, rvsdg, node);
    }
}

pub fn visit_region<V: RegionNodesVisitor>(visitor: &mut V, rvsdg: &Rvsdg, region: Region) {
    for node in rvsdg[region].nodes() {
        visitor.visit_node(rvsdg, *node);
    }
}

pub fn visit_node<V: RegionNodesVisitor>(visitor: &mut V, rvsdg: &Rvsdg, node: Node) {
    match rvsdg[node].kind() {
        NodeKind::Switch(node) => {
            for branch in node.branches() {
                visitor.visit_region(rvsdg, *branch);
            }
        }
        NodeKind::Loop(node) => {
            visitor.visit_region(rvsdg, node.loop_region());
        }
        NodeKind::Function(node) => {
            visitor.visit_region(rvsdg, node.body_region());
        }
        _ => (),
    }
}
