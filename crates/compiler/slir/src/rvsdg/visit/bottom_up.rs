use crate::rvsdg::{
    Connectivity, Node, NodeKind, Region, Rvsdg, StateOrigin, ValueInput, ValueOrigin,
};

pub trait BottomUpVisitor: Sized {
    fn visit_region(&mut self, rvsdg: &Rvsdg, region: Region) {
        visit_region_bottom_up(self, rvsdg, region)
    }

    fn visit_node(&mut self, rvsdg: &Rvsdg, node: Node) {
        visit_node_bottom_up(self, rvsdg, node)
    }
}

fn visit_value_inputs<V: BottomUpVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    value_inputs: &[ValueInput],
) {
    for input in value_inputs {
        if let ValueOrigin::Output { producer, .. } = &input.origin {
            visitor.visit_node(rvsdg, *producer)
        }
    }
}

fn visit_state_origin<V: BottomUpVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    state_origin: &StateOrigin,
) {
    if let StateOrigin::Node(node) = state_origin {
        visitor.visit_node(rvsdg, *node)
    }
}

pub fn visit_region_bottom_up<V: BottomUpVisitor>(visitor: &mut V, rvsdg: &Rvsdg, region: Region) {
    let data = &rvsdg[region];

    visit_value_inputs(visitor, rvsdg, data.value_results());
    visit_state_origin(visitor, rvsdg, data.state_result());
}

pub fn visit_node_bottom_up<V: BottomUpVisitor>(visitor: &mut V, rvsdg: &Rvsdg, node: Node) {
    let data = &rvsdg[node];

    match data.kind() {
        NodeKind::Switch(n) => {
            for branch in n.branches() {
                visit_region_bottom_up(visitor, rvsdg, *branch);
            }
        }
        NodeKind::Loop(n) => {
            visit_region_bottom_up(visitor, rvsdg, n.loop_region());
        }
        _ => (),
    }

    visit_value_inputs(visitor, rvsdg, data.value_inputs());

    if let Some(state_origin) = data.state().map(|s| &s.origin) {
        visit_state_origin(visitor, rvsdg, state_origin);
    }
}
