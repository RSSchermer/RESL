use crate::rvsdg::{
    Connectivity, Node, Region, RegionData, Rvsdg, SimpleNode, StateOrigin, StateUser, ValueInput,
    ValueOrigin, ValueOutput, ValueUser,
};

pub trait TopDownRegionVisitor: Sized {
    fn visit_region(&mut self, rvsdg: &Rvsdg, region: Region) {
        visit_region_top_down(self, rvsdg, region)
    }

    fn visit_node(&mut self, rvsdg: &Rvsdg, node: Node) {
        visit_node_top_down(self, rvsdg, node)
    }
}

fn visit_value_outputs<V: TopDownRegionVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    outputs: &[ValueOutput],
) {
    for output in outputs {
        for user in &output.users {
            if let ValueUser::Input { consumer, .. } = user {
                visitor.visit_node(rvsdg, *consumer);
            }
        }
    }
}

fn visit_state_user<V: TopDownRegionVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    state_user: &StateUser,
) {
    if let StateUser::Node(node) = state_user {
        visitor.visit_node(rvsdg, *node);
    }
}

pub fn visit_region_top_down<V: TopDownRegionVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    region: Region,
) {
    let data = &rvsdg[region];

    visit_value_outputs(visitor, rvsdg, data.value_arguments());
    visit_state_user(visitor, rvsdg, data.state_argument());
}

pub fn visit_node_top_down<V: TopDownRegionVisitor>(visitor: &mut V, rvsdg: &Rvsdg, node: Node) {
    let data = &rvsdg[node];

    visit_value_outputs(visitor, rvsdg, data.value_outputs());

    if let Some(state_user) = data.state().map(|s| &s.user) {
        visit_state_user(visitor, rvsdg, state_user);
    }
}

pub trait BottomUpRegionVisitor: Sized {
    fn visit_region(&mut self, rvsdg: &Rvsdg, region: Region) {
        visit_region_bottom_up(self, rvsdg, region)
    }

    fn visit_node(&mut self, rvsdg: &Rvsdg, node: Node) {
        visit_node_bottom_up(self, rvsdg, node)
    }
}

fn visit_value_inputs<V: BottomUpRegionVisitor>(
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

fn visit_state_origin<V: BottomUpRegionVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    state_origin: &StateOrigin,
) {
    if let StateOrigin::Node(node) = state_origin {
        visitor.visit_node(rvsdg, *node)
    }
}

pub fn visit_region_bottom_up<V: BottomUpRegionVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    region: Region,
) {
    let data = &rvsdg[region];

    visit_value_inputs(visitor, rvsdg, data.value_results());
    visit_state_origin(visitor, rvsdg, data.state_result());
}

pub fn visit_node_bottom_up<V: BottomUpRegionVisitor>(visitor: &mut V, rvsdg: &Rvsdg, node: Node) {
    let data = &rvsdg[node];

    visit_value_inputs(visitor, rvsdg, data.value_inputs());

    if let Some(state_origin) = data.state().map(|s| &s.origin) {
        visit_state_origin(visitor, rvsdg, state_origin);
    }
}
