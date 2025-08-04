use crate::rvsdg::{Connectivity, Node, NodeKind, Region, Rvsdg, ValueUser};

pub trait ValueFlowVisitor: Sized {
    /// Returns a boolean that indicates whether to visit the given `user`.
    fn should_visit(&mut self, region: Region, user: ValueUser) -> bool;

    fn visit_value_output(&mut self, rvsdg: &Rvsdg, node: Node, output: u32) {
        visit_value_output(self, rvsdg, node, output)
    }

    fn visit_value_user(&mut self, rvsdg: &Rvsdg, region: Region, user: ValueUser) {
        visit_value_user(self, rvsdg, region, user)
    }

    fn visit_region_result(&mut self, rvsdg: &Rvsdg, region: Region, result: u32) {
        visit_region_result(self, rvsdg, region, result)
    }

    fn visit_value_input(&mut self, rvsdg: &Rvsdg, node: Node, input: u32) {
        visit_value_input(self, rvsdg, node, input)
    }

    fn visit_region_argument(&mut self, rvsdg: &Rvsdg, region: Region, argument: u32) {
        visit_region_argument(self, rvsdg, region, argument)
    }
}

pub fn visit_value_output<V: ValueFlowVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    node: Node,
    output: u32,
) {
    let region = rvsdg[node].region();
    let output = &rvsdg[node].value_outputs()[output as usize];

    for user in &output.users {
        visitor.visit_value_user(rvsdg, region, *user);
    }
}

pub fn visit_value_user<V: ValueFlowVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    region: Region,
    user: ValueUser,
) {
    if visitor.should_visit(region, user) {
        match user {
            ValueUser::Result(result) => visitor.visit_region_result(rvsdg, region, result),
            ValueUser::Input { consumer, input } => {
                visitor.visit_value_input(rvsdg, consumer, input)
            }
        }
    }
}

pub fn visit_region_result<V: ValueFlowVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    region: Region,
    result: u32,
) {
    let owner = rvsdg[region].owner();

    match rvsdg[owner].kind() {
        NodeKind::Switch(_) => visitor.visit_value_output(rvsdg, owner, result),
        NodeKind::Loop(_) => {
            let outer_region = rvsdg[owner].region();
            let index = result - 1;

            visitor.visit_value_user(
                rvsdg,
                outer_region,
                ValueUser::Input {
                    consumer: owner,
                    input: index,
                },
            );
            visitor.visit_value_output(rvsdg, owner, index);
        }
        NodeKind::Function(_) => {
            // We're done
        }
        _ => unreachable!("node kind cannot own a region"),
    }
}

pub fn visit_value_input<V: ValueFlowVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    node: Node,
    input: u32,
) {
    match rvsdg[node].kind() {
        NodeKind::Switch(n) => {
            let argument = input - 1;

            for branch in n.branches() {
                visitor.visit_region_argument(rvsdg, *branch, argument);
            }
        }
        NodeKind::Loop(n) => {
            visitor.visit_region_argument(rvsdg, n.loop_region(), input);
        }
        NodeKind::Simple(_) => {
            let output_count = rvsdg[node].value_outputs().len() as u32;

            for i in 0..output_count {
                visitor.visit_value_output(rvsdg, node, i);
            }
        }
        _ => panic!("top-down value visitor should not be used in the global region"),
    }
}

pub fn visit_region_argument<V: ValueFlowVisitor>(
    visitor: &mut V,
    rvsdg: &Rvsdg,
    region: Region,
    argument: u32,
) {
    let output = &rvsdg[region].value_arguments()[argument as usize];

    for user in &output.users {
        visitor.visit_value_user(rvsdg, region, *user);
    }
}
