use crate::rvsdg::{Node, NodeKind, Rvsdg, SimpleNode};
use crate::Function;

pub struct PtrOffsetElaborator {
    slice_ptr_element_ptr_nodes: Vec<Node>,
}

impl PtrOffsetElaborator {
    pub fn new() -> Self {
        todo!()
    }

    pub fn replace_in_fn(&mut self, rvsdg: &mut Rvsdg, function: Function) {}

    fn collect_slice_ptr_element_ptr_nodes(&mut self, rvsdg: &Rvsdg, function: Function) {}
}
