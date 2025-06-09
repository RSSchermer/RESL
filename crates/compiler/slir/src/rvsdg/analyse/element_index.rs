use crate::rvsdg::{NodeKind, Rvsdg, SimpleNode, ValueOrigin};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ElementIndex {
    Static(u32),
    Dynamic(ValueOrigin),
}

impl ElementIndex {
    pub fn from_origin(rvsdg: &Rvsdg, origin: ValueOrigin) -> Self {
        match origin {
            ValueOrigin::Argument(_) => ElementIndex::Dynamic(origin),
            ValueOrigin::Output { producer, .. } => {
                if let NodeKind::Simple(SimpleNode::ConstU32(n)) = rvsdg[producer].kind() {
                    ElementIndex::Static(n.value())
                } else {
                    ElementIndex::Dynamic(origin)
                }
            }
        }
    }
}
