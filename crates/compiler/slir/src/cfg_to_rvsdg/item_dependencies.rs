use indexmap::IndexSet;
use rustc_hash::FxHashMap;

use crate::cfg::{
    BasicBlockData, Body, Cfg, InlineConst, OpAlloca, OpAssign, OpBinary, OpBoolToBranchPredicate,
    OpCall, OpCaseToBranchPredicate, OpGetDiscriminant, OpLoad, OpOffsetSlicePtr, OpPtrElementPtr,
    OpPtrVariantPtr, OpSetDiscriminant, OpStore, OpUnary, RootIdentifier, Statement, Value,
};
use crate::ty::Type;
use crate::{Function, Module, StorageBinding, UniformBinding, WorkgroupBinding};

pub struct Node(usize);

pub struct NodeData {
    children: IndexSet<Node>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Item {
    Function(Function),
    UniformBinding(UniformBinding),
    StorageBinding(StorageBinding),
    WorkgroupBinding(WorkgroupBinding),
}

impl Item {
    pub fn ty(&self, module: &Module) -> Type {
        match *self {
            Item::Function(f) => module.fn_sigs[f].ty,
            Item::UniformBinding(b) => module.uniform_bindings[b].ty,
            Item::StorageBinding(b) => module.storage_bindings[b].ty,
            Item::WorkgroupBinding(b) => module.workgroup_bindings[b].ty,
        }
    }
}

pub trait WithItemDependencies {
    fn with_item_dependencies<F>(&self, f: F)
    where
        F: FnMut(Item);
}

impl WithItemDependencies for Value {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        if let Value::InlineConst(InlineConst::Ptr(ptr)) = self {
            match ptr.base {
                RootIdentifier::Uniform(b) => {
                    f(Item::UniformBinding(b));
                }
                RootIdentifier::Storage(b) => {
                    f(Item::StorageBinding(b));
                }
                RootIdentifier::Workgroup(b) => {
                    f(Item::WorkgroupBinding(b));
                }
                _ => {}
            }
        }
    }
}

impl WithItemDependencies for OpAlloca {
    fn with_item_dependencies<F>(&self, _: F)
    where
        F: FnMut(Item),
    {
    }
}

impl WithItemDependencies for OpAssign {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.value.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpLoad {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.ptr.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpStore {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.ptr.with_item_dependencies(&mut f);
        self.value.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpPtrElementPtr {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.ptr.with_item_dependencies(&mut f);

        for index in &self.indices {
            index.with_item_dependencies(&mut f);
        }
    }
}

impl WithItemDependencies for OpPtrVariantPtr {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.ptr.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpGetDiscriminant {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.ptr.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpSetDiscriminant {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.ptr.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpOffsetSlicePtr {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.slice_ptr.with_item_dependencies(&mut f);
        self.offset.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpUnary {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.value.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpBinary {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.lhs.with_item_dependencies(&mut f);
        self.rhs.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpCall {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        for arg in &self.args {
            arg.with_item_dependencies(&mut f);
        }

        f(Item::Function(self.function));
    }
}

impl WithItemDependencies for OpCaseToBranchPredicate {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.value.with_item_dependencies(&mut f);
    }
}

impl WithItemDependencies for OpBoolToBranchPredicate {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        self.value.with_item_dependencies(&mut f);
    }
}

macro_rules! impl_collect_dependencies_statement {
    ($($op:ident,)*) => {
        impl WithItemDependencies for Statement {
            fn with_item_dependencies<F>(&self, mut f: F) where F: FnMut(Item) {
                match self {
                    $(Statement::$op(s) => s.with_item_dependencies(&mut f),)*
                }
            }
        }
    };
}

impl_collect_dependencies_statement! {
    OpAlloca,
    OpAssign,
    OpLoad,
    OpStore,
    OpPtrElementPtr,
    OpPtrVariantPtr,
    OpGetDiscriminant,
    OpSetDiscriminant,
    OpOffsetSlicePtr,
    OpUnary,
    OpBinary,
    OpCall,
    OpCaseToBranchPredicate,
    OpBoolToBranchPredicate,
}

impl WithItemDependencies for BasicBlockData {
    fn with_item_dependencies<F>(&self, mut f: F)
    where
        F: FnMut(Item),
    {
        for statement in &self.statements {
            statement.with_item_dependencies(&mut f);
        }
    }
}

fn collect_body_dependencies(body: &Body) -> IndexSet<Item> {
    let mut dependencies = IndexSet::new();

    for bb in body.basic_blocks.values() {
        bb.with_item_dependencies(|item| {
            dependencies.insert(item);
        });
    }

    dependencies
}

pub type ItemDependencies = FxHashMap<Item, IndexSet<Item>>;

pub fn item_dependencies(cfg: &Cfg) -> ItemDependencies {
    let mut dep_map = FxHashMap::default();

    for (function, body) in cfg.function_body.iter() {
        let dependencies = collect_body_dependencies(body);

        dep_map.insert(Item::Function(*function), dependencies);
    }

    dep_map
}
