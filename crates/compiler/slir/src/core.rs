use std::fmt;
use std::fmt::Display;
use std::ops::{Deref, Index, IndexMut};

use indexmap::set::MutableValues;
use indexmap::IndexSet;
use internment::Intern;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use slotmap::SlotMap;

use crate::ty::{Type, TypeRegistry};

slotmap::new_key_type! {
    pub struct UniformBinding;
    pub struct StorageBinding;
    pub struct WorkgroupBinding;
}

pub type Symbol = Intern<String>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Struct(usize);

impl Struct {
    pub fn to_usize(&self) -> usize {
        self.0
    }
}

impl From<usize> for Struct {
    fn from(value: usize) -> Self {
        Struct(value)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Function {
    pub name: Symbol,
    pub module: Symbol,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct ModuleId(u64);

impl ModuleId {
    pub fn to_u64(&self) -> u64 {
        self.0
    }
}

impl From<u64> for ModuleId {
    fn from(value: u64) -> Self {
        ModuleId(value)
    }
}

#[derive(Clone, Serialize, Deserialize, Default, Debug)]
pub struct StructRegistry {
    store: IndexSet<StructData>,
}

impl StructRegistry {
    pub fn register(&mut self, struct_data: StructData) -> Struct {
        let (index, _) = self.store.insert_full(struct_data);

        Struct(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = Struct> + use<'_> {
        self.store
            .iter()
            .map(|s| Struct(self.store.get_index_of(s).unwrap()))
    }
}

impl Index<Struct> for StructRegistry {
    type Output = StructData;

    fn index(&self, struct_handle: Struct) -> &Self::Output {
        self.store
            .get_index(struct_handle.0)
            .expect("unregistered struct")
    }
}

impl IndexMut<Struct> for StructRegistry {
    fn index_mut(&mut self, struct_handle: Struct) -> &mut Self::Output {
        self.store
            .get_index_mut2(struct_handle.0)
            .expect("unregistered struct")
    }
}

#[derive(Clone, Serialize, Deserialize, Default, Debug)]
pub struct FnSigRegistry {
    store: FxHashMap<Function, FnSig>,
}

impl FnSigRegistry {
    pub fn register(&mut self, function: Function, sig: FnSig) {
        self.store.insert(function, sig);
    }

    pub fn contains(&self, function: Function) -> bool {
        self.store.contains_key(&function)
    }

    pub fn get(&self, function: Function) -> Option<&FnSig> {
        self.store.get(&function)
    }

    pub fn get_mut(&mut self, function: Function) -> Option<&mut FnSig> {
        self.store.get_mut(&function)
    }

    pub fn keys(&self) -> impl Iterator<Item = Function> + use<'_> {
        self.store.keys().copied()
    }
}

impl Index<Function> for FnSigRegistry {
    type Output = FnSig;

    fn index(&self, function: Function) -> &Self::Output {
        self.get(function).expect("function not registered")
    }
}

impl IndexMut<Function> for FnSigRegistry {
    fn index_mut(&mut self, function: Function) -> &mut Self::Output {
        self.get_mut(function).expect("function not registered")
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct UniformBindingData {
    pub ty: Type,
    pub resource_binding: ResourceBinding,
}

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct UniformBindingRegistry {
    store: SlotMap<UniformBinding, UniformBindingData>,
}

impl UniformBindingRegistry {
    pub fn register(&mut self, data: UniformBindingData) -> UniformBinding {
        self.store.insert(data)
    }

    pub fn contains(&self, binding: UniformBinding) -> bool {
        self.store.contains_key(binding)
    }

    pub fn get(&self, binding: UniformBinding) -> Option<&UniformBindingData> {
        self.store.get(binding)
    }

    pub fn keys(&self) -> impl Iterator<Item = UniformBinding> + use<'_> {
        self.store.keys()
    }
}

impl Index<UniformBinding> for UniformBindingRegistry {
    type Output = UniformBindingData;

    fn index(&self, binding: UniformBinding) -> &Self::Output {
        self.get(binding).expect("binding not registered")
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct StorageBindingData {
    pub ty: Type,
    pub resource_binding: ResourceBinding,
    pub writable: bool,
}

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct StorageBindingRegistry {
    store: SlotMap<StorageBinding, StorageBindingData>,
}

impl StorageBindingRegistry {
    pub fn register(&mut self, data: StorageBindingData) -> StorageBinding {
        self.store.insert(data)
    }

    pub fn contains(&self, binding: StorageBinding) -> bool {
        self.store.contains_key(binding)
    }

    pub fn get(&self, binding: StorageBinding) -> Option<&StorageBindingData> {
        self.store.get(binding)
    }

    pub fn keys(&self) -> impl Iterator<Item = StorageBinding> + use<'_> {
        self.store.keys()
    }
}

impl Index<StorageBinding> for StorageBindingRegistry {
    type Output = StorageBindingData;

    fn index(&self, binding: StorageBinding) -> &Self::Output {
        self.get(binding).expect("binding not registered")
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct WorkgroupBindingData {
    pub ty: Type,
}

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct WorkgroupBindingRegistry {
    store: SlotMap<WorkgroupBinding, WorkgroupBindingData>,
}

impl WorkgroupBindingRegistry {
    pub fn register(&mut self, data: WorkgroupBindingData) -> WorkgroupBinding {
        self.store.insert(data)
    }

    pub fn contains(&self, binding: WorkgroupBinding) -> bool {
        self.store.contains_key(binding)
    }

    pub fn get(&self, binding: WorkgroupBinding) -> Option<&WorkgroupBindingData> {
        self.store.get(binding)
    }

    pub fn keys(&self) -> impl Iterator<Item = WorkgroupBinding> + use<'_> {
        self.store.keys()
    }
}

impl Index<WorkgroupBinding> for WorkgroupBindingRegistry {
    type Output = WorkgroupBindingData;

    fn index(&self, binding: WorkgroupBinding) -> &Self::Output {
        self.get(binding).expect("binding not registered")
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Module {
    pub name: Symbol,
    pub ty: TypeRegistry,
    pub structs: StructRegistry,
    pub fn_sigs: FnSigRegistry,
    pub uniform_bindings: UniformBindingRegistry,
    pub storage_bindings: StorageBindingRegistry,
    pub workgroup_bindings: WorkgroupBindingRegistry,
}

impl Module {
    pub fn new(name: Symbol) -> Self {
        Module {
            name,
            ty: Default::default(),
            structs: Default::default(),
            fn_sigs: Default::default(),
            uniform_bindings: Default::default(),
            storage_bindings: Default::default(),
            workgroup_bindings: Default::default(),
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct FnSig {
    pub name: Symbol,
    pub ty: Type,
    pub args: Vec<FnArg>,
    pub ret_ty: Option<Type>,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct FnArg {
    pub ty: Type,
    pub shader_io_binding: Option<ShaderIOBinding>,
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct StructData {
    pub fields: Vec<StructField>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct StructField {
    pub offset: u64,
    pub ty: Type,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum BlendSrc {
    Zero,
    One,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ShaderIOBinding {
    VertexIndex,
    InstanceIndex,
    Position {
        invariant: bool,
    },
    FrontFacing,
    FragDepth,
    SampleIndex,
    SampleMask,
    LocalInvocationId,
    LocalInvocationIndex,
    GlobalInvocationId,
    WorkgroupId,
    NumWorkgroups,
    Location {
        location: u32,
        blend_src: Option<BlendSrc>,
        interpolation: Option<Interpolation>,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Interpolation {
    pub tpe: InterpolationType,
    pub sampling: Option<InterpolationSampling>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum InterpolationType {
    Perspective,
    Linear,
    Flat,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum InterpolationSampling {
    Center,
    Centroid,
    Sample,
    First,
    Either,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct ResourceBinding {
    pub group: u32,
    pub binding: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum UnaryOperator {
    Not,
    Neg,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Not => write!(f, "!"),
            UnaryOperator::Neg => write!(f, "-"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum BinaryOperator {
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    Eq,
    NotEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
            BinaryOperator::Shl => write!(f, "<<"),
            BinaryOperator::Shr => write!(f, ">>"),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::NotEq => write!(f, "!="),
            BinaryOperator::Gt => write!(f, ">"),
            BinaryOperator::GtEq => write!(f, ">="),
            BinaryOperator::Lt => write!(f, "<"),
            BinaryOperator::LtEq => write!(f, "<="),
        }
    }
}
