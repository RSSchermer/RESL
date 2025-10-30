use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Index;

use indexmap::set::MutableValues;
use internment::Intern;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use slotmap::SlotMap;

use crate::scf::Expression;
use crate::ty::{Type, TypeRegistry, TY_BOOL};

slotmap::new_key_type! {
    pub struct UniformBinding;
    pub struct StorageBinding;
    pub struct WorkgroupBinding;
}

pub type Symbol = Intern<String>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Constant {
    pub name: Symbol,
    pub module: Symbol,
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
pub struct FnSigRegistry {
    store: FxHashMap<Function, FnSig>,
}

impl FnSigRegistry {
    pub fn register(&mut self, function: Function, sig: FnSig) {
        if self.contains(function) {
            panic!("function already registered");
        }

        self.store.insert(function, sig);
    }

    pub fn contains(&self, function: Function) -> bool {
        self.store.contains_key(&function)
    }

    pub fn get(&self, function: Function) -> Option<&FnSig> {
        self.store.get(&function)
    }

    pub fn keys(&self) -> impl Iterator<Item = Function> + use<'_> {
        self.store.keys().copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Function, &FnSig)> + use<'_> {
        self.store.iter()
    }
}

impl Index<Function> for FnSigRegistry {
    type Output = FnSig;

    fn index(&self, function: Function) -> &Self::Output {
        self.get(function).expect("function not registered")
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
    pub value: Constant,
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

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct ConstantData {
    ty: Type,
    kind: ConstantKind,
}

impl ConstantData {
    pub fn ty(&self) -> Type {
        self.ty
    }

    pub fn kind(&self) -> &ConstantKind {
        &self.kind
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum ConstantKind {
    ByteData(Vec<u8>),
    Expression,
}

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct ConstantRegistry {
    store: FxHashMap<Constant, ConstantData>,
}

impl ConstantRegistry {
    pub fn register_byte_data(&mut self, constant: Constant, ty: Type, data: Vec<u8>) {
        if let Some(data) = self.store.get(&constant) {
            assert_eq!(
                data.ty, ty,
                "cannot reregister a constant with a different type"
            );
        }

        self.store.insert(
            constant,
            ConstantData {
                ty,
                kind: ConstantKind::ByteData(data),
            },
        );
    }

    pub fn contains(&self, constant: Constant) -> bool {
        self.store.contains_key(&constant)
    }

    pub fn get(&self, constant: Constant) -> Option<&ConstantData> {
        self.store.get(&constant)
    }

    pub fn keys(&self) -> impl Iterator<Item = Constant> + use<'_> {
        self.store.keys().copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Constant, &ConstantData)> + use<'_> {
        self.store.iter()
    }
}

impl Index<Constant> for ConstantRegistry {
    type Output = ConstantData;

    fn index(&self, index: Constant) -> &Self::Output {
        self.store.get(&index).expect("constant not registered")
    }
}

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct EntryPointRegistry {
    data: FxHashMap<Function, EntryPointKind>,
}

impl EntryPointRegistry {
    pub fn register(&mut self, function: Function, entry_point: EntryPointKind) {
        self.data.insert(function, entry_point);
    }

    pub fn get_kind(&self, function: Function) -> Option<&EntryPointKind> {
        self.data.get(&function)
    }

    pub fn is_entry_point(&self, function: Function) -> bool {
        self.data.contains_key(&function)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Function, EntryPointKind)> + use<'_> {
        self.data.iter().map(|(f, e)| (*f, *e))
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Module {
    pub name: Symbol,
    pub ty: TypeRegistry,
    pub fn_sigs: FnSigRegistry,
    pub uniform_bindings: UniformBindingRegistry,
    pub storage_bindings: StorageBindingRegistry,
    pub workgroup_bindings: WorkgroupBindingRegistry,
    pub constants: ConstantRegistry,
    pub entry_points: EntryPointRegistry,
}

impl Module {
    pub fn new(name: Symbol) -> Self {
        Module {
            name,
            ty: Default::default(),
            fn_sigs: Default::default(),
            uniform_bindings: Default::default(),
            storage_bindings: Default::default(),
            workgroup_bindings: Default::default(),
            constants: Default::default(),
            entry_points: Default::default(),
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum EntryPointKind {
    Vertex,
    Fragment,
    Compute(u32, u32, u32),
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
