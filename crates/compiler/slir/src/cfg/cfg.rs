use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

use ordered_float::OrderedFloat;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use slotmap::SlotMap;
use smallvec::{smallvec, SmallVec};
use thin_vec::ThinVec;

use crate::ty::{Type, TY_BOOL, TY_F32, TY_I32, TY_PREDICATE, TY_U32};
use crate::{
    BinaryOperator, FnSig, Function, Module, StorageBinding, UnaryOperator, UniformBinding,
    WorkgroupBinding,
};

slotmap::new_key_type! {
    pub struct LocalValue;
    pub struct BasicBlock;
}

#[derive(Clone, Serialize, Deserialize, Default, Debug)]
pub struct Cfg {
    pub function_body: FunctionBodyRegistry,
}

#[derive(Clone, Serialize, Deserialize, Default, Debug)]
pub struct FunctionBodyRegistry {
    store: FxHashMap<Function, Body>,
}

impl FunctionBodyRegistry {
    pub fn contains(&self, function: Function) -> bool {
        self.store.contains_key(&function)
    }

    pub fn insert(&mut self, function: Function, body: Body) {
        self.store.insert(function, body);
    }

    pub fn get_or_create(&mut self, function: Function) -> &mut Body {
        self.store.entry(function).or_default()
    }

    pub fn get(&self, function: Function) -> Option<&Body> {
        self.store.get(&function)
    }

    pub fn get_mut(&mut self, function: Function) -> Option<&mut Body> {
        self.store.get_mut(&function)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Function, &Body)> {
        self.store.iter()
    }

    pub fn keys(&self) -> impl Iterator<Item = Function> + use<'_> {
        self.store.keys().copied()
    }
}

impl Index<Function> for FunctionBodyRegistry {
    type Output = Body;

    fn index(&self, function: Function) -> &Self::Output {
        self.get(function).expect("function not registered")
    }
}

impl IndexMut<Function> for FunctionBodyRegistry {
    fn index_mut(&mut self, function: Function) -> &mut Self::Output {
        self.get_mut(function).expect("function not registered")
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Default, Serialize, Deserialize, Debug)]
pub struct LocalValueData {
    pub ty: Option<Type>,
}

impl LocalValueData {
    pub fn u32() -> Self {
        LocalValueData { ty: Some(TY_U32) }
    }

    pub fn predicate() -> Self {
        LocalValueData {
            ty: Some(TY_PREDICATE),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum RootIdentifier {
    Local(LocalValue),
    Uniform(UniformBinding),
    Storage(StorageBinding),
    Workgroup(WorkgroupBinding),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Ptr {
    pub pointee_ty: Type,
    pub base: RootIdentifier,
    pub offset: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum InlineConst {
    U32(u32),
    I32(i32),
    F32(OrderedFloat<f32>),
    Bool(bool),
    Ptr(Ptr),
}

impl From<f32> for InlineConst {
    fn from(value: f32) -> Self {
        InlineConst::F32(OrderedFloat(value))
    }
}

impl InlineConst {
    pub fn ty(&self) -> Type {
        match self {
            InlineConst::U32(_) => TY_U32,
            InlineConst::I32(_) => TY_I32,
            InlineConst::F32(_) => TY_F32,
            InlineConst::Bool(_) => TY_BOOL,
            InlineConst::Ptr(ptr) => ptr.pointee_ty,
        }
    }
}

impl fmt::Display for InlineConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InlineConst::U32(v) => write!(f, "{}u32", v),
            InlineConst::I32(v) => write!(f, "{}i32", v),
            InlineConst::F32(v) => write!(f, "{}f32", v),
            InlineConst::Bool(v) => write!(f, "{}", v),
            InlineConst::Ptr(_) => write!(f, "ptr"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum Value {
    Local(LocalValue),
    InlineConst(InlineConst),
    Const,
}

impl Value {
    pub fn is_local(&self) -> bool {
        matches!(self, Value::Local(_))
    }

    pub fn expect_local(&self) -> LocalValue {
        if let Value::Local(v) = self {
            *v
        } else {
            panic!("expected local value")
        }
    }

    pub fn expect_inline_const(&self) -> InlineConst {
        if let Value::InlineConst(v) = self {
            *v
        } else {
            panic!("expected inline const")
        }
    }
}

impl From<LocalValue> for Value {
    fn from(value: LocalValue) -> Self {
        Value::Local(value)
    }
}

impl From<&'_ LocalValue> for Value {
    fn from(value: &LocalValue) -> Self {
        Value::Local(*value)
    }
}

impl From<InlineConst> for Value {
    fn from(value: InlineConst) -> Self {
        Value::InlineConst(value)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        InlineConst::U32(value).into()
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        InlineConst::I32(value).into()
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        InlineConst::F32(OrderedFloat(value)).into()
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        InlineConst::Bool(value).into()
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpAlloca {
    pub ty: Type,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpAssign {
    pub value: Value,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpLoad {
    pub ptr: Value,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpStore {
    pub ptr: Value,
    pub value: Value,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpPtrElementPtr {
    pub element_ty: Type,
    pub ptr: Value,
    pub indices: ThinVec<Value>,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpPtrVariantPtr {
    pub ptr: Value,
    pub variant_index: u32,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpGetDiscriminant {
    pub ptr: Value,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpSetDiscriminant {
    pub ptr: Value,
    pub variant_index: u32,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpOffsetSlicePtr {
    pub slice_ptr: Value,
    pub offset: Value,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpUnary {
    pub operator: UnaryOperator,
    pub value: Value,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpBinary {
    pub operator: BinaryOperator,
    pub lhs: Value,
    pub rhs: Value,
    pub result: LocalValue,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpCall {
    pub function: Function,
    pub args: ThinVec<Value>,
    pub result: Option<LocalValue>,
}

/// Converts an integer [value] into a branch selector predicate by comparing it against a list of
/// cases.
///
/// If it matches one case at index `n` in the [cases] list, then the predicate produced will select
/// branch `n`. If it matches multiple cases, then `n` will be the index of the first case matched
/// in list-order. If it matches none of the cases, then the predicate will select branch
/// [cases.len()].
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpCaseToBranchPredicate {
    pub value: Value,
    pub cases: Vec<u32>,
    pub result: LocalValue,
}

/// Converts a boolean [value] into a branch selector predicate.
///
/// If [value] is [true], then the predicate will select branch `0`. If [value] is [false] then the
/// predicate will select branch `1`.
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct OpBoolToBranchPredicate {
    pub value: Value,
    pub result: LocalValue,
}

macro_rules! gen_statement {
    ($($op:ident,)*) => {
        #[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
        pub enum Statement {
            $($op($op),)*
        }

        $(impl From<$op> for Statement {
            fn from(op: $op) -> Self {
                Statement::$op(op)
            }
        })*
    };
}

gen_statement! {
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

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct Body {
    pub local_values: SlotMap<LocalValue, LocalValueData>,
    pub basic_blocks: SlotMap<BasicBlock, BasicBlockData>,
    pub ret: LocalValue,
    pub params: Vec<LocalValue>,
    pub entry: Option<BasicBlock>,
}

impl Body {
    pub fn init(sig: &FnSig) -> Self {
        let mut values: SlotMap<LocalValue, LocalValueData> = SlotMap::with_key();
        let ret = values.insert(LocalValueData { ty: sig.ret_ty });
        let mut params = Vec::with_capacity(sig.args.len());

        for arg in &sig.args {
            let value = values.insert(LocalValueData { ty: Some(arg.ty) });

            params.push(value);
        }

        Body {
            local_values: values,
            basic_blocks: Default::default(),
            ret,
            params,
            entry: None,
        }
    }

    pub fn append_block(&mut self) -> BasicBlock {
        let bb = self.basic_blocks.insert(BasicBlockData::default());

        if self.entry.is_none() {
            self.entry = Some(bb)
        }

        bb
    }

    pub fn value_ty(&self, _module: &Module, value: &Value) -> Option<Type> {
        match value {
            Value::Local(v) => self[*v].ty,
            Value::InlineConst(c) => Some(c.ty()),
            Value::Const => unimplemented!(),
        }
    }
}

impl Index<LocalValue> for Body {
    type Output = LocalValueData;

    fn index(&self, value: LocalValue) -> &Self::Output {
        self.local_values.get(value).expect("undefined value")
    }
}

impl IndexMut<LocalValue> for Body {
    fn index_mut(&mut self, value: LocalValue) -> &mut Self::Output {
        self.local_values.get_mut(value).expect("undefined value")
    }
}

#[derive(Clone, PartialEq, Default, Serialize, Deserialize, Debug)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Default, Debug)]
pub struct Branch {
    /// A local value that selects the branch.
    ///
    /// If `None` then the first branch is selected.
    pub selector: Option<LocalValue>,
    pub branches: SmallVec<[BasicBlock; 2]>,
}

impl Branch {
    pub fn single(dest: BasicBlock) -> Self {
        Branch {
            selector: None,
            branches: smallvec![dest],
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum Terminator {
    Branch(Branch),
    Return(Option<Value>),
}

impl Terminator {
    pub fn expect_return(&self) -> &Option<Value> {
        if let Terminator::Return(v) = self {
            v
        } else {
            panic!("expected terminator to return")
        }
    }

    pub fn expect_return_mut(&mut self) -> &mut Option<Value> {
        if let Terminator::Return(v) = self {
            v
        } else {
            panic!("expected terminator to return")
        }
    }

    pub fn expect_branch(&self) -> &Branch {
        if let Terminator::Branch(b) = self {
            b
        } else {
            panic!("expected terminator to branch")
        }
    }

    pub fn expect_branch_mut(&mut self) -> &mut Branch {
        if let Terminator::Branch(b) = self {
            b
        } else {
            panic!("expected terminator to branch")
        }
    }

    pub fn get_or_make_branch(&mut self) -> &mut Branch {
        match self {
            Terminator::Branch(b) => b,
            terminator @ Terminator::Return(_) => {
                *terminator = Terminator::Branch(Branch::default());

                terminator.expect_branch_mut()
            }
        }
    }
}

impl Default for Terminator {
    fn default() -> Self {
        Terminator::Return(None)
    }
}
