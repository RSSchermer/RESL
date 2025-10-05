use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::{Arc, PoisonError, RwLock};
use std::{fmt, mem};

use indexmap::IndexSet;
use serde::{Deserialize, Serialize};

use crate::ty::ScalarKind::Bool;
use crate::ty::TypeKind::Predicate;
use crate::{BinaryOperator, Function, Module};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Type(TypeInner);

impl Type {
    pub fn registration_id(&self) -> Option<usize> {
        if let TypeInner::Registered(id) = &self.0 {
            Some(*id)
        } else {
            None
        }
    }

    pub fn from_registration_id(id: usize) -> Self {
        Type(TypeInner::Registered(id))
    }

    pub fn to_string(&self, ty_registry: &TypeRegistry) -> String {
        ty_registry.kind(*self).to_string(ty_registry)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
enum TypeInner {
    U32,
    I32,
    F32,
    Bool,
    Vec2U32,
    Vec2I32,
    Vec2F32,
    Vec2Bool,
    Vec3U32,
    Vec3I32,
    Vec3F32,
    Vec3Bool,
    Vec4U32,
    Vec4I32,
    Vec4F32,
    Vec4Bool,
    Mat2x2,
    Mat2x3,
    Mat2x4,
    Mat3x2,
    Mat3x3,
    Mat3x4,
    Mat4x2,
    Mat4x3,
    Mat4x4,
    AtomicU32,
    AtomicI32,
    AtomicF32,
    AtomicBool,
    Predicate,
    PtrU32,
    Dummy,
    Registered(usize),
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Struct {
    pub fields: Vec<StructField>,
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Enum {
    pub variants: Vec<Type>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct StructField {
    pub offset: u64,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum TypeKind {
    Scalar(ScalarKind),
    Atomic(ScalarKind),
    Vector {
        scalar: ScalarKind,
        size: VectorSize,
    },
    Matrix {
        rows: VectorSize,
        columns: VectorSize,
        scalar: ScalarKind,
    },
    Array {
        element_ty: Type,
        count: u64,
    },
    Slice {
        element_ty: Type,
    },
    Struct(Struct),
    Enum(Enum),
    Ptr(Type),
    Function(Function),
    Predicate,
    Dummy,
}

impl TypeKind {
    pub fn is_scalar(&self) -> bool {
        matches!(self, TypeKind::Scalar(_))
    }

    pub fn expect_scalar(&self) -> &ScalarKind {
        if let TypeKind::Scalar(scalar) = self {
            scalar
        } else {
            panic!("not a scalar type");
        }
    }

    pub fn expect_fn(&self) -> &Function {
        if let TypeKind::Function(function) = self {
            function
        } else {
            panic!("not a function type");
        }
    }

    pub fn expect_struct(&self) -> &Struct {
        if let TypeKind::Struct(struct_data) = self {
            struct_data
        } else {
            panic!("not an struct type");
        }
    }

    pub fn is_enum(&self) -> bool {
        matches!(self, TypeKind::Enum(_))
    }

    pub fn expect_enum(&self) -> &Enum {
        if let TypeKind::Enum(enum_data) = self {
            enum_data
        } else {
            panic!("not an enum type");
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, TypeKind::Ptr(_))
    }

    pub fn expect_ptr(&self) -> Type {
        if let TypeKind::Ptr(ptr) = self {
            *ptr
        } else {
            panic!("not a pointer type");
        }
    }

    pub fn is_aggregate(&self) -> bool {
        matches!(
            self,
            TypeKind::Struct(_) | TypeKind::Enum(_) | TypeKind::Array { .. }
        )
    }

    pub fn is_slice(&self) -> bool {
        matches!(self, TypeKind::Slice { .. })
    }
}

impl TypeKind {
    fn to_string(&self, ty_registry: &TypeRegistry) -> String {
        match self {
            TypeKind::Scalar(scalar) => format!("{}", scalar),
            TypeKind::Atomic(scalar) => format!("atomic<{}>", scalar),
            TypeKind::Vector { size, scalar } => match size {
                VectorSize::Two => format!("vec2<{}>", scalar),
                VectorSize::Three => format!("vec3<{}>", scalar),
                VectorSize::Four => format!("vec4<{}>", scalar),
            },
            TypeKind::Matrix {
                rows,
                columns,
                scalar,
            } => match (columns, rows) {
                (VectorSize::Two, VectorSize::Two) => format!("mat2x2<{}>", scalar),
                (VectorSize::Two, VectorSize::Three) => format!("mat2x3<{}>", scalar),
                (VectorSize::Two, VectorSize::Four) => format!("mat2x4<{}>", scalar),
                (VectorSize::Three, VectorSize::Two) => format!("mat3x2<{}>", scalar),
                (VectorSize::Three, VectorSize::Three) => format!("mat3x3<{}>", scalar),
                (VectorSize::Three, VectorSize::Four) => format!("mat3x4<{}>", scalar),
                (VectorSize::Four, VectorSize::Two) => format!("mat4x2<{}>", scalar),
                (VectorSize::Four, VectorSize::Three) => format!("mat4x3<{}>", scalar),
                (VectorSize::Four, VectorSize::Four) => format!("mat4x4<{}>", scalar),
            },
            TypeKind::Array { element_ty, count } => {
                format!("array<{}, {}>", element_ty.to_string(ty_registry), count)
            }
            TypeKind::Slice { element_ty } => {
                format!("array<{}>", element_ty.to_string(ty_registry))
            }
            TypeKind::Struct(_) => "struct".to_string(),
            TypeKind::Enum(_) => "enum".to_string(),
            TypeKind::Ptr(pointee_ty) => format!("ptr<{}>", pointee_ty.to_string(ty_registry)),
            TypeKind::Function(f) => format!("Function_{}_{}", f.module, f.name),
            TypeKind::Predicate => format!("predicate"),
            TypeKind::Dummy => "dummy".to_string(),
        }
    }
}

impl From<Struct> for TypeKind {
    fn from(value: Struct) -> Self {
        TypeKind::Struct(value)
    }
}

impl From<Enum> for TypeKind {
    fn from(value: Enum) -> Self {
        TypeKind::Enum(value)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ScalarKind {
    I32,
    U32,
    F32,
    Bool,
}

impl ScalarKind {
    pub fn is_numeric(&self) -> bool {
        match self {
            ScalarKind::I32 | ScalarKind::U32 | ScalarKind::F32 => true,
            ScalarKind::Bool => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            ScalarKind::I32 | ScalarKind::U32 => true,
            ScalarKind::F32 | ScalarKind::Bool => false,
        }
    }
}

impl fmt::Display for ScalarKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScalarKind::I32 => write!(f, "i32"),
            ScalarKind::U32 => write!(f, "u32"),
            ScalarKind::F32 => write!(f, "f32"),
            ScalarKind::Bool => write!(f, "bool"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum VectorSize {
    Two,
    Three,
    Four,
}

impl VectorSize {
    pub fn to_u32(&self) -> u32 {
        match self {
            VectorSize::Two => 2,
            VectorSize::Three => 3,
            VectorSize::Four => 4,
        }
    }
}

pub const TY_KIND_U32: TypeKind = TypeKind::Scalar(ScalarKind::U32);
pub const TY_KIND_I32: TypeKind = TypeKind::Scalar(ScalarKind::I32);
pub const TY_KIND_F32: TypeKind = TypeKind::Scalar(ScalarKind::F32);
pub const TY_KIND_BOOL: TypeKind = TypeKind::Scalar(ScalarKind::Bool);
pub const TY_KIND_VEC2_U32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::U32,
    size: VectorSize::Two,
};
pub const TY_KIND_VEC2_I32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::I32,
    size: VectorSize::Two,
};
pub const TY_KIND_VEC2_F32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::F32,
    size: VectorSize::Two,
};
pub const TY_KIND_VEC2_BOOL: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::Bool,
    size: VectorSize::Two,
};
pub const TY_KIND_VEC3_U32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::U32,
    size: VectorSize::Three,
};
pub const TY_KIND_VEC3_I32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::I32,
    size: VectorSize::Three,
};
pub const TY_KIND_VEC3_F32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::F32,
    size: VectorSize::Three,
};
pub const TY_KIND_VEC3_BOOL: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::Bool,
    size: VectorSize::Three,
};
pub const TY_KIND_VEC4_U32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::U32,
    size: VectorSize::Four,
};
pub const TY_KIND_VEC4_I32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::I32,
    size: VectorSize::Four,
};
pub const TY_KIND_VEC4_F32: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::F32,
    size: VectorSize::Four,
};
pub const TY_KIND_VEC4_BOOL: TypeKind = TypeKind::Vector {
    scalar: ScalarKind::Bool,
    size: VectorSize::Four,
};
pub const TY_KIND_MAT2X2: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Two,
    columns: VectorSize::Two,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT2X3: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Three,
    columns: VectorSize::Two,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT2X4: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Four,
    columns: VectorSize::Two,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT3X2: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Two,
    columns: VectorSize::Three,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT3X3: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Three,
    columns: VectorSize::Three,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT3X4: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Four,
    columns: VectorSize::Three,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT4X2: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Two,
    columns: VectorSize::Four,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT4X3: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Three,
    columns: VectorSize::Four,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_MAT4X4: TypeKind = TypeKind::Matrix {
    rows: VectorSize::Four,
    columns: VectorSize::Four,
    scalar: ScalarKind::F32,
};
pub const TY_KIND_ATOMIC_U32: TypeKind = TypeKind::Atomic(ScalarKind::U32);
pub const TY_KIND_ATOMIC_I32: TypeKind = TypeKind::Atomic(ScalarKind::I32);
pub const TY_KIND_ATOMIC_F32: TypeKind = TypeKind::Atomic(ScalarKind::F32);
pub const TY_KIND_ATOMIC_BOOL: TypeKind = TypeKind::Atomic(ScalarKind::Bool);
pub const TY_KIND_PREDICATE: TypeKind = TypeKind::Predicate;
pub const TY_KIND_PTR_U32: TypeKind = TypeKind::Ptr(TY_U32);
pub const TY_KIND_DUMMY: TypeKind = TypeKind::Dummy;

pub const TY_U32: Type = Type(TypeInner::U32);
pub const TY_I32: Type = Type(TypeInner::I32);
pub const TY_F32: Type = Type(TypeInner::F32);
pub const TY_BOOL: Type = Type(TypeInner::Bool);
pub const TY_VEC2_U32: Type = Type(TypeInner::Vec2U32);
pub const TY_VEC2_I32: Type = Type(TypeInner::Vec2I32);
pub const TY_VEC2_F32: Type = Type(TypeInner::Vec2F32);
pub const TY_VEC2_BOOL: Type = Type(TypeInner::Vec2Bool);
pub const TY_VEC3_U32: Type = Type(TypeInner::Vec3U32);
pub const TY_VEC3_I32: Type = Type(TypeInner::Vec3I32);
pub const TY_VEC3_F32: Type = Type(TypeInner::Vec3F32);
pub const TY_VEC3_BOOL: Type = Type(TypeInner::Vec3Bool);
pub const TY_VEC4_U32: Type = Type(TypeInner::Vec4U32);
pub const TY_VEC4_I32: Type = Type(TypeInner::Vec4I32);
pub const TY_VEC4_F32: Type = Type(TypeInner::Vec4F32);
pub const TY_VEC4_BOOL: Type = Type(TypeInner::Vec4Bool);
pub const TY_MAT2X2: Type = Type(TypeInner::Mat2x2);
pub const TY_MAT2X3: Type = Type(TypeInner::Mat2x3);
pub const TY_MAT2X4: Type = Type(TypeInner::Mat2x4);
pub const TY_MAT3X2: Type = Type(TypeInner::Mat3x2);
pub const TY_MAT3X3: Type = Type(TypeInner::Mat3x3);
pub const TY_MAT3X4: Type = Type(TypeInner::Mat3x4);
pub const TY_MAT4X2: Type = Type(TypeInner::Mat4x2);
pub const TY_MAT4X3: Type = Type(TypeInner::Mat4x3);
pub const TY_MAT4X4: Type = Type(TypeInner::Mat4x4);
pub const TY_ATOMIC_U32: Type = Type(TypeInner::AtomicU32);
pub const TY_ATOMIC_I32: Type = Type(TypeInner::AtomicI32);
pub const TY_ATOMIC_F32: Type = Type(TypeInner::AtomicF32);
pub const TY_ATOMIC_BOOL: Type = Type(TypeInner::AtomicBool);
pub const TY_PREDICATE: Type = Type(TypeInner::Predicate);
pub const TY_PTR_U32: Type = Type(TypeInner::PtrU32);
pub const TY_DUMMY: Type = Type(TypeInner::Dummy);

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct TypeRegistry {
    #[serde(with = "crate::serde::arc_rwlock")]
    store: Arc<RwLock<IndexSet<Box<TypeKind>>>>,
}

impl TypeRegistry {
    pub fn register(&self, ty_kind: TypeKind) -> Type {
        match &ty_kind {
            TypeKind::Scalar(ScalarKind::I32) => return TY_I32,
            TypeKind::Scalar(ScalarKind::U32) => return TY_U32,
            TypeKind::Scalar(ScalarKind::F32) => return TY_F32,
            TypeKind::Scalar(ScalarKind::Bool) => return TY_BOOL,
            TypeKind::Atomic(ScalarKind::U32) => return TY_ATOMIC_U32,
            TypeKind::Atomic(ScalarKind::I32) => return TY_ATOMIC_I32,
            TypeKind::Atomic(ScalarKind::F32) => return TY_ATOMIC_F32,
            TypeKind::Atomic(ScalarKind::Bool) => return TY_ATOMIC_BOOL,
            TypeKind::Vector {
                scalar: ScalarKind::U32,
                size: VectorSize::Two,
            } => return TY_VEC2_U32,
            TypeKind::Vector {
                scalar: ScalarKind::I32,
                size: VectorSize::Two,
            } => return TY_VEC2_I32,
            TypeKind::Vector {
                scalar: ScalarKind::F32,
                size: VectorSize::Two,
            } => return TY_VEC2_F32,
            TypeKind::Vector {
                scalar: ScalarKind::Bool,
                size: VectorSize::Two,
            } => return TY_VEC2_BOOL,
            TypeKind::Vector {
                scalar: ScalarKind::U32,
                size: VectorSize::Three,
            } => return TY_VEC3_U32,
            TypeKind::Vector {
                scalar: ScalarKind::I32,
                size: VectorSize::Three,
            } => return TY_VEC3_I32,
            TypeKind::Vector {
                scalar: ScalarKind::F32,
                size: VectorSize::Three,
            } => return TY_VEC3_F32,
            TypeKind::Vector {
                scalar: ScalarKind::Bool,
                size: VectorSize::Three,
            } => return TY_VEC3_BOOL,
            TypeKind::Vector {
                scalar: ScalarKind::U32,
                size: VectorSize::Four,
            } => return TY_VEC4_U32,
            TypeKind::Vector {
                scalar: ScalarKind::I32,
                size: VectorSize::Four,
            } => return TY_VEC4_I32,
            TypeKind::Vector {
                scalar: ScalarKind::F32,
                size: VectorSize::Four,
            } => return TY_VEC4_F32,
            TypeKind::Vector {
                scalar: ScalarKind::Bool,
                size: VectorSize::Four,
            } => return TY_VEC4_BOOL,
            TypeKind::Matrix {
                rows: VectorSize::Two,
                columns: VectorSize::Two,
                scalar: ScalarKind::F32,
            } => return TY_MAT2X2,
            TypeKind::Matrix {
                rows: VectorSize::Three,
                columns: VectorSize::Two,
                scalar: ScalarKind::F32,
            } => return TY_MAT2X3,
            TypeKind::Matrix {
                rows: VectorSize::Four,
                columns: VectorSize::Two,
                scalar: ScalarKind::F32,
            } => return TY_MAT2X4,
            TypeKind::Matrix {
                rows: VectorSize::Two,
                columns: VectorSize::Three,
                scalar: ScalarKind::F32,
            } => return TY_MAT3X2,
            TypeKind::Matrix {
                rows: VectorSize::Three,
                columns: VectorSize::Three,
                scalar: ScalarKind::F32,
            } => return TY_MAT3X3,
            TypeKind::Matrix {
                rows: VectorSize::Four,
                columns: VectorSize::Three,
                scalar: ScalarKind::F32,
            } => return TY_MAT3X4,
            TypeKind::Matrix {
                rows: VectorSize::Two,
                columns: VectorSize::Four,
                scalar: ScalarKind::F32,
            } => return TY_MAT4X2,
            TypeKind::Matrix {
                rows: VectorSize::Three,
                columns: VectorSize::Four,
                scalar: ScalarKind::F32,
            } => return TY_MAT4X3,
            TypeKind::Matrix {
                rows: VectorSize::Four,
                columns: VectorSize::Four,
                scalar: ScalarKind::F32,
            } => return TY_MAT4X4,
            TypeKind::Predicate => return TY_PREDICATE,
            TypeKind::Ptr(TY_U32) => return TY_PTR_U32,
            #[cfg(test)]
            TypeKind::Dummy => return TY_DUMMY,
            _ => (),
        }

        // We check if the type kind was already registered via a read lock first. Only if it wasn't
        // already registered, do we request a write lock.
        let read_lock = self.store.read().unwrap_or_else(PoisonError::into_inner);

        let index = if let Some(index) = read_lock.get_index_of(&ty_kind) {
            index
        } else {
            // Make sure we drop the read lock before the try to get the write lock, otherwise this
            // will immediately deadlock.
            mem::drop(read_lock);

            self.store
                .write()
                .unwrap_or_else(PoisonError::into_inner)
                .insert_full(Box::new(ty_kind))
                .0
        };

        Type(TypeInner::Registered(index))
    }

    pub fn kind(&self, ty: Type) -> KindRef {
        match ty.0 {
            TypeInner::U32 => KindRef::from_static(&TY_KIND_U32),
            TypeInner::I32 => KindRef::from_static(&TY_KIND_I32),
            TypeInner::F32 => KindRef::from_static(&TY_KIND_F32),
            TypeInner::Bool => KindRef::from_static(&TY_KIND_BOOL),
            TypeInner::Vec2U32 => KindRef::from_static(&TY_KIND_VEC2_U32),
            TypeInner::Vec2I32 => KindRef::from_static(&TY_KIND_VEC2_I32),
            TypeInner::Vec2F32 => KindRef::from_static(&TY_KIND_VEC2_F32),
            TypeInner::Vec2Bool => KindRef::from_static(&TY_KIND_VEC2_BOOL),
            TypeInner::Vec3U32 => KindRef::from_static(&TY_KIND_VEC3_U32),
            TypeInner::Vec3I32 => KindRef::from_static(&TY_KIND_VEC3_I32),
            TypeInner::Vec3F32 => KindRef::from_static(&TY_KIND_VEC3_F32),
            TypeInner::Vec3Bool => KindRef::from_static(&TY_KIND_VEC3_BOOL),
            TypeInner::Vec4U32 => KindRef::from_static(&TY_KIND_VEC4_U32),
            TypeInner::Vec4I32 => KindRef::from_static(&TY_KIND_VEC4_I32),
            TypeInner::Vec4F32 => KindRef::from_static(&TY_KIND_VEC4_F32),
            TypeInner::Vec4Bool => KindRef::from_static(&TY_KIND_VEC4_BOOL),
            TypeInner::Mat2x2 => KindRef::from_static(&TY_KIND_MAT2X2),
            TypeInner::Mat2x3 => KindRef::from_static(&TY_KIND_MAT2X3),
            TypeInner::Mat2x4 => KindRef::from_static(&TY_KIND_MAT2X4),
            TypeInner::Mat3x2 => KindRef::from_static(&TY_KIND_MAT3X2),
            TypeInner::Mat3x3 => KindRef::from_static(&TY_KIND_MAT3X3),
            TypeInner::Mat3x4 => KindRef::from_static(&TY_KIND_MAT3X4),
            TypeInner::Mat4x2 => KindRef::from_static(&TY_KIND_MAT4X2),
            TypeInner::Mat4x3 => KindRef::from_static(&TY_KIND_MAT4X3),
            TypeInner::Mat4x4 => KindRef::from_static(&TY_KIND_MAT4X4),
            TypeInner::AtomicU32 => KindRef::from_static(&TY_KIND_ATOMIC_U32),
            TypeInner::AtomicI32 => KindRef::from_static(&TY_KIND_ATOMIC_I32),
            TypeInner::AtomicF32 => KindRef::from_static(&TY_KIND_ATOMIC_F32),
            TypeInner::AtomicBool => KindRef::from_static(&TY_KIND_ATOMIC_BOOL),
            TypeInner::Predicate => KindRef::from_static(&TY_KIND_PREDICATE),
            TypeInner::PtrU32 => KindRef::from_static(&TY_KIND_PTR_U32),
            TypeInner::Dummy => KindRef::from_static(&TY_KIND_DUMMY),
            TypeInner::Registered(index) => {
                let store = self.store.read().unwrap_or_else(PoisonError::into_inner);
                let boxed = store.get_index(index).expect("unregistered type");
                let ptr = boxed.as_ref() as *const TypeKind;

                KindRef {
                    ptr,
                    _marker: Default::default(),
                }
            }
        }
    }

    pub fn import(&self, other_reg: &TypeRegistry, ty: Type) -> Type {
        let mut ty_kind = other_reg.kind(ty).deref().clone();

        match &mut ty_kind {
            TypeKind::Array {
                element_ty: base, ..
            } => {
                *base = self.import(other_reg, *base);
            }
            TypeKind::Struct(struct_data) => {
                for field in &mut struct_data.fields {
                    field.ty = self.import(other_reg, field.ty);
                }
            }
            TypeKind::Enum(enum_data) => {
                for variant in &mut enum_data.variants {
                    *variant = self.import(other_reg, *variant);
                }
            }
            _ => (),
        }

        self.register(ty_kind)
    }

    pub fn is_compatible(&self, t0: Type, t1: Type) -> bool {
        if t0 == t1 {
            // A type is always compatible with itself
            return true;
        }

        return true;

        use ScalarKind::*;
        use TypeKind::*;

        let k0 = self.kind(t0);
        let k1 = self.kind(t1);

        match (&*k0, &*k1) {
            // TODO: the predicate compatibilities are a bit of a stop-gap. I think the predicate
            // type itself needs to be rethought.
            (Predicate, Scalar(U32))
            | (Scalar(U32), Predicate)
            | (Predicate, Scalar(Bool))
            | (Scalar(Bool), Predicate) => return true,
            _ => {}
        }

        false
    }
}

pub struct KindRef<'a> {
    ptr: *const TypeKind,
    _marker: PhantomData<&'a TypeKind>,
}

impl KindRef<'static> {
    fn from_static(kind: &'static TypeKind) -> Self {
        KindRef {
            ptr: kind as *const TypeKind,
            _marker: Default::default(),
        }
    }
}

impl Clone for KindRef<'_> {
    fn clone(&self) -> Self {
        KindRef {
            ptr: self.ptr,
            _marker: Default::default(),
        }
    }
}

impl Copy for KindRef<'_> {}

impl AsRef<TypeKind> for KindRef<'_> {
    fn as_ref(&self) -> &TypeKind {
        // SAFETY
        //
        // There are 2 ways a `KindRef` can be created:
        //
        // 1. From a static reference to a `TypeKind`. In this case the `TypeKind` will never drop
        //    and the pointer will always remain valid.
        // 2. By registering with a TypeRegistry. In this case the TyRef's lifetime ensures that the
        //    `TypeRegistry` store with which it is associated cannot have dropped. As the interface
        //    does not expose a mechanism for removing registered types from the store, that
        //    implies that the `Box` the `TyRef`'s pointer points to will also not have dropped.
        //    Therefore, the pointer must still be valid.
        unsafe { &*self.ptr }
    }
}

impl Deref for KindRef<'_> {
    type Target = TypeKind;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

fn check_arith_ty(lhs: Type, rhs: Type) -> Result<Type, ()> {
    match (lhs, rhs) {
        (TY_U32, TY_U32) => Ok(TY_U32),
        (TY_I32, TY_I32) => Ok(TY_I32),
        (TY_F32, TY_F32) => Ok(TY_F32),
        (TY_VEC2_U32, TY_VEC2_U32) => Ok(TY_VEC2_U32),
        (TY_VEC2_U32, TY_U32) => Ok(TY_VEC2_U32),
        (TY_U32, TY_VEC2_U32) => Ok(TY_VEC2_U32),
        (TY_VEC2_I32, TY_VEC2_I32) => Ok(TY_VEC2_I32),
        (TY_VEC2_I32, TY_I32) => Ok(TY_VEC2_I32),
        (TY_I32, TY_VEC2_I32) => Ok(TY_VEC2_I32),
        (TY_VEC2_F32, TY_VEC2_F32) => Ok(TY_VEC2_F32),
        (TY_VEC2_F32, TY_F32) => Ok(TY_VEC2_F32),
        (TY_F32, TY_VEC2_F32) => Ok(TY_VEC2_F32),
        (TY_VEC3_U32, TY_VEC3_U32) => Ok(TY_VEC3_U32),
        (TY_VEC3_U32, TY_U32) => Ok(TY_VEC3_U32),
        (TY_U32, TY_VEC3_U32) => Ok(TY_VEC3_U32),
        (TY_VEC3_I32, TY_VEC3_I32) => Ok(TY_VEC3_I32),
        (TY_VEC3_I32, TY_I32) => Ok(TY_VEC3_I32),
        (TY_I32, TY_VEC3_I32) => Ok(TY_VEC3_I32),
        (TY_VEC3_F32, TY_VEC3_F32) => Ok(TY_VEC3_F32),
        (TY_VEC3_F32, TY_F32) => Ok(TY_VEC3_F32),
        (TY_F32, TY_VEC3_F32) => Ok(TY_VEC3_F32),
        (TY_VEC4_U32, TY_VEC4_U32) => Ok(TY_VEC4_U32),
        (TY_VEC4_U32, TY_U32) => Ok(TY_VEC4_U32),
        (TY_U32, TY_VEC4_U32) => Ok(TY_VEC4_U32),
        (TY_VEC4_I32, TY_VEC4_I32) => Ok(TY_VEC4_I32),
        (TY_VEC4_I32, TY_I32) => Ok(TY_VEC4_I32),
        (TY_I32, TY_VEC4_I32) => Ok(TY_VEC4_I32),
        (TY_VEC4_F32, TY_VEC4_F32) => Ok(TY_VEC4_F32),
        (TY_VEC4_F32, TY_F32) => Ok(TY_VEC4_F32),
        (TY_F32, TY_VEC4_F32) => Ok(TY_VEC4_F32),
        _ => Err(()),
    }
}

fn check_shift_ty(lhs: Type, rhs: Type) -> Result<Type, ()> {
    match (lhs, rhs) {
        (TY_U32, TY_U32) => Ok(TY_U32),
        (TY_I32, TY_U32) => Ok(TY_I32),
        (TY_VEC2_U32, TY_VEC2_U32) => Ok(TY_VEC2_U32),
        (TY_VEC2_I32, TY_VEC2_U32) => Ok(TY_VEC2_I32),
        (TY_VEC3_U32, TY_VEC3_U32) => Ok(TY_VEC3_U32),
        (TY_VEC3_I32, TY_VEC3_U32) => Ok(TY_VEC3_I32),
        (TY_VEC4_U32, TY_VEC4_U32) => Ok(TY_VEC4_U32),
        (TY_VEC4_I32, TY_VEC4_U32) => Ok(TY_VEC4_I32),
        _ => Err(()),
    }
}

impl TypeRegistry {
    /// Checks if the `lhs` and `rhs` operand are of valid types for the given `op`.
    ///
    /// Returns the result type for the operation if the operands are valid, or a string describing
    /// the error otherwise.
    pub fn check_binary_op(
        &self,
        op: BinaryOperator,
        lhs: Type,
        rhs: Type,
    ) -> Result<Type, String> {
        match op {
            BinaryOperator::And | BinaryOperator::Or => self.check_logic_op(op, lhs, rhs),
            BinaryOperator::Add | BinaryOperator::Sub => self.check_add_or_sub_op(op, lhs, rhs),
            BinaryOperator::Mul => self.check_mul_op(lhs, rhs),
            BinaryOperator::Div | BinaryOperator::Mod => self.check_div_or_mod_op(op, lhs, rhs),
            BinaryOperator::Shl | BinaryOperator::Shr => self.check_shift_op(op, lhs, rhs),
            BinaryOperator::Eq | BinaryOperator::NotEq => self.check_eq_op(op, lhs, rhs),
            BinaryOperator::Gt
            | BinaryOperator::GtEq
            | BinaryOperator::Lt
            | BinaryOperator::LtEq => self.check_ord_op(op, lhs, rhs),
        }
    }

    fn check_logic_op(&self, op: BinaryOperator, lhs: Type, rhs: Type) -> Result<Type, String> {
        if lhs == TY_BOOL && rhs == TY_BOOL {
            Ok(TY_BOOL)
        } else {
            Err(format!(
                "the `{}` operator expects boolean operands (lhs: `{}`, rhs: `{}`)",
                op,
                lhs.to_string(self),
                rhs.to_string(self)
            ))
        }
    }

    fn check_add_or_sub_op(
        &self,
        op: BinaryOperator,
        lhs: Type,
        rhs: Type,
    ) -> Result<Type, String> {
        match &*self.kind(lhs) {
            TypeKind::Scalar(kind) if kind.is_numeric() => {
                match &*self.kind(rhs) {
                    TypeKind::Scalar(kind) if kind == kind => Ok(lhs),
                    TypeKind::Vector { scalar, .. } if scalar == kind => Ok(rhs),
                    TypeKind::Matrix { scalar, .. } if scalar == kind => Ok(rhs),
                    _ => Err(format!(
                    "if the left-hand-side operand to the `{}` operator is a `{}` value, then the \
                    right-hand-side value must be a numeric scalar, vector or matrix of the same \
                    (element) type (got `{}`)",
                    op, lhs.to_string(self), rhs.to_string(self))),
                }
            }
            TypeKind::Vector { scalar, size } if scalar.is_numeric() => match &*self.kind(rhs) {
                TypeKind::Scalar(kind) if scalar == kind => Ok(lhs),
                TypeKind::Vector {
                    scalar: other_scalar,
                    size: other_size,
                } if scalar == other_scalar && size == other_size => Ok(lhs),
                _ => Err(format!(
                    "if the left-hand-side operand to the `{}` operator is a `{}` vector, then the \
                    right-hand-side value must be a vector of the same type, or a numeric scalar \
                    that matches the element type of the vector (got `{}`)",
                    op, lhs.to_string(self), rhs.to_string(self))),
            },
            TypeKind::Matrix {
                rows,
                columns,
                scalar,
            } if scalar.is_numeric() => match &*self.kind(rhs) {
                TypeKind::Scalar(kind) if scalar == kind => Ok(lhs),
                TypeKind::Matrix {
                    rows: other_rows,
                    columns: other_columns,
                    scalar: other_scalar,
                } if rows == other_rows && columns == other_columns && scalar == other_scalar => {
                    Ok(lhs)
                }
                _ => Err(format!(
                    "if the left-hand-side operand to the `{}` operator is a `{}` matrix, then the \
                    right-hand-side value must be a matrix of the same size and type, or a numeric \
                    scalar that matches the element type of the matrix (got `{}`)",
                    op, lhs.to_string(self), rhs.to_string(self))),
            },
            _ => {
                Err(format!(
                "the `{}` operator expects a numeric scalar, a numeric vector or a matrix as its \
                left-hand-side operand (got `{}`)",
                op, lhs.to_string(self)))
            }
        }
    }

    fn check_mul_op(&self, lhs: Type, rhs: Type) -> Result<Type, String> {
        match &*self.kind(lhs) {
            TypeKind::Scalar(kind) if kind.is_numeric() => match &*self.kind(rhs) {
                TypeKind::Scalar(kind) if kind == kind => Ok(lhs),
                TypeKind::Vector { scalar, .. } if scalar == kind => Ok(rhs),
                TypeKind::Matrix { scalar, .. } if scalar == kind => Ok(rhs),
                _ => Err(format!(
                    "if the left-hand-side operand to the `*` operator is a `{}` value, then the \
                    right-hand-side value must be a numeric scalar, vector or matrix of the same \
                    (element) type (got `{}`)",
                    lhs.to_string(self),
                    rhs.to_string(self)
                )),
            },
            TypeKind::Vector { scalar, size } if scalar.is_numeric() => match &*self.kind(rhs) {
                TypeKind::Scalar(kind) if scalar == kind => Ok(lhs),
                TypeKind::Vector {
                    scalar: other_scalar,
                    size: other_size,
                } if scalar == other_scalar && size == other_size => Ok(lhs),
                TypeKind::Matrix {
                    rows,
                    columns,
                    scalar: matrix_scalar,
                } if scalar == matrix_scalar && size == rows => {
                    Ok(self.register(TypeKind::Vector {
                        scalar: *scalar,
                        size: *columns,
                    }))
                }
                _ => Err(format!(
                    "if the left-hand-side operand to the `*` operator is a `{}` vector, then the \
                    right-hand-side value must be a vector of the same type, or a numeric scalar \
                    that matches the element type of the vector, or matrix with a matching element \
                    type and a row-size equal to the vector's size (got `{}`)",
                    lhs.to_string(self),
                    rhs.to_string(self)
                )),
            },
            TypeKind::Matrix {
                rows,
                columns,
                scalar,
            } if scalar.is_numeric() => match &*self.kind(rhs) {
                TypeKind::Scalar(kind) if scalar == kind => Ok(lhs),
                TypeKind::Matrix {
                    rows: other_rows,
                    columns: other_columns,
                    scalar: other_scalar,
                } if columns == other_rows && scalar == other_scalar => {
                    Ok(self.register(TypeKind::Matrix {
                        rows: *rows,
                        columns: *other_columns,
                        scalar: *scalar,
                    }))
                }
                _ => Err(format!(
                    "if the left-hand-side operand to the `*` operator is a `{}` matrix, then the \
                    right-hand-side value must be a matrix of a matching element type and a \
                    row-size that matches the matrix's column-size, or a numeric  scalar that \
                    matches the element type of the matrix (got `{}`)",
                    lhs.to_string(self),
                    rhs.to_string(self)
                )),
            },
            _ => Err(format!(
                "the `*` operator expects a numeric scalar, a numeric vector or a matrix as its \
                left-hand-side operand (got `{}`)",
                lhs.to_string(self)
            )),
        }
    }

    fn check_div_or_mod_op(
        &self,
        op: BinaryOperator,
        lhs: Type,
        rhs: Type,
    ) -> Result<Type, String> {
        match &*self.kind(lhs) {
            TypeKind::Scalar(kind) if kind.is_numeric() => match &*self.kind(rhs) {
                TypeKind::Scalar(kind) if kind == kind => Ok(lhs),
                TypeKind::Vector { scalar, .. } if scalar == kind => Ok(rhs),
                TypeKind::Matrix { scalar, .. } if scalar == kind => Ok(rhs),
                _ => Err(format!(
                    "if the left-hand-side operand to the `{}` operator is a `{}` value, then the \
                    right-hand-side value must be a scalar or a vector of the same (element) type \
                    (got `{}`)",
                    op,
                    lhs.to_string(self),
                    rhs.to_string(self)
                )),
            },
            TypeKind::Vector { scalar, size } if scalar.is_numeric() => match &*self.kind(rhs) {
                TypeKind::Scalar(kind) if scalar == kind => Ok(lhs),
                TypeKind::Vector {
                    scalar: other_scalar,
                    size: other_size,
                } if scalar == other_scalar && size == other_size => Ok(lhs),
                _ => Err(format!(
                    "if the left-hand-side operand to the `{}` operator is a `{}` vector, then the \
                    right-hand-side value must be a vector of the same type, or a numeric scalar \
                    that matches the element type of the vector (got `{}`)",
                    op,
                    lhs.to_string(self),
                    rhs.to_string(self)
                )),
            },
            _ => Err(format!(
                "the `{}` operator expects a numeric scalar or a numeric vector as its \
                left-hand-side operand (got `{}`)",
                op,
                lhs.to_string(self)
            )),
        }
    }

    fn check_shift_op(&self, op: BinaryOperator, lhs: Type, rhs: Type) -> Result<Type, String> {
        match &*self.kind(lhs) {
            TypeKind::Scalar(kind) if kind.is_integer() => {
                if rhs == TY_U32 {
                    Ok(lhs)
                } else {
                    Err(format!(
                    "if the left-hand-side operand to the `{}` operator is a `{}` value, then the \
                    right-hand-side value must be a `{}` (got `{}`)",
                    op,
                    lhs.to_string(self),
                    TY_U32.to_string(self),
                        rhs.to_string(self)
                ))
                }
            }
            TypeKind::Vector { scalar, size } if scalar.is_integer() => {
                let expected_rhs = TypeKind::Vector {
                    size: *size,
                    scalar: ScalarKind::U32,
                };

                if &*self.kind(rhs) == &expected_rhs {
                    Ok(lhs)
                } else {
                    Err(format!(
                        "if the left-hand-side operand to the `{}` operator is a `{}` vector, then \
                        the right-hand-side value must be a vector of equal size and element type \
                        `{}` (got `{}`)",
                        op,
                        lhs.to_string(self),
                        TY_U32.to_string(self),
                        rhs.to_string(self)
                    ))
                }
            }
            _ => Err(format!(
                "the `{}` operator expects a integer scalar or an integer vector as its \
                left-hand-side operand (got `{}`)",
                op,
                lhs.to_string(self)
            )),
        }
    }

    fn check_eq_op(&self, op: BinaryOperator, lhs: Type, rhs: Type) -> Result<Type, String> {
        if self.kind(lhs).is_scalar() && lhs == rhs {
            Ok(TY_BOOL)
        } else {
            Err(format!(
                "the `{}` operator expects its operand to be scalar values of the same type (got \
                `{}` and `{}`)",
                op,
                lhs.to_string(self),
                rhs.to_string(self)
            ))
        }
    }

    fn check_ord_op(&self, op: BinaryOperator, lhs: Type, rhs: Type) -> Result<Type, String> {
        if let TypeKind::Scalar(kind) = &*self.kind(lhs)
            && kind.is_numeric()
            && lhs == rhs
        {
            Ok(TY_BOOL)
        } else {
            Err(format!(
                "the `{}` operator expects its operand to be numeric scalar values of the same \
                type (got `{}` and `{}`)",
                op,
                lhs.to_string(self),
                rhs.to_string(self)
            ))
        }
    }
}
