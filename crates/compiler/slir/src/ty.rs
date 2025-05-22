use std::fmt;
use std::fmt::Formatter;
use std::ops::{Deref, Index};

use indexmap::IndexSet;
use serde::{Deserialize, Serialize};

use crate::{Function, Module, Struct};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Type(TypeInner);

impl Type {
    pub fn to_string(&self, module: &Module) -> String {
        module.ty[*self].to_string(module)
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
    Dummy,
    Registered(usize),
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
        base: Type,
        stride: u64,
        count: u64,
    },
    Struct(Struct),
    Ptr(Type),
    Function(Function),
    Dummy,
}

impl TypeKind {
    pub fn expect_fn(&self) -> &Function {
        if let TypeKind::Function(function) = self {
            function
        } else {
            panic!("not a function type");
        }
    }

    pub fn is_ptr(&self) -> bool {
        if let TypeKind::Ptr(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_aggregate(&self) -> bool {
        match self {
            TypeKind::Struct(_) | TypeKind::Array { .. } => true,
            _ => false,
        }
    }
}

impl TypeKind {
    fn to_string(&self, module: &Module) -> String {
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
            TypeKind::Array { base, count, .. } => {
                format!("array<{}, {}>", base.to_string(module), count)
            }
            TypeKind::Struct(struct_handle) => format!("Struct_{}", struct_handle.to_usize()),
            TypeKind::Ptr(pointee_ty) => format!("ptr<{}>", pointee_ty.to_string(module)),
            TypeKind::Function(f) => format!("Function_{}_{}", f.module, f.name),
            TypeKind::Dummy => "dummy".to_string(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ScalarKind {
    I32,
    U32,
    F32,
    Bool,
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
pub const TY_DUMMY: Type = Type(TypeInner::Dummy);

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct TypeRegistry {
    store: IndexSet<TypeKind>,
}

impl TypeRegistry {
    pub fn register(&mut self, ty_kind: TypeKind) -> Type {
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
            #[cfg(test)]
            TypeKind::Dummy => return TY_DUMMY,
            _ => (),
        }

        let index = self.store.insert_full(ty_kind).0;

        Type(TypeInner::Registered(index))
    }
}

impl Index<Type> for TypeRegistry {
    type Output = TypeKind;

    fn index(&self, ty: Type) -> &Self::Output {
        match ty.0 {
            TypeInner::U32 => &TY_KIND_U32,
            TypeInner::I32 => &TY_KIND_I32,
            TypeInner::F32 => &TY_KIND_F32,
            TypeInner::Bool => &TY_KIND_BOOL,
            TypeInner::Vec2U32 => &TY_KIND_VEC2_U32,
            TypeInner::Vec2I32 => &TY_KIND_VEC2_I32,
            TypeInner::Vec2F32 => &TY_KIND_VEC2_F32,
            TypeInner::Vec2Bool => &TY_KIND_VEC2_BOOL,
            TypeInner::Vec3U32 => &TY_KIND_VEC3_U32,
            TypeInner::Vec3I32 => &TY_KIND_VEC3_I32,
            TypeInner::Vec3F32 => &TY_KIND_VEC3_F32,
            TypeInner::Vec3Bool => &TY_KIND_VEC3_BOOL,
            TypeInner::Vec4U32 => &TY_KIND_VEC4_U32,
            TypeInner::Vec4I32 => &TY_KIND_VEC4_I32,
            TypeInner::Vec4F32 => &TY_KIND_VEC4_F32,
            TypeInner::Vec4Bool => &TY_KIND_VEC4_BOOL,
            TypeInner::Mat2x2 => &TY_KIND_MAT2X2,
            TypeInner::Mat2x3 => &TY_KIND_MAT2X3,
            TypeInner::Mat2x4 => &TY_KIND_MAT2X4,
            TypeInner::Mat3x2 => &TY_KIND_MAT3X2,
            TypeInner::Mat3x3 => &TY_KIND_MAT3X3,
            TypeInner::Mat3x4 => &TY_KIND_MAT3X4,
            TypeInner::Mat4x2 => &TY_KIND_MAT4X2,
            TypeInner::Mat4x3 => &TY_KIND_MAT4X3,
            TypeInner::Mat4x4 => &TY_KIND_MAT4X4,
            TypeInner::AtomicU32 => &TY_KIND_ATOMIC_U32,
            TypeInner::AtomicI32 => &TY_KIND_ATOMIC_I32,
            TypeInner::AtomicF32 => &TY_KIND_ATOMIC_F32,
            TypeInner::AtomicBool => &TY_KIND_ATOMIC_BOOL,
            TypeInner::Dummy => &TY_KIND_DUMMY,
            TypeInner::Registered(index) => self.store.get_index(index).expect("unregistered type"),
        }
    }
}
