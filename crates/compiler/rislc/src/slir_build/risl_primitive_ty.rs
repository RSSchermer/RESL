use rustc_middle::bug;
use rustc_public::CrateDef;
use rustc_public::crate_def::Attribute;
use rustc_public::ty::{RigidTy, Ty, TyKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RislPrimitiveTy {
    Vec2F32,
    Vec2U32,
    Vec2I32,
    Vec2Bool,
    Vec3F32,
    Vec3U32,
    Vec3I32,
    Vec3Bool,
    Vec4F32,
    Vec4U32,
    Vec4I32,
    Vec4Bool,
    Mat2x2F32,
    Mat2x3F32,
    Mat2x4F32,
    Mat3x2F32,
    Mat3x3F32,
    Mat3x4F32,
    Mat4x2F32,
    Mat4x3F32,
    Mat4x4F32,
}

impl RislPrimitiveTy {
    pub fn from_ty(ty: Ty) -> Option<Self> {
        let TyKind::RigidTy(RigidTy::Adt(def, _)) = ty.kind() else {
            return None;
        };

        def.tool_attrs(&["rislc".into(), "primitive".into()])
            .first()
            .map(resolve_primitive_ty)
    }
}

fn resolve_primitive_ty(attr: &Attribute) -> RislPrimitiveTy {
    match attr.as_str().trim() {
        "#[rislc::primitive(vec2_f32)]" => RislPrimitiveTy::Vec2F32,
        "#[rislc::primitive(vec2_u32)]" => RislPrimitiveTy::Vec2U32,
        "#[rislc::primitive(vec2_i32)]" => RislPrimitiveTy::Vec2I32,
        "#[rislc::primitive(vec2_bool)]" => RislPrimitiveTy::Vec2Bool,
        "#[rislc::primitive(vec3_f32)]" => RislPrimitiveTy::Vec3F32,
        "#[rislc::primitive(vec3_u32)]" => RislPrimitiveTy::Vec3U32,
        "#[rislc::primitive(vec3_i32)]" => RislPrimitiveTy::Vec3I32,
        "#[rislc::primitive(vec3_bool)]" => RislPrimitiveTy::Vec3Bool,
        "#[rislc::primitive(vec4_f32)]" => RislPrimitiveTy::Vec4F32,
        "#[rislc::primitive(vec4_u32)]" => RislPrimitiveTy::Vec4U32,
        "#[rislc::primitive(vec4_i32)]" => RislPrimitiveTy::Vec4I32,
        "#[rislc::primitive(vec4_bool)]" => RislPrimitiveTy::Vec4Bool,
        "#[rislc::primitive(mat2x2_f32)]" => RislPrimitiveTy::Mat2x2F32,
        "#[rislc::primitive(mat2x3_f32)]" => RislPrimitiveTy::Mat2x3F32,
        "#[rislc::primitive(mat2x4_f32)]" => RislPrimitiveTy::Mat2x4F32,
        "#[rislc::primitive(mat3x2_f32)]" => RislPrimitiveTy::Mat3x2F32,
        "#[rislc::primitive(mat3x3_f32)]" => RislPrimitiveTy::Mat3x3F32,
        "#[rislc::primitive(mat3x4_f32)]" => RislPrimitiveTy::Mat3x4F32,
        "#[rislc::primitive(mat4x2_f32)]" => RislPrimitiveTy::Mat4x2F32,
        "#[rislc::primitive(mat4x3_f32)]" => RislPrimitiveTy::Mat4x3F32,
        "#[rislc::primitive(mat4x4_f32)]" => RislPrimitiveTy::Mat4x4F32,
        _ => bug!("unknown rislc primitive type: {}", attr.as_str()),
    }
}
