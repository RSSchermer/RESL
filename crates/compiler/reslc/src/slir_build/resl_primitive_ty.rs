use rustc_middle::bug;
use stable_mir::crate_def::Attribute;
use stable_mir::ty::{RigidTy, Ty, TyKind};
use stable_mir::CrateDef;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReslPrimitiveTy {
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

impl ReslPrimitiveTy {
    pub fn from_ty(ty: Ty) -> Option<Self> {
        let TyKind::RigidTy(RigidTy::Adt(def, _)) = ty.kind() else {
            return None;
        };

        def.attrs_by_path(&["reslc".into(), "primitive".into()])
            .first()
            .map(resolve_primitive_ty)
    }
}

fn resolve_primitive_ty(attr: &Attribute) -> ReslPrimitiveTy {
    match attr.as_str() {
        "#[reslc::primitive(vec2_f32)]" => ReslPrimitiveTy::Vec2F32,
        "#[reslc::primitive(vec2_u32)]" => ReslPrimitiveTy::Vec2U32,
        "#[reslc::primitive(vec2_i32)]" => ReslPrimitiveTy::Vec2I32,
        "#[reslc::primitive(vec2_bool)]" => ReslPrimitiveTy::Vec2Bool,
        "#[reslc::primitive(vec3_f32)]" => ReslPrimitiveTy::Vec3F32,
        "#[reslc::primitive(vec3_u32)]" => ReslPrimitiveTy::Vec3U32,
        "#[reslc::primitive(vec3_i32)]" => ReslPrimitiveTy::Vec3I32,
        "#[reslc::primitive(vec3_bool)]" => ReslPrimitiveTy::Vec3Bool,
        "#[reslc::primitive(vec4_f32)]" => ReslPrimitiveTy::Vec4F32,
        "#[reslc::primitive(vec4_u32)]" => ReslPrimitiveTy::Vec4U32,
        "#[reslc::primitive(vec4_i32)]" => ReslPrimitiveTy::Vec4I32,
        "#[reslc::primitive(vec4_bool)]" => ReslPrimitiveTy::Vec4Bool,
        "#[reslc::primitive(mat2x2_f32)]" => ReslPrimitiveTy::Mat2x2F32,
        "#[reslc::primitive(mat2x3_f32)]" => ReslPrimitiveTy::Mat2x3F32,
        "#[reslc::primitive(mat2x4_f32)]" => ReslPrimitiveTy::Mat2x4F32,
        "#[reslc::primitive(mat3x2_f32)]" => ReslPrimitiveTy::Mat3x2F32,
        "#[reslc::primitive(mat3x3_f32)]" => ReslPrimitiveTy::Mat3x3F32,
        "#[reslc::primitive(mat3x4_f32)]" => ReslPrimitiveTy::Mat3x4F32,
        "#[reslc::primitive(mat4x2_f32)]" => ReslPrimitiveTy::Mat4x2F32,
        "#[reslc::primitive(mat4x3_f32)]" => ReslPrimitiveTy::Mat4x3F32,
        "#[reslc::primitive(mat4x4_f32)]" => ReslPrimitiveTy::Mat4x4F32,
        _ => bug!("unknown reslc primitive type: {}", attr.as_str()),
    }
}
