use std::ops::Deref;

use crate::cfg::Cfg;
use crate::ty::{Type, TypeKind};
use crate::{Function, Module};

pub fn import_type(from: &Module, to: &Module, ty: Type) -> Type {
    let mut ty_kind = from.ty.kind(ty).deref().clone();

    match &mut ty_kind {
        TypeKind::Array {
            element_ty: base, ..
        } => {
            *base = import_type(from, to, *base);
        }
        TypeKind::Struct(struct_data) => {
            for field in &mut struct_data.fields {
                field.ty = import_type(from, to, field.ty);
            }
        }
        TypeKind::Enum(enum_data) => {
            for variant in &mut enum_data.variants {
                *variant = import_type(from, to, *variant);
            }
        }
        _ => (),
    }

    to.ty.register(ty_kind)
}

pub fn import_fn_cfg(from: (&Module, &Cfg), to: (&mut Module, &mut Cfg), function: Function) {
    let (from_module, from_cfg) = from;
    let (to_module, to_cfg) = to;

    let mut sig = from_module.fn_sigs[function].clone();

    sig.ty = to_module.ty.register(TypeKind::Function(function));

    if let Some(ret_ty) = &mut sig.ret_ty {
        *ret_ty = import_type(from_module, to_module, *ret_ty);
    }

    for arg in &mut sig.args {
        arg.ty = import_type(from_module, to_module, arg.ty);
    }

    to_module.fn_sigs.register(function, sig);

    let mut body = from_cfg.function_body[function].clone();

    for (_, value_data) in &mut body.local_values {
        if let Some(ty) = &mut value_data.ty {
            *ty = import_type(from_module, to_module, *ty);
        }
    }

    to_cfg.function_body.insert(function, body);
}
