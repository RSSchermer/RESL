use crate::cfg::Cfg;
use crate::ty::{Type, TypeKind};
use crate::{Function, Module, Struct};

pub fn import_type(from: &Module, to: &mut Module, ty: Type) -> Type {
    let mut ty_kind = from.ty[ty].clone();

    match &mut ty_kind {
        TypeKind::Array { base, .. } => {
            *base = import_type(from, to, *base);
        }
        TypeKind::Struct(s) => {
            *s = import_struct(from, to, *s);
        }
        _ => (),
    }

    to.ty.register(ty_kind)
}

pub fn import_struct(from: &Module, to: &mut Module, struct_handle: Struct) -> Struct {
    let mut struct_data = from.structs[struct_handle].clone();

    for field in &mut struct_data.fields {
        field.ty = import_type(from, to, field.ty);
    }

    to.structs.register(struct_data)
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
