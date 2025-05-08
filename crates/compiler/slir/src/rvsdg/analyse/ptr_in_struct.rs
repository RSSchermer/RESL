use rustc_hash::FxHashSet;

use crate::ty::{Type, TypeKind};
use crate::{Module, Struct};

fn ptr_in_ty(module: &Module, ty: Type) -> bool {
    match &module.ty[ty] {
        TypeKind::Ptr(_) => true,
        TypeKind::Array { base, .. } => ptr_in_ty(module, *base),
        TypeKind::Struct(struct_handle) => ptr_in_struct(module, *struct_handle),
        _ => false,
    }
}

pub fn ptr_in_struct(module: &Module, struct_handle: Struct) -> bool {
    for field in &module.structs[struct_handle].fields {
        if ptr_in_ty(module, field.ty) {
            return true;
        }
    }

    false
}
