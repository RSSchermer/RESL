use leptos::prelude::*;
use slir::ty::{ScalarKind, TypeKind};
use thaw::*;
use urlencoding::encode as urlencode;

use crate::module_explorer::{ModuleData, ADT_ITEM_LABEL_START};

#[component]
pub fn Type(module: StoredValue<ModuleData>, ty: slir::ty::Type) -> impl IntoView {
    match &*module.read_value().module.ty.kind(ty) {
        TypeKind::Scalar(s) => view! {{s.to_string()}}.into_any(),
        TypeKind::Atomic(s) => view! {{format!("atomic<{}>", s)}}.into_any(),
        TypeKind::Vector(v) => view! {{v.to_string()}}.into_any(),
        TypeKind::Matrix(m) => view! {{m.to_string()}}.into_any(),
        TypeKind::Array { element_ty, count } => {
            view! { "array<" <Type module ty=*element_ty/> ", " {*count} ">" }.into_any()
        }
        TypeKind::Slice { element_ty } => {
            view! { "array<" <Type module ty=*element_ty/> ">" }.into_any()
        }
        TypeKind::Struct(_) => {
            let id = ty.registration_id().unwrap_or_default();

            view! {
                <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), ADT_ITEM_LABEL_START, id)>
                    {format!("S_{}", id)}
                </Link>
            }.into_any()
        }
        TypeKind::Enum(_) => {
            let id = ty.registration_id().unwrap_or_default();

            view! {
                <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), ADT_ITEM_LABEL_START, id)>
                    {format!("E_{}", id)}
                </Link>
            }.into_any()
        }
        TypeKind::Ptr(pointee_ty) => view! {"ptr<" <Type module ty=*pointee_ty/> ">"}.into_any(),
        TypeKind::Function(_) => view! {"fn"}.into_any(),
        TypeKind::Predicate => view! {"predicate"}.into_any(),
        TypeKind::Dummy => view! {"dummy"}.into_any(),
    }
}
