use leptos::prelude::*;
use slir::ty::{ScalarKind, TypeKind};
use thaw::*;
use urlencoding::encode as urlencode;

use crate::module_explorer::{ModuleData, ADT_ITEM_LABEL_START};

#[component]
pub fn Type(module: StoredValue<ModuleData>, ty: slir::ty::Type) -> impl IntoView {
    match &*module.read_value().module.ty.kind(ty) {
        TypeKind::Scalar(scalar) => match scalar {
            ScalarKind::I32 => view! {"i32"}.into_any(),
            ScalarKind::U32 => view! {"u32"}.into_any(),
            ScalarKind::F32 => view! {"f32"}.into_any(),
            ScalarKind::Bool => view! {"bool"}.into_any(),
        },
        TypeKind::Atomic(scalar) => match scalar {
            ScalarKind::I32 => view! {"Atomic<i32>"}.into_any(),
            ScalarKind::U32 => view! {"Atomic<u32>"}.into_any(),
            ScalarKind::F32 => view! {"Atomic<f32>"}.into_any(),
            ScalarKind::Bool => view! {"Atomic<bool>"}.into_any(),
        },
        TypeKind::Vector { scalar, size } => match scalar {
            ScalarKind::I32 => view! {{format!("vec{}<i32>", size.to_u32())}}.into_any(),
            ScalarKind::U32 => view! {{format!("vec{}<u32>", size.to_u32())}}.into_any(),
            ScalarKind::F32 => view! {{format!("vec{}<f32>", size.to_u32())}}.into_any(),
            ScalarKind::Bool => view! {{format!("vec{}<bool>", size.to_u32())}}.into_any(),
        },
        TypeKind::Matrix {
            rows,
            columns,
            scalar,
        } => match scalar {
            ScalarKind::I32 => {
                view! {{format!("mat{}x{}<i32>", columns.to_u32(), rows.to_u32())}}.into_any()
            }
            ScalarKind::U32 => {
                view! {{format!("mat{}x{}<u32>", columns.to_u32(), rows.to_u32())}}.into_any()
            }
            ScalarKind::F32 => {
                view! {{format!("mat{}x{}<f32>", columns.to_u32(), rows.to_u32())}}.into_any()
            }
            ScalarKind::Bool => {
                view! {{format!("mat{}x{}<bool>", columns.to_u32(), rows.to_u32())}}.into_any()
            }
        },
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
