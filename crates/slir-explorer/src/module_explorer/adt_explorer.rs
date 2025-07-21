use leptos::prelude::*;
use thaw::Link;
use urlencoding::encode as urlencode;

use crate::module_explorer::tpe::Type;
use crate::module_explorer::{ModuleData, ADT_ITEM_LABEL_START};

#[component]
pub fn AdtExplorer(module: StoredValue<ModuleData>, ty: slir::ty::Type) -> impl IntoView {
    match *module.read_value().module.ty.kind(ty) {
        slir::ty::TypeKind::Struct(_) => view! {
            <StructExplorer module ty/>
        }
        .into_any(),
        slir::ty::TypeKind::Enum(_) => view! {
            <EnumExplorer module ty/>
        }
        .into_any(),
        _ => view! {
            <p>"Type is not an ADT"</p>
        }
        .into_any(),
    }
}

#[component]
pub fn StructExplorer(module: StoredValue<ModuleData>, ty: slir::ty::Type) -> impl IntoView {
    let id = ty.registration_id().unwrap_or_default();

    view! {
        <div class="info-page-container">
            <h1>{format!("Struct S_{}", id)}</h1>

            <h2>"Fields:"</h2>

            <ul>
                {module.read_value().module.ty.kind(ty).expect_struct().fields.iter().map(|f| {
                    view! {
                        <li>
                            <Type module ty=f.ty/>
                        </li>
                    }
                }).collect_view()}
            </ul>
        </div>
    }
}

#[component]
pub fn EnumExplorer(module: StoredValue<ModuleData>, ty: slir::ty::Type) -> impl IntoView {
    let id = ty.registration_id().unwrap_or_default();

    view! {
        <div class="info-page-container">
            <h1>{format!("Enum E_{}", id)}</h1>

            <h2>"Variants:"</h2>

            <ul>
                {module.read_value().module.ty.kind(ty).expect_enum().variants.iter().map(|s| {
                    let s = s.registration_id().unwrap_or_default();

                    view! {
                        <li>
                            <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), ADT_ITEM_LABEL_START, s)>
                                {format!("S_{}", s)}
                            </Link>
                        </li>
                    }
                }).collect_view()}
            </ul>
        </div>
    }
}
