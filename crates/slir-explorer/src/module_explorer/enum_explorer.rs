use leptos::prelude::*;
use thaw::Link;
use urlencoding::encode as urlencode;

use crate::module_explorer::{ModuleData, STRUCT_ITEM_LABEL_START};

#[component]
pub fn EnumExplorer(module: StoredValue<ModuleData>, enum_handle: slir::Enum) -> impl IntoView {
    let e = enum_handle.to_usize();

    view! {
        <div class="info-page-container">
            <h1>{format!("Enum E_{}", e)}</h1>

            <h2>"Variants:"</h2>

            <ul>
                {module.read_value().module.enums[enum_handle].variants.iter().map(|s| {
                    let s = s.to_usize();

                    view! {
                        <li>
                            <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), STRUCT_ITEM_LABEL_START, s)>
                                {format!("S_{}", s)}
                            </Link>
                        </li>
                    }
                }).collect_view()}
            </ul>
        </div>
    }
}
