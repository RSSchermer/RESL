use leptos::prelude::*;

use crate::module_explorer::tpe::Type;
use crate::module_explorer::ModuleData;

#[component]
pub fn StructExplorer(
    module: StoredValue<ModuleData>,
    struct_handle: slir::Struct,
) -> impl IntoView {
    let s = struct_handle.to_usize();

    view! {
        <div class="info-page-container">
            <h1>{format!("Struct S_{}", s)}</h1>

            <h2>"Fields:"</h2>

            <ul>
                {module.read_value().module.structs[struct_handle].fields.iter().map(|f| {
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
