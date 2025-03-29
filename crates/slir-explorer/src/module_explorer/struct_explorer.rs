use leptos::prelude::*;

use crate::module_explorer::ModuleData;

#[component]
pub fn StructExplorer(
    module: StoredValue<ModuleData>,
    struct_handle: slir::Struct,
) -> impl IntoView {
    view! {
        "function"
    }
}
