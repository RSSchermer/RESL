use leptos::prelude::*;

use crate::module::use_module_data;

#[component]
pub fn WgslExplorer() -> impl IntoView {
    let module_data = use_module_data();
    let wgsl = module_data.read_value().wgsl.clone();

    view! {
        {{match wgsl {
            Some(wgsl) => view! {
                <div class="wgsl-container">
                    {wgsl}
                </div>
            }.into_any(),
            None => view! {
                <div class="info-page-container">
                    <h1>"WGSL Not Found"</h1>

                    <p>"No WGSL file exists in this module's SLIR artifact."</p>
                </div>
            }.into_any()
        }}}
    }
}
