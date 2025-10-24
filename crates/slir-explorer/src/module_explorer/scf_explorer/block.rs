use leptos::prelude::*;
use leptos::{component, IntoView};

use crate::module_explorer::scf_explorer::expression::Expression;
use crate::module_explorer::scf_explorer::local_binding::LocalBinding;
use crate::module_explorer::scf_explorer::statement::Statement;
use crate::module_explorer::scf_explorer::HighlightSignal;
use crate::module_explorer::ModuleData;

#[component]
pub fn Block(
    module: StoredValue<ModuleData>,
    block: slir::scf::Block,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let scf_data = module_data.scf.as_ref().unwrap();
    let block_data = &scf_data[block];

    view! {
        <div class="scf-block scf-indent">
            {move || {
                let m = module.read_value();
                let block = &m.expect_scf()[block];

                block.statements().iter().map(|stmt| {
                    view! { <Statement module statement=*stmt highlight/> }
                }).collect_view()
            }}
            {move || {
                let m = module.read_value();
                let block = &m.expect_scf()[block];

                block.control_flow_var_iter().map(|(var, value)| {
                    view! {
                        <LocalBinding module binding=var highlight />
                        " = "<LocalBinding module binding=value highlight />";"
                    }
                }).collect_view()
            }}
        </div>
    }
}
