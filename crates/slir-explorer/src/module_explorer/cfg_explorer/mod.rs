pub mod basic_block;
pub mod statement;
pub mod terminator;
pub mod value;

use leptos::prelude::*;

use crate::module_explorer::cfg_explorer::basic_block::BasicBlock;
use crate::module_explorer::cfg_explorer::value::Value;
use crate::module_explorer::ModuleData;

type HighlightSignal = (
    ReadSignal<Option<slir::cfg::LocalBinding>>,
    WriteSignal<Option<slir::cfg::LocalBinding>>,
);

#[component]
pub fn CfgExplorer(module: StoredValue<ModuleData>, function: slir::Function) -> impl IntoView {
    let highlight = signal(None);

    view! {
        <div class="params">
            <div class="param-list-header">
                Function Parameters
            </div>
            <ul class="param-list">
                {move || {
                    module.read_value().cfg[function].argument_values().iter().map(|p| view! {
                        <li><Value module function value=(*p).into() highlight/></li>
                    }).collect_view()
                }}
            </ul>
        </div>

        {move || {
            module.read_value().cfg[function].basic_blocks().iter().copied().map(|bb| view! {
                <BasicBlock module function bb highlight/>
            }).collect_view()
        }}
    }
}
