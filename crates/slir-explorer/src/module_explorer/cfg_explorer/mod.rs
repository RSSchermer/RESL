pub mod basic_block;
pub mod instruction;
pub mod terminator;
pub mod value;

use leptos::prelude::*;

use crate::module_explorer::cfg_explorer::basic_block::BasicBlock;
use crate::module_explorer::cfg_explorer::value::Value;
use crate::module_explorer::ModuleData;

type HighlightSignal = (
    ReadSignal<Option<slir::cfg::Value>>,
    WriteSignal<Option<slir::cfg::Value>>,
);

#[component]
pub fn CfgExplorer(module: StoredValue<ModuleData>, function: slir::Function) -> impl IntoView {
    let highlight = signal(None);

    view! {
        <div class="ret">
            "Return value: "
            <Value module function value=module.read_value().1.function_body[function].ret.into() highlight/>
        </div>
        <div class="params">
            <div class="param-list-header">
                Function Parameters
            </div>
            <ul class="param-list">
                {move || {
                    module.read_value().1.function_body[function].params.iter().map(|p| view! {
                        <li><Value module function value=(*p).into() highlight/></li>
                    }).collect_view()
                }}
            </ul>
        </div>

        {move || {
            module.read_value().1.function_body[function].basic_blocks.keys().map(|bb| view! {
                <BasicBlock module function bb highlight/>
            }).collect_view()
        }}
    }
}
