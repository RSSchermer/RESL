use leptos::prelude::*;
use slotmap::Key;

use crate::module_explorer::cfg_explorer::statement::Statement;
use crate::module_explorer::cfg_explorer::terminator::Terminator;
use crate::module_explorer::cfg_explorer::HighlightSignal;
use crate::module_explorer::ModuleData;

#[component]
pub fn BasicBlock(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    bb: slir::cfg::BasicBlock,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <div class="basic-block">
            <div class="basic-block-header" id=format!("BB{}", bb.data().as_ffi())>
                {format!("BB{}", bb.data().as_ffi())}
            </div>
            <div class="basic-block-body">
            {move || {
                module.read_value().cfg[bb].statements().iter().copied().map(|statement| {
                    view! { <Statement module function statement highlight/> }
                }).collect_view()
            }}
            </div>
            <div class="basic-block-terminator">
            {move || {
                let terminator = module.read_value().cfg[bb].terminator().clone();

                view! { <Terminator module function terminator highlight/> }
            }}
            </div>
        </div>
    }
}
