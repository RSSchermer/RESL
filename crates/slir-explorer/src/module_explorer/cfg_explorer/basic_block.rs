use leptos::prelude::*;
use slotmap::Key;

use crate::module_explorer::cfg_explorer::instruction::Instruction;
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
                module.read_value().1.function_body[function].basic_blocks[bb].statements.iter().map(|instruction| {
                    view! { <Instruction module function instruction=instruction.clone() highlight/> }
                }).collect_view()
            }}
            </div>
            <div class="basic-block-terminator">
            {move || {
                let terminator = module.read_value().1.function_body[function].basic_blocks[bb].terminator.clone();

                view! { <Terminator module function terminator highlight/> }
            }}
            </div>
        </div>
    }
}
