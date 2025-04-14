use leptos::prelude::*;
use slotmap::Key;

use crate::module_explorer::cfg_explorer::value::Value;
use crate::module_explorer::cfg_explorer::HighlightSignal;
use crate::module_explorer::ModuleData;

#[component]
pub fn Terminator(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    terminator: slir::cfg::Terminator,
    highlight: HighlightSignal,
) -> impl IntoView {
    match terminator {
        slir::cfg::Terminator::Branch(branch) => {
            if let Some(selector) = branch.selector {
                view! {
                    "branch "<Value module function value=selector.into() highlight/>": "
                    {move || {
                        let mut bb_views = Vec::new();
                        let mut is_first = true;

                        for bb in branch.branches.iter().copied() {
                            if !is_first {
                                bb_views.push(view! {", "}.into_any());
                            }

                            bb_views.push(view!{
                                <a href=format!("#BB{}", bb.data().as_ffi())>
                                    {format!("BB{}", bb.data().as_ffi())}
                                </a>
                            }.into_any());

                            is_first = false;
                        }

                        bb_views
                    }}
                }
                .into_any()
            } else {
                view! { "branch " {format!("BB{}", branch.branches[0].data().as_ffi())}}.into_any()
            }
        }
        slir::cfg::Terminator::Return(None) => view! {
            "return"
        }
        .into_any(),
        slir::cfg::Terminator::Return(Some(value)) => view! {
            "return "<Value module function value highlight/>
        }
        .into_any(),
    }
}
