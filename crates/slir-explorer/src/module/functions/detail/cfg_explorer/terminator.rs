use leptos::prelude::*;
use slotmap::Key;

use crate::module::functions::detail::cfg_explorer::value::Value;

#[component]
pub fn Terminator(terminator: slir::cfg::Terminator) -> impl IntoView {
    match terminator {
        slir::cfg::Terminator::Branch(branch) => {
            if let Some(selector) = branch.selector() {
                view! {
                    "branch "<Value value=selector.into()/>": "
                    {move || {
                        let mut bb_views = Vec::new();
                        let mut is_first = true;

                        for bb in branch.targets().iter().copied() {
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
                view! { "branch " {format!("BB{}", branch.targets()[0].data().as_ffi())}}.into_any()
            }
        }
        slir::cfg::Terminator::Return(None) => view! {
            "return"
        }
        .into_any(),
        slir::cfg::Terminator::Return(Some(value)) => view! {
            "return "<Value value/>
        }
        .into_any(),
    }
}
