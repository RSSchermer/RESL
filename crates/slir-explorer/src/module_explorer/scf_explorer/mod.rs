use leptos::prelude::*;
use leptos::{component, view, IntoView};

use crate::module_explorer::scf_explorer::local_binding::LocalBinding;
use crate::module_explorer::scf_explorer::statement::Statement;
use crate::module_explorer::ModuleData;

pub mod block;
pub mod expression;
pub mod local_binding;
pub mod statement;

type HighlightSignal = (
    ReadSignal<Option<slir::scf::LocalBinding>>,
    WriteSignal<Option<slir::scf::LocalBinding>>,
);

#[component]
pub fn ScfExplorer(module: StoredValue<ModuleData>, function: slir::Function) -> impl IntoView {
    let highlight = signal(None);

    let m = module.read_value();
    let body = m
        .scf
        .as_ref()
        .and_then(|scf| scf.get_function_body(function).cloned());

    if let Some(body) = body {
        let body_block = body.block();

        view! {
            <div class="scf-argument-bindings">
                <h3>Argument Bindings</h3>
                <ul>
                    {move || {
                        body.argument_bindings().iter().map(|binding| view! {
                            <li><LocalBinding module binding=*binding highlight/></li>
                        }).collect_view()
                    }}
                </ul>
            </div>
            <div>
                <h3>Function Body</h3>

                <div class="scf-function-body">
                    {move || {
                        let m = module.read_value();
                        let block = &m.expect_scf()[body_block];

                        block.statements().iter().map(|stmt| {
                            view! { <Statement module statement=*stmt highlight/> }
                        }).collect_view()
                    }}
                </div>
            </div>
        }
        .into_any()
    } else {
        view! {
            <div class="info-page-container">
                <p>"No SCF for the current function."</p>
            </div>
        }
        .into_any()
    }
}
