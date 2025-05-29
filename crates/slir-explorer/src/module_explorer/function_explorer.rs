use leptos::logging;
use leptos::prelude::*;
use thaw::*;

use crate::module_explorer::cfg_explorer::CfgExplorer;
use crate::module_explorer::rvsdg_explorer::{RvsdgExplorer, RvsdgStage};
use crate::module_explorer::tpe::Type;
use crate::module_explorer::ModuleData;

#[component]
pub fn FunctionExplorer(
    module: StoredValue<ModuleData>,
    function: slir::Function,
) -> impl IntoView {
    let mode = RwSignal::new("cfg".to_string());

    if module.read_value().module.fn_sigs.contains(function) {
        view! {
            <div class="function-explorer-container">
                <div class="function-sig">
                    {function.name.to_string()}
                    "("
                    <span> // Without this span around the args, Leptos produces a hydration error...
                        {move || {
                            let mut arg_views = Vec::new();
                            let mut is_first = true;

                            for arg in &module.read_value().module.fn_sigs[function].args {
                                if !is_first {
                                    arg_views.push(view! {", "}.into_any());
                                }

                                arg_views.push(view!{<Type module ty=arg.ty/>}.into_any());

                                is_first = false;
                            }

                            arg_views
                        }}
                    </span>
                    ")"
                </div>
                <TabList selected_value=mode>
                    <Tab value="cfg">
                        "Control-flow Graph"
                    </Tab>
                    <Tab value="rvsdg-initial">
                        "RVSDG-initial"
                    </Tab>
                    <Tab value="rvsdg-transformed">
                        "RVSDG-transformed"
                    </Tab>
                    <Tab value="scf">
                        "Structured Control-flow"
                    </Tab>
                </TabList>
                <div class="function-body">
                    {move || {
                        if mode.read().as_str() == "cfg" {
                            view! {<CfgExplorer module function/>}.into_any()
                        } else if mode.read().as_str() == "rvsdg-initial" {
                            view! {<RvsdgExplorer module function stage=RvsdgStage::Initial/>}.into_any()
                        } else if mode.read().as_str() == "rvsdg-transformed" {
                            view! {<RvsdgExplorer module function stage=RvsdgStage::Transformed/>}.into_any()
                        } else {
                            view! {}.into_any()
                        }
                    }}
                </div>
            </div>
        }
        .into_any()
    } else {
        view! {
            <div class="info-page-container">
                <h1>"Function not found"</h1>

                <p>"No function signature was registered with this module for the given function name."</p>
            </div>
        }.into_any()
    }
}
