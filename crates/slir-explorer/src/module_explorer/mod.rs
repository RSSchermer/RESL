pub mod cfg_explorer;
pub mod function_explorer;
pub mod inner;
pub mod struct_explorer;
pub mod tpe;

use std::fs;

use leptos::prelude::*;
use leptos_router::hooks::use_params;
use leptos_router::params::Params;
use slir::Function;
use urlencoding::encode as urlencode;
use crate::app::MODULE_DIR;
use crate::module_explorer::inner::ModuleExplorerInner;

pub type ModuleData = (slir::Module, slir::cfg::Cfg);

pub const FUNCTION_ITEM_LABEL_START: &'static str = "function-";
pub const STRUCT_ITEM_LABEL_START: &'static str = "struct-";
pub const UNIFORM_ITEM_LABEL_START: &'static str = "uniform-";
pub const STORAGE_ITEM_LABEL_START: &'static str = "storage-";
pub const WORKGROUP_ITEM_LABEL_START: &'static str = "workgroup-";

fn format_function_url(module: slir::Symbol, function: Function) -> String {
    let function_segment = urlencode(&format!(
        "{}{}::{}",
        FUNCTION_ITEM_LABEL_START, &function.module, &function.name
    ))
    .into_owned();

    format!("/{}/{}", urlencode(&module), urlencode(&function_segment))
}

#[derive(Params, PartialEq)]
struct ModuleExplorerParams {
    module_name: String,
    item_label: Option<String>,
}

/// Renders the home page of your application.
#[component]
pub fn ModuleExplorer() -> impl IntoView {
    #[server]
    pub async fn get_module_bytes(module_name: String) -> Result<Vec<u8>, ServerFnError> {
        let filename = format!("{}/{}.slir", MODULE_DIR, module_name);

        fs::read(filename).map_err(|e| ServerFnError::ServerError(e.to_string()))
    }

    let params = use_params::<ModuleExplorerParams>();
    let module_name = move || {
        params
            .read()
            .as_ref()
            .ok()
            .map(|params| params.module_name.clone())
            .unwrap_or_default()
    };
    let module_bytes = Resource::new(module_name, |module_name| async {
        get_module_bytes(module_name).await
    });
    let item_label = move || {
        params
            .read()
            .as_ref()
            .ok()
            .and_then(|p| p.item_label.clone())
    };

    let module = move || {
        module_bytes.read().as_ref().map(|bytes| {
            bytes
                .as_ref()
                .map(|bytes| {
                    bincode::serde::decode_from_slice::<(slir::Module, slir::cfg::Cfg), _>(
                        bytes,
                        bincode::config::standard(),
                    )
                    .unwrap()
                    .0
                })
                .map_err(|err| err.clone())
        })
    };

    view! {
        <Suspense
            fallback=move || view! { <p>"Loading..."</p> }
        >
            <ErrorBoundary
                // the fallback receives a signal containing current errors
                fallback=|errors| view! {
                    <div class="info-page-container">
                        <h1>"Error loading module"</h1>

                        <p>"One or more errors occurred while trying to load this module: "</p>
                        <ul>
                            {move || errors.get()
                                .into_iter()
                                .map(|(_, e)| view! { <li>{e.to_string()}</li>})
                                .collect::<Vec<_>>()
                            }
                        </ul>
                    </div>
                }
            >
                {move || module().map(|ok| ok.map(|module| view! {
                    <ModuleExplorerInner module item_label=item_label()/>
                }))}
            </ErrorBoundary>
        </Suspense>
    }
}
