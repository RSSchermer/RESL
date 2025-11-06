pub mod adt_explorer;
pub mod cfg_explorer;
pub mod function_explorer;
pub mod inner;
pub mod rvsdg_explorer;
pub mod scf_explorer;
pub mod struct_explorer;
pub mod tpe;

use std::fs;
use std::io::Read as IoRead;

use ar::Archive;
use leptos::prelude::*;
use leptos_router::hooks::use_params;
use leptos_router::params::Params;
use slir::Function;
use urlencoding::encode as urlencode;

use crate::app::MODULE_DIR;
use crate::module_explorer::inner::ModuleExplorerInner;

pub const FUNCTION_ITEM_LABEL_START: &'static str = "function-";
pub const ADT_ITEM_LABEL_START: &'static str = "adt-";
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

struct ModuleData {
    pub module: slir::Module,
    pub cfg: slir::cfg::Cfg,
    pub rvsdg_initial: Option<slir::rvsdg::Rvsdg>,
    pub rvsdg_transformed: Option<slir::rvsdg::Rvsdg>,
    pub scf: Option<slir::scf::Scf>,
    pub wgsl: Option<String>,
}

impl ModuleData {
    pub fn expect_scf(&self) -> &slir::scf::Scf {
        self.scf.as_ref().expect("SCF data is not present")
    }
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
                    let mut archive = Archive::new(bytes.as_slice());

                    let mut module = None;
                    let mut cfg = None;
                    let mut rvsdg_initial = None;
                    let mut rvsdg_transformed = None;
                    let mut scf = None;
                    let mut wgsl = None;

                    while let Some(entry_result) = archive.next_entry() {
                        let mut entry = entry_result.unwrap();

                        if entry.header().identifier() == "module".as_bytes() {
                            let decoded: slir::Module = bincode::serde::decode_from_std_read(
                                &mut entry,
                                bincode::config::standard(),
                            )
                            .expect("module encoding was invalid");

                            module = Some(decoded);
                        }

                        if entry.header().identifier() == "cfg".as_bytes() {
                            let decoded: slir::cfg::CfgData = bincode::serde::decode_from_std_read(
                                &mut entry,
                                bincode::config::standard(),
                            )
                            .expect("CFG encoding was invalid");

                            cfg = Some(decoded);
                        }

                        if entry.header().identifier() == "rvsdg_initial".as_bytes() {
                            let decoded: slir::rvsdg::RvsdgData =
                                bincode::serde::decode_from_std_read(
                                    &mut entry,
                                    bincode::config::standard(),
                                )
                                .expect("RSVDG-initial encoding was invalid");

                            rvsdg_initial = Some(decoded);
                        }

                        if entry.header().identifier() == "rvsdg_transformed".as_bytes() {
                            let decoded: slir::rvsdg::RvsdgData =
                                bincode::serde::decode_from_std_read(
                                    &mut entry,
                                    bincode::config::standard(),
                                )
                                .expect("RSVDG-transformed encoding was invalid");

                            rvsdg_transformed = Some(decoded);
                        }

                        if entry.header().identifier() == "scf".as_bytes() {
                            let decoded: slir::scf::ScfData = bincode::serde::decode_from_std_read(
                                &mut entry,
                                bincode::config::standard(),
                            )
                            .expect("SCF encoding was invalid");

                            scf = Some(decoded);
                        }

                        if entry.header().identifier() == "wgsl".as_bytes() {
                            let mut decoded = String::new();

                            entry
                                .read_to_string(&mut decoded)
                                .expect("could not read WGSL");

                            wgsl = Some(decoded);
                        }
                    }

                    let module =
                        module.expect("SLIR arfifact should always contain a `module` entry");
                    let cfg_data = cfg.expect("SLIR arfifact should always contain a `cfg` entry");
                    let cfg = slir::cfg::Cfg::from_ty_and_data(module.ty.clone(), cfg_data);
                    let rvsdg_initial = rvsdg_initial
                        .map(|data| slir::rvsdg::Rvsdg::from_ty_and_data(module.ty.clone(), data));
                    let rvsdg_transformed = rvsdg_transformed
                        .map(|data| slir::rvsdg::Rvsdg::from_ty_and_data(module.ty.clone(), data));
                    let scf =
                        scf.map(|data| slir::scf::Scf::from_ty_and_data(module.ty.clone(), data));

                    ModuleData {
                        module,
                        cfg,
                        rvsdg_initial,
                        rvsdg_transformed,
                        scf,
                        wgsl,
                    }
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
