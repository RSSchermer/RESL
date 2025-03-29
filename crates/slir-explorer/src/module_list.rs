use std::fs;

use leptos::prelude::*;
use thaw::*;
use urlencoding::encode as urlencode;

use crate::app::MODULE_DIR;

#[component]
pub fn ModuleList(open: RwSignal<bool>) -> impl IntoView {
    #[server]
    pub async fn get_module_list() -> Result<Vec<String>, ServerFnError> {
        match fs::read_dir(MODULE_DIR) {
            Ok(entries) => {
                let mut names = Vec::new();

                for entry in entries {
                    match entry {
                        Ok(entry) => {
                            let path = entry.path();

                            if path.extension().map(|e| e == "slir").unwrap_or(false) {
                                let module_name = path
                                    .as_path()
                                    .file_stem()
                                    .and_then(|n| n.to_str())
                                    .unwrap_or("")
                                    .to_string();

                                names.push(module_name);
                            }
                        }
                        Err(err) => {
                            return Err(ServerFnError::ServerError(err.to_string()));
                        }
                    }
                }

                names.sort();

                Ok(names)
            }
            Err(err) => Err(ServerFnError::ServerError(err.to_string())),
        }
    }

    let modules = Resource::new(|| (), |_| async move { get_module_list().await.unwrap() });

    view! {
        <OverlayDrawer open position={DrawerPosition::Left}>
            <DrawerHeader>
                <DrawerHeaderTitle>
                    <DrawerHeaderTitleAction slot>
                        <Button
                            appearance=ButtonAppearance::Subtle
                            on_click=move |_| open.set(false)
                        >
                            "x"
                        </Button>
                    </DrawerHeaderTitleAction>
                    {MODULE_DIR}
                </DrawerHeaderTitle>
            </DrawerHeader>
            <DrawerBody>
                <Suspense
                    fallback=move || view! { <p>"Loading..."</p> }
                >
                    <ul>
                        {move || {
                            modules.get().map(|modules| {
                                modules.into_iter().map(|module| view!{
                                    <li>
                                        <a on:click=move |_| open.set(false) href=format!("/{}", urlencode(&module))>
                                            {module.clone()}
                                        </a>
                                    </li>
                                }).collect_view()
                            })
                        }}
                    </ul>
                </Suspense>
            </DrawerBody>
        </OverlayDrawer>
    }
}
