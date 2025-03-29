use leptos::prelude::*;
use leptos_meta::{provide_meta_context, Stylesheet, Title};
use leptos_router::components::{Route, Router, Routes};
use leptos_router::hooks::use_params;
use leptos_router::params::Params;
use leptos_router::path;
use thaw::*;

use crate::module_explorer::ModuleExplorer;
use crate::module_list::ModuleList;

pub const MODULE_DIR: &'static str = "target/debug/deps";

#[derive(Params, PartialEq)]
struct AppParams {
    module_name: Option<String>,
}

#[component]
pub fn App() -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();

    let open_module_list = RwSignal::new(false);

    view! {
        // injects a stylesheet into the document <head>
        // id=leptos means cargo-leptos will hot-reload this stylesheet
        <Stylesheet id="leptos" href="/pkg/slir-explorer.css"/>

        <Title text="SLIR Explorer"/>

        <ConfigProvider>
            <Router>
                <div class="app-container">
                    <nav class="nav-main">
                        <Button on:click=move |_| open_module_list.set(true)><Icon icon=icondata::BiFileFindRegular/> "Select Module"</Button>
                    </nav>

                    <main>
                        <Routes fallback=NotFound>
                            <Route path=path!("/") view=NoModuleSelected/>
                            <Route path=path!("/:module_name") view=ModuleExplorer/>
                            <Route path=path!("/:module_name/:item_label") view=ModuleExplorer/>
                        </Routes>
                    </main>
                </div>

                <ModuleList open=open_module_list/>
            </Router>
        </ConfigProvider>
    }
}

/// Renders the home page of your application.
#[component]
fn NoModuleSelected() -> impl IntoView {
    view! {
        <p>"No module selected"</p>
    }
}

/// 404 - Not Found
#[component]
fn NotFound() -> impl IntoView {
    // set an HTTP status code 404
    // this is feature gated because it can only be done during
    // initial server-side rendering
    // if you navigate to the 404 page subsequently, the status
    // code will not be set because there is not a new HTTP request
    // to the server
    #[cfg(feature = "ssr")]
    {
        // this can be done inline because it's synchronous
        // if it were async, we'd use a server function
        let resp = expect_context::<leptos_actix::ResponseOptions>();
        resp.set_status(actix_web::http::StatusCode::NOT_FOUND);
    }

    view! {
        <h1>"Not Found"</h1>
    }
}
