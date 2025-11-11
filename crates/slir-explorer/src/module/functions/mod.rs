use leptos::prelude::*;
use leptos_router::nested_router::Outlet;

pub mod detail;
pub mod list;

pub use self::detail::Detail;
pub use self::list::List;

#[component]
pub fn Functions() -> impl IntoView {
    view! {
        <Outlet/>
    }
}
