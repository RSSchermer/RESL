use leptos::prelude::*;
use leptos_router::nested_router::Outlet;
use slir::StorageBinding;
use slotmap::Key;

mod detail;
mod list;

pub use self::detail::Detail;
pub use self::list::List;

pub fn storage_binding_name(binding: StorageBinding) -> String {
    format!("S{}", binding.data().as_ffi())
}

#[component]
pub fn StorageBindings() -> impl IntoView {
    view! {
        <Outlet/>
    }
}
