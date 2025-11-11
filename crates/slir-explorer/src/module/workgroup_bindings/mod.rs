use leptos::prelude::*;
use leptos_router::nested_router::Outlet;
use slir::WorkgroupBinding;
use slotmap::Key;

mod detail;
mod list;

pub use self::detail::Detail;
pub use self::list::List;

pub fn workgroup_binding_name(binding: WorkgroupBinding) -> String {
    format!("W{}", binding.data().as_ffi())
}

#[component]
pub fn WorkgroupBindings() -> impl IntoView {
    view! {
        <Outlet/>
    }
}
