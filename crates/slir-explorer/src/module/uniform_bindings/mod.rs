use leptos::prelude::*;
use leptos_router::nested_router::Outlet;
use slir::UniformBinding;
use slotmap::Key;

mod detail;
mod list;

pub use self::detail::Detail;
pub use self::list::List;

pub fn uniform_binding_name(binding: UniformBinding) -> String {
    format!("U{}", binding.data().as_ffi())
}

#[component]
pub fn UniformBindings() -> impl IntoView {
    view! {
        <Outlet/>
    }
}
