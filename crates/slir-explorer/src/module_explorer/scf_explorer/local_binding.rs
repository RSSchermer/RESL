use leptos::prelude::*;
use leptos::{component, view, IntoView};
use slotmap::Key;
use thaw::{Popover, PopoverPosition, PopoverTrigger};

use crate::module_explorer::scf_explorer::HighlightSignal;
use crate::module_explorer::tpe::Type;
use crate::module_explorer::ModuleData;

#[component]
pub fn LocalBinding(
    module: StoredValue<ModuleData>,
    binding: slir::scf::LocalBinding,
    highlight: HighlightSignal,
) -> impl IntoView {
    let ty = module.read_value().scf.as_ref().unwrap()[binding].ty();
    let (get_highlight, set_highlight) = highlight;

    let is_highlighted = move || get_highlight.get() == Some(binding);

    let update_highlight = move |_| {
        set_highlight.update(|v| {
            if *v == Some(binding) {
                *v = None;
            } else {
                *v = Some(binding);
            }
        })
    };

    view! {
        <Popover position=PopoverPosition::Bottom>
            <PopoverTrigger slot>
                <span class="scf-local-binding" on:click=update_highlight class:highlighted=is_highlighted>
                    {format!("L{}", binding.data().as_ffi())}
                </span>
            </PopoverTrigger>

            <Type module ty/>
        </Popover>
    }
}
