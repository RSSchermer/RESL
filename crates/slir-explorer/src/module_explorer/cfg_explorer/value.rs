use leptos::prelude::*;
use slir::cfg::Value;
use slotmap::Key;
use thaw::{Popover, PopoverPosition, PopoverTrigger};

use crate::module_explorer::cfg_explorer::HighlightSignal;
use crate::module_explorer::tpe::Type;
use crate::module_explorer::ModuleData;

#[component]
pub fn Value(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    value: slir::cfg::Value,
    highlight: HighlightSignal,
) -> impl IntoView {
    match value {
        Value::Local(value) => view! {
            <LocalValue module function value=value highlight=highlight/>
        }
        .into_any(),
        Value::InlineConst(value) => view! {
            <InlineConst module function value=value highlight=highlight/>
        }
        .into_any(),
        Value::Const => unimplemented!(),
    }
}

#[component]
fn LocalValue(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    value: slir::cfg::LocalValue,
    highlight: HighlightSignal,
) -> impl IntoView {
    let (get_highlight, set_highlight) = highlight;

    let is_highlighted_value = move || get_highlight.get() == Some(value);

    let update_highlighted_value = move |_| {
        set_highlight.update(|v| {
            if *v == Some(value) {
                *v = None;
            } else {
                *v = Some(value);
            }
        })
    };

    view! {
        <Popover position=PopoverPosition::Bottom>
            <PopoverTrigger slot>
                <span class="cfg-value" on:click=update_highlighted_value class:highlighted=is_highlighted_value>
                    {format!("V{}", value.data().as_ffi())}
                </span>
            </PopoverTrigger>
            {move || {
                if let Some(ty) = module.read_value().cfg.function_body[function].local_values[value].ty {
                    view! { <Type module ty/> }.into_any()
                } else {
                    view! { "Untyped" }.into_any()
                }
            }}
        </Popover>
    }
}

#[component]
fn InlineConst(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    value: slir::cfg::InlineConst,
    highlight: HighlightSignal,
) -> impl IntoView {
    match value {
        slir::cfg::InlineConst::U32(v) => view! {{format!("{}u32", v)}}.into_any(),
        slir::cfg::InlineConst::I32(v) => view! {{format!("{}i32", v)}}.into_any(),
        slir::cfg::InlineConst::F32(v) => view! {{format!("{}f32", v)}}.into_any(),
        slir::cfg::InlineConst::Bool(v) => view! {{format!("{}", v)}}.into_any(),
        slir::cfg::InlineConst::Ptr(v) => view! {
            "&"
            <RootIdentifier module function=function root_identifier=v.base highlight=highlight/>
            ":"
            {format!("{}", v.offset)}
            "("
            <Type module ty=v.pointee_ty/>
            ")"
        }
        .into_any(),
    }
}

#[component]
fn RootIdentifier(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    root_identifier: slir::cfg::RootIdentifier,
    highlight: HighlightSignal,
) -> impl IntoView {
    match root_identifier {
        slir::cfg::RootIdentifier::Local(v) => {
            view! { <Value module function value=v.into() highlight/> }.into_any()
        }
        slir::cfg::RootIdentifier::Uniform(v) => {
            view! {{format!("U{}", v.data().as_ffi())}}.into_any()
        }
        slir::cfg::RootIdentifier::Storage(v) => {
            view! {{format!("S{}", v.data().as_ffi())}}.into_any()
        }
        slir::cfg::RootIdentifier::Workgroup(v) => {
            view! {{format!("W{}", v.data().as_ffi())}}.into_any()
        }
    }
}
