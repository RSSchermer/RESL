use std::ops::Deref;
use std::sync::LazyLock;

use leptos::prelude::*;
use slir::cfg::{InlineConst, RootIdentifier, Statement, Value};
use slir::ty::{ScalarKind, TypeKind};
use slotmap::Key;
use thaw::*;
use urlencoding::encode as urlencode;

use crate::module_explorer::tpe::Type;
use crate::module_explorer::{
    format_function_url, ModuleData, FUNCTION_ITEM_LABEL_START, STRUCT_ITEM_LABEL_START,
};

type HighlightSignal = (
    ReadSignal<Option<slir::cfg::Value>>,
    WriteSignal<Option<slir::cfg::Value>>,
);

#[component]
pub fn CfgExplorer(module: StoredValue<ModuleData>, function: slir::Function) -> impl IntoView {
    let highlight = signal(None);

    view! {
        <div class="ret">
            "Return value: "
            <Value module function value=module.read_value().1.function_body[function].ret.into() highlight/>
        </div>
        <div class="params">
            <div class="param-list-header">
                Function Parameters
            </div>
            <ul class="param-list">
                {move || {
                    module.read_value().1.function_body[function].params.iter().map(|p| view! {
                        <li><Value module function value=(*p).into() highlight/></li>
                    }).collect_view()
                }}
            </ul>
        </div>

        {move || {
            module.read_value().1.function_body[function].basic_blocks.keys().map(|bb| view! {
                <BasicBlock module function bb highlight/>
            }).collect_view()
        }}
    }
}

#[component]
pub fn BasicBlock(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    bb: slir::cfg::BasicBlock,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <div class="basic-block">
            <div class="basic-block-header" id=format!("BB{}", bb.data().as_ffi())>
                {format!("BB{}", bb.data().as_ffi())}
            </div>
            <div class="basic-block-body">
            {move || {
                module.read_value().1.function_body[function].basic_blocks[bb].statements.iter().map(|instruction| {
                    view! { <Instruction module function instruction=instruction.clone() highlight/> }
                }).collect_view()
            }}
            </div>
            <div class="basic-block-terminator">
            {move || {
                let terminator = module.read_value().1.function_body[function].basic_blocks[bb].terminator.clone();

                view! { <Terminator module function terminator highlight/> }
            }}
            </div>
        </div>
    }
}

#[component]
pub fn Instruction(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    instruction: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let inner = match instruction {
        slir::cfg::Statement::OpAlloca(op) => view! {
            <Value module function value=op.result.into() highlight/> " = alloca"
        }.into_any(),
        slir::cfg::Statement::OpAssign(op) => view! {
            <Value module function value=op.result.into() highlight/>
            " = "
            <Value module function value=op.value highlight/>
        }.into_any(),
        slir::cfg::Statement::OpLoad(op) => view! {
            <Value module function value=op.result.into() highlight/> " = "
                "load "<Value module function value=op.ptr highlight/>
        }.into_any(),
        slir::cfg::Statement::OpStore(op) => view! {
            "store "<Value module function value=op.value highlight/>" into "<Value module function value=op.ptr highlight/>
        }.into_any(),
        slir::cfg::Statement::OpPtrElementPtr(op) => view! {
            <Value module function value=op.result.into() highlight/> " = (&*"<Value module function value=op.ptr highlight/>")"
            {move || {
                op.indices.iter().map(|i| view! {
                    "."<Value module function value=*i highlight/>
                }).collect_view()
            }}
        }.into_any(),
        slir::cfg::Statement::OpUnary(op) => view! {
            <Value module function value=op.result.into() highlight/>
            {format!(" = {}", op.operator)}
            <Value module function value=op.value highlight/>
        }.into_any(),
        slir::cfg::Statement::OpBinary(op) => view! {
            <Value module function value=op.result.into() highlight/>
            " = "
            <Value module function value=op.lhs highlight/>
            {format!(" {} ", op.operator)}
            <Value module function value=op.rhs highlight/>
        }.into_any(),
        slir::cfg::Statement::OpCall(op) => view! {
            {{move || op.result.map(|result | view! {
                <Value module function value=result.into() highlight/>
                " = "
            })}}

            <a href=format_function_url(module.read_value().0.name, op.function)>{op.function.name.to_string()}</a>
            "("
            {move || {
                let mut arg_views = Vec::new();
                let mut is_first = true;

                for arg in op.args.clone() {
                    if !is_first {
                        arg_views.push(view! {", "}.into_any());
                    }

                    arg_views.push(view!{ <Value module function value=arg highlight/> }.into_any());

                    is_first = false;
                }

                arg_views
            }}
            ")"
        }.into_any(),
    };

    view! {
        <p>{inner}</p>
    }
}

#[component]
pub fn Terminator(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    terminator: slir::cfg::Terminator,
    highlight: HighlightSignal,
) -> impl IntoView {
    match terminator {
        slir::cfg::Terminator::Branch(branch) => {
            if let Some(selector) = branch.selector {
                view! {
                    "branch "<Value module function value=selector.into() highlight/>": "
                    {move || {
                        let mut bb_views = Vec::new();
                        let mut is_first = true;

                        for bb in branch.branches.iter().copied() {
                            if !is_first {
                                bb_views.push(view! {", "}.into_any());
                            }

                            bb_views.push(view!{
                                <a href=format!("#BB{}", bb.data().as_ffi())>
                                    {format!("BB{}", bb.data().as_ffi())}
                                </a>
                            }.into_any());

                            is_first = false;
                        }

                        bb_views
                    }}
                }
                .into_any()
            } else {
                view! { "branch " {format!("BB{}", branch.branches[0].data().as_ffi())}}.into_any()
            }
        }
        slir::cfg::Terminator::Return(None) => view! {
            "return"
        }
        .into_any(),
        slir::cfg::Terminator::Return(Some(value)) => view! {
            "return "<Value module function value highlight/>
        }
        .into_any(),
    }
}

#[component]
pub fn Value(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    value: slir::cfg::Value,
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

    match value {
        Value::Local(value) => view! {
            <Popover position=PopoverPosition::Bottom>
                <PopoverTrigger slot>
                    <span class="cfg-value" on:click=update_highlighted_value class:highlighted=is_highlighted_value>
                        {format!("V{}", value.data().as_ffi())}
                    </span>
                </PopoverTrigger>
                {move || {
                    if let Some(ty) = module.read_value().1.function_body[function].local_values[value].ty {
                        view! { <Type module ty/> }.into_any()
                    } else {
                        view! { "Untyped" }.into_any()
                    }
                }}
            </Popover>
        }.into_any(),
        Value::InlineConst(value) => match value {
            slir::cfg::InlineConst::U32(v) => view! {{format!("{}u32", v)}}.into_any(),
            slir::cfg::InlineConst::I32(v) => view! {{format!("{}i32", v)}}.into_any(),
            slir::cfg::InlineConst::F32(v) => view! {{format!("{}f32", v)}}.into_any(),
            slir::cfg::InlineConst::Bool(v) => view! {{format!("{}", v)}}.into_any(),
            slir::cfg::InlineConst::Ptr(v) => view! {
                "&"
                {match v.base {
                    slir::cfg::RootIdentifier::Local(v) => view! { <Value module function value=v.into() highlight/> }.into_any(),
                    slir::cfg::RootIdentifier::Uniform(v) => view! {{format!("U{}", v.data().as_ffi())}}.into_any(),
                    slir::cfg::RootIdentifier::Storage(v) => view! {{format!("S{}", v.data().as_ffi())}}.into_any(),
                    slir::cfg::RootIdentifier::Workgroup(v) => view! {{format!("W{}", v.data().as_ffi())}}.into_any(),
                }}
                ":"
                {format!("{}", v.offset)}
                "("
                <Type module ty=v.ty/>
                ")"
            }.into_any(),
        },
        Value::Const => unimplemented!(),
    }
}
