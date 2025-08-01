use leptos::prelude::*;

use crate::module_explorer::cfg_explorer::{HighlightSignal, Value};
use crate::module_explorer::{format_function_url, ModuleData};

#[component]
pub fn Instruction(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    instruction: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let inner = match instruction {
        slir::cfg::Statement::OpAlloca(op) => {
            view! { <OpAlloca module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpAssign(op) => {
            view! { <OpAssign module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpLoad(op) => {
            view! { <OpLoad module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpStore(op) => {
            view! { <OpStore module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpPtrElementPtr(op) => {
            view! { <OpPtrElementPtr module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpPtrVariantPtr(op) => {
            view! { <OpPtrVariantPtr module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpGetDiscriminant(op) => {
            view! { <OpGetDiscriminant module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpSetDiscriminant(op) => {
            view! { <OpSetDiscriminant module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpOffsetSlicePtr(op) => {
            view! { <OpOffsetSlicePtr module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpUnary(op) => {
            view! { <OpUnary module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpBinary(op) => {
            view! { <OpBinary module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpCall(op) => {
            view! { <OpCall module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpCaseToBranchPredicate(op) => {
            view! { <OpCaseToBranchPredicate module function op highlight/> }.into_any()
        }
        slir::cfg::Statement::OpBoolToBranchPredicate(op) => {
            view! { <OpBoolToBranchPredicate module function op highlight/> }.into_any()
        }
    };

    view! {
        <p>{inner}</p>
    }
}

#[component]
pub fn OpAlloca(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpAlloca,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/> " = alloca"
    }
}

#[component]
pub fn OpAssign(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpAssign,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = "
        <Value module function value=op.value highlight/>
    }
}

#[component]
pub fn OpLoad(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpLoad,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/> " = "
            "load "<Value module function value=op.ptr highlight/>
    }
}

#[component]
pub fn OpStore(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpStore,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        "store "<Value module function value=op.value highlight/>
        " into "<Value module function value=op.ptr highlight/>
    }
}

#[component]
pub fn OpPtrElementPtr(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpPtrElementPtr,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = &"<Value module function value=op.ptr highlight/>
        {move || {
            op.indices.iter().map(|i| view! {
                "."<Value module function value=*i highlight/>
            }).collect_view()
        }}
    }
}

#[component]
pub fn OpPtrVariantPtr(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpPtrVariantPtr,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = variant-ptr "
        <Value module function value=op.ptr highlight/>
        ":"
        {op.variant_index}
    }
}

#[component]
pub fn OpGetDiscriminant(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpGetDiscriminant,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = get-discriminant "
        <Value module function value=op.ptr highlight/>
    }
}

#[component]
pub fn OpSetDiscriminant(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpSetDiscriminant,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        "set-discriminant "
        {op.variant_index}
        " on "
        <Value module function value=op.ptr highlight/>
    }
}

#[component]
pub fn OpOffsetSlicePtr(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpOffsetSlicePtr,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = offset "
        <Value module function value=op.slice_ptr highlight/>
        " by "
        <Value module function value=op.offset highlight/>
    }
}

#[component]
pub fn OpUnary(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpUnary,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        {format!(" = {}", op.operator)}
        <Value module function value=op.value highlight/>
    }
}

#[component]
pub fn OpBinary(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpBinary,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = "
        <Value module function value=op.lhs highlight/>
        {format!(" {} ", op.operator)}
        <Value module function value=op.rhs highlight/>
    }
}

#[component]
pub fn OpCall(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpCall,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        {{move || op.result.map(|result | view! {
            <Value module function value=result.into() highlight/>
            " = "
        })}}

        <a href=format_function_url(module.read_value().module.name, op.function)>
            {op.function.name.to_string()}
        </a>

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
    }
}

#[component]
pub fn OpCaseToBranchPredicate(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpCaseToBranchPredicate,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = predicate-from-case "
        <Value module function value=op.value highlight/>
        " ["
        {op.cases.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")}
        "]"
    }
}

#[component]
pub fn OpBoolToBranchPredicate(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    op: slir::cfg::OpBoolToBranchPredicate,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        <Value module function value=op.result.into() highlight/>
        " = predicate-from-bool "
        <Value module function value=op.value highlight/>
    }
}
