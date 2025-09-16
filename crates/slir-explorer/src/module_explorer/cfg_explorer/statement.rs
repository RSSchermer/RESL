use leptos::prelude::*;

use crate::module_explorer::cfg_explorer::{HighlightSignal, Value};
use crate::module_explorer::{format_function_url, ModuleData};

#[component]
pub fn Statement(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let inner = match &module.read_value().cfg[statement] {
        slir::cfg::StatementData::Assign(_) => {
            view! { <Assign module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::Bind(_) => {
            view! { <Bind module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::Uninitialized(_) => {
            view! { <Uninitialized module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpAlloca(_) => {
            view! { <OpAlloca module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpLoad(_) => {
            view! { <OpLoad module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpStore(_) => {
            view! { <OpStore module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpPtrElementPtr(_) => {
            view! { <OpPtrElementPtr module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpPtrVariantPtr(_) => {
            view! { <OpPtrVariantPtr module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpGetDiscriminant(_) => {
            view! { <OpGetDiscriminant module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpSetDiscriminant(_) => {
            view! { <OpSetDiscriminant module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpOffsetSlicePtr(_) => {
            view! { <OpOffsetSlicePtr module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpUnary(_) => {
            view! { <OpUnary module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpBinary(_) => {
            view! { <OpBinary module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpCall(_) => {
            view! { <OpCall module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpCaseToBranchPredicate(_) => {
            view! { <OpCaseToBranchPredicate module function statement highlight/> }.into_any()
        }
        slir::cfg::StatementData::OpBoolToBranchPredicate(_) => {
            view! { <OpBoolToBranchPredicate module function statement highlight/> }.into_any()
        }
    };

    view! {
        <p>{inner}</p>
    }
}

#[component]
pub fn Assign(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_assign();
    let binding = stmt.local_binding();
    let value = stmt.value();

    view! {
        <Value module function value=binding.into() highlight/>
        " = "
        <Value module function value highlight/>
    }
}

#[component]
pub fn Bind(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_bind();
    let binding = stmt.local_binding();
    let value = stmt.value();

    view! {
        <Value module function value=binding.into() highlight/>
        " = "
        <Value module function value highlight/>
    }
}

#[component]
pub fn Uninitialized(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_uninitialized();
    let binding = stmt.local_binding();

    view! {
        <Value module function value=binding.into() highlight/>" = uninitialized"
    }
}

#[component]
pub fn OpAlloca(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_alloca();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/> " = alloca"
    }
}

#[component]
pub fn OpLoad(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_load();
    let binding = stmt.result();
    let pointer = stmt.pointer();

    view! {
        <Value module function value=binding.into() highlight/> " = "
            "load "<Value module function value=pointer highlight/>
    }
}

#[component]
pub fn OpStore(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_store();
    let pointer = stmt.pointer();
    let value = stmt.value();

    view! {
        "store "<Value module function value highlight/>
        " into "<Value module function value=pointer highlight/>
    }
}

#[component]
pub fn OpPtrElementPtr(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_ptr_element_ptr();
    let pointer = stmt.pointer();
    let indices = stmt.indices().to_vec();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        " = &"<Value module function value=pointer highlight/>
        {move || {
            indices.iter().map(|i| view! {
                "."<Value module function value=*i highlight/>
            }).collect_view()
        }}
    }
}

#[component]
pub fn OpPtrVariantPtr(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_ptr_variant_ptr();
    let pointer = stmt.pointer();
    let variant_index = stmt.variant_index();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        " = variant-ptr "
        <Value module function value=pointer highlight/>
        ":"
        {variant_index}
    }
}

#[component]
pub fn OpGetDiscriminant(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_get_discriminant();
    let pointer = stmt.pointer();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        " = get-discriminant "
        <Value module function value=pointer highlight/>
    }
}

#[component]
pub fn OpSetDiscriminant(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_set_discriminant();
    let pointer = stmt.pointer();
    let variant_index = stmt.variant_index();

    view! {
        "set-discriminant "
        {variant_index}
        " on "
        <Value module function value=pointer highlight/>
    }
}

#[component]
pub fn OpOffsetSlicePtr(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_offset_slice_ptr();
    let pointer = stmt.pointer();
    let offset = stmt.offset();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        " = offset "
        <Value module function value=pointer highlight/>
        " by "
        <Value module function value=offset highlight/>
    }
}

#[component]
pub fn OpUnary(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_unary();
    let operand = stmt.operand();
    let operator = stmt.operator();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        {format!(" = {}", operator)}
        <Value module function value=operand highlight/>
    }
}

#[component]
pub fn OpBinary(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_binary();
    let operator = stmt.operator();
    let lhs = stmt.lhs();
    let rhs = stmt.rhs();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        " = "
        <Value module function value=lhs highlight/>
        {format!(" {} ", operator)}
        <Value module function value=rhs highlight/>
    }
}

#[component]
pub fn OpCall(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_call();
    let callee = stmt.callee();
    let binding = stmt.result();

    let mut arg_views = Vec::new();
    let mut is_first = true;

    for arg in stmt.arguments().iter().copied() {
        if !is_first {
            arg_views.push(view! {", "}.into_any());
        }

        arg_views.push(view! { <Value module function value=arg highlight/> }.into_any());

        is_first = false;
    }

    view! {
        {{move || binding.map(|binding | view! {
            <Value module function value=binding.into() highlight/>
            " = "
        })}}

        <a href=format_function_url(module.read_value().module.name, callee)>
            {callee.name.to_string()}
        </a>
        "("{arg_views}")"
    }
}

#[component]
pub fn OpCaseToBranchPredicate(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_case_to_branch_predicate();
    let value = stmt.value();
    let cases = stmt
        .cases()
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        " = predicate-from-case "
        <Value module function value highlight/>
        " ["{cases}"]"
    }
}

#[component]
pub fn OpBoolToBranchPredicate(
    module: StoredValue<ModuleData>,
    function: slir::Function,
    statement: slir::cfg::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let data = module.read_value();
    let stmt = data.cfg[statement].expect_op_bool_to_branch_predicate();
    let value = stmt.value();
    let binding = stmt.result();

    view! {
        <Value module function value=binding.into() highlight/>
        " = predicate-from-bool "
        <Value module function value highlight/>
    }
}
