use leptos::prelude::*;
use leptos::{component, view, IntoView};
use slir::scf::ExpressionKind;
use slotmap::Key;

use crate::module_explorer::scf_explorer::local_binding::LocalBinding;
use crate::module_explorer::scf_explorer::HighlightSignal;
use crate::module_explorer::ModuleData;

#[component]
pub fn Expression(
    module: StoredValue<ModuleData>,
    expression: slir::scf::Expression,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let expr_data = &module_data.scf.as_ref().unwrap()[expression];

    match expr_data.kind() {
        ExpressionKind::LocalValue(binding) => view! {
            <LocalBinding module binding=*binding highlight />
        }
        .into_any(),
        ExpressionKind::UniformValue(binding) => view! {
            <UniformBinding binding=*binding />
        }
        .into_any(),
        ExpressionKind::StorageValue(binding) => view! {
            <StorageBinding binding=*binding />
        }
        .into_any(),
        ExpressionKind::WorkgroupValue(binding) => view! {
            <WorkgroupBinding binding=*binding />
        }
        .into_any(),
        ExpressionKind::FallbackValue(_) => "fallback".into_any(),
        ExpressionKind::ConstU32(v) => format!("{}u32", v).into_any(),
        ExpressionKind::ConstI32(v) => format!("{}i32", v).into_any(),
        ExpressionKind::ConstF32(v) => format!("{}f32", v).into_any(),
        ExpressionKind::ConstBool(v) => v.to_string().into_any(),
        ExpressionKind::ConstPtr(expr) => view! {
            "&"<Expression module expression=*expr highlight />
        }
        .into_any(),
        ExpressionKind::OpUnary(op) => view! {
            {op.operator().to_string()}<Expression module expression=op.operand() highlight />
        }
        .into_any(),
        ExpressionKind::OpBinary(op) => view! {
            <Expression module expression=op.lhs() highlight />
            {format!(" {} ", op.operator())}
            <Expression module expression=op.rhs() highlight />
        }
        .into_any(),
        ExpressionKind::OpPtrElementPtr(op) => view! {
            "&"<Expression module expression=op.pointer() highlight />

            {
                op.indices()
                    .iter()
                    .map(|i| view! {
                        "."<Expression module expression=*i highlight />
                    }.into_any())
                    .collect::<Vec<_>>()
            }
        }
        .into_any(),
        ExpressionKind::OpExtractElement(op) => view! {
            <Expression module expression=op.value() highlight />

            {
                op.indices()
                    .iter()
                    .map(|i| view! {
                        "."<Expression module expression=*i highlight />
                    }.into_any())
                    .collect::<Vec<_>>()
            }
        }
        .into_any(),
        ExpressionKind::OpLoad(pointer) => view! {
            "*"<Expression module expression=*pointer highlight />
        }
        .into_any(),
        ExpressionKind::OpBoolToSwitchPredicate(value) => view! {
            "bool-to-pred("<Expression module expression=*value highlight />")"
        }
        .into_any(),
        ExpressionKind::OpCaseToSwitchPredicate(op) => view! {
            "case-to-pred("<Expression module expression=op.case() highlight />":["{
                op.cases()
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
            }"])"
        }
        .into_any(),
        ExpressionKind::OpCallBuiltin(op) => view! {
            {op.callee().ident().as_str()}"("{
                op.arguments().iter().map(|arg| view! {
                    <Expression module expression=*arg highlight />
                }.into_any())
                .intersperse_with(|| view! {", "}.into_any())
                .collect_view()
            }")"
        }
        .into_any(),
    }
}

#[component]
pub fn UniformBinding(binding: slir::UniformBinding) -> impl IntoView {
    view! {
        {format!("U{}", binding.data().as_ffi())}
    }
}

#[component]
pub fn StorageBinding(binding: slir::StorageBinding) -> impl IntoView {
    view! {
        {format!("S{}", binding.data().as_ffi())}
    }
}

#[component]
pub fn WorkgroupBinding(binding: slir::WorkgroupBinding) -> impl IntoView {
    view! {
        {format!("W{}", binding.data().as_ffi())}
    }
}
