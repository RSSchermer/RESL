use leptos::prelude::*;
use leptos::{component, view, IntoView};
use slir::scf::{ExpressionKind, GlobalPtr};
use slotmap::Key;

use crate::module_explorer::scf_explorer::local_binding::LocalBinding;
use crate::module_explorer::scf_explorer::HighlightSignal;
use crate::module_explorer::ModuleData;

#[component]
pub fn Expression(
    module: StoredValue<ModuleData>,
    expr_binding: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let expr_data = &module_data.scf.as_ref().unwrap()[expr_binding]
        .expect_expr_binding()
        .expression();

    match expr_data.kind() {
        ExpressionKind::FallbackValue => "fallback".into_any(),
        ExpressionKind::ConstU32(v) => format!("{}u32", v).into_any(),
        ExpressionKind::ConstI32(v) => format!("{}i32", v).into_any(),
        ExpressionKind::ConstF32(v) => format!("{}f32", v).into_any(),
        ExpressionKind::ConstBool(v) => v.to_string().into_any(),
        ExpressionKind::GlobalPtr(ptr) => match ptr {
            GlobalPtr::Uniform(binding) => {
                format!("&U{}", binding.data().as_ffi())
            }
            GlobalPtr::Storage(binding) => {
                format!("&S{}", binding.data().as_ffi())
            }
            GlobalPtr::Workgroup(binding) => {
                format!("&W{}", binding.data().as_ffi())
            }
            GlobalPtr::Constant(constant) => format!("&{}", constant.name),
        }
        .into_any(),
        ExpressionKind::OpUnary(op) => view! {
            {op.operator().to_string()}<LocalBinding module binding=op.operand() highlight />
        }
        .into_any(),
        ExpressionKind::OpBinary(op) => view! {
            <LocalBinding module binding=op.lhs() highlight />
            {format!(" {} ", op.operator())}
            <LocalBinding module binding=op.rhs() highlight />
        }
        .into_any(),
        ExpressionKind::OpVector(op) => view! {
            {op.vector_ty().to_string()}"("{
                op.elements().iter().copied().map(|binding| view! {
                    <LocalBinding module binding highlight />
                }.into_any())
                .intersperse_with(|| view! {", "}.into_any())
                .collect_view()
            }")"
        }
        .into_any(),
        ExpressionKind::OpMatrix(op) => view! {
            {op.matrix_ty().to_string()}"("{
                op.columns().iter().copied().map(|binding| view! {
                    <LocalBinding module binding highlight />
                }.into_any())
                .intersperse_with(|| view! {", "}.into_any())
                .collect_view()
            }")"
        }
        .into_any(),
        ExpressionKind::OpPtrElementPtr(op) => view! {
            "&"<LocalBinding module binding=op.pointer() highlight />

            {
                op.indices()
                    .iter()
                .copied()
                    .map(|binding| view! {
                        "."<LocalBinding module binding highlight />
                    }.into_any())
                    .collect::<Vec<_>>()
            }
        }
        .into_any(),
        ExpressionKind::OpExtractElement(op) => view! {
            <LocalBinding module binding=op.value() highlight />

            {
                op.indices()
                    .iter()
                .copied()
                    .map(|binding| view! {
                        "."<LocalBinding module binding highlight />
                    }.into_any())
                    .collect::<Vec<_>>()
            }
        }
        .into_any(),
        ExpressionKind::OpLoad(pointer) => view! {
            "*"<LocalBinding module binding=*pointer highlight />
        }
        .into_any(),
        ExpressionKind::OpCallBuiltin(op) => view! {
            {op.callee().ident().as_str()}"("{
                op.arguments().iter().copied().map(|binding| view! {
                    <LocalBinding module binding highlight />
                }.into_any())
                .intersperse_with(|| view! {", "}.into_any())
                .collect_view()
            }")"
        }
        .into_any(),
    }
}
