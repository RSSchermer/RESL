use leptos::prelude::*;
use leptos::{component, IntoView};
use slir::scf::{LoopControl, StatementKind};

use crate::module_explorer::scf_explorer::block::Block;
use crate::module_explorer::scf_explorer::expression::Expression;
use crate::module_explorer::scf_explorer::local_binding::LocalBinding;
use crate::module_explorer::scf_explorer::HighlightSignal;
use crate::module_explorer::tpe::Type;
use crate::module_explorer::ModuleData;

#[component]
pub fn Statement(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let stmt_data = &module_data.scf.as_ref().unwrap()[statement];

    match stmt_data.kind() {
        StatementKind::If(_) => view! {
            <If module statement highlight /><br/>
        }
        .into_any(),
        StatementKind::Switch(_) => view! {
            <Switch module statement highlight /><br/>
        }
        .into_any(),
        StatementKind::Loop(_) => view! {
            <Loop module statement highlight /><br/>
        }
        .into_any(),
        StatementKind::Return(_) => view! {
            <Return module statement highlight /><br/>
        }
        .into_any(),
        StatementKind::ExprBinding(_) => view! {
            <ExprBinding module statement highlight /><br/>
        }
        .into_any(),
        StatementKind::Store(_) => view! {
            <Store module statement highlight /><br/>
        }
        .into_any(),
        StatementKind::CallBuiltin(_) => view! {
            <CallBuiltin module statement highlight /><br/>
        }
        .into_any(),
    }
}

#[component]
pub fn If(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let stmt = module_data.expect_scf()[statement].kind().expect_if();

    view! {
        {move || {
            let m = module.read_value();
            let stmt = m.expect_scf()[statement].kind().expect_if();

            stmt.out_vars().iter().map(|var| {
                let ty = m.scf.as_ref().unwrap()[*var].ty();

                view!{
                    "var "<LocalBinding module binding=*var highlight />": "<Type module ty />";"<br/>
                }
            }).collect_view()
        }}
        <br/>
        "if " <LocalBinding module binding=stmt.condition() highlight /> " {"<br/>
            <Block module block=stmt.then_block() highlight />
        "} "
        {move || {
            let m = module.read_value();
            let stmt = m.expect_scf()[statement].kind().expect_if();

            stmt.else_block().map(|else_block| {
                view! {
                    "else {"<br/>
                        <Block module block=else_block highlight />
                    "}"
                }
            })
        }}
    }
}

#[component]
pub fn Switch(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let stmt = module_data.expect_scf()[statement].kind().expect_switch();

    view! {
        {move || {
            let m = module.read_value();
            let stmt = m.expect_scf()[statement].kind().expect_switch();

            stmt.out_vars().iter().map(|var| {
                let ty = m.scf.as_ref().unwrap()[*var].ty();

                view!{
                    "var "<LocalBinding module binding=*var highlight />": "<Type module ty />";"<br/>
                }
            }).collect_view()
        }}
        <br/>
        "switch " <LocalBinding module binding=stmt.on() highlight /> " {"<br/>
            <div class="scf-indent">
                {move || {
                    let m = module.read_value();
                    let stmt = m.expect_scf()[statement].kind().expect_switch();

                    stmt.cases().iter().map(|case| {
                        view! {
                            "case "{case.case()}": {"<br/>
                                <Block module block=case.block() highlight />
                            "}"<br/>
                        }
                    }).collect_view()
                }}
                "default: {"<br/>
                    <Block module block=stmt.default() highlight />
                "}"
            </div>
        "}"
    }
}

#[component]
pub fn Loop(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    view! {
        {move || {
            let m = module.read_value();
            let stmt = m.expect_scf()[statement].kind().expect_loop();

            stmt.loop_vars().iter().map(|var| {
                view!{
                    "var "<LocalBinding module binding=var.binding() highlight />
                    " = "<LocalBinding module binding=var.initial_value() highlight />";"<br/>
                }
            }).collect_view()
        }}
        <br/>
        {move || {
            let m = module.read_value();
            let stmt = m.expect_scf()[statement].kind().expect_loop();

            match stmt.control() {
                LoopControl::Head(binding) => view!{
                    "while "<LocalBinding module binding highlight />" {"<br/>
                        <Block module block=stmt.block() highlight />
                    "}"<br/>
                }.into_any(),
                LoopControl::Tail(binding) => view!{
                    "do {"<br/>
                        <Block module block=stmt.block() highlight />
                    "} while "<LocalBinding module binding highlight />";"<br/>
                }.into_any(),
                LoopControl::Infinite => view!{
                    "loop {"<br/>
                        <Block module block=stmt.block() highlight />
                    "}"<br/>
                }.into_any()
            }
        }}
    }
}

#[component]
pub fn Return(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let stmt = module_data.expect_scf()[statement].kind().expect_return();

    if let Some(binding) = stmt.value() {
        view! {
            "return "<LocalBinding module binding highlight />";"
        }
        .into_any()
    } else {
        view! {
            "return;"
        }
        .into_any()
    }
}

#[component]
pub fn ExprBinding(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let stmt = module_data.expect_scf()[statement]
        .kind()
        .expect_expr_binding();

    view! {
        "let "<LocalBinding module binding=stmt.binding() highlight />
        " = "<Expression module expr_binding=statement highlight />";"
    }
}

#[component]
pub fn Store(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let stmt = module_data.expect_scf()[statement].kind().expect_store();

    view! {
        "*"<LocalBinding module binding=stmt.pointer() highlight />
        " = "
        <LocalBinding module binding=stmt.value() highlight />";"
    }
}

#[component]
pub fn CallBuiltin(
    module: StoredValue<ModuleData>,
    statement: slir::scf::Statement,
    highlight: HighlightSignal,
) -> impl IntoView {
    let module_data = module.read_value();
    let stmt = module_data.expect_scf()[statement]
        .kind()
        .expect_call_builtin();

    view! {
        {stmt.callee().ident().as_str()}"("{
            stmt.arguments().iter().copied().map(|binding| view! {
                <LocalBinding module binding highlight />
            }.into_any())
            .intersperse_with(|| view! {", "}.into_any())
            .collect_view()
        }")"
    }
}
