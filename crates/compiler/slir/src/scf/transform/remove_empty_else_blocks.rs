use crate::scf::{Block, Scf, StatementKind};
use crate::{Function, Module};

fn visit_block(scf: &mut Scf, block: Block) {
    let stmt_count = scf[block].statements().len();

    for i in 0..stmt_count {
        let stmt = scf[block].statements()[i];

        match scf[stmt].kind() {
            StatementKind::If(stmt_data) => {
                let then_block = stmt_data.then_block();
                let else_block = stmt_data.else_block();

                visit_block(scf, then_block);

                if let Some(else_block) = else_block {
                    if scf[else_block].is_empty() {
                        scf.remove_else_block(stmt);
                    } else {
                        visit_block(scf, else_block);
                    }
                }
            }
            StatementKind::Switch(stmt_data) => {
                let case_count = stmt_data.cases().len();
                let default_block = stmt_data.default();

                for j in 0..case_count {
                    let block = scf[stmt].expect_switch().cases()[j].block();

                    visit_block(scf, block);
                }

                visit_block(scf, default_block);
            }
            StatementKind::Loop(stmt_data) => {
                visit_block(scf, stmt_data.block());
            }
            _ => {}
        }
    }
}

pub fn transform_fn(scf: &mut Scf, function: Function) {
    let body_block = scf
        .get_function_body(function)
        .expect("function body not registered")
        .block();

    visit_block(scf, body_block);
}

pub fn transform_entry_points(module: &Module, scf: &mut Scf) {
    for (entry_point, _) in module.entry_points.iter() {
        transform_fn(scf, entry_point);
    }
}
