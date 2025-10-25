use crate::scf::{Block, Scf, Statement, StatementKind};

pub trait TopDownVisitor: Sized {
    fn visit_block(&mut self, scf: &Scf, block: Block) {
        visit_block_top_down(self, scf, block);
    }

    fn visit_statement(&mut self, scf: &Scf, statement: Statement) {
        visit_statement_top_down(self, scf, statement);
    }
}

pub fn visit_block_top_down<V: TopDownVisitor>(visitor: &mut V, scf: &Scf, block: Block) {
    for statement in scf[block].statements() {
        visitor.visit_statement(scf, *statement)
    }
}

pub fn visit_statement_top_down<V: TopDownVisitor>(
    visitor: &mut V,
    scf: &Scf,
    statement: Statement,
) {
    match scf[statement].kind() {
        StatementKind::Switch(stmt) => {
            for case in stmt.cases() {
                visitor.visit_block(scf, case.block());
            }

            visitor.visit_block(scf, stmt.default());
        }
        StatementKind::If(stmt) => {
            visitor.visit_block(scf, stmt.then_block());

            if let Some(else_block) = stmt.else_block() {
                visitor.visit_block(scf, else_block);
            }
        }
        StatementKind::Loop(stmt) => {
            visitor.visit_block(scf, stmt.block());
        }
        _ => {}
    }
}

pub trait BottomUpVisitor: Sized {
    fn visit_block(&mut self, scf: &Scf, block: Block) {
        visit_block_bottom_up(self, scf, block);
    }

    fn visit_statement(&mut self, scf: &Scf, statement: Statement) {
        visit_statement_bottom_up(self, scf, statement);
    }
}

pub fn visit_block_bottom_up<V: BottomUpVisitor>(visitor: &mut V, scf: &Scf, block: Block) {
    for statement in scf[block].statements().iter().rev() {
        visitor.visit_statement(scf, *statement);
    }
}

pub fn visit_statement_bottom_up<V: BottomUpVisitor>(
    visitor: &mut V,
    scf: &Scf,
    statement: Statement,
) {
    match scf[statement].kind() {
        StatementKind::Switch(stmt) => {
            for case in stmt.cases() {
                visitor.visit_block(scf, case.block());
            }

            visitor.visit_block(scf, stmt.default());
        }
        StatementKind::If(stmt) => {
            visitor.visit_block(scf, stmt.then_block());

            if let Some(else_block) = stmt.else_block() {
                visitor.visit_block(scf, else_block);
            }
        }
        StatementKind::Loop(stmt) => {
            visitor.visit_block(scf, stmt.block());
        }
        _ => {}
    }
}
