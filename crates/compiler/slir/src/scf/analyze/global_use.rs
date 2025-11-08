use rustc_hash::FxHashSet;

use crate::scf::visit::TopDownVisitor;
use crate::scf::{ExpressionKind, GlobalPtr, Scf, Statement, StatementKind, visit};
use crate::{Constant, Module, WorkgroupBinding};

pub struct GlobalUse {
    pub workgroup_bindings: FxHashSet<WorkgroupBinding>,
    pub constants: FxHashSet<Constant>,
    // We don't currently care to count uniform and storage bindings here, as we always include all
    // uniform and storage bindings in the final compilation output, even when unused, because they
    // are part of a module's public interface.
}

struct GlobalUseVisitor {
    workgroup_bindings: FxHashSet<WorkgroupBinding>,
    constants: FxHashSet<Constant>,
}

impl TopDownVisitor for GlobalUseVisitor {
    fn visit_statement(&mut self, scf: &Scf, statement: Statement) {
        if let StatementKind::ExprBinding(expr_binding) = scf[statement].kind()
            && let ExpressionKind::GlobalPtr(ptr) = expr_binding.expression().kind()
        {
            match ptr {
                GlobalPtr::Workgroup(b) => {
                    self.workgroup_bindings.insert(*b);
                }
                GlobalPtr::Constant(c) => {
                    self.constants.insert(*c);
                }
                _ => {}
            }
        }

        visit::visit_statement_top_down(self, scf, statement);
    }
}

pub fn collect_used_global_bindings(module: &Module, scf: &Scf) -> GlobalUse {
    let mut visitor = GlobalUseVisitor {
        workgroup_bindings: Default::default(),
        constants: Default::default(),
    };

    for (entry_point, _) in module.entry_points.iter() {
        if let Some(body) = scf.get_function_body(entry_point) {
            visitor.visit_block(scf, body.block());
        }
    }

    GlobalUse {
        workgroup_bindings: visitor.workgroup_bindings,
        constants: visitor.constants,
    }
}
