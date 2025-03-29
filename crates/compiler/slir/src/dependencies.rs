use rustc_hash::{FxHashMap, FxHashSet};

use crate::cfg::{Cfg, Statement};
use crate::{Function, Symbol};

pub fn gather_dependencies(cfg: &Cfg) -> FxHashMap<Symbol, FxHashSet<Function>> {
    gather_dependencies_with_filter(cfg, cfg.function_body.keys())
}

pub fn gather_dependencies_with_filter(
    cfg: &Cfg,
    filter: impl IntoIterator<Item = Function>,
) -> FxHashMap<Symbol, FxHashSet<Function>> {
    let mut dependencies = FxHashMap::default();

    for function in filter {
        if let Some(body) = cfg.function_body.get(function) {
            for bb in body.basic_blocks.values() {
                for statement in &bb.statements {
                    if let Statement::OpCall(op_call) = statement {
                        if !cfg.function_body.contains(op_call.function) {
                            dependencies
                                .entry(op_call.function.module)
                                .or_insert_with(FxHashSet::default)
                                .insert(op_call.function);
                        }
                    }
                }
            }
        }
    }

    dependencies
}
