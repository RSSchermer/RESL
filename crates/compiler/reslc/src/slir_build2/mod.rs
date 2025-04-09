use std::io::Read;

use indexmap::IndexSet;
use rustc_middle::mir::mono::{MonoItem};
use rustc_smir::rustc_internal::stable;
use crate::context::ReslContext;
use crate::slir_build2::builder::Builder;
use crate::slir_build2::context::CodegenContext;
use crate::stable_cg::MonoItemExt;

pub mod builder;
pub mod context;
pub mod ty;
pub mod value;

pub fn build_shader_module<'tcx>(
    rcx: &ReslContext<'tcx>,
    name: slir::Symbol,
    items: &IndexSet<MonoItem<'tcx>>,
) -> (slir::Module, slir::cfg::Cfg) {
    let codegen_cx = CodegenContext::new(rcx, name);

    for item in items {
        let item = stable(item);

        item.predefine::<Builder>(&codegen_cx);
    }

    for item in items {
        let item = stable(item);

        item.define::<Builder>(&codegen_cx);
    }

    codegen_cx.finish()
}
