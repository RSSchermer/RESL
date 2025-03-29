use std::io::Read;

use indexmap::IndexSet;
use rustc_codegen_ssa::mono_item::MonoItemExt;
use rustc_middle::mir::mono::{Linkage, MonoItem, Visibility};

use crate::context::ReslContext;
use crate::slir_build::builder::Builder;
use crate::slir_build::context::CodegenContext;

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
        item.predefine::<Builder>(&codegen_cx, Linkage::Private, Visibility::Hidden);
    }

    for item in items {
        item.define::<Builder>(&codegen_cx);
    }

    codegen_cx.finish()
}
