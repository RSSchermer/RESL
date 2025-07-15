use std::io::Read;

use indexmap::IndexSet;
use rustc_middle::mir::mono::MonoItem;
use rustc_smir::rustc_internal::stable;

use crate::context::ReslContext;
use crate::slir_build::builder::Builder;
use crate::slir_build::context::CodegenContext;
use crate::slir_build::special_cases::maybe_define_special_case;
use crate::stable_cg::MonoItemExt;

pub mod builder;
pub mod context;
mod special_cases;
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

        // Certain items in `core` that we want to support use instructions internally that we don't
        // support in user-defined RESL. Rather than trying to convert such instruction sequences,
        // we instead treat these as special case intrinsics.
        if let Some(item) = maybe_define_special_case(item, &codegen_cx) {
            // No special case; send it down the regular codegen path.
            item.define::<Builder>(&codegen_cx);
        }
    }

    codegen_cx.finish()
}
