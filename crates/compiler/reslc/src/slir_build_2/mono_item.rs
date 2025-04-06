use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::mir::interpret::ErrorHandled;
use rustc_middle::mir::mono::{Linkage, Visibility};
use rustc_middle::ty::layout::{HasTyCtxt, LayoutOf};
use rustc_middle::ty::Instance;
use rustc_middle::{bug, span_bug, ty};
use stable_mir::mir::mono::MonoItem;
use tracing::debug;

use crate::slir_build_2::mir::codegen_mir;
use crate::slir_build_2::traits::*;

pub trait MonoItemExt<'a> {
    fn define<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx);
    fn predefine<Bx: BuilderMethods<'a>>(
        &self,
        cx: &'a Bx::CodegenCx,
        linkage: Linkage,
        visibility: Visibility,
    );
    fn to_raw_string(&self) -> String;
}

impl<'a> MonoItemExt<'a> for MonoItem {
    fn define<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx) {
        debug!("BEGIN IMPLEMENTING '{} ({})'", self, self.to_raw_string());

        match self.clone() {
            MonoItem::Static(def_id) => {
                cx.codegen_static(*def_id);
            }
            MonoItem::GlobalAsm(..) => {
                bug!("Global ASM not supported in RESL");
            }
            MonoItem::Fn(instance) => {
                codegen_mir::<Bx>(cx, instance);
            }
        }

        debug!("END IMPLEMENTING '{} ({})'", self, self.to_raw_string(),);
    }

    fn predefine<Bx: BuilderMethods<'a>>(
        &self,
        cx: &'a Bx::CodegenCx,
        linkage: Linkage,
        visibility: Visibility,
    ) {
        debug!("BEGIN PREDEFINING '{} ({})'", self, self.to_raw_string(),);

        let symbol_name = self.symbol_name(cx.tcx()).name;

        debug!("symbol {symbol_name}");

        match self.clone() {
            MonoItem::Static(def_id) => {
                cx.predefine_static(def_id, linkage, visibility, symbol_name);
            }
            MonoItem::Fn(instance) => {
                cx.predefine_fn(instance, linkage, visibility, symbol_name);
            }
            MonoItem::GlobalAsm(item_id) => {
                bug!("Global ASM not supported in RESL");
            }
        }

        debug!("END PREDEFINING '{} ({})'", self, self.to_raw_string());
    }

    fn to_raw_string(&self) -> String {
        match self {
            MonoItem::Fn(instance) => format!("Fn({:?})", instance.def),
            MonoItem::Static(id) => format!("Static({id:?})"),
            MonoItem::GlobalAsm(id) => bug!("Global ASM not supported in RESL"),
        }
    }
}
