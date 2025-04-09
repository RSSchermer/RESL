use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::mir::interpret::ErrorHandled;
use rustc_middle::mir::mono::{Linkage, Visibility};
use rustc_middle::ty::layout::{HasTyCtxt, LayoutOf};
use rustc_middle::ty::Instance;
use rustc_middle::{bug, span_bug, ty};
use stable_mir::mir::mono::MonoItem;
use tracing::debug;

use crate::stable_cg::mir::codegen_mir;
use crate::stable_cg::traits::*;

pub trait MonoItemExt<'a> {
    fn define<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx);
    fn predefine<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx, symbol_name: &str);
    fn to_raw_string(&self) -> String;
}

impl<'a> MonoItemExt<'a> for MonoItem {
    fn define<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx) {
        debug!("BEGIN IMPLEMENTING '{:?} ({})'", self, self.to_raw_string());

        match self.clone() {
            MonoItem::Static(def) => {
                cx.codegen_static(def);
            }
            MonoItem::GlobalAsm(..) => {
                bug!("Global ASM not supported in RESL");
            }
            MonoItem::Fn(instance) => {
                codegen_mir::<Bx>(cx, instance);
            }
        }

        debug!("END IMPLEMENTING '{:?} ({})'", self, self.to_raw_string(),);
    }

    fn predefine<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx, symbol_name: &str) {
        debug!("BEGIN PREDEFINING '{:?} ({})'", self, self.to_raw_string(),);
        debug!("symbol {symbol_name}");

        match self.clone() {
            MonoItem::Static(def) => {
                cx.predefine_static(def, symbol_name);
            }
            MonoItem::Fn(instance) => {
                cx.predefine_fn(instance, symbol_name);
            }
            MonoItem::GlobalAsm(item_id) => {
                bug!("Global ASM not supported in RESL");
            }
        }

        debug!("END PREDEFINING '{:?} ({})'", self, self.to_raw_string());
    }

    fn to_raw_string(&self) -> String {
        match self {
            MonoItem::Fn(instance) => format!("Fn({:?})", instance.def),
            MonoItem::Static(id) => format!("Static({id:?})"),
            MonoItem::GlobalAsm(id) => bug!("Global ASM not supported in RESL"),
        }
    }
}
