use rustc_middle::bug;
use rustc_public::mir::mono::{Instance, MonoItem};
use tracing::debug;

use crate::stable_cg::mir::codegen_mir;
use crate::stable_cg::traits::*;

pub trait MonoItemExt<'a> {
    fn define<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx);
    fn predefine<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx);
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

    fn predefine<Bx: BuilderMethods<'a>>(&self, cx: &'a Bx::CodegenCx) {
        debug!("BEGIN PREDEFINING '{:?} ({})'", self, self.to_raw_string(),);

        match self.clone() {
            MonoItem::Static(def) => {
                let symbol_name = Instance::from(def).mangled_name();

                debug!("symbol {symbol_name}");

                cx.predefine_static(def, symbol_name.as_str());
            }
            MonoItem::Fn(instance) => {
                let symbol_name = instance.mangled_name();

                debug!("symbol {symbol_name}");

                cx.predefine_fn(instance, symbol_name.as_str());
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
