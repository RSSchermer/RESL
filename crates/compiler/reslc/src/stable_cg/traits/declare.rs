use stable_mir::mir::mono::{Instance, StaticDef};
use stable_mir::DefId;

pub trait PreDefineCodegenMethods {
    fn predefine_static(&self, def: StaticDef, symbol_name: &str);
    fn predefine_fn(&self, instance: Instance, symbol_name: &str);
}
