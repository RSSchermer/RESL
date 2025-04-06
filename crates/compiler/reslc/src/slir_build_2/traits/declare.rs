use stable_mir::mir::mono::Instance;
use stable_mir::DefId;

pub trait PreDefineCodegenMethods {
    fn predefine_static(&self, def_id: DefId, visibility: Visibility, symbol_name: &str);
    fn predefine_fn(&self, instance: Instance, visibility: Visibility, symbol_name: &str);
}
