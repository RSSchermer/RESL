use stable_mir::mir::mono::Instance;

use super::BackendTypes;

pub trait MiscCodegenMethods: BackendTypes {
    fn get_fn(&self, instance: &Instance) -> Self::Function;
    fn get_fn_addr(&self, instance: &Instance) -> Self::Value;
}
