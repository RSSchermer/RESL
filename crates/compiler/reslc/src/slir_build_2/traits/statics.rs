use stable_mir::ty::Align;
use stable_mir::DefId;

use super::BackendTypes;

pub trait StaticCodegenMethods: BackendTypes {
    fn static_addr_of(&self, cv: Self::Value, align: Align, kind: Option<&str>) -> Self::Value;
    fn codegen_static(&self, def_id: DefId);
}

pub trait StaticBuilderMethods: BackendTypes {
    fn get_static(&mut self, def_id: DefId) -> Self::Value;
}
