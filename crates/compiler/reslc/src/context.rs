use std::sync::RwLock;

use rustc_ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::traits::{
    AsmCodegenMethods, BackendTypes, DebugInfoCodegenMethods, GlobalAsmOperandRef,
    PreDefineCodegenMethods,
};
use rustc_hash::FxHashMap;
use rustc_hir::{HirId, ItemId, Mod};
use rustc_middle::hir;
use rustc_middle::mir::mono::{Linkage, Visibility};
use rustc_middle::mir::Body;
use rustc_middle::ty::{Instance, PolyExistentialTraitRef, Ty, TyCtxt};
use rustc_span::def_id::{CrateNum, DefId, LocalModDefId};
use rustc_span::{SourceFile, Span, Symbol};
use rustc_target::callconv::FnAbi;

use crate::hir_ext::{
    ExtendedItem, FieldExt, FnExt, GenericParamExt, HirExt, ModExt, ParamExt, StructExt, TraitExt,
};
use crate::hir_ext_build;

pub fn generate_crate_name_to_num(tcx: TyCtxt) -> FxHashMap<Symbol, CrateNum> {
    let mut crate_name_to_num = FxHashMap::default();

    for crate_num in tcx.crates(()) {
        let crate_name = tcx.crate_name(*crate_num);

        crate_name_to_num.insert(crate_name, *crate_num);
    }

    crate_name_to_num
}

pub struct ReslContext<'tcx> {
    tcx: TyCtxt<'tcx>,
    hir_ext: HirExt,
    crate_name_to_num: FxHashMap<Symbol, CrateNum>,
}

impl<'tcx> ReslContext<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        ReslContext {
            tcx,
            hir_ext: HirExt::new(),
            crate_name_to_num: generate_crate_name_to_num(tcx),
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }

    pub fn crate_num_for_name(&self, name: Symbol) -> CrateNum {
        *self.crate_name_to_num.get(&name).expect("crate not found")
    }

    pub fn build_hir_ext(&mut self) {
        hir_ext_build::build(&mut self.hir_ext, self.tcx);
    }

    pub fn hir_ext(&self) -> &HirExt {
        &self.hir_ext
    }

    pub fn extended_item<'ext>(&'ext self, item_id: ItemId) -> Option<ExtendedItem<'tcx, 'ext>> {
        self.hir_ext.extend_item(self.tcx.hir().item(item_id))
    }

    pub fn extended_module<'ext>(
        &'ext self,
        id: LocalModDefId,
    ) -> Option<(&'tcx Mod<'tcx>, &'ext ModExt)> {
        self.hir_ext.get_mod_ext(id).map(|ext| {
            let (mod_, _, _) = self.tcx.hir().get_module(id);

            (mod_, ext)
        })
    }
}
