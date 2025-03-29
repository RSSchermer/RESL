use std::ops::Deref;

use indexmap::IndexMap;
use rustc_ast::{IsAuto, Mutability};
use rustc_hir::{
    BodyId, FnSig, GenericBounds, Generics, HirId, Impl, Item, ItemId, ItemKind, Mod, Safety,
    TraitItemRef, Ty, VariantData,
};
use rustc_middle::hir;
use rustc_span::def_id::{DefId, LocalDefId, LocalModDefId};
use rustc_span::source_map::Spanned;
use rustc_span::Span;

use crate::context::ReslContext as Cx;

pub struct HirExt {
    pub shader_source_requests: Vec<ShaderSourceRequest>,
    pub mod_ext: IndexMap<LocalModDefId, ModExt>,
    pub fn_ext: IndexMap<LocalDefId, FnExt>,
    pub struct_ext: IndexMap<ItemId, StructExt>,
    pub trait_ext: IndexMap<ItemId, TraitExt>,
    pub const_ext: IndexMap<ItemId, ConstExt>,
    pub static_ext: IndexMap<ItemId, StaticExt>,
    pub impl_ext: IndexMap<ItemId, ImplExt>,
    pub param_ext: IndexMap<HirId, ParamExt>,
    pub generic_param_ext: IndexMap<HirId, GenericParamExt>,
    pub field_ext: IndexMap<HirId, FieldExt>,
}

impl HirExt {
    pub fn new() -> Self {
        HirExt {
            shader_source_requests: vec![],
            mod_ext: Default::default(),
            fn_ext: Default::default(),
            struct_ext: Default::default(),
            trait_ext: Default::default(),
            const_ext: Default::default(),
            static_ext: Default::default(),
            impl_ext: Default::default(),
            param_ext: Default::default(),
            generic_param_ext: Default::default(),
            field_ext: Default::default(),
        }
    }

    pub fn extend_item<'ext, 'hir>(
        &'ext self,
        item: &'hir Item,
    ) -> Option<ExtendedItem<'hir, 'ext>> {
        match &item.kind {
            ItemKind::Static(ty, mutability, body_id) => {
                self.static_ext
                    .get(&item.item_id())
                    .map(|ext| ExtendedItem {
                        item,
                        kind: ExtendedItemKind::Static(ty, *mutability, *body_id, ext),
                    })
            }
            ItemKind::Const(ty, generics, body_id) => {
                self.const_ext.get(&item.item_id()).map(|ext| ExtendedItem {
                    item,
                    kind: ExtendedItemKind::Const(ty, generics, *body_id, ext),
                })
            }
            ItemKind::Fn {
                sig,
                generics,
                body,
                ..
            } => self
                .fn_ext
                .get(&item.item_id().owner_id.def_id)
                .map(|ext| ExtendedItem {
                    item,
                    kind: ExtendedItemKind::Fn(sig, generics, *body, ext),
                }),
            ItemKind::Mod(m) => {
                let id = LocalModDefId::new_unchecked(item.owner_id.def_id);

                self.mod_ext.get(&id).map(|ext| ExtendedItem {
                    item,
                    kind: ExtendedItemKind::Mod(m, ext),
                })
            }
            ItemKind::Struct(variant_data, generics) => {
                self.struct_ext
                    .get(&item.item_id())
                    .map(|ext| ExtendedItem {
                        item,
                        kind: ExtendedItemKind::Struct(variant_data, generics, ext),
                    })
            }
            ItemKind::Trait(is_auto, safety, generics, bounds, items) => {
                self.trait_ext.get(&item.item_id()).map(|ext| ExtendedItem {
                    item,
                    kind: ExtendedItemKind::Trait(*is_auto, *safety, generics, bounds, items, ext),
                })
            }
            ItemKind::Impl(i) => self.impl_ext.get(&item.item_id()).map(|ext| ExtendedItem {
                item,
                kind: ExtendedItemKind::Impl(i, ext),
            }),
            _ => None,
        }
    }

    pub fn get_mod_ext(&self, mod_: LocalModDefId) -> Option<&ModExt> {
        self.mod_ext.get(&mod_)
    }

    pub fn param_ext(&self, id: HirId) -> Option<&ParamExt> {
        self.param_ext.get(&id)
    }

    pub fn expect_param_ext(&self, id: HirId) -> &ParamExt {
        self.param_ext(id).expect("node does not have a `ParamExt`")
    }

    pub fn generic_param_ext(&self, id: HirId) -> Option<&GenericParamExt> {
        self.generic_param_ext.get(&id)
    }

    pub fn expect_generic_param_ext(&self, id: HirId) -> &GenericParamExt {
        self.generic_param_ext(id)
            .expect("node does not have a `GenericParamExt`")
    }

    pub fn fn_ext(&self, id: LocalDefId) -> Option<&FnExt> {
        self.fn_ext.get(&id)
    }

    pub fn expect_fn_ext(&self, id: LocalDefId) -> &FnExt {
        self.fn_ext(id).expect("node does not have a `FnExt`")
    }

    pub fn static_ext(&self, id: ItemId) -> Option<&StaticExt> {
        self.static_ext.get(&id)
    }

    pub fn expect_static_ext(&self, id: ItemId) -> &StaticExt {
        self.static_ext(id)
            .expect("node does not have a `StaticExt`")
    }
}

#[derive(Debug)]
pub struct ShaderSourceRequest {
    pub shader_mod: DefId,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum BlendSrc {
    Zero,
    One,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ShaderIOBinding {
    VertexIndex,
    InstanceIndex,
    Position {
        invariant: Option<Span>,
    },
    FrontFacing,
    FragDepth,
    SampleIndex,
    SampleMask,
    LocalInvocationId,
    LocalInvocationIndex,
    GlobalInvocationId,
    WorkgroupId,
    NumWorkgroups,
    Location {
        location: Spanned<u32>,
        blend_src: Option<Spanned<BlendSrc>>,
        interpolation: Option<Interpolation>,
    },
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Interpolation {
    pub tpe: InterpolationType,
    pub sampling: Option<InterpolationSampling>,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum InterpolationType {
    Perspective,
    Linear,
    Flat,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum InterpolationSampling {
    Center,
    Centroid,
    Sample,
    First,
    Either,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ParamExt {
    pub shader_io_binding: Option<ShaderIOBinding>,
}

#[derive(Debug)]
pub struct GenericParamExt {
    pub specialize: bool,
}

#[derive(Debug)]
pub struct ModExt {
    pub is_shader_module: bool,
}

#[derive(Debug)]
pub struct WorkgroupSize {
    pub x: u32,
    pub y: u32,
    pub z: u32,
    pub span: Span,
}

#[derive(Debug)]
pub struct ExtendedItem<'hir, 'ext> {
    pub item: &'hir Item<'hir>,
    pub kind: ExtendedItemKind<'hir, 'ext>,
}

impl<'hir, 'ext> ExtendedItem<'hir, 'ext> {
    pub fn expect_fn(self) -> (&'hir FnSig<'hir>, &'hir Generics<'hir>, BodyId, &'ext FnExt) {
        if let ExtendedItemKind::Fn(sig, generics, body, ext) = self.kind {
            (sig, generics, body, ext)
        } else {
            panic!("expected fn")
        }
    }

    pub fn expect_impl(self) -> (&'hir Impl<'hir>, &'ext ImplExt) {
        if let ExtendedItemKind::Impl(i, ext) = self.kind {
            (i, ext)
        } else {
            panic!("expected impl")
        }
    }

    pub fn expect_trait(
        self,
    ) -> (
        IsAuto,
        Safety,
        &'hir Generics<'hir>,
        GenericBounds<'hir>,
        &'hir [TraitItemRef],
        &'ext TraitExt,
    ) {
        if let ExtendedItemKind::Trait(is_auto, safety, generics, bounds, items, ext) = self.kind {
            (is_auto, safety, generics, bounds, items, ext)
        } else {
            panic!("expected trait")
        }
    }

    pub fn expect_struct(
        self,
    ) -> (
        &'hir VariantData<'hir>,
        &'hir Generics<'hir>,
        &'ext StructExt,
    ) {
        if let ExtendedItemKind::Struct(variant_data, generics, ext) = self.kind {
            (variant_data, generics, ext)
        } else {
            panic!("expected struct")
        }
    }

    pub fn expect_const(self) -> (&'hir Ty<'hir>, &'hir Generics<'hir>, BodyId, &'ext ConstExt) {
        if let ExtendedItemKind::Const(ty, generics, body_id, ext) = self.kind {
            (ty, generics, body_id, ext)
        } else {
            panic!("expected const")
        }
    }

    pub fn expect_static(self) -> (&'hir Ty<'hir>, Mutability, BodyId, &'ext StaticExt) {
        if let ExtendedItemKind::Static(ty, mutability, body_id, ext) = self.kind {
            (ty, mutability, body_id, ext)
        } else {
            panic!("expected static")
        }
    }

    pub fn expect_mod(self) -> (&'hir Mod<'hir>, &'ext ModExt) {
        if let ExtendedItemKind::Mod(m, ext) = self.kind {
            (m, ext)
        } else {
            panic!("expected mod")
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ExtendedItemKind<'hir, 'ext> {
    Fn(&'hir FnSig<'hir>, &'hir Generics<'hir>, BodyId, &'ext FnExt),
    Impl(&'hir Impl<'hir>, &'ext ImplExt),
    Trait(
        IsAuto,
        Safety,
        &'hir Generics<'hir>,
        GenericBounds<'hir>,
        &'hir [TraitItemRef],
        &'ext TraitExt,
    ),
    Struct(
        &'hir VariantData<'hir>,
        &'hir Generics<'hir>,
        &'ext StructExt,
    ),
    Const(&'hir Ty<'hir>, &'hir Generics<'hir>, BodyId, &'ext ConstExt),
    Static(&'hir Ty<'hir>, Mutability, BodyId, &'ext StaticExt),
    Mod(&'hir Mod<'hir>, &'ext ModExt),
}

#[derive(Debug)]
pub enum FnExt {
    GpuFn,
    Compute(Option<WorkgroupSize>),
    VertexEntryPoint,
    FragmentEntryPoint,
}

#[derive(Debug)]
pub struct ImplExt {}

#[derive(Debug)]
pub struct TraitExt {}

#[derive(Debug)]
pub struct StructExt {}

#[derive(Debug)]
pub struct FieldExt {
    pub shader_io_binding: Option<ShaderIOBinding>,
}

#[derive(Debug)]
pub struct OverrideId {
    pub value: u32,
    pub span: Span,
}

#[derive(Debug)]
pub struct ConstExt {
    pub id: OverrideId,
    pub required: bool,
}

#[derive(Debug)]
pub struct ResourceBindingGroup {
    pub value: u32,
    pub span: Span,
}

#[derive(Debug)]
pub struct ResourceBindingBinding {
    pub value: u32,
    pub span: Span,
}

#[derive(Debug)]
pub struct ResourceBinding {
    pub group: ResourceBindingGroup,
    pub binding: ResourceBindingBinding,
}

#[derive(Debug)]
pub enum StaticExt {
    Uniform(ResourceBinding),
    Storage(ResourceBinding),
    Workgroup,
}
