use std::cell::RefCell;

use rustc_abi::{
    AddressSpace, Align, BackendRepr, FieldsShape, Float, HasDataLayout, Integer, Primitive, Reg,
    Size, TargetDataLayout, Variants,
};
use rustc_ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::common::TypeKind;
use rustc_codegen_ssa::mir::debuginfo::{FunctionDebugContext, VariableKind};
use rustc_codegen_ssa::mono_item::MonoItemExt;
use rustc_codegen_ssa::traits::{
    AsmCodegenMethods, BackendTypes, BaseTypeCodegenMethods, ConstCodegenMethods,
    DebugInfoCodegenMethods, GlobalAsmOperandRef, LayoutTypeCodegenMethods, MiscCodegenMethods,
    PreDefineCodegenMethods, StaticCodegenMethods, TypeMembershipCodegenMethods,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_middle::mir::interpret::{ConstAllocation, GlobalAlloc, Scalar};
use rustc_middle::mir::mono::{CodegenUnit, Linkage, MonoItem, Visibility};
use rustc_middle::mir::Body;
use rustc_middle::ty;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOf, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, HasTypingEnv, LayoutError,
    LayoutOf, LayoutOfHelpers, MaybeResult, TyAndLayout,
};
use rustc_middle::ty::{
    FloatTy, Instance, IntTy, PolyExistentialTraitRef, Ty, TyCtxt, TyKind, TypingEnv, UintTy,
};
use rustc_session::Session;
use rustc_span::def_id::{DefId, LOCAL_CRATE};
use rustc_span::{SourceFile, Span, Symbol};
use rustc_target::callconv::{CastTarget, FnAbi};
use slir::cfg::InlineConst;
use slir::Module;

use crate::context::ReslContext;
use crate::hir_ext::{
    BlendSrc, Interpolation, InterpolationSampling, InterpolationType, ResourceBinding,
    ShaderIOBinding, StaticExt,
};
use crate::slir_build::builder::Builder;
use crate::slir_build::ty::Type;
use crate::slir_build::value::Value;

fn scalar_ty_kind(scalar: rustc_abi::Scalar) -> slir::ty::TypeKind {
    if scalar.is_bool() {
        slir::ty::TypeKind::Scalar(slir::ty::ScalarKind::Bool)
    } else {
        match scalar.primitive() {
            Primitive::Int(Integer::I32, true) => {
                slir::ty::TypeKind::Scalar(slir::ty::ScalarKind::I32)
            }
            Primitive::Int(Integer::I32, false) => {
                slir::ty::TypeKind::Scalar(slir::ty::ScalarKind::U32)
            }
            Primitive::Float(Float::F32) => slir::ty::TypeKind::Scalar(slir::ty::ScalarKind::F32),
            _ => slir::ty::TypeKind::Ptr,
        }
    }
}

fn scalar_resolve(cx: &CodegenContext, scalar: rustc_abi::Scalar) -> slir::ty::Type {
    *cx.scalar_to_slir
        .borrow_mut()
        .entry(scalar)
        .or_insert_with(|| cx.module.borrow_mut().ty.register(scalar_ty_kind(scalar)))
}

pub fn ty_and_layout_resolve<'tcx>(
    cx: &CodegenContext<'_, 'tcx>,
    layout: TyAndLayout<'tcx>,
) -> slir::ty::Type {
    // Note: this cannot be inlined into the if-let expression, because then the borrow guard would
    // not drop in time, which would result in an "already borrowed" error.
    let ty = cx.ty_to_slir.borrow().get(&layout).copied();

    if let Some(ty) = ty {
        ty
    } else {
        let ty = ty_and_layout_register(cx, layout);

        cx.ty_to_slir.borrow_mut().insert(layout, ty);

        ty
    }
}

fn ty_and_layout_register<'tcx>(
    cx: &CodegenContext<'_, 'tcx>,
    layout: TyAndLayout<'tcx>,
) -> slir::ty::Type {
    match layout.backend_repr {
        BackendRepr::Scalar(scalar) => {
            return scalar_resolve(cx, scalar);
        }
        BackendRepr::ScalarPair(s0, s1) => {
            let t0 = scalar_resolve(cx, s0);
            let t1 = scalar_resolve(cx, s1);

            // My understanding from rustc_codegen_llvm is that there is never any padding between
            // the values of a scalar pair (as they are never meant to be stored to memory).
            let fields = vec![
                slir::StructField { offset: 0, ty: t0 },
                slir::StructField {
                    offset: s0.size(cx).bytes(),
                    ty: t1,
                },
            ];

            let struct_handle = cx
                .module
                .borrow_mut()
                .structs
                .register(slir::StructData { fields });
            let ty = cx
                .module
                .borrow_mut()
                .ty
                .register(slir::ty::TypeKind::Struct(struct_handle));

            cx.ty_to_slir.borrow_mut().insert(layout, ty);

            return ty;
        }
        BackendRepr::Vector { element, count } => {
            let base = scalar_resolve(cx, element);
            let stride = element.size(cx).bytes();
            let ty = cx
                .module
                .borrow_mut()
                .ty
                .register(slir::ty::TypeKind::Array {
                    base,
                    stride,
                    count,
                });

            cx.ty_to_slir.borrow_mut().insert(layout, ty);

            return ty;
        }
        _ => {}
    }

    if let Variants::Multiple { .. } = &layout.variants {
        panic!("cannot codegen enums with multiple variants (should have been caught during the analysis phase)")
    }

    match layout.fields {
        FieldsShape::Primitive => panic!("primitive should have been handled earlier"),
        FieldsShape::Array { stride, count } => {
            let element_layout = layout.field(cx, 0);
            let base = ty_and_layout_resolve(cx, element_layout);
            let ty = cx
                .module
                .borrow_mut()
                .ty
                .register(slir::ty::TypeKind::Array {
                    base,
                    stride: stride.bytes(),
                    count,
                });

            cx.ty_to_slir.borrow_mut().insert(layout, ty);

            return ty;
        }
        FieldsShape::Arbitrary { ref offsets, .. } => {
            let fields = layout
                .fields
                .index_by_increasing_offset()
                .map(|i| {
                    let layout = layout.field(cx, i);
                    let ty = ty_and_layout_resolve(cx, layout);

                    slir::StructField {
                        offset: offsets[i.into()].bytes(),
                        ty,
                    }
                })
                .collect();

            let struct_handle = cx
                .module
                .borrow_mut()
                .structs
                .register(slir::StructData { fields });
            let ty = cx
                .module
                .borrow_mut()
                .ty
                .register(slir::ty::TypeKind::Struct(struct_handle));

            cx.ty_to_slir.borrow_mut().insert(layout, ty);

            return ty;
        }
        _ => {}
    }

    panic!("unsupported type and layout");
}

fn blend_src_to_slir(blend_src: BlendSrc) -> slir::BlendSrc {
    match blend_src {
        BlendSrc::Zero => slir::BlendSrc::Zero,
        BlendSrc::One => slir::BlendSrc::One,
    }
}

fn interpolation_tpe_to_slir(ty: InterpolationType) -> slir::InterpolationType {
    match ty {
        InterpolationType::Perspective => slir::InterpolationType::Perspective,
        InterpolationType::Linear => slir::InterpolationType::Linear,
        InterpolationType::Flat => slir::InterpolationType::Flat,
    }
}

fn interpolation_sampling_to_slir(sampling: InterpolationSampling) -> slir::InterpolationSampling {
    match sampling {
        InterpolationSampling::Center => slir::InterpolationSampling::Center,
        InterpolationSampling::Centroid => slir::InterpolationSampling::Centroid,
        InterpolationSampling::Sample => slir::InterpolationSampling::Sample,
        InterpolationSampling::First => slir::InterpolationSampling::First,
        InterpolationSampling::Either => slir::InterpolationSampling::Either,
    }
}

fn interpolation_to_slir(interpolation: Interpolation) -> slir::Interpolation {
    slir::Interpolation {
        tpe: interpolation_tpe_to_slir(interpolation.tpe),
        sampling: interpolation.sampling.map(interpolation_sampling_to_slir),
    }
}

fn shader_io_binding_to_slir(shader_io_binding: ShaderIOBinding) -> slir::ShaderIOBinding {
    match shader_io_binding {
        ShaderIOBinding::VertexIndex => slir::ShaderIOBinding::VertexIndex,
        ShaderIOBinding::InstanceIndex => slir::ShaderIOBinding::InstanceIndex,
        ShaderIOBinding::Position { invariant } => slir::ShaderIOBinding::Position {
            invariant: invariant.is_some(),
        },
        ShaderIOBinding::FrontFacing => slir::ShaderIOBinding::FrontFacing,
        ShaderIOBinding::FragDepth => slir::ShaderIOBinding::FragDepth,
        ShaderIOBinding::SampleIndex => slir::ShaderIOBinding::SampleIndex,
        ShaderIOBinding::SampleMask => slir::ShaderIOBinding::SampleMask,
        ShaderIOBinding::LocalInvocationId => slir::ShaderIOBinding::LocalInvocationId,
        ShaderIOBinding::LocalInvocationIndex => slir::ShaderIOBinding::LocalInvocationIndex,
        ShaderIOBinding::GlobalInvocationId => slir::ShaderIOBinding::GlobalInvocationId,
        ShaderIOBinding::WorkgroupId => slir::ShaderIOBinding::WorkgroupId,
        ShaderIOBinding::NumWorkgroups => slir::ShaderIOBinding::NumWorkgroups,
        ShaderIOBinding::Location {
            location,
            blend_src,
            interpolation,
        } => slir::ShaderIOBinding::Location {
            location: location.node,
            blend_src: blend_src.map(|s| blend_src_to_slir(s.node)),
            interpolation: interpolation.map(interpolation_to_slir),
        },
    }
}

fn resource_binding_to_slir(resource_binding: &ResourceBinding) -> slir::ResourceBinding {
    slir::ResourceBinding {
        group: resource_binding.group.value,
        binding: resource_binding.binding.value,
    }
}

#[derive(Clone, Copy)]
pub enum SlirStatic {
    Uniform(slir::UniformBinding),
    Storage(slir::StorageBinding),
    Workgroup(slir::WorkgroupBinding),
}

pub struct CodegenContext<'a, 'tcx> {
    pub rcx: &'a ReslContext<'tcx>,
    pub module_name: slir::Symbol,
    pub module: RefCell<slir::Module>,
    pub cfg: RefCell<slir::cfg::Cfg>,
    pub scalar_to_slir: RefCell<FxHashMap<rustc_abi::Scalar, slir::ty::Type>>,
    pub ty_to_slir: RefCell<FxHashMap<TyAndLayout<'tcx>, slir::ty::Type>>,
    pub instance_to_slir: RefCell<FxHashMap<Instance<'tcx>, slir::Function>>,
    pub static_to_slir: RefCell<FxHashMap<Instance<'tcx>, SlirStatic>>,
}

impl<'a, 'tcx> CodegenContext<'a, 'tcx> {
    pub fn new(rcx: &'a ReslContext<'tcx>, module_name: slir::Symbol) -> Self {
        CodegenContext {
            rcx,
            module_name,
            module: RefCell::new(slir::Module::new(module_name)),
            cfg: RefCell::new(Default::default()),
            scalar_to_slir: RefCell::new(Default::default()),
            ty_to_slir: RefCell::new(Default::default()),
            instance_to_slir: RefCell::new(Default::default()),
            static_to_slir: RefCell::new(Default::default()),
        }
    }

    pub fn finish(self) -> (slir::Module, slir::cfg::Cfg) {
        (self.module.into_inner(), self.cfg.into_inner())
    }
}

impl<'a, 'tcx> PreDefineCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {
    fn predefine_static(
        &self,
        def_id: DefId,
        linkage: Linkage,
        visibility: Visibility,
        symbol_name: &str,
    ) {
        let item = self.tcx().hir().expect_item(def_id.expect_local());
        let (_, mutability, _, ext) = self
            .rcx
            .hir_ext()
            .extend_item(item)
            .expect("statics referenced by RESL functions must be extended")
            .expect_static();

        let instance = Instance::mono(self.tcx(), def_id);
        let ty = instance.ty(self.tcx(), TypingEnv::fully_monomorphized());
        let layout = self.layout_of(ty);
        let ty = ty_and_layout_resolve(self, layout);

        let mut module = self.module.borrow_mut();

        match ext {
            StaticExt::Uniform(resource_binding) => {
                let b = module.uniform_bindings.register(slir::UniformBindingData {
                    ty,
                    resource_binding: resource_binding_to_slir(resource_binding),
                });

                self.static_to_slir
                    .borrow_mut()
                    .insert(instance, SlirStatic::Uniform(b));
            }
            StaticExt::Storage(resource_binding) => {
                let b = module.storage_bindings.register(slir::StorageBindingData {
                    ty,
                    resource_binding: resource_binding_to_slir(resource_binding),
                    writable: mutability.is_mut(),
                });

                self.static_to_slir
                    .borrow_mut()
                    .insert(instance, SlirStatic::Storage(b));
            }
            StaticExt::Workgroup => {
                let b = module
                    .workgroup_bindings
                    .register(slir::WorkgroupBindingData { ty });

                self.static_to_slir
                    .borrow_mut()
                    .insert(instance, SlirStatic::Workgroup(b));
            }
        }
    }

    fn predefine_fn(
        &self,
        instance: Instance<'tcx>,
        _linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let function_name = slir::Symbol::from_ref(symbol_name);
        let function = slir::Function {
            name: function_name,
            module: self.module_name,
        };
        let def_id = instance.def_id();

        let abi = self.fn_abi_of_instance(instance, ty::List::empty());

        let mut arg_io_bindings = vec![None; abi.args.len()];

        if let Some(local_id) = def_id.as_local()
            && let Some(fn_ext) = self.rcx.hir_ext().fn_ext(local_id)
        {
            let body = self.rcx.tcx().hir().body_owned_by(local_id);

            for (i, param) in body.params.iter().enumerate() {
                if let Some(param_ext) = self.rcx.hir_ext().param_ext(param.hir_id)
                    && let Some(shader_io_binding) = param_ext.shader_io_binding
                {
                    arg_io_bindings[i] = Some(shader_io_binding)
                }
            }
        }

        let args = abi
            .args
            .iter()
            .zip(arg_io_bindings)
            .map(|(arg, binding)| {
                let ty = ty_and_layout_resolve(self, arg.layout);

                slir::FnArg {
                    ty,
                    shader_io_binding: binding.map(shader_io_binding_to_slir),
                }
            })
            .collect();

        let ret_ty = if abi.ret.is_ignore() {
            None
        } else {
            Some(ty_and_layout_resolve(self, abi.ret.layout))
        };

        let function_ty = self
            .module
            .borrow_mut()
            .ty
            .register(slir::ty::TypeKind::Function(function));

        let sig = slir::FnSig {
            name: function_name,
            ty: function_ty,
            args,
            ret_ty,
        };

        self.cfg
            .borrow_mut()
            .function_body
            .insert(function, slir::cfg::Body::init(&sig));
        self.module.borrow_mut().fn_sigs.register(function, sig);

        self.instance_to_slir
            .borrow_mut()
            .insert(instance, function);
    }
}

impl<'a, 'tcx> AsmCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {
    fn codegen_global_asm(
        &self,
        template: &[InlineAsmTemplatePiece],
        operands: &[GlobalAsmOperandRef<'tcx>],
        options: InlineAsmOptions,
        line_spans: &[Span],
    ) {
        todo!()
    }

    fn mangled_name(&self, instance: Instance<'tcx>) -> String {
        todo!()
    }
}

impl<'a, 'tcx> DebugInfoCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {
    fn create_vtable_debuginfo(
        &self,
        ty: Ty<'tcx>,
        trait_ref: Option<PolyExistentialTraitRef<'tcx>>,
        vtable: Self::Value,
    ) {
        todo!()
    }

    fn create_function_debug_context(
        &self,
        instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        llfn: Self::Function,
        mir: &Body<'tcx>,
    ) -> Option<FunctionDebugContext<'tcx, Self::DIScope, Self::DILocation>> {
        None
    }

    fn dbg_scope_fn(
        &self,
        instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        maybe_definition_llfn: Option<Self::Function>,
    ) -> Self::DIScope {
        todo!()
    }

    fn dbg_loc(
        &self,
        scope: Self::DIScope,
        inlined_at: Option<Self::DILocation>,
        span: Span,
    ) -> Self::DILocation {
        todo!()
    }

    fn extend_scope_to_file(
        &self,
        scope_metadata: Self::DIScope,
        file: &SourceFile,
    ) -> Self::DIScope {
        todo!()
    }

    fn debuginfo_finalize(&self) {
        todo!()
    }

    fn create_dbg_var(
        &self,
        variable_name: Symbol,
        variable_type: Ty<'tcx>,
        scope_metadata: Self::DIScope,
        variable_kind: VariableKind,
        span: Span,
    ) -> Self::DIVariable {
        todo!()
    }
}

impl<'a, 'tcx> BackendTypes for CodegenContext<'a, 'tcx> {
    type Value = Value;
    type Metadata = ();
    type Function = slir::Function;
    type BasicBlock = (slir::Function, slir::cfg::BasicBlock);
    type Type = Type;
    type Funclet = ();
    type DIScope = ();
    type DILocation = ();
    type DIVariable = ();
}

impl<'a, 'tcx> StaticCodegenMethods for CodegenContext<'a, 'tcx> {
    fn static_addr_of(&self, cv: Self::Value, align: Align, kind: Option<&str>) -> Self::Value {
        todo!()
    }

    fn codegen_static(&self, def_id: DefId) {
        // TODO
    }

    fn add_used_global(&self, global: Self::Value) {
        todo!()
    }

    fn add_compiler_used_global(&self, global: Self::Value) {
        todo!()
    }
}

impl<'a, 'tcx> ConstCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        todo!()
    }

    fn const_undef(&self, t: Self::Type) -> Self::Value {
        todo!()
    }

    fn is_undef(&self, v: Self::Value) -> bool {
        todo!()
    }

    fn const_poison(&self, t: Self::Type) -> Self::Value {
        todo!()
    }

    fn const_bool(&self, val: bool) -> Self::Value {
        todo!()
    }

    fn const_i8(&self, i: i8) -> Self::Value {
        todo!()
    }

    fn const_i16(&self, i: i16) -> Self::Value {
        todo!()
    }

    fn const_i32(&self, i: i32) -> Self::Value {
        todo!()
    }

    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        todo!()
    }

    fn const_u8(&self, i: u8) -> Self::Value {
        todo!()
    }

    fn const_u32(&self, i: u32) -> Self::Value {
        todo!()
    }

    fn const_u64(&self, i: u64) -> Self::Value {
        todo!()
    }

    fn const_u128(&self, i: u128) -> Self::Value {
        todo!()
    }

    fn const_usize(&self, i: u64) -> Self::Value {
        slir::cfg::Value::InlineConst(slir::cfg::InlineConst::U32(i as u32)).into()
    }

    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        todo!()
    }

    fn const_uint_big(&self, t: Self::Type, u: u128) -> Self::Value {
        todo!()
    }

    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        todo!()
    }

    fn const_str(&self, s: &str) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn const_struct(&self, elts: &[Self::Value], packed: bool) -> Self::Value {
        todo!()
    }

    fn const_vector(&self, elts: &[Self::Value]) -> Self::Value {
        todo!()
    }

    fn const_to_opt_uint(&self, v: Self::Value) -> Option<u64> {
        todo!()
    }

    fn const_to_opt_u128(&self, v: Self::Value, _sign_ext: bool) -> Option<u128> {
        match v.expect_value() {
            slir::cfg::Value::InlineConst(v) => match v {
                InlineConst::U32(v) => Some(v as u128),
                InlineConst::I32(_) => unimplemented!(),
                InlineConst::F32(_) => unimplemented!(),
                InlineConst::Bool(_) => unimplemented!(),
                InlineConst::Ptr(_) => unimplemented!(),
            },
            _ => None,
        }
    }

    fn const_data_from_alloc(&self, alloc: ConstAllocation<'tcx>) -> Self::Value {
        todo!()
    }

    fn scalar_to_backend(
        &self,
        cv: Scalar,
        layout: rustc_abi::Scalar,
        llty: Self::Type,
    ) -> Self::Value {
        let ty = llty.expect_slir_type();

        match cv {
            Scalar::Int(int) => {
                let module = self.module.borrow();

                let value = if ty == slir::ty::TY_U32 {
                    slir::cfg::InlineConst::U32(int.to_u32())
                } else if ty == slir::ty::TY_I32 {
                    slir::cfg::InlineConst::I32(int.to_i32())
                } else if ty == slir::ty::TY_BOOL {
                    slir::cfg::InlineConst::Bool(int.try_to_bool().unwrap())
                } else {
                    self.tcx().dcx().fatal("unsupported scalar type")
                };

                slir::cfg::Value::InlineConst(value).into()
            }
            Scalar::Ptr(ptr, _) => {
                let (prov, offset) = ptr.into_parts();
                let alloc_id = prov.alloc_id();

                let (base, ty) = match self.tcx().global_alloc(alloc_id) {
                    GlobalAlloc::Function { .. } => panic!("function pointers are not supported"),
                    GlobalAlloc::VTable(_, _) => panic!("V-tables are not supported"),
                    GlobalAlloc::Static(def_id) => {
                        let instance = Instance::mono(self.tcx(), def_id);
                        let slir_static = *self
                            .static_to_slir
                            .borrow()
                            .get(&instance)
                            .expect("pointer to undefined static");

                        let base = match slir_static {
                            SlirStatic::Uniform(b) => slir::cfg::RootIdentifier::Uniform(b),
                            SlirStatic::Storage(b) => slir::cfg::RootIdentifier::Storage(b),
                            SlirStatic::Workgroup(b) => slir::cfg::RootIdentifier::Workgroup(b),
                        };
                        let ty = instance.ty(self.tcx(), TypingEnv::fully_monomorphized());
                        let layout = self.layout_of(ty);
                        let ty = ty_and_layout_resolve(self, layout);

                        (base, ty)
                    }
                    GlobalAlloc::Memory(_) => panic!("memory"),
                };

                slir::cfg::Value::InlineConst(slir::cfg::InlineConst::Ptr(slir::cfg::Ptr {
                    ty,
                    base,
                    offset: offset.bytes() as u32,
                }))
                .into()
            }
        }
    }

    fn const_ptr_byte_offset(&self, val: Self::Value, offset: Size) -> Self::Value {
        todo!()
    }
}

impl<'a, 'tcx> TypeMembershipCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {}

impl<'a, 'tcx> LayoutTypeCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        ty_and_layout_resolve(self, layout).into()
    }

    fn cast_backend_type(&self, ty: &CastTarget) -> Self::Type {
        todo!()
    }

    fn fn_decl_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        let ret_ty = if fn_abi.ret.is_ignore() {
            None
        } else {
            Some(ty_and_layout_resolve(self, fn_abi.ret.layout))
        };

        Type::FnDecl { ret_ty }
    }

    fn fn_ptr_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        todo!()
    }

    fn reg_backend_type(&self, ty: &Reg) -> Self::Type {
        todo!()
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        ty_and_layout_resolve(self, layout).into()
    }

    fn is_backend_immediate(&self, layout: TyAndLayout<'tcx>) -> bool {
        match layout.backend_repr {
            BackendRepr::Scalar(_) | BackendRepr::Vector { .. } => true,
            BackendRepr::ScalarPair(..) => false,
            BackendRepr::Uninhabited | BackendRepr::Memory { .. } => layout.is_zst(),
        }
    }

    fn is_backend_scalar_pair(&self, layout: TyAndLayout<'tcx>) -> bool {
        false
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout<'tcx>,
        index: usize,
        immediate: bool,
    ) -> Self::Type {
        todo!()
    }
}

impl<'a, 'tcx> MiscCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {
    fn vtables(
        &self,
    ) -> &RefCell<FxHashMap<(Ty<'tcx>, Option<PolyExistentialTraitRef<'tcx>>), Self::Value>> {
        todo!()
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Self::Function {
        let def_id = instance.def_id();

        let module_name = if def_id.krate == LOCAL_CRATE {
            self.module_name
        } else {
            let crate_name = self.rcx.tcx().crate_name(def_id.krate);

            slir::Symbol::from_ref(crate_name.as_str())
        };

        let instance_name = self.rcx.tcx().symbol_name(instance).name;
        let name = slir::Symbol::from_ref(instance_name);

        slir::Function {
            module: module_name,
            name,
        }
    }

    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        self.get_fn(instance).into()
    }

    fn eh_personality(&self) -> Self::Value {
        todo!()
    }

    fn sess(&self) -> &Session {
        self.rcx.tcx().sess
    }

    fn codegen_unit(&self) -> &'tcx CodegenUnit<'tcx> {
        todo!()
    }

    fn set_frame_pointer_type(&self, llfn: Self::Function) {
        todo!()
    }

    fn apply_target_cpu_attr(&self, llfn: Self::Function) {
        todo!()
    }

    fn declare_c_main(&self, fn_type: Self::Type) -> Option<Self::Function> {
        todo!()
    }
}

impl<'a, 'tcx> BaseTypeCodegenMethods<'tcx> for CodegenContext<'a, 'tcx> {
    fn type_i8(&self) -> Self::Type {
        slir::ty::TY_I32.into()
    }

    fn type_i16(&self) -> Self::Type {
        todo!()
    }

    fn type_i32(&self) -> Self::Type {
        todo!()
    }

    fn type_i64(&self) -> Self::Type {
        todo!()
    }

    fn type_i128(&self) -> Self::Type {
        todo!()
    }

    fn type_isize(&self) -> Self::Type {
        todo!()
    }

    fn type_f16(&self) -> Self::Type {
        todo!()
    }

    fn type_f32(&self) -> Self::Type {
        todo!()
    }

    fn type_f64(&self) -> Self::Type {
        todo!()
    }

    fn type_f128(&self) -> Self::Type {
        todo!()
    }

    fn type_array(&self, ty: Self::Type, len: u64) -> Self::Type {
        todo!()
    }

    fn type_func(&self, args: &[Self::Type], ret: Self::Type) -> Self::Type {
        todo!()
    }

    fn type_kind(&self, ty: Self::Type) -> TypeKind {
        todo!()
    }

    fn type_ptr(&self) -> Self::Type {
        todo!()
    }

    fn type_ptr_ext(&self, address_space: AddressSpace) -> Self::Type {
        todo!()
    }

    fn element_type(&self, ty: Self::Type) -> Self::Type {
        todo!()
    }

    fn vector_length(&self, ty: Self::Type) -> usize {
        todo!()
    }

    fn float_width(&self, ty: Self::Type) -> usize {
        todo!()
    }

    fn int_width(&self, ty: Self::Type) -> u64 {
        todo!()
    }

    fn val_ty(&self, v: Self::Value) -> Self::Type {
        todo!()
    }
}

impl<'a, 'tcx> HasDataLayout for CodegenContext<'a, 'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        &self.tcx().data_layout
    }
}

impl<'a, 'tcx> HasTyCtxt<'tcx> for CodegenContext<'a, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.rcx.tcx()
    }
}

impl<'a, 'tcx> HasTypingEnv<'tcx> for CodegenContext<'a, 'tcx> {
    fn typing_env(&self) -> TypingEnv<'tcx> {
        TypingEnv::fully_monomorphized()
    }
}

impl<'a, 'tcx> LayoutOfHelpers<'tcx> for CodegenContext<'a, 'tcx> {
    fn handle_layout_err(
        &self,
        err: LayoutError<'tcx>,
        span: Span,
        ty: Ty<'tcx>,
    ) -> <Self::LayoutOfResult as MaybeResult<TyAndLayout<'tcx>>>::Error {
        todo!()
    }
}

impl<'a, 'tcx> FnAbiOfHelpers<'tcx> for CodegenContext<'a, 'tcx> {
    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> <Self::FnAbiOfResult as MaybeResult<&'tcx FnAbi<'tcx, Ty<'tcx>>>>::Error {
        todo!()
    }
}
