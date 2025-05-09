use std::cell::RefCell;

use rustc_hash::FxHashMap;
use rustc_middle::bug;
use rustc_smir::rustc_internal::internal;
use rustc_span::def_id::LOCAL_CRATE;
use slir::Module;
use stable_mir::abi::{
    ArgAbi, FieldsShape, FloatLength, FnAbi, IntegerLength, PassMode, Primitive, TyAndLayout,
    ValueAbi, VariantsShape, WrappingRange,
};
use stable_mir::mir::alloc::GlobalAlloc;
use stable_mir::mir::mono::{Instance, StaticDef};
use stable_mir::target::MachineInfo;
use stable_mir::ty::ConstantKind::Ty;
use stable_mir::ty::{Align, Allocation, RigidTy, TyKind};
use stable_mir::{abi, CrateDef};

use crate::context::ReslContext;
use crate::hir_ext::{
    BlendSrc, FnExt, Interpolation, InterpolationSampling, InterpolationType, ResourceBinding,
    ShaderIOBinding, StaticExt,
};
use crate::slir_build::builder::Builder;
use crate::slir_build::ty::Type;
use crate::slir_build::value::Value;
use crate::stable_cg::traits::{
    BackendTypes, BaseTypeCodegenMethods, ConstCodegenMethods, LayoutTypeCodegenMethods,
    MiscCodegenMethods, PreDefineCodegenMethods, StaticCodegenMethods,
};
use crate::stable_cg::{Scalar, ScalarExt, TyAndLayoutExt, TypeKind};

fn scalar_ty(cx: &CodegenContext, scalar: abi::Scalar, layout: TyAndLayout) -> slir::ty::Type {
    if matches!(
        scalar,
        abi::Scalar::Initialized {
            value: Primitive::Int {
                length: IntegerLength::I8,
                signed: false
            },
            valid_range: WrappingRange { start: 0, end: 1 }
        }
    ) {
        return slir::ty::TY_BOOL;
    }

    let primitive = match scalar {
        abi::Scalar::Initialized { value, .. } => value,
        abi::Scalar::Union { value, .. } => value,
    };

    match primitive {
        Primitive::Int { signed: true, .. } => slir::ty::TY_I32,
        Primitive::Int { signed: false, .. } => slir::ty::TY_U32,
        Primitive::Float {
            length: FloatLength::F32,
        } => slir::ty::TY_F32,
        Primitive::Pointer(_) => {
            let TyKind::RigidTy(ty) = layout.ty.kind() else {
                bug!("primitive type must be rigid")
            };

            match ty {
                RigidTy::Ref(_, pointee_ty, _) | RigidTy::RawPtr(pointee_ty, _) => {
                    let pointee_layout = pointee_ty
                        .layout()
                        .expect("type must have known layout during codegen");
                    let pointee_ty = ty_and_layout_resolve(
                        cx,
                        TyAndLayout {
                            ty: pointee_ty,
                            layout: pointee_layout,
                        },
                    );

                    cx.module
                        .borrow_mut()
                        .ty
                        .register(slir::ty::TypeKind::Ptr(pointee_ty))
                }
                _ => bug!("pointer primitive type must be a ref or raw ptr"),
            }
        }
        _ => bug!("primitive type not supported by SLIR"),
    }
}

pub fn ty_and_layout_resolve(cx: &CodegenContext, layout: TyAndLayout) -> slir::ty::Type {
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

fn ty_and_layout_register(cx: &CodegenContext, layout: TyAndLayout) -> slir::ty::Type {
    let shape = layout.layout.shape();

    match shape.abi {
        ValueAbi::Scalar(scalar) => {
            return scalar_ty(cx, scalar, layout);
        }
        ValueAbi::ScalarPair(s0, s1) => {
            let t0 = scalar_ty(cx, s0, layout.field(0));
            let t1 = scalar_ty(cx, s1, layout.field(1));

            // My understanding from rustc_codegen_llvm is that there is never any padding between
            // the values of a scalar pair (as they are never meant to be stored to memory).
            let fields = vec![
                slir::StructField { offset: 0, ty: t0 },
                slir::StructField {
                    offset: s0.size(&MachineInfo::target()).bytes() as u64,
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
        ValueAbi::Vector { element, count } => {
            let base = scalar_ty(cx, element, layout.field(0));
            let stride = element.size(&MachineInfo::target()).bytes() as u64;
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

    if let VariantsShape::Multiple { .. } = &shape.variants {
        panic!("cannot stable_cg enums with multiple variants (should have been caught during the analysis phase)")
    }

    match shape.fields {
        FieldsShape::Primitive => panic!("primitive should have been handled earlier"),
        FieldsShape::Array { stride, count } => {
            let element_layout = layout.field(0);
            let base = ty_and_layout_resolve(cx, element_layout);
            let ty = cx
                .module
                .borrow_mut()
                .ty
                .register(slir::ty::TypeKind::Array {
                    base,
                    stride: stride.bytes() as u64,
                    count,
                });

            cx.ty_to_slir.borrow_mut().insert(layout, ty);

            return ty;
        }
        FieldsShape::Arbitrary { ref offsets, .. } => {
            let fields = shape
                .fields
                .fields_by_offset_order()
                .into_iter()
                .map(|i| {
                    let layout = layout.field(i);
                    let ty = ty_and_layout_resolve(cx, layout);

                    slir::StructField {
                        offset: offsets[i].bytes() as u64,
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
    pub ty_to_slir: RefCell<FxHashMap<TyAndLayout, slir::ty::Type>>,
    pub instance_to_slir: RefCell<FxHashMap<Instance, slir::Function>>,
    pub static_to_slir: RefCell<FxHashMap<StaticDef, SlirStatic>>,
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

impl<'a, 'tcx> PreDefineCodegenMethods for CodegenContext<'a, 'tcx> {
    fn predefine_static(&self, def: StaticDef, symbol_name: &str) {
        let def_id = internal(self.rcx.tcx(), def.0);
        let item = self.rcx.tcx().hir().expect_item(def_id.expect_local());
        let (_, mutability, _, ext) = self
            .rcx
            .hir_ext()
            .extend_item(item)
            .expect("statics referenced by RESL functions must be extended")
            .expect_static();

        let ty = def.ty();
        let ty = ty_and_layout_resolve(self, TyAndLayout::expect_from_ty(ty));

        let mut module = self.module.borrow_mut();

        match ext {
            StaticExt::Uniform(resource_binding) => {
                let b = module.uniform_bindings.register(slir::UniformBindingData {
                    ty,
                    resource_binding: resource_binding_to_slir(resource_binding),
                });

                self.static_to_slir
                    .borrow_mut()
                    .insert(def, SlirStatic::Uniform(b));
            }
            StaticExt::Storage(resource_binding) => {
                let b = module.storage_bindings.register(slir::StorageBindingData {
                    ty,
                    resource_binding: resource_binding_to_slir(resource_binding),
                    writable: mutability.is_mut(),
                });

                self.static_to_slir
                    .borrow_mut()
                    .insert(def, SlirStatic::Storage(b));
            }
            StaticExt::Workgroup => {
                let b = module
                    .workgroup_bindings
                    .register(slir::WorkgroupBindingData { ty });

                self.static_to_slir
                    .borrow_mut()
                    .insert(def, SlirStatic::Workgroup(b));
            }
        }
    }

    fn predefine_fn(&self, instance: Instance, symbol_name: &str) {
        let function_name = slir::Symbol::from_ref(symbol_name);
        let function = slir::Function {
            name: function_name,
            module: self.module_name,
        };
        let def_id = internal(self.rcx.tcx(), instance.def.def_id());

        let abi = instance.fn_abi().unwrap();

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

            match fn_ext {
                FnExt::Compute(size) => {
                    let (x, y, z) = if let Some(size) = size {
                        (size.x, size.y, size.z)
                    } else {
                        (1, 1, 1)
                    };

                    self.module
                        .borrow_mut()
                        .entry_points
                        .register(function, slir::EntryPointKind::Compute(x, y, z))
                }
                FnExt::VertexEntryPoint => self
                    .module
                    .borrow_mut()
                    .entry_points
                    .register(function, slir::EntryPointKind::Vertex),
                FnExt::FragmentEntryPoint => self
                    .module
                    .borrow_mut()
                    .entry_points
                    .register(function, slir::EntryPointKind::Fragment),
                _ => {}
            }
        }

        let mut args = Vec::new();

        if matches!(abi.ret.mode, PassMode::Indirect { .. }) {
            let ret_ty = ty_and_layout_resolve(
                self,
                TyAndLayout {
                    ty: abi.ret.ty,
                    layout: abi.ret.layout,
                },
            );

            args.push(slir::FnArg {
                ty: self
                    .module
                    .borrow_mut()
                    .ty
                    .register(slir::ty::TypeKind::Ptr(ret_ty)),
                shader_io_binding: None,
            })
        }

        for (arg, binding) in abi.args.iter().zip(arg_io_bindings) {
            match arg.mode {
                PassMode::Ignore => {}
                PassMode::Direct(_) => {
                    let ty = ty_and_layout_resolve(
                        self,
                        TyAndLayout {
                            ty: arg.ty,
                            layout: arg.layout,
                        },
                    );

                    args.push(slir::FnArg {
                        ty,
                        shader_io_binding: binding.map(shader_io_binding_to_slir),
                    });
                }
                PassMode::Pair(..) => {
                    let ValueAbi::ScalarPair(a, b) = arg.layout.shape().abi else {
                        bug!("value ABI does not match pass-mode")
                    };

                    let layout = TyAndLayout {
                        ty: arg.ty,
                        layout: arg.layout,
                    };

                    let a_ty = scalar_ty(self, a, layout.field(0));
                    let b_ty = scalar_ty(self, b, layout.field(1));

                    args.push(slir::FnArg {
                        ty: a_ty,
                        shader_io_binding: binding.map(shader_io_binding_to_slir),
                    });
                    args.push(slir::FnArg {
                        ty: b_ty,
                        shader_io_binding: None,
                    });
                }
                PassMode::Indirect { .. } => {
                    let ty = ty_and_layout_resolve(
                        self,
                        TyAndLayout {
                            ty: arg.ty,
                            layout: arg.layout,
                        },
                    );

                    args.push(slir::FnArg {
                        ty: self
                            .module
                            .borrow_mut()
                            .ty
                            .register(slir::ty::TypeKind::Ptr(ty)),
                        shader_io_binding: binding.map(shader_io_binding_to_slir),
                    });
                }
                PassMode::Cast { .. } => bug!("not supported by RESL"),
            }
        }

        let ret_ty = if matches!(abi.ret.mode, PassMode::Ignore | PassMode::Indirect { .. }) {
            None
        } else {
            Some(ty_and_layout_resolve(
                self,
                TyAndLayout {
                    ty: abi.ret.ty,
                    layout: abi.ret.layout,
                },
            ))
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

impl<'a, 'tcx> BackendTypes for CodegenContext<'a, 'tcx> {
    type Value = Value;
    type Local = slir::cfg::LocalValue;
    type Function = slir::Function;
    type BasicBlock = (slir::Function, slir::cfg::BasicBlock);
    type Type = Type;
}

impl<'a, 'tcx> StaticCodegenMethods for CodegenContext<'a, 'tcx> {
    fn static_addr_of(&self, cv: Self::Value, align: Align, kind: Option<&str>) -> Self::Value {
        todo!()
    }

    fn codegen_static(&self, def: StaticDef) {
        // TODO
    }
}

impl<'a, 'tcx> ConstCodegenMethods for CodegenContext<'a, 'tcx> {
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
        slir::cfg::Value::InlineConst(slir::cfg::InlineConst::U32(i as u32)).into()
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
                slir::cfg::InlineConst::U32(v) => Some(v as u128),
                _ => unimplemented!(),
            },
            _ => None,
        }
    }

    fn const_data_from_alloc(&self, alloc: &Allocation) -> Self::Value {
        todo!()
    }

    fn scalar_to_backend(&self, scalar: Scalar) -> Self::Value {
        let inline_const = match scalar {
            Scalar::U32(v) => slir::cfg::InlineConst::U32(v),
            Scalar::I32(v) => slir::cfg::InlineConst::I32(v),
            Scalar::F32(v) => slir::cfg::InlineConst::from(v),
            Scalar::Pointer(ptr) => {
                let (base, ty) = match GlobalAlloc::from(ptr.alloc_id) {
                    GlobalAlloc::Function(_) => bug!("function pointers are not supported by SLIR"),
                    GlobalAlloc::VTable(_, _) => bug!("V-tables are not supported by SLIR"),
                    GlobalAlloc::Static(def) => {
                        let slir_static = *self
                            .static_to_slir
                            .borrow()
                            .get(&def)
                            .expect("static should have been pre-defined");

                        let base = match slir_static {
                            SlirStatic::Uniform(b) => slir::cfg::RootIdentifier::Uniform(b),
                            SlirStatic::Storage(b) => slir::cfg::RootIdentifier::Storage(b),
                            SlirStatic::Workgroup(b) => slir::cfg::RootIdentifier::Workgroup(b),
                        };

                        let ty = def.ty();
                        let ty = ty_and_layout_resolve(self, TyAndLayout::expect_from_ty(ty));

                        (base, ty)
                    }
                    GlobalAlloc::Memory(_) => todo!(),
                };

                slir::cfg::InlineConst::Ptr(slir::cfg::Ptr {
                    pointee_ty: ty,
                    base,
                    offset: ptr.offset as u32,
                })
            }
        };

        inline_const.into()
    }
}

impl<'a, 'tcx> LayoutTypeCodegenMethods for CodegenContext<'a, 'tcx> {
    fn backend_type(&self, layout: TyAndLayout) -> Self::Type {
        ty_and_layout_resolve(self, layout).into()
    }

    fn fn_decl_backend_type(&self, fn_abi: &FnAbi) -> Self::Type {
        let ret_ty = if matches!(
            fn_abi.ret.mode,
            PassMode::Ignore | PassMode::Indirect { .. }
        ) {
            None
        } else {
            Some(ty_and_layout_resolve(
                self,
                TyAndLayout {
                    ty: fn_abi.ret.ty,
                    layout: fn_abi.ret.layout,
                },
            ))
        };

        Type::FnDecl { ret_ty }
    }

    fn immediate_backend_type(&self, layout: TyAndLayout) -> Self::Type {
        ty_and_layout_resolve(self, layout).into()
    }

    fn is_backend_immediate(&self, layout: TyAndLayout) -> bool {
        let shape = layout.layout.shape();

        match shape.abi {
            ValueAbi::Scalar(_) | ValueAbi::Vector { .. } => true,
            ValueAbi::ScalarPair(..) => false,
            ValueAbi::Uninhabited | ValueAbi::Aggregate { .. } => shape.is_1zst(),
        }
    }

    fn is_backend_scalar_pair(&self, layout: TyAndLayout) -> bool {
        let shape = layout.layout.shape();

        matches!(shape.abi, ValueAbi::ScalarPair(..))
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout,
        index: usize,
        immediate: bool,
    ) -> Self::Type {
        todo!()
    }
}

impl<'a, 'tcx> MiscCodegenMethods for CodegenContext<'a, 'tcx> {
    fn get_fn(&self, instance: &Instance) -> Self::Function {
        let krate = instance.def.krate();

        let module_name = if krate.is_local {
            self.module_name
        } else {
            slir::Symbol::from_ref(krate.name.as_str())
        };

        let name = slir::Symbol::from_ref(instance.mangled_name().as_str());

        slir::Function {
            module: module_name,
            name,
        }
    }

    fn get_fn_addr(&self, instance: &Instance) -> Self::Value {
        self.get_fn(instance).into()
    }
}

impl<'a, 'tcx> BaseTypeCodegenMethods for CodegenContext<'a, 'tcx> {
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
