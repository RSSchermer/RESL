use std::ops::{Deref, Range};

use rustc_abi::{Align, BackendRepr, HasDataLayout, Scalar, Size, TargetDataLayout, WrappingRange};
use rustc_ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::common::{
    AtomicOrdering, AtomicRmwBinOp, IntPredicate, RealPredicate, SynchronizationScope,
};
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiBuilderMethods, AsmBuilderMethods, BackendTypes, BuilderMethods,
    ConstCodegenMethods, CoverageInfoBuilderMethods, DebugInfoBuilderMethods, InlineAsmOperandRef,
    IntrinsicCallBuilderMethods, LayoutTypeCodegenMethods, OverflowOp, StaticBuilderMethods,
};
use rustc_codegen_ssa::MemFlags;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrs;
use rustc_middle::mir::coverage::CoverageKind;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOf, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, HasTypingEnv, LayoutError,
    LayoutOf, LayoutOfHelpers, MaybeResult, TyAndLayout,
};
use rustc_middle::ty::{Instance, Ty, TyCtxt, TypingEnv};
use rustc_span::def_id::DefId;
use rustc_span::Span;
use rustc_target::callconv::{ArgAbi, FnAbi};
use smallvec::smallvec;

use crate::slir_build::context::{ty_and_layout_resolve, CodegenContext as Cx};
use crate::slir_build::ty::Type;
use crate::slir_build::value::Value;

pub struct Builder<'a, 'tcx> {
    cx: &'a Cx<'a, 'tcx>,
    function: slir::Function,
    basic_block: slir::cfg::BasicBlock,
}

impl<'a, 'tcx> LayoutOfHelpers<'tcx> for Builder<'a, 'tcx> {
    fn handle_layout_err(
        &self,
        err: LayoutError<'tcx>,
        span: Span,
        ty: Ty<'tcx>,
    ) -> <Self::LayoutOfResult as MaybeResult<TyAndLayout<'tcx>>>::Error {
        todo!()
    }
}

impl<'a, 'tcx> HasDataLayout for Builder<'a, 'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        todo!()
    }
}

impl<'a, 'tcx> HasTyCtxt<'tcx> for Builder<'a, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.cx.tcx()
    }
}

impl<'a, 'tcx> HasTypingEnv<'tcx> for Builder<'a, 'tcx> {
    fn typing_env(&self) -> TypingEnv<'tcx> {
        self.cx.typing_env()
    }
}

impl<'a, 'tcx> FnAbiOfHelpers<'tcx> for Builder<'a, 'tcx> {
    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> <Self::FnAbiOfResult as MaybeResult<&'tcx FnAbi<'tcx, Ty<'tcx>>>>::Error {
        todo!()
    }
}

impl<'a, 'tcx> Deref for Builder<'a, 'tcx> {
    type Target = Cx<'a, 'tcx>;

    fn deref(&self) -> &Self::Target {
        &self.cx
    }
}

impl<'a, 'tcx> CoverageInfoBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn add_coverage(&mut self, instance: Instance<'tcx>, kind: &CoverageKind) {
        todo!()
    }
}

impl<'a, 'tcx> DebugInfoBuilderMethods for Builder<'a, 'tcx> {
    fn dbg_var_addr(
        &mut self,
        dbg_var: Self::DIVariable,
        dbg_loc: Self::DILocation,
        variable_alloca: Self::Value,
        direct_offset: Size,
        indirect_offsets: &[Size],
        fragment: Option<Range<Size>>,
    ) {
        todo!()
    }

    fn set_dbg_loc(&mut self, dbg_loc: Self::DILocation) {
        todo!()
    }

    fn clear_dbg_loc(&mut self) {
        todo!()
    }

    fn get_dbg_loc(&self) -> Option<Self::DILocation> {
        todo!()
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        todo!()
    }

    fn set_var_name(&mut self, value: Self::Value, name: &str) {
        todo!()
    }
}

impl<'a, 'tcx> BackendTypes for Builder<'a, 'tcx> {
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

impl<'a, 'tcx> ArgAbiBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn store_fn_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        todo!()
    }

    fn store_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        val: Self::Value,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        todo!()
    }

    fn arg_memory_ty(&self, arg_abi: &ArgAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        todo!()
    }
}

impl<'a, 'tcx> AbiBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn get_param(&mut self, index: usize) -> Self::Value {
        self.cx.cfg.borrow().function_body[self.function].params[index].into()
    }
}

impl<'a, 'tcx> IntrinsicCallBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, Self::Value>],
        llresult: Self::Value,
        span: Span,
    ) -> Result<(), Instance<'tcx>> {
        todo!()
    }

    fn abort(&mut self) {
        todo!()
    }

    fn assume(&mut self, val: Self::Value) {
        todo!()
    }

    fn expect(&mut self, cond: Self::Value, expected: bool) -> Self::Value {
        todo!()
    }

    fn type_test(&mut self, pointer: Self::Value, typeid: Self::Metadata) -> Self::Value {
        todo!()
    }

    fn type_checked_load(
        &mut self,
        llvtable: Self::Value,
        vtable_byte_offset: u64,
        typeid: Self::Metadata,
    ) -> Self::Value {
        todo!()
    }

    fn va_start(&mut self, val: Self::Value) -> Self::Value {
        todo!()
    }

    fn va_end(&mut self, val: Self::Value) -> Self::Value {
        todo!()
    }
}

impl<'a, 'tcx> AsmBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn codegen_inline_asm(
        &mut self,
        template: &[InlineAsmTemplatePiece],
        operands: &[InlineAsmOperandRef<'tcx, Self>],
        options: InlineAsmOptions,
        line_spans: &[Span],
        instance: Instance<'_>,
        dest: Option<Self::BasicBlock>,
        catch_funclet: Option<(Self::BasicBlock, Option<&Self::Funclet>)>,
    ) {
        todo!()
    }
}

impl<'a, 'tcx> StaticBuilderMethods for Builder<'a, 'tcx> {
    fn get_static(&mut self, def_id: DefId) -> Self::Value {
        todo!()
    }
}

macro_rules! unary_builder_methods {
    ($($method:ident => $op:ident,)*) => {
        $(
            fn $method(&mut self, value: Self::Value) -> Self::Value {
                let value = value.expect_value();

                let module = self.module.borrow();
                let mut cfg = self.cfg.borrow_mut();

                let body = &mut cfg.function_body[self.function];
                let ty = body.value_ty(&module, &value);
                let mut bb = &mut body.basic_blocks[self.basic_block];

                let result = body.local_values.insert(slir::cfg::LocalValueData {
                    ty
                });

                bb.statements.push(slir::cfg::OpUnary {
                    operator: slir::UnaryOperator::$op,
                    value,
                    result
                }.into());

                result.into()
            }
        )*
    };
}

macro_rules! binary_builder_methods {
    ($($method:ident => $op:ident,)*) => {
        $(
            fn $method(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
                let lhs = lhs.expect_value();
                let rhs = rhs.expect_value();

                let module = self.module.borrow();
                let mut cfg = self.cfg.borrow_mut();

                let body = &mut cfg.function_body[self.function];
                let ty = body.value_ty(&module, &lhs);
                let mut bb = &mut body.basic_blocks[self.basic_block];

                let result = body.local_values.insert(slir::cfg::LocalValueData {
                    ty
                });

                bb.statements.push(slir::cfg::OpBinary {
                    operator: slir::BinaryOperator::$op,
                    lhs,
                    rhs,
                    result
                }.into());

                result.into()
            }
        )*
    };
}

impl<'a, 'tcx> BuilderMethods<'a, 'tcx> for Builder<'a, 'tcx> {
    type CodegenCx = Cx<'a, 'tcx>;

    fn build(cx: &'a Self::CodegenCx, llbb: Self::BasicBlock) -> Self {
        let (function, basic_block) = llbb;

        Builder {
            cx,
            function,
            basic_block,
        }
    }

    fn cx(&self) -> &Self::CodegenCx {
        self.cx
    }

    fn llbb(&self) -> Self::BasicBlock {
        (self.function, self.basic_block)
    }

    fn set_span(&mut self, span: Span) {}

    fn append_block(
        cx: &'a Self::CodegenCx,
        function: Self::Function,
        _name: &str,
    ) -> Self::BasicBlock {
        let bb = cx.cfg.borrow_mut().function_body[function].append_block();

        (function, bb)
    }

    fn append_sibling_block(&mut self, _name: &str) -> Self::BasicBlock {
        let bb = self.cfg.borrow_mut().function_body[self.function].append_block();

        (self.function, bb)
    }

    fn switch_to_block(&mut self, llbb: Self::BasicBlock) {
        let (function, basic_block) = llbb;

        self.function = function;
        self.basic_block = basic_block;
    }

    fn ret_void(&mut self) {
        self.cfg.borrow_mut().function_body[self.function].basic_blocks[self.basic_block]
            .terminator = slir::cfg::Terminator::Return(None);
    }

    fn ret(&mut self, v: Self::Value) {
        let value = v.expect_value();

        self.cfg.borrow_mut().function_body[self.function].basic_blocks[self.basic_block]
            .terminator = slir::cfg::Terminator::Return(Some(value));
    }

    fn br(&mut self, dest: Self::BasicBlock) {
        self.cfg.borrow_mut().function_body[self.function].basic_blocks[self.basic_block]
            .terminator = slir::cfg::Terminator::Branch(slir::cfg::Branch::single(dest.1))
    }

    fn cond_br(
        &mut self,
        cond: Self::Value,
        then_llbb: Self::BasicBlock,
        else_llbb: Self::BasicBlock,
    ) {
        self.cfg.borrow_mut().function_body[self.function].basic_blocks[self.basic_block]
            .terminator = slir::cfg::Terminator::Branch(slir::cfg::Branch {
            selector: Some(cond.expect_value().expect_local()),
            branches: smallvec![then_llbb.1, else_llbb.1],
        })
    }

    fn switch(
        &mut self,
        v: Self::Value,
        else_llbb: Self::BasicBlock,
        cases: impl ExactSizeIterator<Item = (u128, Self::BasicBlock)>,
    ) {
        todo!()
    }

    fn invoke(
        &mut self,
        llty: Self::Type,
        fn_attrs: Option<&CodegenFnAttrs>,
        fn_abi: Option<&FnAbi<'tcx, Ty<'tcx>>>,
        llfn: Self::Value,
        args: &[Self::Value],
        then: Self::BasicBlock,
        catch: Self::BasicBlock,
        funclet: Option<&Self::Funclet>,
        instance: Option<Instance<'tcx>>,
    ) -> Self::Value {
        todo!()
    }

    fn unreachable(&mut self) {
        todo!()
    }

    unary_builder_methods! {
        neg => Neg,
        fneg => Neg,
        not => Not,
    }

    binary_builder_methods! {
        add => Add,
        fadd => Add,
        sub => Sub,
        fsub => Sub,
        mul => Mul,
        fmul => Mul,
        udiv => Div,
        sdiv => Div,
        fdiv => Div,
        shl => Shl,
        and => And,
        or => Or,
    }

    fn fadd_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fadd_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn exactudiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn exactsdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn urem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn srem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn lshr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn ashr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_sadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_uadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_ssub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_usub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_smul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_umul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn checked_binop(
        &mut self,
        oop: OverflowOp,
        ty: Ty<'_>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn from_immediate(&mut self, val: Self::Value) -> Self::Value {
        val
    }

    fn to_immediate_scalar(&mut self, val: Self::Value, _scalar: Scalar) -> Self::Value {
        val
    }

    fn alloca(&mut self, size: Size, align: Align) -> Self::Value {
        let mut cfg = self.cfg.borrow_mut();
        let mut body = &mut cfg.function_body[self.function];
        let mut bb = &mut body.basic_blocks[self.basic_block];

        let result = body
            .local_values
            .insert(slir::cfg::LocalValueData { ty: None });

        bb.statements.push(slir::cfg::OpAlloca { result }.into());

        result.into()
    }

    fn dynamic_alloca(&mut self, size: Self::Value, align: Align) -> Self::Value {
        todo!()
    }

    fn load(&mut self, ty: Self::Type, ptr: Self::Value, _align: Align) -> Self::Value {
        let ty = ty.expect_slir_type();
        let ptr = ptr.expect_value();

        let mut cfg = self.cfg.borrow_mut();
        let mut body = &mut cfg.function_body[self.function];
        let mut bb = &mut body.basic_blocks[self.basic_block];

        let result = body
            .local_values
            .insert(slir::cfg::LocalValueData { ty: Some(ty) });

        bb.statements.push(slir::cfg::OpLoad { ptr, result }.into());

        result.into()
    }

    fn volatile_load(&mut self, ty: Self::Type, ptr: Self::Value) -> Self::Value {
        todo!()
    }

    fn atomic_load(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        order: AtomicOrdering,
        size: Size,
    ) -> Self::Value {
        todo!()
    }

    fn load_operand(
        &mut self,
        place: PlaceRef<'tcx, Self::Value>,
    ) -> OperandRef<'tcx, Self::Value> {
        if place.layout.is_zst() {
            return OperandRef::zero_sized(place.layout);
        }

        let val = if place.val.llextra.is_some() {
            OperandValue::Ref(place.val)
        } else if self.cx.is_backend_immediate(place.layout) {
            let llval = self.load(
                ty_and_layout_resolve(self.cx, place.layout).into(),
                place.val.llval,
                place.val.align,
            );
            OperandValue::Immediate(self.to_immediate(llval, place.layout))
        } else if let BackendRepr::ScalarPair(a, b) = place.layout.backend_repr {
            let b_offset = a
                .primitive()
                .size(self)
                .align_to(b.primitive().align(self).abi);

            let mut load = |i, scalar: Scalar, align| {
                let llptr = if i == 0 {
                    place.val.llval
                } else {
                    self.inbounds_ptradd(place.val.llval, self.const_usize(b_offset.bytes()))
                };
                let load = self.load(
                    self.scalar_pair_element_backend_type(place.layout, i, false),
                    llptr,
                    align,
                );
                self.to_immediate_scalar(load, scalar)
            };

            OperandValue::Pair(
                load(0, a, place.val.align),
                load(1, b, place.val.align.restrict_for_offset(b_offset)),
            )
        } else {
            OperandValue::Ref(place.val)
        };

        OperandRef {
            val,
            layout: place.layout,
        }
    }

    fn write_operand_repeatedly(
        &mut self,
        elem: OperandRef<'tcx, Self::Value>,
        count: u64,
        dest: PlaceRef<'tcx, Self::Value>,
    ) {
        todo!()
    }

    fn range_metadata(&mut self, load: Self::Value, range: WrappingRange) {
        todo!()
    }

    fn nonnull_metadata(&mut self, load: Self::Value) {
        todo!()
    }

    fn store(&mut self, val: Self::Value, ptr: Self::Value, align: Align) -> Self::Value {
        self.store_with_flags(val, ptr, align, MemFlags::empty())
    }

    fn store_with_flags(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        align: Align,
        flags: MemFlags,
    ) -> Self::Value {
        let value = val.expect_value();
        let ptr = ptr.expect_value();

        if flags != MemFlags::empty() {
            panic!("non-empty mem-flags are not supported");
        }

        let mut cfg = self.cfg.borrow_mut();
        let mut body = &mut cfg.function_body[self.function];
        let mut bb = &mut body.basic_blocks[self.basic_block];

        bb.statements.push(slir::cfg::OpStore { ptr, value }.into());

        Value::Void
    }

    fn atomic_store(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        order: AtomicOrdering,
        size: Size,
    ) {
        todo!()
    }

    fn gep(&mut self, ty: Self::Type, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        todo!()
    }

    fn inbounds_gep(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        indices: &[Self::Value],
    ) -> Self::Value {
        let ty = ty.expect_slir_type();
        let indices = indices.iter().map(|i| i.expect_value()).collect();

        let mut cfg = self.cfg.borrow_mut();
        let mut body = &mut cfg.function_body[self.function];
        let mut bb = &mut body.basic_blocks[self.basic_block];

        let result = body
            .local_values
            .insert(slir::cfg::LocalValueData { ty: Some(ty) });

        bb.statements.push(
            slir::cfg::OpPtrElementPtr {
                ty,
                ptr: ptr.expect_value(),
                indices,
                result,
            }
            .into(),
        );

        result.into()
    }

    fn trunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptoui_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptosi_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn ptrtoint(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        unimplemented!()
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
        todo!()
    }

    fn pointercast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        unimplemented!()
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fcmp(&mut self, op: RealPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn memcpy(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        todo!()
    }

    fn memmove(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        todo!()
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        align: Align,
        flags: MemFlags,
    ) {
        todo!()
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        todo!()
    }

    fn va_arg(&mut self, list: Self::Value, ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn extract_element(&mut self, vec: Self::Value, idx: Self::Value) -> Self::Value {
        todo!()
    }

    fn vector_splat(&mut self, num_elts: usize, elt: Self::Value) -> Self::Value {
        todo!()
    }

    fn extract_value(&mut self, agg_val: Self::Value, idx: u64) -> Self::Value {
        todo!()
    }

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        todo!()
    }

    fn set_personality_fn(&mut self, personality: Self::Value) {
        todo!()
    }

    fn cleanup_landing_pad(&mut self, pers_fn: Self::Value) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn filter_landing_pad(&mut self, pers_fn: Self::Value) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn resume(&mut self, exn0: Self::Value, exn1: Self::Value) {
        todo!()
    }

    fn cleanup_pad(&mut self, parent: Option<Self::Value>, args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn cleanup_ret(&mut self, funclet: &Self::Funclet, unwind: Option<Self::BasicBlock>) {
        todo!()
    }

    fn catch_pad(&mut self, parent: Self::Value, args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn catch_switch(
        &mut self,
        parent: Option<Self::Value>,
        unwind: Option<Self::BasicBlock>,
        handlers: &[Self::BasicBlock],
    ) -> Self::Value {
        todo!()
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: Self::Value,
        cmp: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        weak: bool,
    ) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
    ) -> Self::Value {
        todo!()
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, scope: SynchronizationScope) {
        todo!()
    }

    fn set_invariant_load(&mut self, load: Self::Value) {
        todo!()
    }

    fn lifetime_start(&mut self, ptr: Self::Value, size: Size) {
        todo!()
    }

    fn lifetime_end(&mut self, ptr: Self::Value, size: Size) {
        todo!()
    }

    fn call(
        &mut self,
        llty: Self::Type,
        _fn_attrs: Option<&CodegenFnAttrs>,
        _fn_abi: Option<&FnAbi<'tcx, Ty<'tcx>>>,
        llfn: Self::Value,
        args: &[Self::Value],
        _funclet: Option<&Self::Funclet>,
        _instance: Option<Instance<'tcx>>,
    ) -> Self::Value {
        let function = llfn.expect_fn_addr();
        let args = args.iter().map(|a| a.expect_value()).collect();

        let mut cfg = self.cfg.borrow_mut();
        let mut body = &mut cfg.function_body[self.function];

        let result = llty.fn_decl_ret_ty().map(|ty| {
            body.local_values
                .insert(slir::cfg::LocalValueData { ty: Some(ty) })
        });

        body.basic_blocks[self.basic_block].statements.push(
            slir::cfg::OpCall {
                function,
                args,
                result,
            }
            .into(),
        );

        if let Some(result) = result {
            result.into()
        } else {
            Value::Void
        }
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn apply_attrs_to_cleanup_callsite(&mut self, llret: Self::Value) {
        todo!()
    }
}
