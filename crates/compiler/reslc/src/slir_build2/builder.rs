use std::ops::{Deref, Range};

use smallvec::smallvec;
use stable_mir::abi;
use stable_mir::abi::{ArgAbi, FnAbi, ValueAbi};
use stable_mir::mir::mono::{Instance, StaticDef};
use stable_mir::target::MachineSize;
use stable_mir::ty::{Align, Span};

use crate::slir_build2::context::{ty_and_layout_resolve, CodegenContext as Cx};
use crate::slir_build2::ty::Type;
use crate::slir_build2::value::Value;
use crate::stable_cg::traits::{
    AbiBuilderMethods, ArgAbiBuilderMethods, BackendTypes, BuilderMethods,
    IntrinsicCallBuilderMethods, LayoutTypeCodegenMethods, StaticBuilderMethods,
};
use crate::stable_cg::{
    AtomicOrdering, AtomicRmwBinOp, IntPredicate, OperandRef, OperandValue, PlaceRef, PlaceValue,
    RealPredicate, Scalar, SynchronizationScope,
};

pub struct Builder<'a, 'tcx> {
    cx: &'a Cx<'a, 'tcx>,
    function: slir::Function,
    basic_block: slir::cfg::BasicBlock,
}

impl<'a, 'tcx> Deref for Builder<'a, 'tcx> {
    type Target = Cx<'a, 'tcx>;

    fn deref(&self) -> &Self::Target {
        &self.cx
    }
}

impl<'a, 'tcx> BackendTypes for Builder<'a, 'tcx> {
    type Value = Value;
    type Local = slir::cfg::LocalValue;
    type Function = slir::Function;
    type BasicBlock = (slir::Function, slir::cfg::BasicBlock);
    type Type = Type;
}

impl<'a, 'tcx> ArgAbiBuilderMethods for Builder<'a, 'tcx> {
    fn store_fn_arg(&mut self, arg_abi: &ArgAbi, idx: &mut usize, dst: PlaceRef<Self::Value>) {
        todo!()
    }

    fn store_arg(&mut self, arg_abi: &ArgAbi, val: Self::Value, dst: PlaceRef<Self::Value>) {
        todo!()
    }

    fn arg_memory_ty(&self, arg_abi: &ArgAbi) -> Self::Type {
        todo!()
    }
}

impl<'a, 'tcx> AbiBuilderMethods for Builder<'a, 'tcx> {
    fn get_param(&mut self, index: usize) -> Self::Value {
        self.cx.cfg.borrow().function_body[self.function].params[index].into()
    }
}

impl<'a, 'tcx> IntrinsicCallBuilderMethods for Builder<'a, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        instance: Instance,
        fn_abi: &FnAbi,
        args: &[OperandRef<Self::Value>],
        llresult: Self::Value,
        span: Span,
    ) -> Result<(), Instance> {
        todo!()
    }
}

impl<'a, 'tcx> StaticBuilderMethods for Builder<'a, 'tcx> {
    fn get_static(&mut self, def: StaticDef) -> Self::Value {
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

impl<'a, 'tcx> BuilderMethods<'a> for Builder<'a, 'tcx> {
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

    fn as_local(&mut self, val: Self::Value) -> Self::Local {
        let val = val.expect_value();

        // If the value already represents a local, return that local. Otherwise, initialize a new
        // local with the value.
        let ty = match val {
            slir::cfg::Value::Local(local) => return local,
            slir::cfg::Value::InlineConst(c) => c.ty(),
            slir::cfg::Value::Const => todo!(),
        };

        let mut cfg = self.cfg.borrow_mut();
        let mut body = &mut cfg.function_body[self.function];

        let local = body
            .local_values
            .insert(slir::cfg::LocalValueData { ty: Some(ty) });

        let mut bb = &mut body.basic_blocks[self.basic_block];

        bb.statements.push(
            slir::cfg::OpAssign {
                value: val,
                result: local,
            }
            .into(),
        );

        local
    }

    fn local_value(&mut self, local: Self::Local) -> Self::Value {
        Value::Value(slir::cfg::Value::Local(local))
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
        cases: impl IntoIterator<Item = (u128, Self::BasicBlock)>,
    ) {
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

    fn from_immediate(&mut self, val: Self::Value) -> Self::Value {
        val
    }

    fn to_immediate_scalar(&mut self, val: Self::Value, _scalar: abi::Scalar) -> Self::Value {
        val
    }

    fn alloca(&mut self, size: MachineSize, align: Align) -> Self::Value {
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

    fn assign(&mut self, local: Self::Local, value: Self::Value) {
        let mut cfg = self.cfg.borrow_mut();
        let mut body = &mut cfg.function_body[self.function];
        let mut bb = &mut body.basic_blocks[self.basic_block];

        bb.statements.push(
            slir::cfg::OpAssign {
                value: value.expect_value(),
                result: local,
            }
            .into(),
        );
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
        size: MachineSize,
    ) -> Self::Value {
        todo!()
    }

    fn load_operand(&mut self, place: PlaceRef<Self::Value>) -> OperandRef<Self::Value> {
        let shape = place.layout.layout.shape();

        if shape.is_1zst() {
            return OperandRef::zero_sized(place.layout);
        }

        let val = if place.val.llextra.is_some() {
            OperandValue::Ref(place.val)
        } else if self.is_backend_immediate(place.layout) {
            let llval = self.load(
                ty_and_layout_resolve(self.cx, place.layout).into(),
                place.val.llval,
                place.val.align,
            );

            OperandValue::Immediate(self.to_immediate(llval, place.layout))
        } else if let ValueAbi::ScalarPair(a, b) = shape.abi {
            return OperandRef::from_immediate_or_packed_pair(self, place.val.llval, place.layout);
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
        elem: OperandRef<Self::Value>,
        count: u64,
        dest: PlaceRef<Self::Value>,
    ) {
        todo!()
    }

    fn store(&mut self, val: Self::Value, ptr: Self::Value, align: Align) -> Self::Value {
        let value = val.expect_value();
        let ptr = ptr.expect_value();

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
        size: MachineSize,
    ) {
        todo!()
    }

    fn gep(&mut self, ty: Self::Type, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        self.inbounds_gep(ty, ptr, indices)
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

        let result = body.local_values.insert(slir::cfg::LocalValueData {
            ty: Some(slir::ty::TY_PTR),
        });

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

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
        todo!()
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fcmp(&mut self, op: RealPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
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

    fn lifetime_start(&mut self, ptr: Self::Value, size: MachineSize) {
        todo!()
    }

    fn lifetime_end(&mut self, ptr: Self::Value, size: MachineSize) {
        todo!()
    }

    fn call(
        &mut self,
        llty: Self::Type,
        _fn_abi: Option<&FnAbi>,
        llfn: Self::Value,
        args: &[Self::Value],
        _instance: Option<&Instance>,
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
}
