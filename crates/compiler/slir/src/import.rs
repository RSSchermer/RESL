use rustc_hash::FxHashMap;

use crate::cfg::{
    BasicBlock, BlockPosition, Cfg, ConstPtr, InlineConst, LocalBinding, RootIdentifier, Statement,
    StatementData, Terminator, Value,
};
use crate::ty::TypeKind;
use crate::{Function, Module};

pub struct FunctionImporter {
    bb_mapping: FxHashMap<BasicBlock, BasicBlock>,
    local_value_mapping: FxHashMap<LocalBinding, LocalBinding>,
}

impl FunctionImporter {
    pub fn new() -> Self {
        Self {
            bb_mapping: FxHashMap::default(),
            local_value_mapping: FxHashMap::default(),
        }
    }

    pub fn import_function(
        &mut self,
        from: (&Module, &Cfg),
        to: (&mut Module, &mut Cfg),
        function: Function,
    ) {
        let (src_module, src_cfg) = from;
        let (dst_module, dst_cfg) = to;

        self.import_function_sig(src_module, dst_module, function);
        self.import_function_body(dst_module, src_cfg, dst_cfg, function);
    }

    fn import_function_sig(
        &mut self,
        src_module: &Module,
        dst_module: &mut Module,
        function: Function,
    ) {
        let mut sig = src_module.fn_sigs[function].clone();

        sig.ty = dst_module.ty.register(TypeKind::Function(function));

        if let Some(ret_ty) = &mut sig.ret_ty {
            *ret_ty = dst_module.ty.import(&src_module.ty, *ret_ty);
        }

        for arg in &mut sig.args {
            arg.ty = dst_module.ty.import(&src_module.ty, arg.ty);
        }

        dst_module.fn_sigs.register(function, sig);
    }

    fn import_function_body(
        &mut self,
        dst_module: &Module,
        src_cfg: &Cfg,
        dst_cfg: &mut Cfg,
        function: Function,
    ) {
        self.bb_mapping.clear();
        self.local_value_mapping.clear();

        let src_body = src_cfg
            .get_function_body(function)
            .expect("function not registered in source CFG");
        let dst_body = dst_cfg.register_function(dst_module, function);

        for (src_arg, dst_arg) in src_body
            .argument_values()
            .iter()
            .zip(dst_body.argument_values())
        {
            self.local_value_mapping.insert(*src_arg, *dst_arg);
        }

        // First make sure all basic blocks are present in the destination CFG so we can connect the
        // branching terminators in any order.
        for src_bb in src_body.basic_blocks() {
            let dst_bb = dst_cfg.add_basic_block(function);

            self.bb_mapping.insert(*src_bb, dst_bb);
        }

        for src_bb in src_body.basic_blocks() {
            self.import_basic_block(dst_module, src_cfg, dst_cfg, *src_bb);
        }
    }

    fn import_basic_block(
        &mut self,
        module: &Module,
        src: &Cfg,
        dst: &mut Cfg,
        src_bb: BasicBlock,
    ) {
        let dst_bb = *self.bb_mapping.get(&src_bb).unwrap();

        for statement in src[src_bb].statements() {
            self.import_statement(module, src, dst, dst_bb, *statement);
        }

        let terminator = match src[src_bb].terminator() {
            Terminator::Branch(t) => {
                if let Some(selector) = t.selector() {
                    Terminator::branch_multiple(
                        self.dst_local_value(selector),
                        t.targets().iter().map(|&bb| self.dst_bb(bb)),
                    )
                } else {
                    Terminator::branch_single(self.dst_bb(t.targets()[0]))
                }
            }
            Terminator::Return(Some(value)) => {
                Terminator::return_value(self.dst_value(dst, *value))
            }
            Terminator::Return(None) => Terminator::return_void(),
        };

        dst.set_terminator(dst_bb, terminator);
    }

    fn import_statement(
        &mut self,
        module: &Module,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        match &src[src_stmt] {
            StatementData::Bind(_) => self.import_stmt_bind(src, dst, dst_bb, src_stmt),
            StatementData::Assign(_) => self.import_stmt_assign(src, dst, dst_bb, src_stmt),
            StatementData::Uninitialized(_) => {
                self.import_stmt_uninitialized(src, dst, dst_bb, src_stmt)
            }
            StatementData::OpAlloca(_) => self.import_stmt_op_alloca(src, dst, dst_bb, src_stmt),
            StatementData::OpLoad(_) => self.import_stmt_op_load(src, dst, dst_bb, src_stmt),
            StatementData::OpStore(_) => self.import_stmt_op_store(src, dst, dst_bb, src_stmt),
            StatementData::OpPtrElementPtr(_) => {
                self.import_stmt_op_ptr_element_ptr(src, dst, dst_bb, src_stmt)
            }
            StatementData::OpPtrVariantPtr(_) => {
                self.import_stmt_op_ptr_variant_ptr(src, dst, dst_bb, src_stmt)
            }
            StatementData::OpGetDiscriminant(_) => {
                self.import_stmt_op_get_discriminant(src, dst, dst_bb, src_stmt)
            }
            StatementData::OpSetDiscriminant(_) => {
                self.import_stmt_op_set_discriminant(src, dst, dst_bb, src_stmt)
            }
            StatementData::OpOffsetSlicePtr(_) => {
                self.import_stmt_op_offset_slice_ptr(src, dst, dst_bb, src_stmt)
            }
            StatementData::OpUnary(_) => self.import_stmt_op_unary(src, dst, dst_bb, src_stmt),
            StatementData::OpBinary(_) => self.import_stmt_op_binary(src, dst, dst_bb, src_stmt),
            StatementData::OpCall(_) => {
                self.import_stmt_op_call(module, src, dst, dst_bb, src_stmt)
            }
            StatementData::OpCaseToBranchPredicate(_) => {}
            StatementData::OpBoolToBranchPredicate(_) => {}
        }
    }

    fn import_stmt_assign(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_assign();
        let dst_local = self.dst_local_value(src_data.local_binding());
        let dst_value = self.dst_value(dst, src_data.value());

        dst.add_stmt_assign(dst_bb, BlockPosition::Append, dst_local, dst_value);
    }

    fn import_stmt_bind(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_bind();
        let dst_value = self.dst_value(dst, src_data.value());

        let (_, dst_local) = dst.add_stmt_bind(dst_bb, BlockPosition::Append, dst_value);

        self.local_value_mapping
            .insert(src_data.local_binding(), dst_local);
    }

    fn import_stmt_uninitialized(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_uninitialized();
        let src_local = src_data.local_binding();
        let src_ty = src[src_local].ty();
        let dst_ty = dst.ty().import(src.ty(), src_ty);

        let (_, dst_local) = dst.add_stmt_uninitialized(dst_bb, BlockPosition::Append, dst_ty);

        self.local_value_mapping.insert(src_local, dst_local);
    }

    fn import_stmt_op_alloca(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_alloca();
        let ty = dst.ty().import(src.ty(), src_data.ty());
        let (_, dst_result) = dst.add_stmt_op_alloca(dst_bb, BlockPosition::Append, ty);

        self.local_value_mapping
            .insert(src_data.result(), dst_result);
    }

    fn import_stmt_op_load(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_load();
        let dst_pointer = self.dst_value(dst, src_data.pointer());

        let (_, dst_result) = dst.add_stmt_op_load(dst_bb, BlockPosition::Append, dst_pointer);

        self.local_value_mapping
            .insert(src_data.result(), dst_result);
    }

    fn import_stmt_op_store(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_store();
        let dst_pointer = self.dst_value(dst, src_data.pointer());
        let dst_value = self.dst_value(dst, src_data.value());

        dst.add_stmt_op_store(dst_bb, BlockPosition::Append, dst_pointer, dst_value);
    }

    fn import_stmt_op_ptr_element_ptr(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_ptr_element_ptr();
        let dst_pointer = self.dst_value(dst, src_data.pointer());
        let dst_indices = src_data
            .indices()
            .iter()
            .map(|v| self.dst_value(dst, *v))
            .collect::<Vec<_>>();
        let element_ty = src_data.element_ty();

        let (_, dst_result) = dst.add_stmt_op_ptr_element_ptr(
            dst_bb,
            BlockPosition::Append,
            element_ty,
            dst_pointer,
            dst_indices,
        );

        self.local_value_mapping
            .insert(src_data.result(), dst_result);
    }

    fn import_stmt_op_ptr_variant_ptr(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_ptr_variant_ptr();
        let dst_pointer = self.dst_value(dst, src_data.pointer());
        let dst_variant_index = src_data.variant_index();

        let (_, dst_result) = dst.add_stmt_op_ptr_variant_ptr(
            dst_bb,
            BlockPosition::Append,
            dst_pointer,
            dst_variant_index,
        );

        self.local_value_mapping
            .insert(src_data.result(), dst_result);
    }

    fn import_stmt_op_get_discriminant(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_get_discriminant();
        let dst_pointer = self.dst_value(dst, src_data.pointer());

        let (_, dst_result) =
            dst.add_stmt_op_get_discriminant(dst_bb, BlockPosition::Append, dst_pointer);

        self.local_value_mapping
            .insert(src_data.result(), dst_result);
    }

    fn import_stmt_op_set_discriminant(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_set_discriminant();
        let dst_pointer = self.dst_value(dst, src_data.pointer());
        let variant_index = src_data.variant_index();

        dst.add_stmt_op_set_discriminant(dst_bb, BlockPosition::Append, dst_pointer, variant_index);
    }

    fn import_stmt_op_offset_slice_ptr(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_offset_slice_ptr();
        let dst_pointer = self.dst_value(dst, src_data.pointer());
        let dst_offset = self.dst_value(dst, src_data.offset());

        let (_, result) = dst.add_stmt_op_offset_slice_pointer(
            dst_bb,
            BlockPosition::Append,
            dst_pointer,
            dst_offset,
        );

        self.local_value_mapping.insert(src_data.result(), result);
    }

    fn import_stmt_op_unary(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_unary();
        let operator = src_data.operator();
        let dst_operand = self.dst_value(dst, src_data.operand());

        let (_, result) =
            dst.add_stmt_op_unary(dst_bb, BlockPosition::Append, operator, dst_operand);

        self.local_value_mapping.insert(src_data.result(), result);
    }

    fn import_stmt_op_binary(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_binary();
        let operator = src_data.operator();
        let dst_lhs = self.dst_value(dst, src_data.lhs());
        let dst_rhs = self.dst_value(dst, src_data.rhs());

        let (_, result) =
            dst.add_stmt_op_binary(dst_bb, BlockPosition::Append, operator, dst_lhs, dst_rhs);

        self.local_value_mapping.insert(src_data.result(), result);
    }

    fn import_stmt_op_call(
        &mut self,
        module: &Module,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_call();
        let callee = src_data.callee();
        let dst_arguments = src_data
            .arguments()
            .iter()
            .map(|v| self.dst_value(dst, *v))
            .collect::<Vec<_>>();

        let (_, result) = dst.add_stmt_op_call(
            &module.fn_sigs,
            dst_bb,
            BlockPosition::Append,
            callee,
            dst_arguments,
        );

        if let Some(result) = result {
            self.local_value_mapping
                .insert(src_data.result().unwrap(), result);
        }
    }

    fn import_stmt_op_case_to_branch_predicate(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_case_to_branch_predicate();
        let dst_value = self.dst_value(dst, src_data.value());
        let cases = src_data.cases().iter().copied();

        let (_, result) = dst.add_stmt_op_case_to_branch_predicate(
            dst_bb,
            BlockPosition::Append,
            dst_value,
            cases,
        );

        self.local_value_mapping.insert(src_data.result(), result);
    }

    fn import_stmt_op_bool_to_branch_predicate(
        &mut self,
        src: &Cfg,
        dst: &mut Cfg,
        dst_bb: BasicBlock,
        src_stmt: Statement,
    ) {
        let src_data = src[src_stmt].expect_op_bool_to_branch_predicate();
        let dst_value = self.dst_value(dst, src_data.value());

        let (_, result) =
            dst.add_stmt_op_bool_to_branch_predicate(dst_bb, BlockPosition::Append, dst_value);

        self.local_value_mapping.insert(src_data.result(), result);
    }

    fn dst_bb(&self, src_bb: BasicBlock) -> BasicBlock {
        *self
            .bb_mapping
            .get(&src_bb)
            .expect("basic block mapping should be registered earlier")
    }

    fn dst_local_value(&self, src_local: LocalBinding) -> LocalBinding {
        *self
            .local_value_mapping
            .get(&src_local)
            .expect("local mapping should be registered earlier")
    }

    fn dst_value(&self, dst_cfg: &Cfg, src_value: Value) -> Value {
        match src_value {
            Value::Local(local) => Value::Local(self.dst_local_value(local)),
            Value::InlineConst(InlineConst::Ptr(ptr)) => match ptr.root_identifier() {
                RootIdentifier::Local(local) => {
                    let local = self.dst_local_value(local);
                    let inline_const = InlineConst::Ptr(ConstPtr::local_binding(dst_cfg, local));

                    inline_const.into()
                }
                _ => panic!("imported functions may not reference module globals"),
            },
            Value::Const => todo!("import constant"),
            value @ _ => value,
        }
    }
}
