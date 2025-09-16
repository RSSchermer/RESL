use std::io;
use std::sync::LazyLock;

use regex::Regex;
use rustc_middle::bug;
use slir::cfg::{
    BlockPosition, Branch, InlineConst, LocalBindingData, OpBinary, OpBoolToBranchPredicate,
    OpOffsetSlicePtr, OpPtrElementPtr, OpPtrVariantPtr, OpSetDiscriminant, OpStore, Terminator,
    Value,
};
use slir::ty::{TypeKind, TY_BOOL, TY_PREDICATE, TY_PTR_U32, TY_U32};
use slir::BinaryOperator;
use smallvec::smallvec;
use stable_mir::mir::mono::{Instance, MonoItem};
use thin_vec::thin_vec;

use crate::slir_build::context::CodegenContext;
use crate::stable_cg::traits::MiscCodegenMethods;

static PAT_USIZE_SLICE_INDEX_GET: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^<usize as std::slice::SliceIndex<\[[^]]+]>>::get$").unwrap());

static PAT_RANGE_USIZE_SLICE_INDEX_GET: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^<std::ops::Range<usize> as std::slice::SliceIndex<\[[^]]+]>>::get$").unwrap()
});

pub fn maybe_define_special_case(item: MonoItem, cx: &CodegenContext) -> Option<MonoItem> {
    let MonoItem::Fn(instance) = item else {
        return Some(item);
    };

    let name = instance.name().to_string();

    if PAT_USIZE_SLICE_INDEX_GET.is_match(&name) {
        define_usize_slice_index_get(instance, cx);

        None
    } else if PAT_RANGE_USIZE_SLICE_INDEX_GET.is_match(&name) {
        define_range_usize_slice_index_get(instance, cx);

        None
    } else {
        Some(MonoItem::Fn(instance))
    }
}

fn define_usize_slice_index_get(instance: Instance, cx: &CodegenContext) {
    let function = cx.get_fn(&instance);
    let mut module = cx.module.borrow_mut();
    let ret_ty = module.fn_sigs[function].args[0].ty;

    let TypeKind::Ptr(pointee_ty) = *module.ty.kind(ret_ty) else {
        bug!("first argument should be a return pointer");
    };
    let TypeKind::Enum(option_enum) = &*module.ty.kind(pointee_ty) else {
        bug!("`Option` type should be represented in SLIR by an enum`");
    };
    let some_variant_ty = option_enum.variants[1];
    let some_struct_data = module.ty.kind(some_variant_ty);
    let some_struct_data = some_struct_data.expect_struct();
    let elem_ptr_ty = some_struct_data.fields[0].ty;
    let TypeKind::Ptr(elem_ty) = *module.ty.kind(elem_ptr_ty) else {
        bug!("`Some` variant payload must be a pointer");
    };

    let mut cfg = cx.cfg.borrow_mut();
    let body = cfg
        .get_function_body(function)
        .expect("function should have been predefined");

    let ret = body.argument_values()[0];
    let index = body.argument_values()[1];
    let slice_ptr = body.argument_values()[2];
    let len = body.argument_values()[3];

    let bb0 = body.entry_block();
    let bb1 = cfg.add_basic_block(function);
    let bb2 = cfg.add_basic_block(function);

    let (_, in_bounds) = cfg.add_stmt_op_binary(
        bb0,
        BlockPosition::Append,
        BinaryOperator::Lt,
        index.into(),
        len.into(),
    );
    let (_, predicate) =
        cfg.add_stmt_op_bool_to_branch_predicate(bb0, BlockPosition::Append, in_bounds.into());
    cfg.set_terminator(
        bb0,
        Terminator::branch_multiple(predicate.into(), [bb1, bb2]),
    );

    // The "in bounds" branch
    cfg.add_stmt_op_set_discriminant(bb1, BlockPosition::Append, ret.into(), 1);
    let (_, some_ptr) = cfg.add_stmt_op_ptr_variant_ptr(bb1, BlockPosition::Append, ret.into(), 1);
    let (_, payload_ptr) = cfg.add_stmt_op_ptr_element_ptr(
        bb1,
        BlockPosition::Append,
        elem_ptr_ty,
        some_ptr.into(),
        [InlineConst::U32(0).into()],
    );
    let (_, elem_ptr) = cfg.add_stmt_op_ptr_element_ptr(
        bb1,
        BlockPosition::Append,
        elem_ty,
        slice_ptr.into(),
        [index.into()],
    );
    cfg.add_stmt_op_store(
        bb1,
        BlockPosition::Append,
        payload_ptr.into(),
        elem_ptr.into(),
    );
    cfg.set_terminator(bb1, Terminator::return_void());

    // The "not in bounds" branch
    cfg.add_stmt_op_set_discriminant(bb2, BlockPosition::Append, ret.into(), 0);
    cfg.set_terminator(bb2, Terminator::return_void());
}

fn define_range_usize_slice_index_get(instance: Instance, cx: &CodegenContext) {
    let function = cx.get_fn(&instance);

    let mut module = cx.module.borrow_mut();
    let ret_ty = module.fn_sigs[function].args[0].ty;

    let TypeKind::Ptr(pointee_ty) = *module.ty.kind(ret_ty) else {
        bug!("first argument should be a return pointer");
    };
    let TypeKind::Enum(option_enum) = &*module.ty.kind(pointee_ty) else {
        bug!("`Option` type should be represented in SLIR by an enum`");
    };
    let some_variant_ty = option_enum.variants[1];
    let some_variant_ptr_ty = module.ty.register(TypeKind::Ptr(some_variant_ty));
    let some_struct_data = module.ty.kind(some_variant_ty);
    let some_struct_data = some_struct_data.expect_struct();
    let fat_ptr_ty = some_struct_data.fields[0].ty;
    let fat_ptr_kind = module.ty.kind(fat_ptr_ty);
    let fat_ptr_data = fat_ptr_kind.expect_struct();
    let slice_ptr_ty = fat_ptr_data.fields[0].ty;

    let mut cfg = cx.cfg.borrow_mut();

    let body = cfg
        .get_function_body(function)
        .expect("function should have been predefined");

    let ret = body.argument_values()[0];
    let start = body.argument_values()[1];
    let end = body.argument_values()[2];
    let in_slice_ptr = body.argument_values()[3];
    let in_slice_len = body.argument_values()[4];

    let bb0 = body.entry_block();
    let bb_some = cfg.add_basic_block(function);
    let bb_none = cfg.add_basic_block(function);

    // Check if the range is non-empty
    let (_, start_lt_end) = cfg.add_stmt_op_binary(
        bb0,
        BlockPosition::Append,
        BinaryOperator::Lt,
        start.into(),
        end.into(),
    );
    // Check if the `end` is in bounds. Because of the above check, this will also imply that the
    // `start` is in bounds.
    let (_, end_in_bounds) = cfg.add_stmt_op_binary(
        bb0,
        BlockPosition::Append,
        BinaryOperator::LtEq,
        end.into(),
        in_slice_len.into(),
    );
    // Create a predicate that selects the `Some` branch if both of the above checks are `true`, or
    // the `None` branch otherwise.
    let (_, condition) = cfg.add_stmt_op_binary(
        bb0,
        BlockPosition::Append,
        BinaryOperator::And,
        start_lt_end.into(),
        end_in_bounds.into(),
    );
    let (_, predicate) =
        cfg.add_stmt_op_bool_to_branch_predicate(bb0, BlockPosition::Append, condition.into());
    cfg.set_terminator(
        bb0,
        Terminator::branch_multiple(predicate.into(), [bb_some, bb_none]),
    );

    // `Some` branch
    let (_, some_variant_ptr) =
        cfg.add_stmt_op_ptr_variant_ptr(bb_some, BlockPosition::Append, ret.into(), 1);
    // Compute the new slice pointer
    let (_, out_slice_ptr) = cfg.add_stmt_op_offset_slice_pointer(
        bb_some,
        BlockPosition::Append,
        in_slice_ptr.into(),
        start.into(),
    );
    // Store the new slice pointer
    let (_, out_slice_ptr_ptr) = cfg.add_stmt_op_ptr_element_ptr(
        bb_some,
        BlockPosition::Append,
        slice_ptr_ty,
        some_variant_ptr.into(),
        [InlineConst::U32(0).into(), InlineConst::U32(0).into()],
    );
    cfg.add_stmt_op_store(
        bb_some,
        BlockPosition::Append,
        out_slice_ptr_ptr.into(),
        out_slice_ptr.into(),
    );
    // Compute the new len
    let (_, out_len) = cfg.add_stmt_op_binary(
        bb_some,
        BlockPosition::Append,
        BinaryOperator::Sub,
        end.into(),
        start.into(),
    );
    // Store the new len
    let (_, out_len_ptr) = cfg.add_stmt_op_ptr_element_ptr(
        bb_some,
        BlockPosition::Append,
        TY_U32,
        some_variant_ptr.into(),
        [InlineConst::U32(0).into(), InlineConst::U32(1).into()],
    );
    cfg.add_stmt_op_store(
        bb_some,
        BlockPosition::Append,
        out_len_ptr.into(),
        out_len.into(),
    );
    // Set the discriminant and return
    cfg.add_stmt_op_set_discriminant(bb_some, BlockPosition::Append, ret.into(), 1);
    cfg.set_terminator(bb_some, Terminator::return_void());

    // `None` branch
    cfg.add_stmt_op_set_discriminant(bb_none, BlockPosition::Append, ret.into(), 0);
    cfg.set_terminator(bb_none, Terminator::return_void());
}
