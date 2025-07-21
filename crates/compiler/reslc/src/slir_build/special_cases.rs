use std::sync::LazyLock;

use regex::Regex;
use rustc_middle::bug;
use slir::cfg::{
    Branch, InlineConst, LocalValueData, OpBinary, OpBoolToBranchPredicate, OpPtrElementPtr,
    OpPtrVariantPtr, OpSetDiscriminant, OpStore, Terminator, Value,
};
use slir::ty::{TypeKind, TY_BOOL};
use slir::BinaryOperator;
use smallvec::smallvec;
use stable_mir::mir::mono::{Instance, MonoItem};
use thin_vec::thin_vec;

use crate::slir_build::context::CodegenContext;
use crate::stable_cg::traits::MiscCodegenMethods;

static PAT_USIZE_SLICE_INDEX_GET: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^<usize as std::slice::SliceIndex<\[[^]]+]>>::get$").unwrap());

pub fn maybe_define_special_case(item: MonoItem, cx: &CodegenContext) -> Option<MonoItem> {
    let MonoItem::Fn(instance) = item else {
        return Some(item);
    };

    let name = instance.name().to_string();

    if PAT_USIZE_SLICE_INDEX_GET.is_match(&name) {
        define_usize_slice_index_get(instance, cx);

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
    let some_variant_ptr_ty = module.ty.register(TypeKind::Ptr(some_variant_ty));
    let some_struct_data = module.ty.kind(some_variant_ty);
    let some_struct_data = some_struct_data.expect_struct();
    let elem_ptr_ty = some_struct_data.fields[0].ty;
    let elem_ptr_ptr_ty = module.ty.register(TypeKind::Ptr(elem_ptr_ty));
    let TypeKind::Ptr(elem_ty) = *module.ty.kind(elem_ptr_ty) else {
        bug!("`Some` variant payload must be a pointer");
    };

    let body = &mut cx.cfg.borrow_mut().function_body[function];

    let ret = body.params[0];
    let index = body.params[1];
    let slice_ptr = body.params[2];
    let len = body.params[3];
    let in_bounds = body
        .local_values
        .insert(LocalValueData { ty: Some(TY_BOOL) });
    let predicate = body.local_values.insert(LocalValueData::predicate());
    let elem_ptr = body.local_values.insert(LocalValueData {
        ty: Some(elem_ptr_ty),
    });
    let some_ptr = body.local_values.insert(LocalValueData {
        ty: Some(some_variant_ptr_ty),
    });
    let payload_ptr = body.local_values.insert(LocalValueData {
        ty: Some(elem_ptr_ptr_ty),
    });

    let bb0 = body.append_block();
    let bb1 = body.append_block();
    let bb2 = body.append_block();

    body.basic_blocks[bb0].statements.push(
        OpBinary {
            operator: BinaryOperator::Lt,
            lhs: index.into(),
            rhs: len.into(),
            result: in_bounds,
        }
        .into(),
    );
    body.basic_blocks[bb0].statements.push(
        OpBoolToBranchPredicate {
            value: in_bounds.into(),
            result: predicate,
        }
        .into(),
    );
    body.basic_blocks[bb0].terminator = Terminator::Branch(Branch {
        selector: Some(predicate),
        branches: smallvec![bb1, bb2],
    });

    // The "in bounds" branch
    body.basic_blocks[bb1].statements.push(
        OpSetDiscriminant {
            ptr: ret.into(),
            variant_index: 1,
        }
        .into(),
    );
    body.basic_blocks[bb1].statements.push(
        OpPtrVariantPtr {
            ptr: ret.into(),
            variant_index: 1,
            result: some_ptr,
        }
        .into(),
    );
    body.basic_blocks[bb1].statements.push(
        OpPtrElementPtr {
            element_ty: elem_ptr_ty,
            ptr: some_ptr.into(),
            indices: thin_vec![InlineConst::U32(0).into()],
            result: payload_ptr,
        }
        .into(),
    );
    body.basic_blocks[bb1].statements.push(
        OpPtrElementPtr {
            element_ty: elem_ty,
            ptr: slice_ptr.into(),
            indices: thin_vec![index.into()],
            result: elem_ptr,
        }
        .into(),
    );
    body.basic_blocks[bb1].statements.push(
        OpStore {
            ptr: payload_ptr.into(),
            value: elem_ptr.into(),
        }
        .into(),
    );
    body.basic_blocks[bb1].terminator = Terminator::Return(None);

    // The "not in bounds" branch
    body.basic_blocks[bb2].statements.push(
        OpSetDiscriminant {
            ptr: ret.into(),
            variant_index: 0,
        }
        .into(),
    );
    body.basic_blocks[bb2].terminator = Terminator::Return(None);
}
