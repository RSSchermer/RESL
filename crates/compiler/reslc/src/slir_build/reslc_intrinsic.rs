use std::sync::LazyLock;

use regex::Regex;
use rustc_middle::bug;
use slir::builtin_function::BuiltinFunction;
use slir::cfg::{BlockPosition, Terminator};
use slir::ty::TY_U32;
use stable_mir::abi::ValueAbi;
use stable_mir::crate_def::Attribute;
use stable_mir::mir::mono::{Instance, MonoItem};
use stable_mir::CrateDef;

use crate::slir_build::context::CodegenContext;
use crate::stable_cg::traits::MiscCodegenMethods;

pub fn maybe_reslc_intrinsic(item: MonoItem, cx: &CodegenContext) -> Option<MonoItem> {
    let MonoItem::Fn(instance) = item else {
        return Some(item);
    };

    if let Some(attr) = instance
        .def
        .attrs_by_path(&["reslc".into(), "intrinsic".into()])
        .first()
    {
        match resolve_intrinsic(attr) {
            Intrinsic::MemResourceAsRef => define_mem_resource_as_ref(instance, cx),
        }

        None
    } else {
        Some(MonoItem::Fn(instance))
    }
}

fn resolve_intrinsic(attr: &Attribute) -> Intrinsic {
    match attr.as_str() {
        "#[reslc::intrinsic(mem_resource_as_ref)]" => Intrinsic::MemResourceAsRef,
        _ => bug!("unsupported reslc intrinsic: {}", attr.as_str()),
    }
}

enum Intrinsic {
    MemResourceAsRef,
}

fn define_mem_resource_as_ref(instance: Instance, cx: &CodegenContext) {
    let function = cx.get_fn(&instance);

    let module = cx.module.borrow();
    let fn_sig = &module.fn_sigs[function];
    let ptr_ty = fn_sig.args[0].ty;
    let ret_ty = fn_sig.ret_ty.expect("should return a reference");

    let fn_abi = instance
        .fn_abi()
        .expect("intrinsic must have known ABI at codegen time");
    let ret_abi = fn_abi.ret.layout.shape().abi;

    let mut cfg = cx.cfg.borrow_mut();
    let body = cfg
        .get_function_body(function)
        .expect("function should have been predefined");
    let bb = body.entry_block();
    let ptr_arg = body.argument_values()[0];

    // Note that the pointer argument type will have already been unpacked, see
    // super::context::CodegenContext::try_register_as_reslc_mem_resource_ty.

    if matches!(ret_abi, ValueAbi::ScalarPair(_, _)) {
        // The mem resource contains an unsized type. In this case we'll have to return a pair
        // consisting of the pointer and the length of the array or the unsized tail value.
        let (_, alloca_ptr) = cfg.add_stmt_op_alloca(bb, BlockPosition::Append, ret_ty);

        let (_, ptr_ptr) = cfg.add_stmt_op_ptr_element_ptr(
            bb,
            BlockPosition::Append,
            ptr_ty,
            alloca_ptr.into(),
            [0u32.into()],
        );
        cfg.add_stmt_op_store(bb, BlockPosition::Append, ptr_ptr.into(), ptr_arg.into());

        let (_, len_ptr) = cfg.add_stmt_op_ptr_element_ptr(
            bb,
            BlockPosition::Append,
            TY_U32,
            alloca_ptr.into(),
            [1u32.into()],
        );
        let array_len_builtin = BuiltinFunction::array_len(ptr_ty);
        let (_, len) = cfg.add_stmt_op_call_builtin(
            bb,
            BlockPosition::Append,
            array_len_builtin,
            [ptr_arg.into()],
        );
        cfg.add_stmt_op_store(
            bb,
            BlockPosition::Append,
            len_ptr.into(),
            len.unwrap().into(),
        );

        let (_, pair) = cfg.add_stmt_op_load(bb, BlockPosition::Append, alloca_ptr.into());

        cfg.set_terminator(bb, Terminator::return_value(pair.into()));
    } else {
        // The mem resource contains a sized type. In this case we simply return the pointer
        // argument.
        cfg.set_terminator(bb, Terminator::return_value(ptr_arg.into()));
    }
}
