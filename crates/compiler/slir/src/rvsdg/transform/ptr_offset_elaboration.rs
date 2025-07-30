use crate::rvsdg::transform::scalar_replacement::ScalarReplacer;
use crate::rvsdg::{Node, NodeKind, Rvsdg, SimpleNode, ValueInput, ValueOrigin};
use crate::ty::{Type, TypeKind, TypeRegistry, TY_U32};
use crate::{BinaryOperator, Function, Module};

fn is_slice_ptr_ty(type_registry: &TypeRegistry, ty: Type) -> bool {
    if let TypeKind::Ptr(pointee_ty) = *type_registry.kind(ty) {
        type_registry.kind(pointee_ty).is_slice()
    } else {
        false
    }
}

fn elaborate_ptr_element_ptr_input(rvsdg: &mut Rvsdg, node: Node) {
    let region = rvsdg[node].region();
    let data = rvsdg[node].expect_op_ptr_element_ptr();
    let ptr_input = *data.ptr_input();
    let index_input = data.index_inputs()[0];

    let get_offset_node = rvsdg.add_op_get_ptr_offset(region, ptr_input);
    let add_node = rvsdg.add_op_binary(
        region,
        BinaryOperator::Add,
        ValueInput::output(TY_U32, get_offset_node, 0),
        index_input,
    );

    rvsdg.reconnect_value_input(
        node,
        1,
        ValueOrigin::Output {
            producer: add_node,
            output: 0,
        },
    );
}

pub fn elaborate_ptr_offset_in_fn(rvsdg: &mut Rvsdg, function: Function) {
    let body_region = rvsdg
        .get_function_node(function)
        .map(|n| rvsdg[n].expect_function().body_region());

    if let Some(body_region) = body_region {
        let node_count = rvsdg[body_region].nodes().len();

        for i in 0..node_count {
            use NodeKind::*;
            use SimpleNode::*;

            let node = rvsdg[body_region].nodes()[i];

            if let Simple(OpPtrElementPtr(op)) = rvsdg[node].kind()
                && is_slice_ptr_ty(rvsdg.ty(), op.ptr_input().ty)
            {
                elaborate_ptr_element_ptr_input(rvsdg, node)
            }
        }
    }
}

pub fn entry_points_ptr_offset_elaboration(module: &Module, rvsdg: &mut Rvsdg) {
    for (entry_point, _) in module.entry_points.iter() {
        elaborate_ptr_offset_in_fn(rvsdg, entry_point);
    }
}
