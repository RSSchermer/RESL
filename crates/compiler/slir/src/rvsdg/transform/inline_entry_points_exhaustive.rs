use std::collections::VecDeque;

use crate::rvsdg::transform::inline::inline_function;
use crate::rvsdg::Rvsdg;
use crate::Module;

/// For all entry points in the given `module`, finds all "apply" nodes and inlines the
/// corresponding function, iteratively inlining any new "apply" amongst the inlined nodes until
/// the entry points no longer contain any apply operations for user-defined functions.
pub fn inline_entry_points_exhaustive(module: &mut Module, rvsdg: &mut Rvsdg) {
    let entry_points = module
        .entry_points
        .iter()
        .map(|(f, _)| f)
        .collect::<Vec<_>>();

    let mut apply_node_queue = VecDeque::new();

    for entry_point in entry_points {
        if let Some(function_node) = rvsdg.get_function_node(entry_point) {
            let body_region = rvsdg[function_node].expect_function().body_region();

            for node in rvsdg[body_region].nodes() {
                if rvsdg[*node].is_op_apply() {
                    apply_node_queue.push_back(*node);
                }
            }

            // Inline the nodes currently in the queue, then check for new apply nodes and add them
            // to the queue; keep iterating until the queue is empty.
            while !apply_node_queue.is_empty() {
                while let Some(node) = apply_node_queue.pop_front() {
                    inline_function(module, rvsdg, node);
                }

                // We may be able to do something more efficient here by keeping track of which
                // nodes are getting inlined and only checking for new apply nodes on those nodes.
                // That would, however, increase the complexity of the inlining algorithm, and I'm
                // not sure if this search contributes significantly to the overall compile time, so
                // we'd have to measure whether that's worth it.
                for node in rvsdg[body_region].nodes() {
                    if rvsdg[*node].is_op_apply() {
                        apply_node_queue.push_back(*node);
                    }
                }
            }

            // Any function dependencies that were inlined will now be unused, so clean up the entry
            // point's dependencies.
            rvsdg.remove_unused_dependencies(function_node);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;
    use crate::rvsdg::{StateOrigin, ValueInput, ValueOrigin};
    use crate::ty::{TypeKind, TY_U32};
    use crate::{BinaryOperator, EntryPointKind, FnArg, FnSig, Function, Symbol};

    #[test]
    fn test_inline_entry_points_exhaustive() {
        let mut module = Module::new(Symbol::from_ref(""));

        let add_1 = Function {
            name: Symbol::from_ref("add_1"),
            module: Symbol::from_ref(""),
        };
        let add_1_ty = module.ty.register(TypeKind::Function(add_1));

        module.fn_sigs.register(
            add_1,
            FnSig {
                name: Default::default(),
                ty: add_1_ty,
                args: vec![FnArg {
                    ty: TY_U32,
                    shader_io_binding: None,
                }],
                ret_ty: Some(TY_U32),
            },
        );

        let add_2 = Function {
            name: Symbol::from_ref("add_2"),
            module: Symbol::from_ref(""),
        };
        let add_2_ty = module.ty.register(TypeKind::Function(add_2));

        module.fn_sigs.register(
            add_2,
            FnSig {
                name: Default::default(),
                ty: add_2_ty,
                args: vec![FnArg {
                    ty: TY_U32,
                    shader_io_binding: None,
                }],
                ret_ty: Some(TY_U32),
            },
        );

        let entry_point = Function {
            name: Symbol::from_ref("entry_point"),
            module: Symbol::from_ref(""),
        };
        let entry_point_ty = module.ty.register(TypeKind::Function(entry_point));

        module.fn_sigs.register(
            entry_point,
            FnSig {
                name: Default::default(),
                ty: entry_point_ty,
                args: vec![],
                ret_ty: Some(TY_U32),
            },
        );
        module
            .entry_points
            .register(entry_point, EntryPointKind::Compute(1, 1, 1));

        let mut rvsdg = Rvsdg::new();

        // Build add_1
        let (add_1_node, add_1_region) = rvsdg.register_function(&module, add_1, iter::empty());

        let add_1_node_0 = rvsdg.add_const_u32(add_1_region, 1);
        let add_1_node_1 = rvsdg.add_op_binary(
            add_1_region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::output(TY_U32, add_1_node_0, 0),
        );

        rvsdg.reconnect_region_result(
            add_1_region,
            0,
            ValueOrigin::Output {
                producer: add_1_node_1,
                output: 0,
            },
        );

        // Build add_2
        let (add_2_node, add_2_region) = rvsdg.register_function(&module, add_2, [add_1_node]);

        let add_2_node_0 = rvsdg.add_op_apply(
            &module,
            add_2_region,
            ValueInput::argument(add_1_ty, 0),
            [ValueInput::argument(TY_U32, 1)],
            StateOrigin::Argument,
        );
        let add_2_node_1 = rvsdg.add_op_apply(
            &module,
            add_2_region,
            ValueInput::argument(add_1_ty, 0),
            [ValueInput::output(TY_U32, add_2_node_0, 0)],
            StateOrigin::Node(add_2_node_0),
        );

        rvsdg.reconnect_region_result(
            add_2_region,
            0,
            ValueOrigin::Output {
                producer: add_2_node_1,
                output: 0,
            },
        );

        // Build entry_point
        let (entry_point_node, entry_point_region) =
            rvsdg.register_function(&module, entry_point, [add_2_node]);

        let entry_point_node_0 = rvsdg.add_const_u32(entry_point_region, 10);
        let entry_point_node_1 = rvsdg.add_op_apply(
            &module,
            entry_point_region,
            ValueInput::argument(add_2_ty, 0),
            [ValueInput::output(TY_U32, entry_point_node_0, 0)],
            StateOrigin::Argument,
        );

        rvsdg.reconnect_region_result(
            entry_point_region,
            0,
            ValueOrigin::Output {
                producer: entry_point_node_1,
                output: 0,
            },
        );

        inline_entry_points_exhaustive(&mut module, &mut rvsdg);

        // The entry_point function should no longer contain any apply nodes.
        assert_eq!(
            rvsdg[entry_point_region]
                .nodes()
                .into_iter()
                .filter(|n| rvsdg[**n].is_op_apply())
                .count(),
            0
        );

        // The entry_point function should no longer have any dependencies.
        assert!(rvsdg[entry_point_node]
            .expect_function()
            .dependencies()
            .is_empty());
    }
}
