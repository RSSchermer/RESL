use std::mem;

use indexmap::IndexSet;

use crate::cfg::{BasicBlock, LocalValue, Terminator, Value};
use crate::cfg_to_rvsdg::control_tree::control_tree::{
    BranchingNode, ControlTree, ControlTreeNode, ControlTreeNodeKind, LinearNode, LoopNode,
};
use crate::cfg_to_rvsdg::control_tree::slice_annotation::SliceAnnotation;

struct DemandAnnotationVisitor<'a> {
    control_tree: &'a ControlTree,
    read_annotations: &'a SliceAnnotation<LocalValue>,
    write_annotations: &'a SliceAnnotation<LocalValue>,
    annotations: SliceAnnotation<LocalValue>,
    accum: IndexSet<LocalValue>,
}

impl<'a> DemandAnnotationVisitor<'a> {
    fn new(
        control_tree: &'a ControlTree,
        read_annotations: &'a SliceAnnotation<LocalValue>,
        write_annotations: &'a SliceAnnotation<LocalValue>,
    ) -> Self {
        DemandAnnotationVisitor {
            control_tree,
            read_annotations,
            write_annotations,
            annotations: SliceAnnotation::new(control_tree.node_count()),
            accum: Default::default(),
        }
    }

    fn visit(&mut self, node: ControlTreeNode) {
        match &self.control_tree[node] {
            ControlTreeNodeKind::BasicBlock(bb) => self.visit_basic_block((node, *bb)),
            ControlTreeNodeKind::Linear(data) => self.visit_linear_node((node, data)),
            ControlTreeNodeKind::Branching(data) => self.visit_branching_node((node, data)),
            ControlTreeNodeKind::Loop(data) => self.visit_loop_node((node, data)),
        }
    }

    fn visit_basic_block(&mut self, (node, _): (ControlTreeNode, BasicBlock)) {
        for value in self.write_annotations.get(node) {
            self.accum.shift_remove(value);
        }

        self.accum.extend(self.read_annotations.get(node));

        self.annotations.set(node, self.accum.iter().copied());
    }

    fn visit_linear_node(&mut self, (node, data): (ControlTreeNode, &LinearNode)) {
        for child in data.children.iter().rev() {
            self.visit(*child);
        }

        for value in self.write_annotations.get(node) {
            self.accum.shift_remove(value);
        }

        self.accum.extend(self.read_annotations.get(node));
        self.annotations.set(node, self.accum.iter().copied());
    }

    fn visit_branching_node(&mut self, (node, data): (ControlTreeNode, &BranchingNode)) {
        for branch in &data.branches {
            let accum_copy = self.accum.clone();

            self.visit(*branch);

            // Restore the accumulated state to the copy we made before visiting the branch
            let _ = mem::replace(&mut self.accum, accum_copy);
        }

        for value in self.write_annotations.get(node) {
            self.accum.shift_remove(value);
        }

        self.accum.extend(self.read_annotations.get(node));
        self.annotations.set(node, self.accum.iter().copied());
    }

    fn visit_loop_node(&mut self, (node, data): (ControlTreeNode, &LoopNode)) {
        self.accum.extend(self.read_annotations.get(node));
        self.annotations.set(node, self.accum.iter().copied());
        self.visit(data.inner);
    }

    fn into_annotation_state(self) -> SliceAnnotation<LocalValue> {
        self.annotations
    }
}

pub fn annotate_demand(
    control_tree: &ControlTree,
    read_annotations: &SliceAnnotation<LocalValue>,
    write_annotations: &SliceAnnotation<LocalValue>,
) -> SliceAnnotation<LocalValue> {
    let mut visitor =
        DemandAnnotationVisitor::new(control_tree, read_annotations, write_annotations);

    visitor.visit(control_tree.root());
    visitor.into_annotation_state()
}

#[cfg(test)]
mod tests {
    use smallvec::smallvec;

    use super::*;
    use crate::cfg::{Body, Branch, InlineConst, LocalValueData, OpAssign, OpBinary, Terminator};
    use crate::cfg_to_rvsdg::control_flow_restructuring::{
        restructure_branches, restructure_loops, Graph,
    };
    use crate::cfg_to_rvsdg::control_tree::read_write_annotation::annotate_read_write;
    use crate::ty::{TY_BOOL, TY_DUMMY, TY_U32};
    use crate::{BinaryOperator, FnArg, FnSig};

    #[test]
    fn annotate_demand_test() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![
                FnArg {
                    ty: TY_U32,
                    shader_io_binding: None,
                },
                FnArg {
                    ty: TY_U32,
                    shader_io_binding: None,
                },
            ],
            ret_ty: Some(TY_U32),
        });

        let x = body.params[0];
        let y = body.params[1];
        let c = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_BOOL) });
        let t = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });
        let r = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });

        let enter = body.append_block();
        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();
        let bb3 = body.append_block();
        let bb4 = body.append_block();
        let exit = body.append_block();

        body.basic_blocks[enter].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb0],
        });

        body.basic_blocks[bb0].statements.push(
            OpBinary {
                operator: BinaryOperator::NotEq,
                lhs: y.into(),
                rhs: 0u32.into(),
                result: c,
            }
            .into(),
        );
        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch {
            selector: Some(c),
            branches: smallvec![bb1, bb2],
        });

        body.basic_blocks[bb1].statements.push(
            OpAssign {
                value: 0u32.into(),
                result: r,
            }
            .into(),
        );
        body.basic_blocks[bb1].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb4],
        });

        body.basic_blocks[bb2].statements.push(
            OpAssign {
                value: y.into(),
                result: t,
            }
            .into(),
        );
        body.basic_blocks[bb2].statements.push(
            OpBinary {
                operator: BinaryOperator::Mod,
                lhs: x.into(),
                rhs: y.into(),
                result: y.into(),
            }
            .into(),
        );
        body.basic_blocks[bb2].statements.push(
            OpAssign {
                value: t.into(),
                result: x,
            }
            .into(),
        );
        body.basic_blocks[bb2].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb3],
        });

        body.basic_blocks[bb3].statements.push(
            OpAssign {
                value: Value::InlineConst(InlineConst::U32(1)),
                result: r,
            }
            .into(),
        );
        body.basic_blocks[bb3].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb4],
        });

        body.basic_blocks[bb4].terminator = Terminator::Branch(Branch {
            selector: Some(r),
            branches: smallvec![exit, bb0],
        });

        body.basic_blocks[exit].terminator = Terminator::Return(Some(x.into()));

        let mut graph = Graph::init(body);
        let reentry_edges = restructure_loops(&mut graph);
        let branch_info = restructure_branches(&mut graph, &reentry_edges);

        let control_tree = ControlTree::generate(&graph, &reentry_edges, &branch_info);
        let body = graph.into_inner();

        // Control-tree layout:
        //
        //                      Linear (A)
        //                          |
        //              -------------------------
        //              |           |           |
        //          BB:enter (B)   Loop (C)   BB:exit (D)
        //                          |
        //                      Linear (E)
        //                          |
        //              -----------------------------
        //              |           |               |
        //         BB:bb0 (F)    Branching (G)   BB:bb4 (H)
        //                          |
        //              ----------------------------
        //              |                          |
        //          Linear (I)                 Linear (J)
        //              |                          |
        //           BB:bb1 (K)             ----------------
        //                                  |              |
        //                               BB:bb2 (L)     BB:bb3 (M)
        //
        //
        // Expected demand annotations:
        //
        //     | Node | Read       | Write      | Demand     |
        //     |------|------------|------------|------------|
        //     | A    | x, y       | r, c       | x, y       |
        //     | B    |            |            | x, y       |
        //     | C    | x, y       | r, c       | x, y       |
        //     | D    | x          |            | x          |
        //     | E    | x, y       | r, c       | x, y       |
        //     | F    | y          | c          | x, y       |
        //     | G    | x, y       | r          | x, y       |
        //     | H    | r          |            | x, y, r    |
        //     | I    |            | r          | x, y       |
        //     | J    | x, y       | r, x, y, t | x, y       |
        //     | K    |            | r          | x, y       |
        //     | L    | x, y       | x, y, t    | x, y       |
        //     | M    |            | r          | x, y       |
        //

        let (read, write) = annotate_read_write(&control_tree, &body);
        let demand = annotate_demand(&control_tree, &read, &write);

        #[allow(non_snake_case)]
        {
            let A = control_tree.root();
            let A_children = &control_tree[A].expect_linear().children;
            let B = A_children[0];
            let C = A_children[1];
            let D = A_children[2];
            let E = control_tree[C].expect_loop().inner;
            let E_children = &control_tree[E].expect_linear().children;
            let F = E_children[0];
            let G = E_children[1];
            let G_branches = &control_tree[G].expect_branching().branches;
            let H = E_children[2];
            let I = G_branches[0];
            let I_children = &control_tree[I].expect_linear().children;
            let J = G_branches[1];
            let J_children = &control_tree[J].expect_linear().children;
            let K = I_children[0];
            let L = J_children[0];
            let M = J_children[1];

            assert_eq!(demand.get(A), &[x, y]);
            assert_eq!(demand.get(B), &[x, y]);
            assert_eq!(demand.get(C), &[x, y]);
            assert_eq!(demand.get(D), &[x]);
            assert_eq!(demand.get(E), &[x, y]);
            assert_eq!(demand.get(F), &[x, y]);
            assert_eq!(demand.get(G), &[x, y]);
            assert_eq!(demand.get(H), &[x, y, r]);
            assert_eq!(demand.get(I), &[x, y]);
            assert_eq!(demand.get(J), &[x, y]);
            assert_eq!(demand.get(K), &[x, y]);
            assert_eq!(demand.get(L), &[x, y]);
            assert_eq!(demand.get(M), &[x, y]);
        }
    }
}
