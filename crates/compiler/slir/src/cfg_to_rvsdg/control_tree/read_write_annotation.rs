use indexmap::IndexSet;

use crate::cfg::{
    BasicBlock, Body, LocalValue, OpAlloca, OpAssign, OpBinary, OpCall, OpLoad, OpPtrElementPtr,
    OpStore, OpUnary, Statement, Terminator, Value,
};
use crate::cfg_to_rvsdg::control_tree::control_tree::{
    BranchingNode, ControlTree, ControlTreeNode, ControlTreeNodeKind, LinearNode, LoopNode,
};
use crate::cfg_to_rvsdg::control_tree::slice_annotation::SliceAnnotation;

pub trait WithReadValues {
    fn with_read_values<F>(&self, f: F)
    where
        F: FnMut(&Value);
}

impl WithReadValues for OpAlloca {
    fn with_read_values<F>(&self, _: F)
    where
        F: FnMut(&Value),
    {
    }
}

impl WithReadValues for OpAssign {
    fn with_read_values<F>(&self, mut f: F)
    where
        F: FnMut(&Value),
    {
        f(&self.value);
    }
}

impl WithReadValues for OpLoad {
    fn with_read_values<F>(&self, mut f: F)
    where
        F: FnMut(&Value),
    {
        f(&self.ptr);
    }
}

impl WithReadValues for OpStore {
    fn with_read_values<F>(&self, mut f: F)
    where
        F: FnMut(&Value),
    {
        f(&self.ptr);
        f(&self.value);
    }
}

impl WithReadValues for OpPtrElementPtr {
    fn with_read_values<F>(&self, mut f: F)
    where
        F: FnMut(&Value),
    {
        f(&self.ptr);
        self.indices.iter().for_each(f);
    }
}

impl WithReadValues for OpUnary {
    fn with_read_values<F>(&self, mut f: F)
    where
        F: FnMut(&Value),
    {
        f(&self.value);
    }
}

impl WithReadValues for OpBinary {
    fn with_read_values<F>(&self, mut f: F)
    where
        F: FnMut(&Value),
    {
        f(&self.lhs);
        f(&self.rhs);
    }
}

impl WithReadValues for OpCall {
    fn with_read_values<F>(&self, f: F)
    where
        F: FnMut(&Value),
    {
        self.args.iter().for_each(f);
    }
}

macro_rules! impl_with_read_values_statement {
    ($($op:ident,)*) => {
        impl WithReadValues for Statement {
            fn with_read_values<F>(&self, f: F)
            where
                F: FnMut(&Value),
            {
                match self {
                    $(Statement::$op(op) => op.with_read_values(f),)*
                }
            }
        }
    };
}

impl_with_read_values_statement! {
    OpAlloca,
    OpAssign,
    OpLoad,
    OpStore,
    OpPtrElementPtr,
    OpUnary,
    OpBinary,
    OpCall,
}

pub trait WithWrittenValues {
    fn with_written_values<F>(&self, f: F)
    where
        F: FnMut(&LocalValue);
}

impl WithWrittenValues for OpStore {
    fn with_written_values<F>(&self, _: F)
    where
        F: FnMut(&LocalValue),
    {
    }
}

impl WithWrittenValues for OpCall {
    fn with_written_values<F>(&self, mut f: F)
    where
        F: FnMut(&LocalValue),
    {
        if let Some(result) = &self.result {
            f(result);
        }
    }
}

macro_rules! impl_with_written_values_op {
    ($($op:ident,)*) => {
        $(
            impl WithWrittenValues for $op {
                fn with_written_values<F>(&self, mut f: F)
                where
                    F: FnMut(&LocalValue),
                {
                    f(&self.result);
                }
            }
        )*
    };
}

impl_with_written_values_op! {
    OpAlloca,
    OpAssign,
    OpLoad,
    OpPtrElementPtr,
    OpUnary,
    OpBinary,
}

macro_rules! impl_with_written_values_statement {
    ($($op:ident,)*) => {
        impl WithWrittenValues for Statement {
            fn with_written_values<F>(&self, f: F)
            where
                F: FnMut(&LocalValue),
            {
                match self {
                    $(Statement::$op(op) => op.with_written_values(f),)*
                }
            }
        }
    };
}

impl_with_written_values_statement! {
    OpAlloca,
    OpAssign,
    OpLoad,
    OpStore,
    OpPtrElementPtr,
    OpUnary,
    OpBinary,
    OpCall,
}

pub struct ReadWriteAnnotationVisitor<'a> {
    control_tree: &'a ControlTree,
    body: &'a Body,
    read_state: SliceAnnotation<LocalValue>,
    write_state: SliceAnnotation<LocalValue>,
    read_accum: IndexSet<LocalValue>,
    write_accum: IndexSet<LocalValue>,
}

impl<'a> ReadWriteAnnotationVisitor<'a> {
    fn new(control_tree: &'a ControlTree, body: &'a Body) -> Self {
        ReadWriteAnnotationVisitor {
            control_tree,
            body,
            read_state: SliceAnnotation::new(control_tree.node_count()),
            write_state: SliceAnnotation::new(control_tree.node_count()),
            read_accum: Default::default(),
            write_accum: Default::default(),
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

    fn visit_basic_block(&mut self, (node, bb): (ControlTreeNode, BasicBlock)) {
        self.read_accum.clear();
        self.write_accum.clear();

        match &self.body.basic_blocks[bb].terminator {
            Terminator::Branch(b) => {
                if let Some(selector) = b.selector {
                    self.read_accum.insert(selector);
                }
            }
            Terminator::Return(value) => {
                if let Some(Value::Local(value)) = value {
                    self.read_accum.insert(*value);
                }
            }
        }

        for statement in self.body.basic_blocks[bb].statements.iter().rev() {
            statement.with_written_values(|v| {
                self.read_accum.shift_remove(v);
                self.write_accum.insert(*v);
            });

            statement.with_read_values(|value| {
                if let Value::Local(value) = value {
                    self.read_accum.insert(*value);
                }
            });
        }

        self.read_state.set(node, self.read_accum.iter().copied());
        self.write_state.set(node, self.write_accum.iter().copied());
    }

    fn visit_linear_node(&mut self, (node, data): (ControlTreeNode, &LinearNode)) {
        for child in &data.children {
            self.visit(*child);
        }

        self.read_accum.clear();
        self.write_accum.clear();

        for child in data.children.iter().rev() {
            for value in self.write_state.get(*child) {
                self.read_accum.shift_remove(value);
                self.write_accum.insert(*value);
            }

            self.read_accum.extend(self.read_state.get(*child));
        }

        self.read_state.set(node, self.read_accum.iter().copied());
        self.write_state.set(node, self.write_accum.iter().copied());
    }

    fn visit_branching_node(&mut self, (node, data): (ControlTreeNode, &BranchingNode)) {
        for branch in &data.branches {
            self.visit(*branch);
        }

        self.write_accum.clear();

        for (i, branch) in data.branches.iter().enumerate() {
            if i == 0 {
                self.write_accum.extend(self.write_state.get(*branch));
            } else {
                // Retain only the intersection

                // Temporarily re-use the read accumulation set to help construct the write set
                // intersection.
                self.read_accum.clear();
                self.read_accum.extend(self.write_state.get(*branch));

                self.write_accum.retain(|v| self.read_accum.contains(v));
            }
        }

        self.read_accum.clear();

        for branch in &data.branches {
            self.read_accum.extend(self.read_state.get(*branch));
        }

        self.read_state.set(node, self.read_accum.iter().copied());
        self.write_state.set(node, self.write_accum.iter().copied());
    }

    fn visit_loop_node(&mut self, (node, data): (ControlTreeNode, &LoopNode)) {
        self.visit(data.inner);

        self.read_state.copy(data.inner, node);
        self.write_state.copy(data.inner, node);
    }

    fn into_annotation_state(self) -> (SliceAnnotation<LocalValue>, SliceAnnotation<LocalValue>) {
        (self.read_state, self.write_state)
    }
}

pub fn annotate_read_write(
    control_tree: &ControlTree,
    body: &Body,
) -> (SliceAnnotation<LocalValue>, SliceAnnotation<LocalValue>) {
    let mut visitor = ReadWriteAnnotationVisitor::new(control_tree, body);

    visitor.visit(control_tree.root());
    visitor.into_annotation_state()
}

#[cfg(test)]
mod tests {
    use smallvec::smallvec;

    use super::*;
    use crate::cfg::{Branch, InlineConst, LocalValueData, OpAssign, OpBinary, Terminator};
    use crate::cfg_to_rvsdg::control_flow_restructuring::{
        restructure_branches, restructure_loops, Graph,
    };
    use crate::ty::{TY_BOOL, TY_DUMMY, TY_U32};
    use crate::{BinaryOperator, FnArg, FnSig};

    #[test]
    fn annotate_read_write_bb_test() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![FnArg {
                ty: TY_U32,
                shader_io_binding: None,
            }],
            ret_ty: Some(TY_U32),
        });

        let x = body.params[0];
        let c = body
            .local_values
            .insert(LocalValueData { ty: Some(TY_U32) });

        let bb0 = body.append_block();

        body.basic_blocks[bb0].statements.push(
            OpBinary {
                operator: BinaryOperator::Add,
                lhs: x.into(),
                rhs: 1u32.into(),
                result: c.into(),
            }
            .into(),
        );

        let mut graph = Graph::init(body);
        let reentry_edges = restructure_loops(&mut graph);
        let branch_info = restructure_branches(&mut graph, &reentry_edges);

        let control_tree = ControlTree::generate(&graph, &reentry_edges, &branch_info);
        let body = graph.into_inner();

        let (read, write) = annotate_read_write(&control_tree, &body);

        #[allow(non_snake_case)]
        {
            let A = control_tree.root();
            let A_children = &control_tree[A].expect_linear().children;
            let B = A_children[0];

            assert_eq!(read.get(A), &[x]);
            assert_eq!(read.get(B), &[x]);

            assert_eq!(write.get(A), &[c]);
            assert_eq!(write.get(B), &[c]);
        }
    }

    #[test]
    fn annotate_read_write_test() {
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
        // Expected read and write annotations:
        //
        //     | Node | Read       | Write      |
        //     |------|------------|------------|
        //     | A    | x, y       | r, c       |
        //     | B    |            |            |
        //     | C    | x, y       | r, c       |
        //     | D    | x          |            |
        //     | E    | x, y       | r, c       |
        //     | F    | y          | c          |
        //     | G    | x, y       | r          |
        //     | H    | r          |            |
        //     | I    |            | r          |
        //     | J    | x, y       | r, x, y, t |
        //     | K    |            | r          |
        //     | L    | x, y       | x, y, t    |
        //     | M    |            | r          |
        //

        let (read, write) = annotate_read_write(&control_tree, &body);

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

            assert_eq!(read.get(A), &[x, y]);
            assert_eq!(read.get(B), &[]);
            assert_eq!(read.get(C), &[x, y]);
            assert_eq!(read.get(D), &[x]);
            assert_eq!(read.get(E), &[x, y]);
            assert_eq!(read.get(F), &[y]);
            assert_eq!(read.get(G), &[x, y]);
            assert_eq!(read.get(H), &[r]);
            assert_eq!(read.get(I), &[]);
            assert_eq!(read.get(J), &[x, y]);
            assert_eq!(read.get(K), &[]);
            assert_eq!(read.get(L), &[x, y]);
            assert_eq!(read.get(M), &[]);

            assert_eq!(write.get(A), &[r, c]);
            assert_eq!(write.get(B), &[]);
            assert_eq!(write.get(C), &[r, c]);
            assert_eq!(write.get(D), &[]);
            assert_eq!(write.get(E), &[r, c]);
            assert_eq!(write.get(F), &[c]);
            assert_eq!(write.get(G), &[r]);
            assert_eq!(write.get(H), &[]);
            assert_eq!(write.get(I), &[r]);
            assert_eq!(write.get(J), &[r, x, y, t]);
            assert_eq!(write.get(K), &[r]);
            assert_eq!(write.get(L), &[x, y, t]);
            assert_eq!(write.get(M), &[r]);
        }
    }
}
