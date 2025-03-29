use crate::cfg::{BasicBlock, Body, Branch, OpAssign, Terminator};

pub fn restructure_exit(body: &mut Body) -> BasicBlock {
    let mut exit_nodes = Vec::new();

    for (bb, data) in &body.basic_blocks {
        if matches!(&data.terminator, Terminator::Return(_)) {
            exit_nodes.push(bb);
        }
    }

    if exit_nodes.len() > 1 {
        let new_exit = body.append_block();
        let mut returns_value = false;

        for old_exit in exit_nodes {
            if let Some(value) = *body.basic_blocks[old_exit].terminator.expect_return() {
                body.basic_blocks[old_exit].statements.push(
                    OpAssign {
                        value,
                        result: body.ret,
                    }
                    .into(),
                );

                returns_value = true;
            }

            body.basic_blocks[old_exit].terminator = Terminator::Branch(Branch::single(new_exit));
        }

        if returns_value {
            body.basic_blocks[new_exit].terminator = Terminator::Return(Some(body.ret.into()));
        }

        new_exit
    } else if exit_nodes.len() == 1 {
        exit_nodes[0]
    } else {
        // An empty block that returns `void`
        body.append_block()
    }
}

#[cfg(test)]
mod tests {
    use smallvec::smallvec;

    use super::*;
    use crate::ty::TY_DUMMY;
    use crate::FnSig;

    #[test]
    fn test_restructure_exit() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![],
            ret_ty: None,
        });

        // Before:
        //
        //       bb0
        //       /  \
        //      /    \
        //     v      v
        //    bb1    bb2
        //

        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();

        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb1, bb2],
        });

        let mut reference = body.clone();

        restructure_exit(&mut body);

        let exit = reference.append_block();

        assert_eq!(body.basic_blocks.len(), 4);

        assert_eq!(
            body.basic_blocks[bb1].terminator,
            Terminator::Branch(Branch::single(exit))
        );
        assert_eq!(
            body.basic_blocks[bb2].terminator,
            Terminator::Branch(Branch::single(exit))
        );
        assert_eq!(body.basic_blocks[exit].terminator, Terminator::Return(None));
    }

    #[test]
    fn test_restructure_exit_infinite_loop() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![],
            ret_ty: None,
        });

        // Before:
        //
        //       bb0
        //       /  ^
        //      /    \
        //     v      \
        //    bb1--->bb2
        //

        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();

        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch::single(bb1));
        body.basic_blocks[bb1].terminator = Terminator::Branch(Branch::single(bb2));
        body.basic_blocks[bb2].terminator = Terminator::Branch(Branch::single(bb0));

        let mut reference = body.clone();

        restructure_exit(&mut body);

        let exit = reference.append_block();

        assert_eq!(body.basic_blocks.len(), 4);

        assert_eq!(
            body.basic_blocks[bb0].terminator,
            Terminator::Branch(Branch::single(bb1))
        );
        assert_eq!(
            body.basic_blocks[bb1].terminator,
            Terminator::Branch(Branch::single(bb2))
        );
        assert_eq!(
            body.basic_blocks[bb2].terminator,
            Terminator::Branch(Branch::single(bb0))
        );
        assert_eq!(body.basic_blocks[exit].terminator, Terminator::Return(None));
    }
}
