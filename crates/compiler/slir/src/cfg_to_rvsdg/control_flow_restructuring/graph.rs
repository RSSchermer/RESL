use rustc_hash::{FxBuildHasher, FxHashMap};
use slotmap::SlotMap;

use crate::cfg::{
    BasicBlock, BasicBlockData, Body, LocalValue, LocalValueData, Statement, Terminator,
};
use crate::cfg_to_rvsdg::control_flow_restructuring::exit_restructuring::restructure_exit;

fn inverse_graph(
    graph: &SlotMap<BasicBlock, BasicBlockData>,
) -> FxHashMap<BasicBlock, Vec<BasicBlock>> {
    let mut inverse: FxHashMap<BasicBlock, Vec<BasicBlock>> =
        FxHashMap::with_capacity_and_hasher(graph.len(), FxBuildHasher::default());

    for (source, bb) in graph {
        if let Terminator::Branch(b) = &bb.terminator {
            for dest in &b.branches {
                inverse.entry(*dest).or_default().push(source);
            }
        }
    }

    inverse
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Edge {
    pub source: BasicBlock,
    pub dest: BasicBlock,
}

#[derive(Debug)]
pub struct Graph {
    body: Body,
    inverse_graph: FxHashMap<BasicBlock, Vec<BasicBlock>>,
    entry: BasicBlock,
    exit: BasicBlock,
}

impl Graph {
    pub fn init(mut body: Body) -> Self {
        let entry = body.entry.expect("body must have an entry block");
        let exit = restructure_exit(&mut body);

        let inverse_graph = inverse_graph(&body.basic_blocks);

        Graph {
            body: body.clone(),
            inverse_graph,
            entry,
            exit,
        }
    }

    pub fn entry(&self) -> BasicBlock {
        self.entry
    }

    pub fn exit(&self) -> BasicBlock {
        self.exit
    }

    pub fn nodes(&self) -> impl Iterator<Item = BasicBlock> + use<'_> {
        self.body.basic_blocks.keys()
    }

    pub fn append_block(&mut self) -> BasicBlock {
        self.body.append_block()
    }

    pub fn statements(&self, bb: BasicBlock) -> &[Statement] {
        &self.body.basic_blocks[bb].statements
    }

    pub fn statements_mut(&mut self, bb: BasicBlock) -> &mut Vec<Statement> {
        &mut self.body.basic_blocks[bb].statements
    }

    pub fn add_value(&mut self, value_data: LocalValueData) -> LocalValue {
        self.body.local_values.insert(value_data)
    }

    pub fn selector(&self, bb: BasicBlock) -> Option<LocalValue> {
        self.body.basic_blocks[bb]
            .terminator
            .expect_branch()
            .selector
    }

    pub fn set_selector(&mut self, bb: BasicBlock, selector: Option<LocalValue>) {
        self.body.basic_blocks[bb]
            .terminator
            .get_or_make_branch()
            .selector = selector;
    }

    pub fn connect(&mut self, edge: Edge) {
        let Edge { source, dest } = edge;

        self.body.basic_blocks[source]
            .terminator
            .get_or_make_branch()
            .branches
            .push(dest);
        self.inverse_graph.entry(dest).or_default().push(source);
    }

    pub fn reconnect_dest(&mut self, edge: Edge, new_dest: BasicBlock) {
        let Edge { source, dest } = edge;

        // First point the out-edge from the source to the new destination

        let mut src_out_found = false;

        for bb in &mut self.body.basic_blocks[source]
            .terminator
            .get_or_make_branch()
            .branches
        {
            if *bb == dest {
                *bb = new_dest;
                src_out_found = true;
            }
        }

        assert!(src_out_found, "edge source does not connect to edge dest");

        // Then disconnect the in-ege frmo the old destination

        let mut dest_in_found = false;
        let dst_in = self.inverse_graph.get_mut(&dest).unwrap();

        for i in 0..dst_in.len() {
            if dst_in[i] == source {
                dst_in.remove(i);
                dest_in_found = true;

                break;
            }
        }

        assert!(dest_in_found, "edge dest does not connect to edge source");

        // Finally, add an in-edge to the new destination

        self.inverse_graph.entry(new_dest).or_default().push(source);
    }

    pub fn divert(&mut self, edge: Edge, via: BasicBlock) {
        let Edge { source, dest } = edge;

        // First point the out-edge from the source to the "via" node

        let mut src_out_found = false;

        for bb in &mut self.body.basic_blocks[source]
            .terminator
            .get_or_make_branch()
            .branches
        {
            if *bb == dest {
                *bb = via;
                src_out_found = true;
            }
        }

        assert!(src_out_found, "edge source does not connect to edge dest");

        // First point the in-edge from the destination to the "via" node

        let mut dest_in_found = false;

        for bb in self.inverse_graph.entry(dest).or_default() {
            if *bb == source {
                *bb = via;
                dest_in_found = true;
            }
        }

        assert!(dest_in_found, "edge dest does not connect to edge source");

        // Finally, add an in-edge and out-edge to the "via" node for the source and destination
        // respectively

        self.body.basic_blocks[via]
            .terminator
            .get_or_make_branch()
            .branches
            .push(dest);
        self.inverse_graph.entry(via).or_default().push(source);
    }

    pub fn parents(&self, bb: BasicBlock) -> &[BasicBlock] {
        self.inverse_graph
            .get(&bb)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    pub fn children(&self, bb: BasicBlock) -> &[BasicBlock] {
        match &self.body.basic_blocks[bb].terminator {
            Terminator::Branch(b) => b.branches.as_slice(),
            Terminator::Return(_) => &[],
        }
    }

    pub fn into_inner(self) -> Body {
        self.body
    }
}

#[cfg(test)]
mod tests {
    use smallvec::smallvec;

    use super::*;
    use crate::cfg::{Branch, Terminator};
    use crate::ty::TY_DUMMY;
    use crate::FnSig;

    #[test]
    fn test_connect() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![],
            ret_ty: None,
        });

        let entry = body.append_block();
        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();
        let bb3 = body.append_block();
        let exit = body.append_block();

        body.basic_blocks[entry].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb0, bb1],
        });
        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch::single(bb2));
        body.basic_blocks[bb1].terminator = Terminator::Branch(Branch::single(bb3));
        body.basic_blocks[bb2].terminator = Terminator::Branch(Branch::single(exit));
        body.basic_blocks[bb3].terminator = Terminator::Branch(Branch::single(exit));

        let mut graph = Graph::init(body.clone());

        graph.connect(Edge {
            source: bb0,
            dest: bb3,
        });

        assert_eq!(graph.children(bb0), &[bb2, bb3]);
        assert_eq!(graph.children(bb1), &[bb3]);
        assert_eq!(graph.children(bb2), &[exit]);
        assert_eq!(graph.children(bb3), &[exit]);

        assert_eq!(graph.parents(bb0), &[entry]);
        assert_eq!(graph.parents(bb1), &[entry]);
        assert_eq!(graph.parents(bb2), &[bb0]);
        assert_eq!(graph.parents(bb3), &[bb1, bb0]);
    }

    #[test]
    fn test_reconnect_dest() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![],
            ret_ty: None,
        });

        let entry = body.append_block();
        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();
        let bb3 = body.append_block();
        let exit = body.append_block();

        body.basic_blocks[entry].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb0, bb1],
        });
        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch::single(bb2));
        body.basic_blocks[bb1].terminator = Terminator::Branch(Branch::single(bb3));
        body.basic_blocks[bb2].terminator = Terminator::Branch(Branch::single(exit));
        body.basic_blocks[bb3].terminator = Terminator::Branch(Branch::single(exit));

        let mut graph = Graph::init(body.clone());

        graph.reconnect_dest(
            Edge {
                source: bb0,
                dest: bb2,
            },
            bb3,
        );

        assert_eq!(graph.children(bb0), &[bb3]);
        assert_eq!(graph.children(bb1), &[bb3]);
        assert_eq!(graph.children(bb2), &[exit]);
        assert_eq!(graph.children(bb3), &[exit]);

        assert_eq!(graph.parents(bb0), &[entry]);
        assert_eq!(graph.parents(bb1), &[entry]);
        assert_eq!(graph.parents(bb2), &[]);
        assert_eq!(graph.parents(bb3), &[bb1, bb0]);
    }
}
