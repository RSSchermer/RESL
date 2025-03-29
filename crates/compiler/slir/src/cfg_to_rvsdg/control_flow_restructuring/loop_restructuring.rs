use indexmap::IndexSet;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::cfg::{BasicBlock, LocalValue, OpAssign};
use crate::cfg_to_rvsdg::control_flow_restructuring::strongly_connected_components::{
    strongly_connected_components, SccStructure,
};
use crate::cfg_to_rvsdg::control_flow_restructuring::{Edge, Graph};

/// Restructures the loops in the graph.
///
/// Returns the set of re-entry edges (edges that connect the tail block of a loop to the entry
/// block of the loop) after restructuring.
pub fn restructure_loops(graph: &mut Graph) -> FxHashSet<Edge> {
    let mut reentry_edges = FxHashSet::default();

    restructure_loops_internal(graph, &mut reentry_edges);

    reentry_edges
}

fn restructure_loops_internal(graph: &mut Graph, reentry_edges: &mut FxHashSet<Edge>) {
    let mut components = strongly_connected_components(graph, reentry_edges);

    while components.len() > 0 {
        for component in &components {
            restructure_loop(graph, reentry_edges, component);
        }

        components = strongly_connected_components(graph, reentry_edges);
    }
}

fn restructure_loop(
    graph: &mut Graph,
    reentry_edges: &mut FxHashSet<Edge>,
    scc: &IndexSet<BasicBlock>,
) {
    let scc_structure = SccStructure::analyse(graph, scc);

    let reentry_edge = if scc_structure.is_tail_controlled_loop() {
        // Already in the desired structure

        scc_structure.repetition_edges[0]
    } else {
        let branch_selector = graph.add_value(Default::default());

        let (entry, branch_mapping) =
            restructure_loop_entry(graph, &scc_structure, branch_selector);

        restructure_loop_tail(
            graph,
            &scc_structure,
            branch_selector,
            entry,
            branch_mapping,
        )
    };

    reentry_edges.insert(reentry_edge);
}

fn restructure_loop_entry(
    graph: &mut Graph,
    structure: &SccStructure,
    branch_selector: LocalValue,
) -> (BasicBlock, FxHashMap<BasicBlock, u32>) {
    let entry = graph.append_block();
    let mut branch_mapping = FxHashMap::default();

    graph.set_selector(entry, Some(branch_selector));

    for (i, edge) in structure.entry_edges.iter().enumerate() {
        let intermediate = graph.append_block();

        graph.statements_mut(intermediate).push(
            OpAssign {
                value: (i as u32).into(),
                result: branch_selector,
            }
            .into(),
        );

        graph.reconnect_dest(*edge, intermediate);
        graph.connect(Edge {
            source: intermediate,
            dest: entry,
        });
        graph.connect(Edge {
            source: entry,
            dest: edge.dest,
        });

        branch_mapping.insert(edge.dest, i as u32);
    }

    (entry, branch_mapping)
}

fn restructure_loop_tail(
    graph: &mut Graph,
    structure: &SccStructure,
    branch_selector: LocalValue,
    entry: BasicBlock,
    entry_branch_mapping: FxHashMap<BasicBlock, u32>,
) -> Edge {
    let tail = graph.append_block();
    let exit = graph.append_block();

    let reentry_selector = graph.add_value(Default::default());

    graph.set_selector(tail, Some(reentry_selector));
    graph.connect(Edge {
        source: tail,
        dest: exit,
    });
    graph.connect(Edge {
        source: tail,
        dest: entry,
    });

    for edge in &structure.repetition_edges {
        let intermediate = graph.append_block();
        let statements = graph.statements_mut(intermediate);

        let branch_index = *entry_branch_mapping
            .get(&edge.dest)
            .expect("no branch for repetition edge");

        statements.push(
            OpAssign {
                value: branch_index.into(),
                result: branch_selector,
            }
            .into(),
        );

        // Set the `reentry_selector` selector to `1` to indicate that we will be repeating the
        // loop.
        statements.push(
            OpAssign {
                value: 1u32.into(),
                result: reentry_selector,
            }
            .into(),
        );

        graph.reconnect_dest(*edge, intermediate);
        graph.connect(Edge {
            source: intermediate,
            dest: tail,
        })
    }

    for (i, edge) in structure.exit_edges.iter().enumerate() {
        let intermediate = graph.append_block();
        let statements = graph.statements_mut(intermediate);

        statements.push(
            OpAssign {
                value: (i as u32).into(),
                result: branch_selector,
            }
            .into(),
        );

        // Set the `reentry_selector` selector to `0` to indicate that we will be exiting the
        // loop.
        statements.push(
            OpAssign {
                value: 0u32.into(),
                result: reentry_selector,
            }
            .into(),
        );

        graph.reconnect_dest(*edge, intermediate);
        graph.connect(Edge {
            source: intermediate,
            dest: tail,
        });
        graph.connect(Edge {
            source: exit,
            dest: edge.dest,
        })
    }

    Edge {
        source: tail,
        dest: entry,
    }
}

#[cfg(test)]
mod tests {
    use smallvec::smallvec;

    use super::*;
    use crate::cfg::{Body, Branch, Terminator};
    use crate::ty::TY_DUMMY;
    use crate::FnSig;

    #[test]
    fn test_loop_restructuring() {
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
        //    |  ^  ^  |
        //    |   \/   |
        //    |   /\   |
        //    v  /  \  v
        //   bb3      bb4
        //    |        |
        //    v        v
        //   bb5      bb6
        //     \      /
        //      \    /
        //       v  v
        //       bb7
        //

        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();
        let bb3 = body.append_block();
        let bb4 = body.append_block();
        let bb5 = body.append_block();
        let bb6 = body.append_block();
        let bb7 = body.append_block();

        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb1, bb2],
        });
        body.basic_blocks[bb1].terminator = Terminator::Branch(Branch::single(bb3));
        body.basic_blocks[bb2].terminator = Terminator::Branch(Branch::single(bb4));
        body.basic_blocks[bb3].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb5, bb2],
        });
        body.basic_blocks[bb4].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb6, bb1],
        });
        body.basic_blocks[bb5].terminator = Terminator::Branch(Branch::single(bb7));
        body.basic_blocks[bb6].terminator = Terminator::Branch(Branch::single(bb7));

        let mut graph = Graph::init(body.clone());

        restructure_loops(&mut graph);

        // Restructured:
        //
        //            bb0
        //           /  \
        //          /    \
        //         v      v
        //        bb9    bb10
        //          \    /
        //           v  v
        //            bb8 <--------|
        //           /  \          |
        //          v    v         |
        //        bb1    bb2       |
        //        |        |       |
        //        v        v       |
        //       bb3      bb4      |
        //       / \      / \      |
        //      v   v    v   v     |
        //    bb14 bb16 bb13 bb15  |
        //       \   |  |   /      |
        //        v  v  v  v       |
        //           bb11 ---------|
        //            |
        //            v
        //           bb12
        //          /   \
        //         v     v
        //       bb5      bb6
        //         \      /
        //          \    /
        //           v  v
        //           bb7
        //

        let bb8 = body.append_block();
        let bb9 = body.append_block();
        let bb10 = body.append_block();
        let bb11 = body.append_block();
        let bb12 = body.append_block();
        let bb13 = body.append_block();
        let bb14 = body.append_block();
        let bb15 = body.append_block();
        let bb16 = body.append_block();

        assert_eq!(
            FxHashSet::from_iter(graph.children(bb0).iter().copied()),
            FxHashSet::from_iter([bb9, bb10])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb9).iter().copied()),
            FxHashSet::from_iter([bb8])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb10).iter().copied()),
            FxHashSet::from_iter([bb8])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb8).iter().copied()),
            FxHashSet::from_iter([bb1, bb2])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb1).iter().copied()),
            FxHashSet::from_iter([bb3])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb2).iter().copied()),
            FxHashSet::from_iter([bb4])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb3).iter().copied()),
            FxHashSet::from_iter([bb14, bb16])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb4).iter().copied()),
            FxHashSet::from_iter([bb13, bb15])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb13).iter().copied()),
            FxHashSet::from_iter([bb11])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb14).iter().copied()),
            FxHashSet::from_iter([bb11])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb15).iter().copied()),
            FxHashSet::from_iter([bb11])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb16).iter().copied()),
            FxHashSet::from_iter([bb11])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb11).iter().copied()),
            FxHashSet::from_iter([bb12, bb8])
        );
        assert_eq!(
            FxHashSet::from_iter(graph.children(bb12).iter().copied()),
            FxHashSet::from_iter([bb5, bb6])
        );
    }
}
