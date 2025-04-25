use indexmap::IndexSet;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::cfg::BasicBlock;
use crate::cfg_to_rvsdg::control_flow_restructuring::{Edge, Graph};

struct Vertex {
    bb: BasicBlock,
    visit_index: Option<usize>,
    low_link: usize,
    on_stack: bool,
}

impl Vertex {
    fn update_low_link(&mut self, linked: usize) {
        self.low_link = usize::min(self.low_link, linked);
    }
}

fn init_vertices(graph: &Graph) -> (Vec<Vertex>, FxHashMap<BasicBlock, usize>) {
    let mut vertices = Vec::new();
    let mut bb_vertex_map = FxHashMap::default();

    for (i, bb) in graph.nodes().enumerate() {
        vertices.push(Vertex {
            bb,
            visit_index: None,
            low_link: 0,
            on_stack: false,
        });
        bb_vertex_map.insert(bb, i);
    }

    (vertices, bb_vertex_map)
}

/// Finds the strongly-connected components in the basic-block graph.
///
/// This is based on Tarjan's algorithm, but modified specifically for our use in "loop
/// restructuring":
///
/// - Trivial SCCs that consist of only a single node never need restructuring, so we don't return
///   those here.
/// - Loop restructuring is recursive: restructured loops may still contain nested loops that also
///   need to be restructured. This function takes an "edge blacklist" as a second argument, to
///   which the restructuring may add the re-entry edge for each loop that has already been
///   restructured. This algorithm will ignore these blacklisted edges, which breaks the strong
///   connectedness of the restructured loop, so that any nested SCCs may then be discovered as
///   further candidates for loop restructuring.
pub fn strongly_connected_components(
    graph: &Graph,
    edge_blacklist: &FxHashSet<Edge>,
) -> Vec<IndexSet<BasicBlock>> {
    let (mut vertices, bb_vertex_map) = init_vertices(graph);
    let mut scc = Vec::new();

    let mut visit_index = 0;
    let mut stack = Vec::new();

    for index in 0..vertices.len() {
        if vertices[index].visit_index.is_none() {
            search_vertex(
                index,
                graph,
                edge_blacklist,
                &mut vertices,
                &bb_vertex_map,
                &mut visit_index,
                &mut stack,
                &mut scc,
            );
        }
    }

    scc
}

fn search_vertex(
    index: usize,
    graph: &Graph,
    edge_blacklist: &FxHashSet<Edge>,
    vertices: &mut [Vertex],
    bb_vertex_map: &FxHashMap<BasicBlock, usize>,
    visit_index: &mut usize,
    stack: &mut Vec<usize>,
    scc: &mut Vec<IndexSet<BasicBlock>>,
) {
    let vertex_visit_index = *visit_index;

    *visit_index += 1;

    let vertex = &mut vertices[index];
    let bb = vertex.bb;

    vertex.visit_index = Some(vertex_visit_index);
    vertex.low_link = vertex_visit_index;

    stack.push(index);
    vertex.on_stack = true;

    for child in graph.children(bb) {
        let edge = Edge {
            source: bb,
            dest: *child,
        };

        if edge_blacklist.contains(&edge) {
            continue;
        }

        let child_index = *bb_vertex_map
            .get(child)
            .expect("invalid basic-block handle");

        if vertices[child_index].visit_index.is_none() {
            search_vertex(
                child_index,
                graph,
                edge_blacklist,
                vertices,
                bb_vertex_map,
                visit_index,
                stack,
                scc,
            );

            vertices[index].update_low_link(vertices[child_index].low_link);
        } else if vertices[child_index].on_stack {
            vertices[index].update_low_link(vertices[child_index].low_link);
        }
    }

    if vertex_visit_index == vertices[index].low_link {
        let mut component = IndexSet::new();

        while let Some(vertex_index) = stack.pop() {
            vertices[vertex_index].on_stack = false;

            component.insert(vertices[vertex_index].bb);

            if vertex_index == index {
                break;
            }
        }

        // The algorithm considers any node reachable from itself, and as such every single node
        // is its own SCC. For our purposes, we're only interested in single node SCCs if the node
        // actually branches to itself.
        if component.len() > 1
            || (component.len() == 1 && graph.branches_to_self(component[0], edge_blacklist))
        {
            scc.push(component);
        }
    }
}

/// Summarizes the connectivity of a strongly connected component (SCC) as a whole.
#[derive(Debug)]
pub struct SccStructure {
    /// Edges from source nodes outside the strongly connected component, to a destination node
    /// inside the strongly connected component.
    pub entry_edges: Vec<Edge>,
    /// Nodes inside the strongly connected component that are the destination for one or more
    /// [entry_edges].
    pub entry_nodes: IndexSet<BasicBlock>,
    /// Edges from source nodes inside the strongly connected component, to a destination node
    /// outside the strongly connected component.
    pub exit_edges: Vec<Edge>,
    /// Nodes outside the strongly connected component that are the destination for one or more
    /// [exit_edges].
    pub exit_nodes: IndexSet<BasicBlock>,
    /// Edges from source nodes inside the strongly connected component, for which the destination
    /// is one of the [entry_nodes].
    pub repetition_edges: Vec<Edge>,
}

impl SccStructure {
    pub fn analyse(graph: &Graph, scc: &IndexSet<BasicBlock>) -> Self {
        let mut entry_nodes = IndexSet::new();
        let mut entry_edges = Vec::new();
        let mut exit_nodes = IndexSet::new();
        let mut exit_edges = Vec::new();

        for bb in scc {
            // If the node is the graph's entry node, then it is trivially also an entry node for
            // the SCC
            if *bb == graph.entry() {
                entry_nodes.insert(*bb);
            }

            for parent in graph.parents(*bb) {
                if !scc.contains(parent) {
                    entry_edges.push(Edge {
                        source: *parent,
                        dest: *bb,
                    });
                    entry_nodes.insert(*bb);
                }
            }

            for child in graph.children(*bb) {
                if !scc.contains(child) {
                    exit_edges.push(Edge {
                        source: *bb,
                        dest: *child,
                    });
                    exit_nodes.insert(*child);
                }
            }
        }

        // Now that we know the entry nodes, iterate over the SCC a second time to find repetition
        // edges from nodes in the SCC back to an entry node
        let mut repetition_edges = Vec::new();

        for bb in scc {
            for child in graph.children(*bb) {
                if entry_nodes.contains(child) {
                    repetition_edges.push(Edge {
                        source: *bb,
                        dest: *child,
                    })
                }
            }
        }

        SccStructure {
            entry_nodes,
            entry_edges,
            exit_nodes,
            exit_edges,
            repetition_edges,
        }
    }

    pub fn is_tail_controlled_loop(&self) -> bool {
        self.entry_nodes.len() == 1
            && self.repetition_edges.len() == 1
            && self.exit_edges.len() == 1
            && self.repetition_edges[0].source == self.exit_edges[0].source
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
    fn test_strongly_connected_components() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![],
            ret_ty: None,
        });

        //
        //  bb0<-----bb3---->bb4--->bb5<--bb7
        //   |  ^     ^         \   |    ^
        //   |   \    |          \  |   /
        //   |    \   |           v v  /
        //   v     \  |            bb6
        //  bb1----->bb2
        //

        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();
        let bb3 = body.append_block();
        let bb4 = body.append_block();
        let bb5 = body.append_block();
        let bb6 = body.append_block();
        let bb7 = body.append_block();

        body.basic_blocks[bb0].terminator = Terminator::Branch(Branch::single(bb1));
        body.basic_blocks[bb1].terminator = Terminator::Branch(Branch::single(bb2));
        body.basic_blocks[bb2].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb0, bb3],
        });
        body.basic_blocks[bb3].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb0, bb4],
        });
        body.basic_blocks[bb4].terminator = Terminator::Branch(Branch {
            selector: None,
            branches: smallvec![bb5, bb6],
        });
        body.basic_blocks[bb5].terminator = Terminator::Branch(Branch::single(bb6));
        body.basic_blocks[bb6].terminator = Terminator::Branch(Branch::single(bb7));
        body.basic_blocks[bb7].terminator = Terminator::Branch(Branch::single(bb5));

        let graph = Graph::init(body);

        let scc = strongly_connected_components(&graph, &FxHashSet::default());

        assert_eq!(scc.len(), 2);

        assert_eq!(&scc[0], &IndexSet::from([bb5, bb6, bb7]));
        assert_eq!(&scc[1], &IndexSet::from([bb0, bb1, bb2, bb3]));
    }

    #[test]
    fn test_scc_structure() {
        let mut body = Body::init(&FnSig {
            name: Default::default(),
            ty: TY_DUMMY,
            args: vec![],
            ret_ty: None,
        });

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
        //       exit
        //

        let bb0 = body.append_block();
        let bb1 = body.append_block();
        let bb2 = body.append_block();
        let bb3 = body.append_block();
        let bb4 = body.append_block();
        let bb5 = body.append_block();
        let bb6 = body.append_block();
        let exit = body.append_block();

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
        body.basic_blocks[bb5].terminator = Terminator::Branch(Branch::single(exit));
        body.basic_blocks[bb6].terminator = Terminator::Branch(Branch::single(exit));

        let graph = Graph::init(body);

        let scc = strongly_connected_components(&graph, &FxHashSet::default());

        assert_eq!(scc.len(), 1);

        assert_eq!(&scc[0], &IndexSet::from([bb1, bb2, bb3, bb4]));

        let structure = SccStructure::analyse(&graph, &scc[0]);

        assert_eq!(
            FxHashSet::from_iter(structure.entry_edges),
            FxHashSet::from_iter([
                Edge {
                    source: bb0,
                    dest: bb1,
                },
                Edge {
                    source: bb0,
                    dest: bb2,
                }
            ])
        );
        assert_eq!(structure.entry_nodes, IndexSet::from([bb1, bb2]));
        assert_eq!(
            FxHashSet::from_iter(structure.exit_edges),
            FxHashSet::from_iter([
                Edge {
                    source: bb3,
                    dest: bb5,
                },
                Edge {
                    source: bb4,
                    dest: bb6,
                }
            ])
        );
        assert_eq!(structure.exit_nodes, IndexSet::from([bb5, bb6]));
        assert_eq!(
            FxHashSet::from_iter(structure.repetition_edges),
            FxHashSet::from_iter([
                Edge {
                    source: bb3,
                    dest: bb2,
                },
                Edge {
                    source: bb4,
                    dest: bb1,
                }
            ])
        );
    }
}
