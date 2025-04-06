// Modified from `rustc_middle` for use with stable-MIR

use bit_set::BitSet;
use bit_vec::BitVec;
use stable_mir::mir::{BasicBlock, BasicBlockIdx, Body};

/// Postorder traversal of a graph.
///
/// Postorder traversal is when each node is visited after all of its successors, except when the
/// successor is only reachable by a back-edge. If you are familiar with some basic graph theory,
/// then this performs a depth first search and returns nodes in order of completion time.
///
///
/// ```text
///
///         A
///        / \
///       /   \
///      B     C
///       \   /
///        \ /
///         D
/// ```
///
/// A Postorder traversal of this graph is `D B C A` or `D C B A`
pub struct Postorder<'a> {
    basic_blocks: &'a [BasicBlock],
    visited: BitSet<BasicBlockIdx>,
    visit_stack: Vec<(BasicBlockIdx, Vec<BasicBlockIdx>)>,
    root_is_start_block: bool,
}

impl<'a> Postorder<'a> {
    pub fn new(basic_blocks: &'a [BasicBlock], root: BasicBlockIdx) -> Self {
        let mut po = Postorder {
            basic_blocks,
            visited: BitSet::from_bit_vec(BitVec::from_elem(basic_blocks.len(), false)),
            visit_stack: Vec::new(),
            root_is_start_block: root == 0,
        };

        po.visit(root);
        po.traverse_successor();

        po
    }

    fn visit(&mut self, bb: BasicBlockIdx) {
        if !self.visited.insert(bb) {
            return;
        }

        let data = &self.basic_blocks[bb];
        let successors = data.terminator.successors();

        self.visit_stack.push((bb, successors));
    }

    fn traverse_successor(&mut self) {
        // This is quite a complex loop due to 1. the borrow checker not liking it much
        // and 2. what exactly is going on is not clear
        //
        // It does the actual traversal of the graph, while the `next` method on the iterator
        // just pops off of the stack. `visit_stack` is a stack containing pairs of nodes and
        // iterators over the successors of those nodes. Each iteration attempts to get the next
        // node from the top of the stack, then pushes that node and an iterator over the
        // successors to the top of the stack. This loop only grows `visit_stack`, stopping when
        // we reach a child that has no children that we haven't already visited.
        //
        // For a graph that looks like this:
        //
        //         A
        //        / \
        //       /   \
        //      B     C
        //      |     |
        //      |     |
        //      |     D
        //       \   /
        //        \ /
        //         E
        //
        // The state of the stack starts out with just the root node (`A` in this case);
        //     [(A, [B, C])]
        //
        // When the first call to `traverse_successor` happens, the following happens:
        //
        //     [(C, [D]),  // `C` taken from the successors of `A`, pushed to the
        //                 // top of the stack along with the successors of `C`
        //      (A, [B])]
        //
        //     [(D, [E]),  // `D` taken from successors of `C`, pushed to stack
        //      (C, []),
        //      (A, [B])]
        //
        //     [(E, []),   // `E` taken from successors of `D`, pushed to stack
        //      (D, []),
        //      (C, []),
        //      (A, [B])]
        //
        // Now that the top of the stack has no successors we can traverse, each item will
        // be popped off during iteration until we get back to `A`. This yields [E, D, C].
        //
        // When we yield `C` and call `traverse_successor`, we push `B` to the stack, but
        // since we've already visited `E`, that child isn't added to the stack. The last
        // two iterations yield `B` and finally `A` for a final traversal of [E, D, C, B, A]
        while let Some(bb) = self
            .visit_stack
            .last_mut()
            .and_then(|(_, iter)| iter.next_back())
        {
            self.visit(bb);
        }
    }
}

impl Iterator for Postorder<'_> {
    type Item = BasicBlockIdx;

    fn next(&mut self) -> Option<Self::Item> {
        let (bb, _) = self.visit_stack.pop()?;

        self.traverse_successor();

        Some(bb)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // All the blocks, minus the number of blocks we've visited.
        let upper = self.basic_blocks.len() - self.visited.len();

        let lower = if self.root_is_start_block {
            // We will visit all remaining blocks exactly once.
            upper
        } else {
            self.visit_stack.len()
        };

        (lower, Some(upper))
    }
}

pub fn reachable_reverse_postorder<'a, 'tcx>(body: &'a Body) -> Vec<BasicBlockIdx> {
    let mut items: Vec<BasicBlockIdx> = Postorder::new(&body.blocks, 0).collect();

    items.reverse();

    items
}
