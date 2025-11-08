use std::ops::Range;

use index_vec::{IndexVec, index_vec};

use crate::cfg_to_rvsdg::control_tree::control_tree::ControlTreeNode;

/// Associates slice-like annotation information with control tree nodes.
///
/// This is used for "dependency", "read-write" and "demand" annotation, where each node is
/// associated with a list of values. This data structure avoids having to allocate a `Vec` for each
/// node individually.
#[derive(Clone, PartialEq, Debug)]
pub struct SliceAnnotation<T> {
    ranges: IndexVec<ControlTreeNode, Option<Range<usize>>>,
    values: Vec<T>,
}

impl<T> SliceAnnotation<T> {
    pub fn new(node_count: usize) -> Self {
        SliceAnnotation {
            ranges: index_vec![None; node_count],
            values: vec![],
        }
    }

    fn ensure_required_len(&mut self, node: ControlTreeNode) {
        let required_len = node.index() + 1;

        if self.ranges.len() < required_len {
            self.ranges.resize(required_len, None);
        }
    }

    pub fn set(&mut self, node: ControlTreeNode, values: impl IntoIterator<Item = T>) {
        let start = self.values.len();

        self.values.extend(values);

        let end = self.values.len();

        self.ensure_required_len(node);
        self.ranges[node] = Some(start..end);
    }

    pub fn get(&self, node: ControlTreeNode) -> &[T] {
        self.ranges
            .get(node)
            .cloned()
            .flatten()
            .map(|r| &self.values[r])
            .unwrap_or(&[])
    }

    pub fn copy(&mut self, from: ControlTreeNode, to: ControlTreeNode)
    where
        T: Copy,
    {
        let from_range = self.ranges[from].clone().expect("no entry for `from` node");
        let to_start = self.values.len();

        for i in from_range {
            self.values.push(self.values[i]);
        }

        let to_end = self.values.len();

        self.ensure_required_len(to);
        self.ranges[to] = Some(to_start..to_end);
    }
}
