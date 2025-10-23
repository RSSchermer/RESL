use std::collections::VecDeque;
use std::mem;

use rustc_hash::FxHashSet;

use crate::rvsdg::{Connectivity, Node, Region, Rvsdg, StateOrigin, ValueOrigin};

/// Groups the nodes of a region into "strata".
///
/// A node will be grouped into stratum `0` if the node either has no inputs (value and state), or
/// all inputs connect to the region's arguments. A node will be grouped into stratum `1` if it did
/// not qualify for stratum `0`, and all of its inputs connect to either the region arguments or
/// outputs of stratum `0` nodes. A node will be grouped into stratum `2` if it did
/// not qualify for stratum `0` or stratum `1`, and all of its inputs connect to either the region
/// arguments or outputs of stratum `0` or `1` nodes. Etc.
pub struct RegionStratifier {
    stratified: FxHashSet<Node>,
    current_queue: VecDeque<Node>,
    next_queue: VecDeque<Node>,
    current_stratum: usize,
    current_stratum_nodes: VecDeque<Node>,
}

impl RegionStratifier {
    pub fn new() -> Self {
        Self {
            stratified: FxHashSet::default(),
            current_queue: Default::default(),
            next_queue: Default::default(),
            current_stratum: 0,
            current_stratum_nodes: Default::default(),
        }
    }

    pub fn stratify<F>(&mut self, rvsdg: &Rvsdg, region: Region, mut callback: F)
    where
        F: FnMut(Node, usize),
    {
        self.stratified.clear();
        self.current_queue.clear();
        self.next_queue.clear();
        self.current_stratum = 0;

        self.current_queue
            .extend(rvsdg[region].nodes().iter().copied());

        while !self.current_queue.is_empty() {
            while let Some(node) = self.current_queue.pop_front() {
                let node_data = &rvsdg[node];

                let value_inputs_ready = node_data.value_inputs().iter().all(|i| match i.origin {
                    ValueOrigin::Argument(_) => true,
                    ValueOrigin::Output { producer, .. } => self.stratified.contains(&producer),
                });

                let state_input_ready = if let Some(state) = node_data.state() {
                    match state.origin {
                        StateOrigin::Argument => true,
                        StateOrigin::Node(producer) => self.stratified.contains(&producer),
                    }
                } else {
                    true
                };

                if value_inputs_ready && state_input_ready {
                    self.current_stratum_nodes.push_back(node);
                } else {
                    self.next_queue.push_back(node);
                }
            }

            while let Some(node) = self.current_stratum_nodes.pop_front() {
                callback(node, self.current_stratum);
                self.stratified.insert(node);
            }

            self.current_stratum += 1;
            mem::swap(&mut self.current_queue, &mut self.next_queue);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use super::*;
    use crate::rvsdg::ValueInput;
    use crate::ty::{TY_DUMMY, TY_PTR_U32, TY_U32};
    use crate::{BinaryOperator, FnArg, FnSig, Function, Module, Symbol};

    #[test]
    fn simple_region_stratification() {
        let mut module = Module::new(Symbol::from_ref(""));
        let function = Function {
            name: Symbol::from_ref(""),
            module: Symbol::from_ref(""),
        };

        module.fn_sigs.register(
            function,
            FnSig {
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
            },
        );

        let mut rvsdg = Rvsdg::new(module.ty.clone());

        let (_, region) = rvsdg.register_function(&module, function, iter::empty());

        let alloca_node = rvsdg.add_op_alloca(region, TY_U32);
        let const_0_node = rvsdg.add_const_u32(region, 1);
        let const_1_node = rvsdg.add_const_u32(region, 2);
        let add_0_node = rvsdg.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::argument(TY_U32, 0),
            ValueInput::argument(TY_U32, 1),
        );
        let add_1_node = rvsdg.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::output(TY_U32, const_0_node, 0),
            ValueInput::output(TY_U32, const_1_node, 0),
        );
        let add_2_node = rvsdg.add_op_binary(
            region,
            BinaryOperator::Add,
            ValueInput::output(TY_U32, add_0_node, 0),
            ValueInput::output(TY_U32, add_1_node, 0),
        );
        let store_node = rvsdg.add_op_store(
            region,
            ValueInput::output(TY_PTR_U32, alloca_node, 0),
            ValueInput::output(TY_U32, add_2_node, 0),
            StateOrigin::Argument,
        );
        let load_node = rvsdg.add_op_load(
            region,
            ValueInput::output(TY_PTR_U32, alloca_node, 0),
            StateOrigin::Node(store_node),
        );

        rvsdg.reconnect_region_result(
            region,
            0,
            ValueOrigin::Output {
                producer: load_node,
                output: 0,
            },
        );

        let mut stratifier = RegionStratifier::new();
        let mut stratified_nodes = Vec::new();

        stratifier.stratify(&rvsdg, region, |node, stratum| {
            stratified_nodes.push((node, stratum))
        });

        assert_eq!(
            stratified_nodes,
            vec![
                (alloca_node, 0),
                (const_0_node, 0),
                (const_1_node, 0),
                (add_0_node, 0),
                (add_1_node, 1),
                (add_2_node, 2),
                (store_node, 3),
                (load_node, 4),
            ]
        );
    }
}
