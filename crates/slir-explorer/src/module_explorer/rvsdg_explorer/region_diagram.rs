use std::mem;
use std::ops::Range;

use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashSet;
use slir::rvsdg::{
    Connectivity, Node, NodeKind, Region, Rvsdg, SimpleNode, StateOrigin, StateUser, ValueOrigin,
    ValueUser,
};
use slir::ty::Type;
use slir::Function;
use smallvec::SmallVec;

pub struct Config {
    pub connector_size: f32,
    pub connector_spacing: f32,
    pub font_width: f32,
    pub font_height: f32,
    pub region_spacing: f32,
    pub region_padding: f32,
    pub node_padding: f32,
    pub node_spacing: f32,
    pub traverser_zone_padding: f32,
    pub traverser_line_spacing: f32,
    pub bypass_zone_padding: f32,
    pub bypass_line_spacing: f32,
}

impl Config {
    fn connector_stride(&self) -> f32 {
        self.connector_size + self.connector_spacing
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {
            connector_size: 6.0,
            connector_spacing: 4.0,
            font_width: 9.0,
            font_height: 15.0,
            region_spacing: 4.0,
            region_padding: 6.0,
            node_padding: 6.0,
            node_spacing: 20.0,
            traverser_zone_padding: 20.0,
            traverser_line_spacing: 4.0,
            bypass_zone_padding: 20.0,
            bypass_line_spacing: 4.0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Default, Debug)]
pub struct Rect {
    pub origin: [f32; 2],
    pub size: [f32; 2],
}

#[derive(Clone, Copy, PartialEq, Default, Debug)]
pub struct Line {
    pub start: [f32; 2],
    pub end: [f32; 2],
}

#[derive(Clone, Copy, PartialEq, Default, Debug)]
pub struct ConnectorElement {
    pub rect: Rect,
    pub ty: Option<Type>,
}

/// Groups a regions nodes into strata, such that for every output, the consumer node is always in
/// a lower stratum (or the output is consumed as a region result).
///
/// This means that when rendering the edges that connect outputs to their consuming inputs,
/// edges will always strictly flow down.
fn stratify_nodes(rvsdg: &Rvsdg, region: Region) -> Vec<IndexSet<Node>> {
    // Note: because a node output can have multiple users, but a node input can only have one
    // origin, it's easier to stratify bottom-up. Once constructed, I think it is more intuitive
    // to work with the strata in top-to-bottom order, so we reverse the order at the end of
    // stratification.

    let mut unassigned_nodes = FxHashSet::from_iter(rvsdg[region].nodes());

    let mut strata = vec![IndexSet::new()];
    let mut current_stratum = 0;
    let mut assigned_current = FxHashSet::default();
    let mut assigned_lower = FxHashSet::default();

    // If a node does not have any outputs, then add to the bottom stratum
    for node in rvsdg[region].nodes() {
        let data = &rvsdg[node];

        if data.value_outputs().is_empty() && data.state().is_none() {
            strata[current_stratum].insert(node);
            unassigned_nodes.remove(&node);
            assigned_current.insert(node);
        }
    }

    while !unassigned_nodes.is_empty() {
        // Attempt to find a candidate node for which all outputs are consumed in a lower stratum,
        // or by the region's results
        let candidate = unassigned_nodes
            .iter()
            .find(|node| {
                let data = &rvsdg[**node];

                // Check the "value" outputs. Note that `all` on an empty iterator returns `true` (which
                // is what we want).
                let values_output_to_lower = data.value_outputs().iter().all(|output| {
                    output.users.iter().all(|user| match user {
                        ValueUser::Result(_) => true,
                        ValueUser::Input { consumer, .. } => assigned_lower.contains(consumer),
                    })
                });

                // Check the "state" output.
                let state_outputs_to_lower = data
                    .state()
                    .map(|state| match &state.user {
                        StateUser::Result => true,
                        StateUser::Node(n) => assigned_lower.contains(n),
                    })
                    .unwrap_or(true);

                values_output_to_lower && state_outputs_to_lower
            })
            .copied();

        // If we found a candidate node, add it to the current stratum. If we cannot find a
        // candidate (and we still have unassigned nodes, as per the outer loop condition), then
        // we add a new stratum.
        if let Some(node) = candidate {
            strata[current_stratum].insert(node);
            unassigned_nodes.remove(&node);
            assigned_current.insert(node);
        } else {
            // Update the set that tracks nodes that have been assigned to "lower" strata, by adding
            // all nodes that were assigned to the current stratum.
            assigned_lower.extend(assigned_current.iter().copied());

            // Add the new stratum
            current_stratum = strata.len();
            strata.push(IndexSet::new());

            assigned_current.clear();
        }
    }

    strata.reverse();

    strata
}

struct RegionLayoutBuilder<'a> {
    config: &'a Config,
    rvsdg: &'a Rvsdg,
    region: Region,
    strata: Vec<Stratum>,
    traverser_zones: Vec<TraverserZone>,
    edges: Vec<Edge>,
    incoming_bypass_edges: Vec<usize>,
    outgoing_bypass_edges: Vec<usize>,
    current_stratum: usize,
    width: f32,
    height: f32,
    argument_connectors: Vec<ConnectorElement>,
    result_connectors: Vec<ConnectorElement>,
    edge_lines: Vec<Line>,
    edge_line_ranges: Vec<Range<usize>>,
}

impl<'a> RegionLayoutBuilder<'a> {
    fn init(config: &'a Config, rvsdg: &'a Rvsdg, region: Region) -> Self {
        let strata = stratify_nodes(rvsdg, region)
            .into_iter()
            .map(|nodes| Stratum::init(config.clone(), rvsdg, nodes))
            .collect::<Vec<_>>();
        let traverser_zones = vec![TraverserZone::default(); strata.len() + 1];

        let data = &rvsdg[region];

        let mut argument_connectors = data
            .value_arguments()
            .iter()
            .map(|arg| ConnectorElement {
                rect: Rect::default(),
                ty: arg.ty,
            })
            .collect::<Vec<_>>();

        let mut result_connectors = data
            .value_results()
            .iter()
            .map(|arg| ConnectorElement {
                rect: Rect::default(),
                ty: Some(arg.ty),
            })
            .collect::<Vec<_>>();

        // There's always a state and result argument, so add connectors for those
        argument_connectors.push(ConnectorElement::default());
        result_connectors.push(ConnectorElement::default());

        RegionLayoutBuilder {
            config,
            rvsdg,
            region,
            strata,
            traverser_zones,
            edges: vec![],
            incoming_bypass_edges: vec![],
            outgoing_bypass_edges: vec![],
            current_stratum: 0,
            width: 0.0,
            height: 0.0,
            argument_connectors,
            result_connectors,
            edge_lines: vec![],
            edge_line_ranges: vec![],
        }
    }

    fn build_argument_edges(&mut self) {
        let needs_bypass = |end, strata: &[Stratum]| match end {
            EdgeEnd::Result(_) => !strata.is_empty(),
            EdgeEnd::Consumer { stratum, .. } => stratum != 0,
        };

        let data = &self.rvsdg[self.region];

        // Initialize edges for the region's value arguments
        for (i, arg) in data.value_arguments().iter().enumerate() {
            let start = EdgeStart::Argument(i as u32);

            for user in &arg.users {
                let end = self.value_user_to_edge_end(user);
                let traverser_lane = self.traverser_zones[0].add_traverser_lane();
                let edge_index = self.edges.len();

                self.edges.push(Edge {
                    is_state_edge: false,
                    start,
                    end,
                    traverser_lane,
                    bypass_segments: Default::default(),
                });

                if needs_bypass(end, &self.strata) {
                    self.outgoing_bypass_edges.push(edge_index)
                }
            }
        }

        // Initialize an edge for the region's state argument
        let start = self.state_origin_to_edge_start(&StateOrigin::Argument);
        let end = self.state_user_to_edge_end(data.state_argument());
        let traverser_lane = self.traverser_zones[0].add_traverser_lane();
        let edge_index = self.edges.len();

        self.edges.push(Edge {
            is_state_edge: true,
            start,
            end,
            traverser_lane,
            bypass_segments: Default::default(),
        });

        if needs_bypass(end, &self.strata) {
            self.outgoing_bypass_edges.push(edge_index);
        }

        self.set_next_stratum_bypass_lanes();

        mem::swap(
            &mut self.incoming_bypass_edges,
            &mut self.outgoing_bypass_edges,
        );
    }

    fn build_current_stratum_edges(&mut self) -> bool {
        if self.current_stratum >= self.strata.len() {
            return false;
        }

        let needs_bypass = |end, strata: &[Stratum], current_stratum| match end {
            EdgeEnd::Result(_) => current_stratum != strata.len() - 1,
            EdgeEnd::Consumer { stratum, .. } => stratum != current_stratum as u32,
        };

        let zone_index = self.current_stratum - 1;

        // Add edges for each node in the current stratum
        for (node, layout) in self.strata[self.current_stratum].nodes.iter() {
            let node_data = &self.rvsdg[*node];

            // Add edges for the value outputs
            for (i, output) in node_data.value_outputs().iter().enumerate() {
                let start = self.value_origin_to_edge_start(&ValueOrigin::Output {
                    producer: *node,
                    output: i as u32,
                });

                for user in &output.users {
                    let end = self.value_user_to_edge_end(user);
                    let traverser_lane = self.traverser_zones[zone_index].add_traverser_lane();
                    let edge_index = self.edges.len();

                    self.edges.push(Edge {
                        is_state_edge: false,
                        start,
                        end,
                        traverser_lane,
                        bypass_segments: Default::default(),
                    });

                    if needs_bypass(end, &self.strata, self.current_stratum) {
                        self.outgoing_bypass_edges.push(edge_index)
                    }
                }
            }

            // Add edge for the state output (if applicable)
            if let Some(state) = node_data.state() {
                let start = self.state_origin_to_edge_start(&state.origin);
                let end = self.state_user_to_edge_end(&state.user);
                let traverser_lane = self.traverser_zones[zone_index].add_traverser_lane();
                let edge_index = self.edges.len();

                self.edges.push(Edge {
                    is_state_edge: true,
                    start,
                    end,
                    traverser_lane,
                    bypass_segments: Default::default(),
                });

                if needs_bypass(end, &self.strata, self.current_stratum) {
                    self.outgoing_bypass_edges.push(edge_index);
                }
            }
        }

        // Process the incoming bypass edges
        for (bypass_lane, edge_index) in self.incoming_bypass_edges.iter().copied().enumerate() {
            let traverser_lane = self.traverser_zones[zone_index].add_traverser_lane();

            self.edges[edge_index].bypass_segments.push(BypassSegment {
                bypass_lane: bypass_lane as u32,
                traverser_lane,
            });

            if needs_bypass(
                self.edges[edge_index].end,
                &self.strata,
                self.current_stratum,
            ) {
                self.outgoing_bypass_edges.push(edge_index);
            }
        }

        self.set_next_stratum_bypass_lanes();

        mem::swap(
            &mut self.incoming_bypass_edges,
            &mut self.outgoing_bypass_edges,
        );
        self.outgoing_bypass_edges.clear();
        self.current_stratum += 1;

        true
    }

    fn set_next_stratum_bypass_lanes(&mut self) {
        if let Some(stratum) = self.strata.get_mut(self.current_stratum + 1) {
            stratum.bypass_lanes = self.outgoing_bypass_edges.len() as u32;
        }
    }

    fn apply_vertical_offsets(&mut self) {
        let mut offset_y = self.config.connector_size;

        for i in 0..self.strata.len() {
            self.traverser_zones[i].offset_y = offset_y;
            offset_y += self.traverser_zones[i].height(&self.config);
            self.strata[i].offset_y = offset_y;
            offset_y += self.strata[i].height();

            self.strata[i].apply_offset_y();
        }

        self.traverser_zones[self.strata.len()].offset_y = offset_y;
    }

    fn update_width(&mut self) {
        let args_width = connectors_width(self.argument_connectors.len() as u32, &self.config);
        let results_width = connectors_width(self.result_connectors.len() as u32, &self.config);
        let max_stratum_width = self
            .strata
            .iter()
            .map(|s| s.width(self.config))
            .fold(0.0, f32::max);
        let inner_width = [args_width, results_width, max_stratum_width]
            .into_iter()
            .fold(0.0, f32::max);

        self.width = inner_width + self.config.region_padding * 2.0;
    }

    fn update_height(&mut self) {
        let args_height = self.config.connector_size;
        let results_height = self.config.connector_size;

        let mut height = args_height + results_height;

        for stratum in &self.strata {
            height += stratum.height();
        }

        for traverser_zone in &self.traverser_zones {
            height += traverser_zone.height(&self.config);
        }

        self.height = height;
    }

    fn place_argument_connectors(&mut self) {
        let mut x = self.config.region_padding;
        let y = 0.0;

        for connector in &mut self.argument_connectors {
            connector.rect = Rect {
                origin: [x, y],
                size: [self.config.connector_size; 2],
            };

            x += self.config.connector_stride();
        }
    }

    fn place_result_connectors(&mut self) {
        let mut x = self.config.region_padding;
        let y = self.height - self.config.connector_size;

        for connector in &mut self.result_connectors {
            connector.rect = Rect {
                origin: [x, y],
                size: [self.config.connector_size; 2],
            };

            x += self.config.connector_stride();
        }
    }

    fn place_nodes(&mut self) {
        for stratum in &mut self.strata {
            for node_layout in stratum.nodes.values_mut() {
                node_layout.place(self.config);
            }
        }
    }

    fn generate_edges_lines(&mut self) {
        for i in 0..self.edges.len() {
            self.generate_edge_lines(i);
        }
    }

    fn generate_edge_lines(&mut self, edge_index: usize) {
        let edge = &self.edges[edge_index];
        let range_start = self.edge_lines.len();

        let base_traverser_zone = match edge.start {
            EdgeStart::Argument(_) => 0,
            EdgeStart::Producer { stratum, .. } => stratum as usize + 1,
        };
        let base_traverser_y = self.traverser_zones[base_traverser_zone]
            .traverser_lane_y(edge.traverser_lane, &self.config);

        let [mut x, mut y] = self.edge_start_coords(edge_index);

        self.edge_lines.push(Line {
            start: [x, y],
            end: [x, base_traverser_y],
        });

        y = base_traverser_y;

        let mut stratum = base_traverser_zone;

        for bypass_segment in &edge.bypass_segments {
            let bypass_lane_x =
                self.strata[stratum].bypass_lane_x(bypass_segment.bypass_lane, self.config);

            // Add traverser line
            self.edge_lines.push(Line {
                start: [x, y],
                end: [bypass_lane_x, y],
            });

            let traverser_lane_y = self.traverser_zones[stratum + 1]
                .traverser_lane_y(bypass_segment.traverser_lane, &self.config);

            // Add bypass line
            self.edge_lines.push(Line {
                start: [bypass_lane_x, y],
                end: [bypass_lane_x, traverser_lane_y],
            });

            x = bypass_lane_x;
            y = traverser_lane_y;

            stratum += 1;
        }

        let end_coords = self.edge_end_coords(edge_index);

        // Add the final traverser line
        self.edge_lines.push(Line {
            start: [x, y],
            end: [end_coords[0], y],
        });

        // Add the line that connects the end connector
        self.edge_lines.push(Line {
            start: [end_coords[0], y],
            end: end_coords,
        });

        let range_end = self.edge_lines.len();

        self.edge_line_ranges.push(range_start..range_end);
    }

    fn value_origin_to_edge_start(&self, origin: &ValueOrigin) -> EdgeStart {
        match origin {
            ValueOrigin::Argument(arg) => EdgeStart::Argument(*arg as u32),
            ValueOrigin::Output { producer, output } => {
                // Find which stratum contains the producer
                for (stratum_index, stratum) in self.strata.iter().enumerate() {
                    if let Some(node_index) = stratum.nodes.get_index_of(producer) {
                        return EdgeStart::Producer {
                            stratum: stratum_index as u32,
                            node: node_index as u32,
                            output: *output,
                        };
                    }
                }

                panic!("producer node not found in any stratum");
            }
        }
    }

    fn value_user_to_edge_end(&self, user: &ValueUser) -> EdgeEnd {
        match user {
            ValueUser::Result(result) => EdgeEnd::Result(*result as u32),
            ValueUser::Input { consumer, input } => {
                // Find which stratum contains the consumer
                for (stratum_index, stratum) in self.strata.iter().enumerate() {
                    if let Some(node_index) = stratum.nodes.get_index_of(consumer) {
                        return EdgeEnd::Consumer {
                            stratum: stratum_index as u32,
                            node: node_index as u32,
                            output: *input,
                        };
                    }
                }

                panic!("consumer node not found in any stratum");
            }
        }
    }

    fn state_origin_to_edge_start(&self, origin: &StateOrigin) -> EdgeStart {
        match origin {
            StateOrigin::Argument => {
                // For state inputs/outputs, we use the last connector
                EdgeStart::Argument(self.argument_connectors.len() as u32 - 1)
            }
            StateOrigin::Node(producer) => {
                // Find which stratum contains the producer
                for (stratum_index, stratum) in self.strata.iter().enumerate() {
                    if let Some(node_index) = stratum.nodes.get_index_of(producer) {
                        // For state inputs/outputs, we use the last connector
                        let layout = &stratum.nodes[producer];
                        return EdgeStart::Producer {
                            stratum: stratum_index as u32,
                            node: node_index as u32,
                            output: layout.output_count() - 1,
                        };
                    }
                }

                panic!("producer node not found in any stratum");
            }
        }
    }

    fn state_user_to_edge_end(&self, user: &StateUser) -> EdgeEnd {
        match user {
            StateUser::Result => {
                // For state inputs/outputs, we use the last connector
                EdgeEnd::Result(self.result_connectors.len() as u32 - 1)
            }
            StateUser::Node(consumer) => {
                // Find which stratum contains the consumer
                for (stratum_index, stratum) in self.strata.iter().enumerate() {
                    if let Some(node_index) = stratum.nodes.get_index_of(consumer) {
                        // For state inputs/outputs, we use the last connector
                        let layout = &stratum.nodes[consumer];
                        return EdgeEnd::Consumer {
                            stratum: stratum_index as u32,
                            node: node_index as u32,
                            output: layout.input_count() - 1,
                        };
                    }
                }

                panic!("consumer node not found in any stratum");
            }
        }
    }

    fn edge_start_coords(&self, edge_index: usize) -> [f32; 2] {
        let edge = &self.edges[edge_index];

        let connector = match edge.start {
            EdgeStart::Argument(arg) => {
                self.argument_connectors[arg as usize]
            }
            EdgeStart::Producer {
                stratum,
                node,
                output,
            } => {
                self.strata[stratum as usize].nodes[node as usize].output_connectors[output as usize]
            }
        };
        
        let Rect {
            origin: [x, y],
            size: [width, height]
        } = connector.rect;
        
        let x = x + 0.5 * width;
        let y = y + height;
        
        [x, y]
    }

    fn edge_end_coords(&self, edge_index: usize) -> [f32; 2] {
        let edge = &self.edges[edge_index];

        let connector = match edge.end {
            EdgeEnd::Result(result) => {
                self.result_connectors[result as usize]
            }
            EdgeEnd::Consumer {
                stratum,
                node,
                output,
            } => {
                self.strata[stratum as usize].nodes[node as usize].input_connectors[output as usize]
            }
        };

        let Rect {
            origin: [x, y],
            size: [width, _]
        } = connector.rect;

        let x = x + 0.5 * width;

        [x, y]
    }

    fn into_layout(self) -> RegionLayout {
        let RegionLayoutBuilder {
            argument_connectors,
            result_connectors,
            strata,
            edge_lines,
            edge_line_ranges,
            width,
            height,
            ..
        } = self;

        let node_layouts = strata
            .into_iter()
            .map(|s| s.nodes.into_values())
            .flatten()
            .collect();

        RegionLayout {
            argument_connectors,
            result_connectors,
            node_layouts,
            edge_lines,
            edge_line_ranges,
            width,
            height,
            translation: [0.0; 2],
        }
    }
}

pub struct RegionLayout {
    argument_connectors: Vec<ConnectorElement>,
    result_connectors: Vec<ConnectorElement>,
    node_layouts: Vec<NodeLayout>,
    edge_lines: Vec<Line>,
    edge_line_ranges: Vec<Range<usize>>,
    width: f32,
    height: f32,
    translation: [f32; 2],
}

impl RegionLayout {
    pub fn generate(config: &Config, rvsdg: &Rvsdg, region: Region) -> Self {
        let mut builder = RegionLayoutBuilder::init(config, rvsdg, region);

        builder.build_argument_edges();
        while builder.build_current_stratum_edges() {}
        builder.apply_vertical_offsets();
        builder.update_width();
        builder.update_height();
        builder.place_argument_connectors();
        builder.place_result_connectors();
        builder.place_nodes();
        builder.generate_edges_lines();

        builder.into_layout()
    }

    fn width(&self) -> f32 {
        self.width
    }

    fn height(&self) -> f32 {
        self.height
    }
    
    pub fn translation(&self) -> [f32; 2] {
        self.translation
    }

    pub fn rect(&self) -> Rect {
        Rect {
            origin: [0.0; 2],
            size: [self.width, self.height],
        }
    }

    pub fn argument_connectors(&self) -> &[ConnectorElement] {
        &self.argument_connectors
    }

    pub fn result_connectors(&self) -> &[ConnectorElement] {
        &self.result_connectors
    }

    pub fn node_layouts(&self) -> &[NodeLayout] {
        &self.node_layouts
    }

    pub fn edge_count(&self) -> usize {
        self.edge_line_ranges.len()
    }

    pub fn edge_lines(&self, edge_index: usize) -> &[Line] {
        let range = self.edge_line_ranges[edge_index].clone();

        &self.edge_lines[range]
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum EdgeStart {
    /// The edge starts at a region argument.
    Argument(u32),
    /// The edge starts at a node in a stratum.
    Producer {
        /// The Stratum the node is in.
        stratum: u32,
        /// The node index with the [stratum].
        node: u32,
        /// The number of the output connector (including a state output as the last output if
        /// applicable).
        output: u32,
    },
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum EdgeEnd {
    /// The edge ends at a region result.
    Result(u32),
    /// The edge ends at a node in a stratum.
    Consumer {
        /// The Stratum the node is in.
        stratum: u32,
        /// The node index with the [stratum].
        node: u32,
        /// The number of the input connector (including a state input as the last input if
        /// applicable).
        output: u32,
    },
}

struct BypassSegment {
    bypass_lane: u32,
    traverser_lane: u32,
}

struct Edge {
    is_state_edge: bool,
    start: EdgeStart,
    end: EdgeEnd,
    traverser_lane: u32,
    bypass_segments: SmallVec<[BypassSegment; 1]>,
}

#[derive(Clone, Default)]
struct TraverserZone {
    offset_y: f32,
    traverser_lanes: u32,
}

impl TraverserZone {
    fn height(&self, config: &Config) -> f32 {
        let lanes_height = if self.traverser_lanes > 1 {
            (self.traverser_lanes - 1) as f32 * config.traverser_line_spacing
        } else {
            0.0
        };

        lanes_height + config.traverser_zone_padding * 2.0
    }

    fn add_traverser_lane(&mut self) -> u32 {
        let lane = self.traverser_lanes;

        self.traverser_lanes += 1;

        lane
    }

    fn traverser_lane_y(&self, lane: u32, config: &Config) -> f32 {
        // We layout the traverse lanes from the bottom up
        self.offset_y + self.height(config)
            - config.traverser_zone_padding
            - (lane as f32 * config.traverser_line_spacing)
    }
}

struct Stratum {
    nodes: IndexMap<Node, NodeLayout>,
    bypass_lanes: u32,
    height: f32,
    nodes_width: f32,
    offset_y: f32,
}

impl Stratum {
    fn init(config: &Config, rvsdg: &Rvsdg, nodes: impl IntoIterator<Item = Node>) -> Self {
        let mut nodes_map = IndexMap::new();
        let mut nodes_width = 0.0;
        let mut height = 0.0;

        for (i, node) in nodes.into_iter().enumerate() {
            let mut layout = NodeLayout::init(config, rvsdg, node);

            if i > 0 {
                nodes_width += config.node_spacing;
            }

            layout.translation[0] = nodes_width;

            nodes_width += layout.width();
            height = f32::max(height, layout.height());

            nodes_map.insert(node, layout);
        }

        // Now that we know the max height, adjust the `y` translation for all nodes to vertically
        // center them in the stratum
        for node_layout in nodes_map.values_mut() {
            let height_dif = height - node_layout.height();
            let translation_y = 0.5 * height_dif;

            node_layout.translation[1] = translation_y;
        }

        Stratum {
            nodes: nodes_map,
            bypass_lanes: 0,
            height,
            nodes_width,
            offset_y: 0.0,
        }
    }

    fn height(&self) -> f32 {
        self.height
    }

    fn width(&self, config: &Config) -> f32 {
        let mut bypass_lanes_width = if self.bypass_lanes > 1 {
            let lanes_width = (self.bypass_lanes - 1) as f32 * config.bypass_line_spacing;

            lanes_width
        } else {
            0.0
        };

        if self.bypass_lanes > 0 {
            bypass_lanes_width += 2.0 * config.bypass_zone_padding;
        }

        self.nodes_width + bypass_lanes_width
    }

    fn bypass_lane_x(&self, lane: u32, config: &Config) -> f32 {
        if lane >= self.bypass_lanes {
            panic!("lane out of bounds");
        }

        self.nodes_width + config.bypass_zone_padding + lane as f32 * config.bypass_line_spacing
    }

    fn apply_offset_y(&mut self) {
        for node_layout in self.nodes.values_mut() {
            node_layout.translation[1] += self.offset_y;
        }
    }
}

pub struct NodeLayout {
    content: NodeContent,
    input_connectors: Vec<ConnectorElement>,
    output_connectors: Vec<ConnectorElement>,
    width: f32,
    height: f32,
    translation: [f32; 2],
}

impl NodeLayout {
    pub fn init(config: &Config, rvsdg: &Rvsdg, node: Node) -> Self {
        let data = &rvsdg[node];

        let content = match data.kind() {
            NodeKind::Switch(node) => {
                let region_layouts = node
                    .branches()
                    .iter()
                    .map(|b| RegionLayout::generate(config, rvsdg, *b))
                    .collect();

                NodeContent::Switch(region_layouts)
            }
            NodeKind::Loop(node) => {
                let region_layout = RegionLayout::generate(config, rvsdg, *node.region());

                NodeContent::Loop(region_layout)
            }
            NodeKind::Simple(simple_node) => match simple_node {
                SimpleNode::ConstU32(v) => NodeContent::PlainText(v.value().to_string()),
                SimpleNode::ConstI32(v) => NodeContent::PlainText(v.value().to_string()),
                SimpleNode::ConstF32(v) => NodeContent::PlainText(v.value().to_string()),
                SimpleNode::ConstBool(v) => NodeContent::PlainText(v.value().to_string()),
                SimpleNode::ConstPtr(_) => NodeContent::PlainText("ptr".to_string()),
                SimpleNode::OpAlloca(_) => NodeContent::PlainText("alloca".to_string()),
                SimpleNode::OpLoad(_) => NodeContent::PlainText("load".to_string()),
                SimpleNode::OpStore(_) => NodeContent::PlainText("store".to_string()),
                SimpleNode::OpPtrElementPtr(_) => NodeContent::PlainText("pep".to_string()),
                SimpleNode::OpApply(_) => NodeContent::PlainText("apply".to_string()),
                SimpleNode::OpUnary(op) => NodeContent::PlainText(op.operator().to_string()),
                SimpleNode::OpBinary(op) => NodeContent::PlainText(op.operator().to_string()),
            },
            _ => panic!("node kind not allowed inside a region"),
        };

        let mut input_connectors = data
            .value_inputs()
            .iter()
            .map(|i| ConnectorElement {
                rect: Default::default(),
                ty: Some(i.ty),
            })
            .collect::<Vec<_>>();

        let mut output_connectors = data
            .value_outputs()
            .iter()
            .map(|o| ConnectorElement {
                rect: Default::default(),
                ty: o.ty,
            })
            .collect::<Vec<_>>();

        if data.state().is_some() {
            input_connectors.push(ConnectorElement::default());
            output_connectors.push(ConnectorElement::default());
        }

        let mut layout = NodeLayout {
            content,
            input_connectors,
            output_connectors,
            width: 0.0,
            height: 0.0,
            translation: [0.0; 2],
        };

        layout.update_width(config);
        layout.update_height(config);

        layout
    }

    pub fn width(&self) -> f32 {
        self.width
    }

    pub fn height(&self) -> f32 {
        self.height
    }

    pub fn input_connectors(&self) -> &[ConnectorElement] {
        &self.input_connectors
    }

    pub fn output_connectors(&self) -> &[ConnectorElement] {
        &self.output_connectors
    }

    pub fn rect(&self) -> Rect {
        Rect {
            origin: self.translation,
            size: [self.width, self.height],
        }
    }

    pub fn content(&self) -> &NodeContent {
        &self.content
    }

    fn translation(&self) -> [f32; 2] {
        self.translation
    }

    fn input_count(&self) -> u32 {
        self.input_connectors.len() as u32
    }

    fn output_count(&self) -> u32 {
        self.output_connectors.len() as u32
    }

    fn update_width(&self, config: &Config) -> f32 {
        let inputs_width = connectors_width(self.input_count(), config);
        let outputs_width = connectors_width(self.output_count(), config);
        let inner_width = self.content.width(config);

        let max_width = [inputs_width, outputs_width, inner_width]
            .into_iter()
            .fold(0.0, f32::max);

        max_width + config.node_padding * 2.0
    }

    fn update_height(&mut self, config: &Config) {
        // We add the connector size twice (once for inputs, once for outputs), regardless of
        // whether the node actually has inputs and outputs. This lines the nodes a little bit more
        // neatly if a stratum contains e.g. output-less nodes.

        self.height =
            self.content.height(config) + config.connector_size * 2.0 + config.node_padding * 2.0;
    }

    fn place(&mut self, config: &Config) {
        self.place_input_connectors(config);
        self.place_output_connectors(config);
        self.place_content(config);
    }

    fn place_input_connectors(&mut self, config: &Config) {
        let mut x = self.translation[0] + config.node_padding;
        let y = 0.0;

        for connector in &mut self.input_connectors {
            connector.rect = Rect {
                origin: [x, y],
                size: [config.connector_size, config.connector_size],
            };

            x += config.connector_stride()
        }
    }

    fn place_output_connectors(&mut self, config: &Config) {
        let mut x = self.translation[0] + config.node_padding;
        let y = self.translation[1] + self.height - config.connector_size;

        for connector in &mut self.output_connectors {
            connector.rect = Rect {
                origin: [x, y],
                size: [config.connector_size, config.connector_size],
            };

            x += config.connector_stride()
        }
    }

    fn place_content(&mut self, config: &Config) {
        match &mut self.content {
            NodeContent::Loop(region_layout) => {
                let x = self.translation[0] + config.node_padding;
                let y = self.translation[1]
                    + config.connector_size
                    + config.node_padding
                    + config.font_height
                    + config.region_spacing;

                region_layout.translation = [x, y];
            }
            NodeContent::Switch(region_layouts) => {
                let mut x = self.translation[0] + config.node_padding;
                let y = self.translation[1]
                    + config.connector_size
                    + config.node_padding
                    + config.font_height
                    + config.region_spacing;

                for region_layout in region_layouts {
                    region_layout.translation = [x, y];

                    x += region_layout.width() + config.region_spacing;
                }
            }
            _ => {}
        }
    }
}

pub enum NodeContent {
    PlainText(String),
    FnApply(Function),
    Loop(RegionLayout),
    Switch(Vec<RegionLayout>),
}

impl NodeContent {
    fn width(&self, config: &Config) -> f32 {
        // Note that our text width calculations require a monospace font type
        match self {
            NodeContent::PlainText(text) => text.chars().count() as f32 * config.font_width,
            NodeContent::FnApply(_) => "apply".chars().count() as f32 * config.font_width,
            NodeContent::Loop(region_layout) => {
                let text_width = "loop".chars().count() as f32 * config.font_width;

                f32::max(text_width, region_layout.width())
            }
            NodeContent::Switch(region_layouts) => {
                let text_width = "switch".chars().count() as f32 * config.font_width;
                let region_width_sum: f32 = region_layouts.iter().map(|r| r.width()).sum();
                let region_spacing = if region_layouts.len() > 1 {
                    (region_layouts.len() - 1) as f32 * config.region_spacing
                } else {
                    0.0
                };
                let regions_width = region_width_sum + region_spacing;

                f32::max(text_width, regions_width)
            }
        }
    }

    fn height(&self, config: &Config) -> f32 {
        match self {
            NodeContent::PlainText(_) => config.font_height,
            NodeContent::FnApply(_) => config.font_height,
            NodeContent::Loop(region_layout) => {
                let text_height = config.font_height;

                text_height + config.region_spacing + region_layout.height()
            }
            NodeContent::Switch(region_layouts) => {
                let text_height = config.font_height;
                let max_region_height = region_layouts
                    .iter()
                    .map(|r| r.height())
                    .fold(0.0, f32::max);

                text_height + config.region_spacing + max_region_height
            }
        }
    }
}

fn connectors_width(count: u32, config: &Config) -> f32 {
    let width_sum = count as f32 * config.connector_size;
    let spacing = if count > 1 {
        (count - 1) as f32 * config.connector_spacing
    } else {
        0.0
    };

    width_sum + spacing
}
