use leptos::prelude::*;
use leptos::{component, IntoView};

use crate::module_explorer::rvsdg_explorer::connector::{Connector, ToolTipPosition};
use crate::module_explorer::rvsdg_explorer::layout::RegionLayout;
use crate::module_explorer::rvsdg_explorer::node::Node;
use crate::module_explorer::ModuleData;

#[component]
pub fn Region(module: StoredValue<ModuleData>, region: RegionLayout) -> impl IntoView {
    let translation = region.translation();
    let rect = region.rect();

    let region = StoredValue::new(region);

    view! {
        <g transform=format!("translate({} {})", translation[0], translation[1])>
            <rect class="region-rect" x=rect.origin[0] y=rect.origin[1] width=rect.size[0] height=rect.size[1]/>

            {move || {
                let region = region.read_value();
                let edge_count = region.edge_count();

                (0..edge_count).map(|i| {
                    let lines = region.edge_lines(i);
                    let is_state_edge = region.is_state_edge(i);

                    view! {
                        <g class="edge-lines" class=("state-edge", move || is_state_edge)>
                            {region.edge_lines(i).iter().map(|line| {
                                view! {
                                    <line class="visible-line" x1=line.start[0] y1=line.start[1] x2=line.end[0] y2=line.end[1] />
                                    <line class="hover-target" x1=line.start[0] y1=line.start[1] x2=line.end[0] y2=line.end[1] />
                                }
                            }).collect_view()}
                        </g>
                    }

                }).collect_view()
            }}

            {move || {
                region.read_value().argument_connectors().iter().cloned().map(|connector| {
                    view! { <Connector module connector tooltip_position=ToolTipPosition::Bottom /> }
                }).collect_view()
            }}

            {move || {
                region.read_value().node_layouts().iter().cloned().map(|node| {
                    view! { <Node module node /> }
                }).collect_view()
            }}

            {move || {
                region.read_value().result_connectors().iter().cloned().map(|connector| {
                    view! { <Connector module connector tooltip_position=ToolTipPosition::Top /> }
                }).collect_view()
            }}
        </g>
    }
}
