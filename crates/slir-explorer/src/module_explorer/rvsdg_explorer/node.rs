use leptos::prelude::{StoredValue, *};
use leptos::svg::*;
use leptos::{component, IntoView};
use slir::rvsdg;
use wasm_bindgen::module;

use crate::module_explorer::rvsdg_explorer::connector::{Connector, ToolTipPosition};
use crate::module_explorer::rvsdg_explorer::layout::{NodeContent, NodeLayout};
use crate::module_explorer::rvsdg_explorer::region::Region;
use crate::module_explorer::{format_function_url, ModuleData};

// It seems that if we set the font-size to e.g. 15px, the text element's height will get some
// top and bottom padding, even if we try to nullify that in CSS or try setting the CSS line-height
// to 15px also. If anyone reading this knows what's going on here please reach out. For now I'm
// dirty-fixing this with a hard-coded adjustment
const TEXT_ADJUST: f32 = -3.0;

#[component]
pub fn Node(module: StoredValue<ModuleData>, node: NodeLayout) -> impl IntoView {
    let node = StoredValue::new(node);
    let rect = move || node.read_value().rect();

    let rect_class = match node.read_value().content() {
        NodeContent::PlainText(_) => "node-rect simple",
        NodeContent::FnApply(_, _) => "node-rect simple",
        NodeContent::Loop(_, _) => "node-rect loop",
        NodeContent::Switch(_, _) => "node-rect switch",
    };

    view! {
        <g>
            <rect class=rect_class x=rect().origin[0] y=rect().origin[1] width=rect().size[0] height=rect().size[1] />

            {move || {
                match node.read_value().content() {
                    NodeContent::PlainText(text) => {
                        let [x, y] = text.translation();

                        view! {
                            <text x=x y=y+TEXT_ADJUST>{text.text().to_owned()}</text>
                        }.into_any()
                    }
                    NodeContent::FnApply(text, f) => {
                        let [x, y] = text.translation();

                        view! {
                            <g>
                                <a href=format_function_url(module.read_value().module.name, *f)>
                                    <text x=x y=y+TEXT_ADJUST>
                                        {text.text().to_owned()}
                                    </text>
                                </a>
                            </g>
                        }.into_any()
                    }
                    NodeContent::Loop(text, region) => {
                        let [x, y] = text.translation();

                        view! {
                            <text x=x y=y+TEXT_ADJUST>{text.text().to_owned()}</text>

                            <Region module region=region.clone() />
                        }.into_any()
                    }
                    NodeContent::Switch(text, regions) => {
                        let [x, y] = text.translation();
                        let regions = regions.clone();

                        view! {
                            <text x=x y=y+TEXT_ADJUST>{text.text().to_owned()}</text>

                            {move || {
                                regions.clone().into_iter().map(|region| {
                                    view! { <Region module region /> }
                                }).collect_view()
                            }}
                        }.into_any()
                    }
                }
            }}

            {move || {
                node.read_value().input_connectors().iter().cloned().map(|connector| {
                    view! { <Connector module connector tooltip_position=ToolTipPosition::Top /> }
                }).collect_view()
            }}

            {move || {
                node.read_value().output_connectors().iter().cloned().map(|connector| {
                    view! { <Connector module connector tooltip_position=ToolTipPosition::Bottom /> }
                }).collect_view()
            }}
        </g>
    }
}
