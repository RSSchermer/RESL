use std::borrow::Cow;

use leptos::leptos_dom::logging::console_log;
use leptos::prelude::{StoredValue, *};
use leptos::svg::*;
use leptos::{component, IntoView};
use slir::ty::{ScalarKind, Type, TypeKind};
use slir::{rvsdg, Module};
use wasm_bindgen::module;

use crate::module_explorer::rvsdg_explorer::layout::ConnectorElement;
use crate::module_explorer::ModuleData;

fn ty_str(module: &Module, ty: Type) -> String {
    match &*module.ty.kind(ty) {
        TypeKind::Scalar(scalar) => match scalar {
            ScalarKind::I32 => "i32".to_owned(),
            ScalarKind::U32 => "u32".to_owned(),
            ScalarKind::F32 => "f32".to_owned(),
            ScalarKind::Bool => "bool".to_owned(),
        },
        TypeKind::Atomic(scalar) => match scalar {
            ScalarKind::I32 => "Atomic<i32>".to_owned(),
            ScalarKind::U32 => "Atomic<u32>".to_owned(),
            ScalarKind::F32 => "Atomic<f32>".to_owned(),
            ScalarKind::Bool => "Atomic<bool>".to_owned(),
        },
        TypeKind::Vector { scalar, size } => match scalar {
            ScalarKind::I32 => format!("vec{}<i32>", size.to_u32()),
            ScalarKind::U32 => format!("vec{}<u32>", size.to_u32()),
            ScalarKind::F32 => format!("vec{}<f32>", size.to_u32()),
            ScalarKind::Bool => format!("vec{}<bool>", size.to_u32()),
        },
        TypeKind::Matrix {
            rows,
            columns,
            scalar,
        } => match scalar {
            ScalarKind::I32 => {
                format!("mat{}x{}<i32>", columns.to_u32(), rows.to_u32())
            }
            ScalarKind::U32 => {
                format!("mat{}x{}<u32>", columns.to_u32(), rows.to_u32())
            }
            ScalarKind::F32 => {
                format!("mat{}x{}<f32>", columns.to_u32(), rows.to_u32())
            }
            ScalarKind::Bool => {
                format!("mat{}x{}<bool>", columns.to_u32(), rows.to_u32())
            }
        },
        TypeKind::Array { element_ty, count } => {
            format!("array<{}, {}>", ty_str(module, *element_ty), count)
        }
        TypeKind::Slice { element_ty } => format!("array<{}>", ty_str(module, *element_ty)),
        TypeKind::Struct(_) => {
            format!("S_{}", ty.registration_id().unwrap_or_default())
        }
        TypeKind::Enum(_) => {
            format!("E_{}", ty.registration_id().unwrap_or_default())
        }
        TypeKind::Ptr(pointee_ty) => format!("ptr<{}>", ty_str(module, *pointee_ty)),
        TypeKind::Function(_) => "fn".to_owned(),
        TypeKind::Predicate => "predicate".to_owned(),
        TypeKind::Dummy => "dummy".to_owned(),
    }
}

pub enum ToolTipPosition {
    Top,
    Bottom,
}

#[derive(Clone)]
struct ToolTip {
    text: String,
    text_width: f32,
}

const TOOLTIP_FONT_HEIGHT: f32 = 15.0;
const TOOLTIP_FONT_RATIO: f32 = 0.6;
const TOOLTIP_PADDING: f32 = 5.0;

#[component]
pub fn Connector(
    module: StoredValue<ModuleData>,
    connector: ConnectorElement,
    tooltip_position: ToolTipPosition,
) -> impl IntoView {
    let rect = connector.rect;

    let tooltip = connector.ty.map(|ty| {
        let ty_str = ty_str(&module.read_value().module, ty);
        let text_width = ty_str.len() as f32 * TOOLTIP_FONT_HEIGHT * TOOLTIP_FONT_RATIO;

        ToolTip {
            text: ty_str,
            text_width,
        }
    });

    view! {
        <g class="connector" transform=format!("translate({} {})", rect.origin[0], rect.origin[1])>
            <rect class="connector-rect" x=0 y=0 width=rect.size[0] height=rect.size[1] />

            {move || {
                tooltip.clone().map(|tt| {
                    let width = tt.text_width + TOOLTIP_PADDING * 2.0;
                    let height = TOOLTIP_FONT_HEIGHT + TOOLTIP_PADDING * 2.0;
                    let text_base = TOOLTIP_PADDING + TOOLTIP_FONT_HEIGHT;

                    let y = match tooltip_position {
                        ToolTipPosition::Top => {
                            -height
                        }
                        ToolTipPosition::Bottom => {
                            rect.size[1]
                        }
                    };

                    view! {
                        <g class="tooltip" transform=format!("translate(0, {})", y)>
                            <rect x=0 y=0 width=width height=height />
                            <text x=TOOLTIP_PADDING y=text_base>{tt.text}</text>
                        </g>
                    }
                })
            }}
        </g>
    }
}
