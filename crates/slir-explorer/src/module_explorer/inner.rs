use std::collections::HashSet;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use leptos::leptos_dom::logging::console_log;
use leptos::prelude::*;
use slotmap::{Key, KeyData};
use thaw::*;
use urlencoding::{decode as urldecode, encode as urlencode};

use crate::module_explorer::enum_explorer::EnumExplorer;
use crate::module_explorer::function_explorer::FunctionExplorer;
use crate::module_explorer::struct_explorer::StructExplorer;
use crate::module_explorer::{
    format_function_url, ModuleData, ENUM_ITEM_LABEL_START, FUNCTION_ITEM_LABEL_START,
    STORAGE_ITEM_LABEL_START, STRUCT_ITEM_LABEL_START, UNIFORM_ITEM_LABEL_START,
    WORKGROUP_ITEM_LABEL_START,
};

enum Item {
    Function(slir::Function),
    Struct(slir::Struct),
    Enum(slir::Enum),
    UniformBinding(slir::UniformBinding),
    StorageBinding(slir::StorageBinding),
    WorkgroupBinding(slir::WorkgroupBinding),
    Invalid,
}

impl Item {
    fn from_label(label: &str) -> Item {
        let label = if let Ok(label) = urldecode(label) {
            label
        } else {
            return Item::Invalid;
        };

        if label.starts_with(FUNCTION_ITEM_LABEL_START) {
            let name = &label[FUNCTION_ITEM_LABEL_START.len()..];
            let mut split = name.split("::");
            let module = split.next().unwrap_or("");
            let name = split.next().unwrap_or("");

            Item::Function(slir::Function {
                name: slir::Symbol::from_ref(name),
                module: slir::Symbol::from_ref(module),
            })
        } else if label.starts_with(STRUCT_ITEM_LABEL_START) {
            let index_str = &label[STRUCT_ITEM_LABEL_START.len()..];
            let index = usize::from_str(index_str).unwrap();

            Item::Struct(slir::Struct::from(index))
        } else if label.starts_with(ENUM_ITEM_LABEL_START) {
            let index_str = &label[ENUM_ITEM_LABEL_START.len()..];
            let index = usize::from_str(index_str).unwrap();

            Item::Enum(slir::Enum::from(index))
        } else if label.starts_with(UNIFORM_ITEM_LABEL_START) {
            let index_str = &label[UNIFORM_ITEM_LABEL_START.len()..];
            let data_ffi = u64::from_str(index_str).unwrap();

            Item::UniformBinding(slir::UniformBinding::from(KeyData::from_ffi(data_ffi)))
        } else if label.starts_with(STORAGE_ITEM_LABEL_START) {
            let index_str = &label[STORAGE_ITEM_LABEL_START.len()..];
            let data_ffi = u64::from_str(index_str).unwrap();

            Item::StorageBinding(slir::StorageBinding::from(KeyData::from_ffi(data_ffi)))
        } else if label.starts_with(WORKGROUP_ITEM_LABEL_START) {
            let index_str = &label[WORKGROUP_ITEM_LABEL_START.len()..];
            let data_ffi = u64::from_str(index_str).unwrap();

            Item::WorkgroupBinding(slir::WorkgroupBinding::from(KeyData::from_ffi(data_ffi)))
        } else {
            Item::Invalid
        }
    }
}

#[component]
pub fn ModuleExplorerInner(module: ModuleData, item_label: Option<String>) -> impl IntoView {
    let module = StoredValue::new(module);

    let functions = move || {
        let mut functions = module
            .read_value()
            .module
            .fn_sigs
            .keys()
            .collect::<Vec<_>>();

        functions.sort_by(|a, b| a.name.cmp(&b.name));

        functions
    };

    let mut open_item_categories = HashSet::new();

    open_item_categories.insert("functions".to_string());

    view! {
        <div class="module-explorer-container">
            <div class="module-items-container">
                <Accordion multiple=true open_items=open_item_categories>
                    <AccordionItem value="structs">
                        <AccordionHeader slot>
                            "Structs"
                        </AccordionHeader>
                        <ul class="module-item-list">
                            {module.read_value().module.structs.iter().map(|s| view! {
                                <li>
                                    <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), STRUCT_ITEM_LABEL_START, s.to_usize())>
                                        {format!("S_{}", s.to_usize())}
                                    </Link>
                                </li>
                            }).collect_view()}
                        </ul>
                    </AccordionItem>
                    <AccordionItem value="enums">
                        <AccordionHeader slot>
                            "Enums"
                        </AccordionHeader>
                        <ul class="module-item-list">
                            {module.read_value().module.enums.iter().map(|e| view! {
                                <li>
                                    <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), ENUM_ITEM_LABEL_START, e.to_usize())>
                                        {format!("E_{}", e.to_usize())}
                                    </Link>
                                </li>
                            }).collect_view()}
                        </ul>
                    </AccordionItem>
                    <AccordionItem value="uniform_bindings">
                        <AccordionHeader slot>
                            "Uniform Bindings"
                        </AccordionHeader>
                        <ul class="module-item-list">
                            {module.read_value().module.uniform_bindings.keys().map(|b| view! {
                                <li>
                                    <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), UNIFORM_ITEM_LABEL_START, b.data().as_ffi())>
                                        {format!("U{}", b.data().as_ffi())}
                                    </Link>
                                </li>
                            }).collect_view()}
                        </ul>
                    </AccordionItem>
                    <AccordionItem value="storage_bindings">
                        <AccordionHeader slot>
                            "Storage Bindings"
                        </AccordionHeader>
                        <ul class="module-item-list">
                            {module.read_value().module.storage_bindings.keys().map(|b| view! {
                                <li>
                                    <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), STORAGE_ITEM_LABEL_START, b.data().as_ffi())>
                                        {format!("S{}", b.data().as_ffi())}
                                    </Link>
                                </li>
                            }).collect_view()}
                        </ul>
                    </AccordionItem>
                    <AccordionItem value="workgroup_bindings">
                        <AccordionHeader slot>
                            "Workgroup Bindings"
                        </AccordionHeader>
                        <ul class="module-item-list">
                            {module.read_value().module.workgroup_bindings.keys().map(|b| view! {
                                <li>
                                    <Link href=format!("/{}/{}{}", urlencode(module.read_value().module.name.as_str()), WORKGROUP_ITEM_LABEL_START, b.data().as_ffi())>
                                        {format!("W{}", b.data().as_ffi())}
                                    </Link>
                                </li>
                            }).collect_view()}
                        </ul>
                    </AccordionItem>
                    <AccordionItem  value="functions">
                        <AccordionHeader slot>
                            "Functions"
                        </AccordionHeader>
                        <ul class="module-item-list">
                            {move || functions().into_iter().map(|f| view! {
                                <li>
                                    <Link href=format_function_url(module.read_value().module.name, f)>
                                        {f.name.to_string()}
                                    </Link>
                                </li>
                            }).collect_view()}
                        </ul>
                    </AccordionItem>
                </Accordion>
            </div>
            <div class="item-explorer-container">
                {
                    if let Some(item_label) = item_label {
                        match Item::from_label(&item_label) {
                            Item::Function(function) => view! { <FunctionExplorer module function/> }.into_any(),
                            Item::Struct(struct_handle) => view! { <StructExplorer module struct_handle/> }.into_any(),
                            Item::Enum(enum_handle) => view! { <EnumExplorer module enum_handle/> }.into_any(),
                            Item::UniformBinding(b) => view! { "uniform placeholder" }.into_any(),
                            Item::StorageBinding(b) => view! { "storage placeholder" }.into_any(),
                            Item::WorkgroupBinding(b) => view! { "workgroup placeholder" }.into_any(),
                            Item::Invalid => view! {
                                <div class="info-page-container">
                                    <h1>"Invalid item identifier"</h1>

                                    <p>"Item identifier format is invalid."</p>
                                </div>
                            }.into_any(),
                        }
                    } else {
                        view! {
                            <div class="info-page-container">
                                <h1>"No item selected."</h1>

                                <p>"Select an item from the side-bar."</p>
                            </div>
                        }.into_any()
                    }
                }
            </div>
        </div>
    }
}
