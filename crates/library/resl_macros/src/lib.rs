#![feature(let_chains)]

use std::env;
use std::sync::LazyLock;

use proc_macro::TokenStream;

mod compute;
mod fragment;
mod gpu;
mod resource;
mod shader_io;
mod shader_module;
mod vertex;
mod workgroup_shared;

static IS_RESLC_PASS: LazyLock<bool> = LazyLock::new(|| env::var("IS_RESLC_PASS").is_ok());

#[proc_macro_attribute]
pub fn compute(attr: TokenStream, item: TokenStream) -> TokenStream {
    compute::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn fragment(attr: TokenStream, item: TokenStream) -> TokenStream {
    fragment::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn gpu(attr: TokenStream, item: TokenStream) -> TokenStream {
    gpu::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn resource(attr: TokenStream, item: TokenStream) -> TokenStream {
    resource::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn shader_io(attr: TokenStream, item: TokenStream) -> TokenStream {
    shader_io::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn shader_module(attr: TokenStream, item: TokenStream) -> TokenStream {
    shader_module::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn vertex(attr: TokenStream, item: TokenStream) -> TokenStream {
    vertex::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn workgroup_shared(attr: TokenStream, item: TokenStream) -> TokenStream {
    workgroup_shared::expand_attribute(attr, item)
}
