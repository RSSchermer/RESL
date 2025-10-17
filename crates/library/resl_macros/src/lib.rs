#![feature(let_chains)]

use std::env;
use std::sync::LazyLock;

use proc_macro::TokenStream;

mod compute;
mod fragment;
mod gpu;
mod impl_mat_mul;
mod resource;
mod shader_io;
mod shader_module;
mod vertex;
mod workgroup_shared;

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

/// Helper macro for generating matrix multiplication implementations.
///
/// This is a utility macro used to generate `mat * mat`, `mat * vec` and `vec * mat`
/// implementations of `core::ops::Mul` inside the `resl` crate, as macro-rules based macros don't
/// allow is to cleanly do this. It is not useful outside of that and not intended for public use.
///
/// It expects a token-stream formatted as follows:
///
/// ```psuedocode
/// // For matrix-matrix multiplication
/// mat<2, 3> * mat<4, 2>
///
/// // For matrix-vector multiplication
/// mat<2, 3> * vec<2>
/// ```
#[doc(hidden)]
#[proc_macro]
pub fn impl_mat_mul(token_stream: TokenStream) -> TokenStream {
    impl_mat_mul::expand_macro(token_stream)
}
