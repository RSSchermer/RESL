use proc_macro::TokenStream;

mod gpu;
mod shader_module;

#[proc_macro_attribute]
pub fn gpu(attr: TokenStream, item: TokenStream) -> TokenStream {
    gpu::expand_attribute(attr, item)
}

#[proc_macro_attribute]
pub fn shader_module(attr: TokenStream, item: TokenStream) -> TokenStream {
    shader_module::expand_attribute(attr, item)
}
