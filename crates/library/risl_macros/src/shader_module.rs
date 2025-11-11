use proc_macro::TokenStream;
use quote::quote;
use syn::{Item, parse_macro_input};

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        return quote! {
            compile_error!("the `shader_module` attribute does not accept any arguments");
        }
        .into();
    }

    let item = parse_macro_input!(item as Item);

    match item {
        Item::Mod(_) => quote! {
            #[cfg_attr(rislc, rislc::shader_module)]
            #item
        }
        .into(),
        _ => quote! {
            compile_error!("the `shader_module` attribute can only be applied to `mod` items");
        }
        .into(),
    }
}
