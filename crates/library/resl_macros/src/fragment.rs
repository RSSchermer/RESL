use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Item};

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        return quote! {
            compile_error!("the `fragment` attribute does not accept any arguments");
        }
        .into();
    }

    let item = parse_macro_input!(item as Item);

    match item {
        Item::Fn(_) => quote! {
            #[cfg_attr(reslc, reslc::fragment)]
            #[allow(unused)]
            #item
        }
        .into(),
        _ => quote! {
            compile_error!("the `fragment` attribute can only be applied to functions");
        }
        .into(),
    }
}
