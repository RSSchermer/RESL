use proc_macro::TokenStream;
use quote::quote;
use syn::{Item, parse_macro_input};

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        return quote! {
            compile_error!("the `gpu` attribute does not accept any arguments");
        }
        .into();
    }

    let item = parse_macro_input!(item as Item);

    match item {
        Item::Fn(_) | Item::Trait(_) | Item::Impl(_) | Item::Struct(_) | Item::Enum(_) => {
            quote! {
                #[cfg_attr(reslc, reslc::gpu)]
                #item
            }
            .into()
        }
        _ => {
            quote! {
                compile_error!("the `gpu` attribute can only be applied to `fn`, `trait` and `impl` items");
            }
            .into()
        }
    }
}
