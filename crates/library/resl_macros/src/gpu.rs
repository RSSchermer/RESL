use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, Item};

use crate::IS_RESLC_PASS;

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        let span = Span::call_site();

        return quote_spanned! { span =>
            compile_error!("the `gpu` attribute does not accept any arguments");
        }
        .into();
    }

    let item = parse_macro_input!(item as Item);

    match item {
        Item::Fn(_) | Item::Trait(_) | Item::Impl(_) | Item::Struct(_) | Item::Enum(_) => {
            if *IS_RESLC_PASS {
                quote! {
                    #[reslc::gpu]
                    #item
                }
                .into()
            } else {
                item.to_token_stream().into()
            }
        }
        _ => {
            let span = Span::call_site();

            quote_spanned! { span =>
                compile_error!("the `gpu` attribute can only be applied to `fn`, `trait` and `impl` items");
            }
            .into()
        }
    }
}
