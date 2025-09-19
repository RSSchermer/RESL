use std::env;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, Item};

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        let span = Span::call_site();

        return quote_spanned! { span =>
            compile_error!("the `shader_module` attribute does not accept any arguments");
        }
        .into();
    }

    let item = parse_macro_input!(item as Item);
    let is_reslc_pass = env::var("RESLC").is_ok();

    match item {
        Item::Mod(_) => {
            if is_reslc_pass {
                quote! {
                    #[resl_tool::shader_module]
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
                compile_error!("the `shader_module` attribute can only be applied to `mod` items");
            }
            .into()
        }
    }
}
