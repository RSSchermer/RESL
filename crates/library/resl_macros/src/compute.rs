use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::{meta, parse_macro_input, Item, LitInt};

use crate::IS_RESLC_PASS;

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut x = LitInt::new("1", Span::call_site());
    let mut y = LitInt::new("1", Span::call_site());
    let mut z = LitInt::new("1", Span::call_site());

    let attr_parser = meta::parser(|meta| {
        if meta.path.is_ident("x") {
            x = meta.value()?.parse()?;
            Ok(())
        } else if meta.path.is_ident("y") {
            y = meta.value()?.parse()?;
            Ok(())
        } else if meta.path.is_ident("z") {
            z = meta.value()?.parse()?;
            Ok(())
        } else {
            Err(meta.error("unsupported property; expected `x`, `y`, or `z`"))
        }
    });

    parse_macro_input!(attr with attr_parser);

    let item = parse_macro_input!(item as Item);

    let expansion = quote! {
        #[allow(unused)]
        #item
    };

    match item {
        Item::Fn(_) => {
            if *IS_RESLC_PASS {
                quote! {
                    #[resl_tool::compute(#x, #y, #z)]
                    #expansion
                }
                .into()
            } else {
                expansion.into()
            }
        }
        _ => {
            let span = Span::call_site();

            quote_spanned! { span =>
                compile_error!("the `compute` attribute can only be applied to functions");
            }
            .into()
        }
    }
}
