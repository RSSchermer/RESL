use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{Item, LitInt, meta, parse_macro_input};

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

    match item {
        Item::Fn(_) => quote! {
            #[allow(unused, unexpected_cfgs)]
            #[cfg_attr(rislc, rislc::compute(#x, #y, #z))]
            #item
        }
        .into(),
        _ => quote! {
            compile_error!("the `compute` attribute can only be applied to functions");
        }
        .into(),
    }
}
