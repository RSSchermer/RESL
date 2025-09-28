use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse, parse_macro_input, Attribute, Field, Item, Meta, Path, PathSegment};

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

    let Item::Struct(mut struct_decl) = item else {
        let span = Span::call_site();

        return quote_spanned! { span =>
            compile_error!("the `shader_io` attribute can only be applied to `struct` items");
        }
        .into();
    };

    struct_decl.fields.iter_mut().for_each(adjust_field);

    if *IS_RESLC_PASS {
        quote! {
            #[reslc::shader_io]
            #struct_decl
        }
        .into()
    } else {
        struct_decl.to_token_stream().into()
    }
}

fn adjust_field(field: &mut Field) {
    field.attrs.retain_mut(|attr| match &mut attr.meta {
        Meta::Path(path) => process_path(path),
        Meta::List(list) => process_path(&mut list.path),
        _ => true,
    })
}

fn process_path(path: &mut Path) -> bool {
    if path.is_ident("location")
        || path.is_ident("builtin")
        || path.is_ident("invariant")
        || path.is_ident("interpolate")
        || path.is_ident("blend_src")
    {
        if *IS_RESLC_PASS {
            path.segments
                .insert(0, Ident::new("reslc", Span::call_site()).into());
        } else {
            return false;
        }
    }

    true
}
