use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Ident, TokenTree};
use quote::{quote, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::token::{Brace, Bracket, Paren};
use syn::{
    Attribute, MacroDelimiter, StaticMutability, Token, Type, Visibility, parse_macro_input,
};

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        return quote! {
            compile_error!("the `workgroup_shared` attribute does not accept any arguments");
        }
        .into();
    }

    let item = parse_macro_input!(item as WorkgroupSharedStatic);

    if !matches!(item.vis, Visibility::Inherited) {
        let span = item.vis.span();

        return quote_spanned! { span =>
            compile_error!("a `static` with the `workgroup_shared` attribute must be private");
        }
        .into();
    }

    if matches!(item.mutability, StaticMutability::Mut(_)) {
        let span = item.mutability.span();

        return quote_spanned! { span =>
            compile_error!("a `static` with the `workgroup_shared` attribute must be immutable");
        }
        .into();
    }

    let WorkgroupSharedStatic {
        attrs,
        static_token,
        ident,
        colon_token,
        ty,
        semi_token,
        ..
    } = item;

    quote! {
        #[cfg_attr(reslc, reslc::workgroup_shared)]
        #(#attrs)*
        #static_token #ident #colon_token #ty = unsafe { core::mem::zeroed() } #semi_token
    }
    .into()
}

pub struct WorkgroupSharedStatic {
    attrs: Vec<Attribute>,
    vis: Visibility,
    static_token: Token![static],
    mutability: StaticMutability,
    ident: Ident,
    colon_token: Token![:],
    ty: Box<Type>,
    semi_token: Token![;],
}

impl Parse for WorkgroupSharedStatic {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(WorkgroupSharedStatic {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            static_token: input.parse()?,
            mutability: input.parse()?,
            ident: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}
