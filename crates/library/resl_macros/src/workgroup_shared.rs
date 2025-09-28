use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Ident, Span, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::token::{Brace, Bracket, Paren};
use syn::{
    meta, parse_macro_input, Attribute, Expr, LitInt, Macro, MacroDelimiter, Path,
    StaticMutability, Token, Type, Visibility,
};

use crate::IS_RESLC_PASS;

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        let span = Span::call_site();

        return quote_spanned! { span =>
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
        eq_token,
        workgroup_macro,
        semi_token,
        ..
    } = item;

    let expr = workgroup_macro.tokens;
    let expansion = quote! {
        #(#attrs)*
        #static_token #ident #colon_token #ty #eq_token unsafe { resl::workgroup::__init_workgroup(#expr) } #semi_token
    };

    if *IS_RESLC_PASS {
        quote! {
            #[reslc::workgroup_shared]
            #expansion
        }
        .into()
    } else {
        expansion.into()
    }
}

mod kw {
    syn::custom_keyword!(workgroup);
}

struct WorkgroupMacro {
    _workgroup_kw: kw::workgroup,
    _bang_token: Token![!],
    _delimiter: MacroDelimiter,
    tokens: proc_macro2::TokenStream,
}

fn parse_delimiter(input: ParseStream) -> syn::Result<(MacroDelimiter, proc_macro2::TokenStream)> {
    input.step(|cursor| {
        if let Some((TokenTree::Group(g), rest)) = cursor.token_tree() {
            let span = g.delim_span();
            let delimiter = match g.delimiter() {
                Delimiter::Parenthesis => MacroDelimiter::Paren(Paren(span)),
                Delimiter::Brace => MacroDelimiter::Brace(Brace(span)),
                Delimiter::Bracket => MacroDelimiter::Bracket(Bracket(span)),
                Delimiter::None => {
                    return Err(cursor.error("expected delimiter"));
                }
            };
            Ok(((delimiter, g.stream()), rest))
        } else {
            Err(cursor.error("expected delimiter"))
        }
    })
}

impl Parse for WorkgroupMacro {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let tokens;

        Ok(WorkgroupMacro {
            _workgroup_kw: input.parse()?,
            _bang_token: input.parse()?,
            _delimiter: {
                let (delimiter, content) = parse_delimiter(input)?;
                tokens = content;
                delimiter
            },
            tokens,
        })
    }
}

pub struct WorkgroupSharedStatic {
    attrs: Vec<Attribute>,
    vis: Visibility,
    static_token: Token![static],
    mutability: StaticMutability,
    ident: Ident,
    colon_token: Token![:],
    ty: Box<Type>,
    eq_token: Token![=],
    workgroup_macro: WorkgroupMacro,
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
            eq_token: input.parse()?,
            workgroup_macro: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}
