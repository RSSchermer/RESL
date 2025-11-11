use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{Attribute, LitInt, StaticMutability, Token, Type, Visibility, meta, parse_macro_input};

pub fn expand_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut group: Option<LitInt> = None;
    let mut binding: Option<LitInt> = None;

    let attr_parser = meta::parser(|meta| {
        if meta.path.is_ident("group") {
            group = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("binding") {
            binding = Some(meta.value()?.parse()?);
            Ok(())
        } else {
            Err(meta.error("unsupported property; expected `group` or `binding`"))
        }
    });

    parse_macro_input!(attr with attr_parser);

    let Some(group) = group else {
        return quote! {
            compile_error!("the `resource` attribute requires a `group` property");
        }
        .into();
    };

    let Some(binding) = binding else {
        return quote! {
            compile_error!("the `resource` attribute requires a `binding` property");
        }
        .into();
    };

    let item = parse_macro_input!(item as BufferBoundStatic);

    if !matches!(item.vis, Visibility::Inherited) {
        let span = item.vis.span();

        return quote_spanned! { span =>
            compile_error!("a `static` with the `resource` attribute must be private");
        }
        .into();
    }

    if matches!(item.mutability, StaticMutability::Mut(_)) {
        let span = item.mutability.span();

        return quote_spanned! { span =>
            compile_error!("a `static` with the `resource` attribute must be immutable");
        }
        .into();
    }

    let BufferBoundStatic {
        attrs,
        static_token,
        ident,
        colon_token,
        ty,
        semi_token,
        ..
    } = item;

    let ty_span = ty.span();
    let assert_resource = quote_spanned! { ty_span =>
        const _: () = {
            const fn assert_resource<T: risl::resource::Resource>() {}

            assert_resource::<#ty>();
        };
    };

    // We need to provide an initializer for the static to make rustc happy. Since we assert that
    // the `ty` is a `risl::resource::Resource` type, which is a sealed trait only implemented by a
    // known set of zero-sized types, we can safely call `core::mem::zeroed()` to do this, as these
    // are all zero-sized types. This way we don't have to expose some hidden unsafe initializer
    // functions for these types, as these types are never meant to be constructed.
    quote! {
        #[cfg_attr(rislc, rislc::resource(#group, #binding))]
        #(#attrs)*
        #static_token #ident #colon_token #ty = unsafe { core::mem::zeroed() } #semi_token

        #assert_resource
    }
    .into()
}

pub struct BufferBoundStatic {
    attrs: Vec<Attribute>,
    vis: Visibility,
    static_token: Token![static],
    mutability: StaticMutability,
    ident: Ident,
    colon_token: Token![:],
    ty: Box<Type>,
    semi_token: Token![;],
}

impl Parse for BufferBoundStatic {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(BufferBoundStatic {
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
