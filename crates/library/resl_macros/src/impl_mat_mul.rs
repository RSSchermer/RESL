use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{LitInt, Token, parse_macro_input};

pub fn expand_macro(token_stream: TokenStream) -> TokenStream {
    let MatMul { lhs, rhs } = parse_macro_input!(token_stream as MatMul);

    match (lhs, rhs) {
        (Operand::Matrix(lhs), Operand::Matrix(rhs)) => generate_mat_mat_mul(lhs, rhs),
        (Operand::Matrix(lhs), Operand::Vector(rhs)) => generate_mat_vec_mul(lhs, rhs),
        (Operand::Vector(lhs), Operand::Matrix(rhs)) => generate_vec_mat_mul(lhs, rhs),
        (Operand::Vector(_), Operand::Vector(_)) => quote! {
            compile_error!("vec * vec is not implemented through this macro");
        }
        .into(),
    }
}

fn generate_mat_mat_mul(lhs: Matrix, rhs: Matrix) -> TokenStream {
    let lhs_ty = mat_ty(&lhs);
    let rhs_ty = mat_ty(&rhs);
    let output_ty = mat_ty(&Matrix {
        cols: rhs.cols,
        rows: lhs.rows,
    });
    let out_col_ty = vec_ty(&Vector { size: lhs.rows });

    if lhs.cols != rhs.rows {
        return quote! {
            compile_error!(
                "the left-hand-side matrix's column count must equal the right-hand-side matrix's \
                row count"
            );
        }
        .into();
    }

    let dot_size = lhs.cols.to_usize();

    let output_cols = (0..rhs.cols.to_usize()).map(|c| {
        let dot_products = (0..lhs.rows.to_usize()).map(|r| {
            let c = syn::Index::from(c);
            let r = syn::Index::from(r);

            let dot_components = (0..dot_size).map(|i| {
                let i = syn::Index::from(i);

                quote!(lhs.#i.#r * rhs.#c.#i)
            });

            quote!(#(#dot_components)+*)
        });

        quote!(#out_col_ty(#(#dot_products),*))
    });

    quote! {
        #[gpu]
        impl core::ops::Mul<#rhs_ty> for #lhs_ty {
            type Output = #output_ty;

            #[cfg_attr(reslc, reslc::intrinsic(mul))]
            fn mul(self, rhs: #rhs_ty) -> Self::Output {
                let lhs = self;

                #output_ty(#(#output_cols),*)
            }
        }
    }
    .into()
}

fn generate_mat_vec_mul(lhs: Matrix, rhs: Vector) -> TokenStream {
    let lhs_ty = mat_ty(&lhs);
    let rhs_ty = vec_ty(&rhs);
    let output_ty = vec_ty(&Vector { size: lhs.rows });

    if lhs.cols != rhs.size {
        return quote! {
            compile_error!(
                "the left-hand-side matrix's column count must equal the right-hand-side vector's \
                size"
            );
        }
        .into();
    }

    let dot_size = lhs.cols.to_usize();

    let dot_products = (0..lhs.rows.to_usize()).map(|r| {
        let r = syn::Index::from(r);

        let dot_components = (0..dot_size).map(|i| {
            let i = syn::Index::from(i);

            quote!(lhs.#i.#r * rhs.#i)
        });

        quote!(#(#dot_components)+*)
    });

    quote! {
        #[gpu]
        impl core::ops::Mul<#rhs_ty> for #lhs_ty {
            type Output = #output_ty;

            #[cfg_attr(reslc, reslc::intrinsic(mul))]
            fn mul(self, rhs: #rhs_ty) -> Self::Output {
                let lhs = self;

                #output_ty(#(#dot_products),*)
            }
        }
    }
    .into()
}

fn generate_vec_mat_mul(lhs: Vector, rhs: Matrix) -> TokenStream {
    let lhs_ty = vec_ty(&lhs);
    let rhs_ty = mat_ty(&rhs);
    let output_ty = vec_ty(&Vector { size: rhs.cols });

    if lhs.size != rhs.rows {
        return quote! {
            compile_error!(
                "the left-hand-side vector's size must equal the right-hand-side matrix's column \
                count"
            );
        }
        .into();
    }

    let dot_size = lhs.size.to_usize();

    let dot_products = (0..rhs.cols.to_usize()).map(|c| {
        let c = syn::Index::from(c);

        let dot_components = (0..dot_size).map(|i| {
            let i = syn::Index::from(i);

            quote!(lhs.#i * rhs.#c.#i)
        });

        quote!(#(#dot_components)+*)
    });

    quote! {
        #[gpu]
        impl core::ops::Mul<#rhs_ty> for #lhs_ty {
            type Output = #output_ty;

            #[cfg_attr(reslc, reslc::intrinsic(mul))]
            fn mul(self, rhs: #rhs_ty) -> Self::Output {
                let lhs = self;

                #output_ty(#(#dot_products),*)
            }
        }
    }
    .into()
}

fn mat_ty(mat: &Matrix) -> proc_macro2::TokenStream {
    match (mat.cols, mat.rows) {
        (Size::Two, Size::Two) => quote!(mat2x2_f32),
        (Size::Two, Size::Three) => quote!(mat2x3_f32),
        (Size::Two, Size::Four) => quote!(mat2x4_f32),
        (Size::Three, Size::Two) => quote!(mat3x2_f32),
        (Size::Three, Size::Three) => quote!(mat3x3_f32),
        (Size::Three, Size::Four) => quote!(mat3x4_f32),
        (Size::Four, Size::Two) => quote!(mat4x2_f32),
        (Size::Four, Size::Three) => quote!(mat4x3_f32),
        (Size::Four, Size::Four) => quote!(mat4x4_f32),
    }
}

fn vec_ty(vec: &Vector) -> proc_macro2::TokenStream {
    match vec.size {
        Size::Two => quote!(vec2_f32),
        Size::Three => quote!(vec3_f32),
        Size::Four => quote!(vec4_f32),
    }
}

mod kw {
    syn::custom_keyword!(vec);
    syn::custom_keyword!(mat);
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Size {
    Two,
    Three,
    Four,
}

impl Size {
    fn to_usize(&self) -> usize {
        match self {
            Size::Two => 2,
            Size::Three => 3,
            Size::Four => 4,
        }
    }
}

impl Parse for Size {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit_int = input.parse::<LitInt>()?;

        match lit_int.base10_parse::<u8>()? {
            2 => Ok(Size::Two),
            3 => Ok(Size::Three),
            4 => Ok(Size::Four),
            _ => Err(syn::Error::new(
                lit_int.span(),
                "invalid size; must be 2, 3, or 4",
            )),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Matrix {
    cols: Size,
    rows: Size,
}

impl Parse for Matrix {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::mat>()?;
        input.parse::<Token![<]>()?;
        let cols = input.parse::<Size>()?;
        input.parse::<Token![,]>()?;
        let rows = input.parse::<Size>()?;
        input.parse::<Token![>]>()?;

        Ok(Matrix { cols, rows })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Vector {
    size: Size,
}

impl Parse for Vector {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::vec>()?;
        input.parse::<Token![<]>()?;
        let size = input.parse::<Size>()?;
        input.parse::<Token![>]>()?;

        Ok(Vector { size })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Operand {
    Matrix(Matrix),
    Vector(Vector),
}

impl From<Matrix> for Operand {
    fn from(mat: Matrix) -> Self {
        Operand::Matrix(mat)
    }
}

impl From<Vector> for Operand {
    fn from(vec: Vector) -> Self {
        Operand::Vector(vec)
    }
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(kw::mat) {
            Ok(input.parse::<Matrix>()?.into())
        } else if lookahead.peek(kw::vec) {
            Ok(input.parse::<Vector>()?.into())
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct MatMul {
    lhs: Operand,
    rhs: Operand,
}

impl Parse for MatMul {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lhs = input.parse::<Operand>()?;
        input.parse::<Token![*]>()?;
        let rhs = input.parse::<Operand>()?;

        Ok(MatMul { lhs, rhs })
    }
}
