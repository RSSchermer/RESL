//! Interface of a Rust stable_cg backend
//!
//! This crate defines all the traits that have to be implemented by a stable_cg backend in order to
//! use the backend-agnostic stable_cg code in `rustc_codegen_ssa`.
//!
//! The interface is designed around two backend-specific data structures, the stable_cg context and
//! the builder. The stable_cg context is supposed to be read-only after its creation and during the
//! actual stable_cg, while the builder stores the information about the function during stable_cg and
//! is used to produce the instructions of the backend IR.
//!
//! The traits contain associated types that are backend-specific, such as the backend's value or
//! basic blocks.

mod abi;
mod backend;
mod builder;
mod consts;
mod declare;
mod intrinsic;
mod misc;
mod statics;
mod type_;

use std::fmt;

pub use self::abi::AbiBuilderMethods;
pub use self::backend::BackendTypes;
pub use self::builder::BuilderMethods;
pub use self::consts::ConstCodegenMethods;
pub use self::declare::PreDefineCodegenMethods;
pub use self::intrinsic::IntrinsicCallBuilderMethods;
pub use self::misc::MiscCodegenMethods;
pub use self::statics::{StaticBuilderMethods, StaticCodegenMethods};
pub use self::type_::{
    ArgAbiBuilderMethods, BaseTypeCodegenMethods, DerivedTypeCodegenMethods,
    LayoutTypeCodegenMethods, TypeCodegenMethods,
};

pub trait CodegenObject = Copy + PartialEq + fmt::Debug;

pub trait CodegenMethods =
    TypeCodegenMethods + ConstCodegenMethods + StaticCodegenMethods + PreDefineCodegenMethods;
