// This module is modified from rustc_codegen_ssa

mod common;
mod layout;
mod mir;
mod mono_item;
mod scalar;

pub mod traits;

pub use self::scalar::{Scalar, Pointer};
pub use self::common::*;
pub use self::layout::{ScalarExt, TyAndLayoutExt};
pub use self::mir::place::{PlaceValue, PlaceRef};
pub use self::mir::operand::{OperandValue, OperandRef};
pub use self::mono_item::MonoItemExt;