use std::borrow::Cow;

use serde::{Deserialize, Serialize};

use crate::ty::{Type, TY_U32};

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct BuiltinFunction {
    ident: BuiltinIdent,
    arguments: Cow<'static, [Type]>,
    return_type: Option<Type>,
}

impl BuiltinFunction {
    pub fn array_len(array_ptr_ty: Type) -> Self {
        BuiltinFunction {
            ident: BuiltinIdent::ArrayLen,
            arguments: vec![array_ptr_ty].into(),
            return_type: Some(TY_U32),
        }
    }

    pub fn ident(&self) -> BuiltinIdent {
        self.ident
    }

    pub fn arguments(&self) -> &[Type] {
        &self.arguments
    }

    pub fn return_type(&self) -> Option<Type> {
        self.return_type
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum BuiltinIdent {
    ArrayLen,
}

impl BuiltinIdent {
    pub fn as_str(&self) -> &'static str {
        match self {
            BuiltinIdent::ArrayLen => "array_len",
        }
    }
}
