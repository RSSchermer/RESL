#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Type {
    SlirType(slir::ty::Type),
    FnDecl { ret_ty: Option<slir::ty::Type> },
}

impl Type {
    pub fn expect_slir_type(&self) -> slir::ty::Type {
        if let Type::SlirType(ty) = self {
            *ty
        } else {
            panic!("expected SLIR type")
        }
    }

    pub fn fn_decl_ret_ty(&self) -> Option<slir::ty::Type> {
        if let Type::FnDecl { ret_ty } = self {
            *ret_ty
        } else {
            panic!("expected a function declaration type")
        }
    }
}

impl From<slir::ty::Type> for Type {
    fn from(value: slir::ty::Type) -> Self {
        Type::SlirType(value)
    }
}
