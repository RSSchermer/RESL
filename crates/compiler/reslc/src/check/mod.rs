use rustc_hir::intravisit::Visitor;

pub fn check_contextless_attributes() {}

pub struct CheckContextlessAttributesVisitor;

impl<'hir> Visitor<'hir> for CheckContextlessAttributesVisitor {}
