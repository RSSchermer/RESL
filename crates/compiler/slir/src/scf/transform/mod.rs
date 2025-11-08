use crate::Module;
use crate::scf::Scf;

pub mod remove_empty_else_blocks;

pub fn transform(module: &mut Module, scf: &mut Scf) {
    remove_empty_else_blocks::transform_entry_points(module, scf);
}
