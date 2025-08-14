pub mod dead_connection_elimination;
pub mod enum_replacement;
pub mod inlining;
pub mod memory_promotion_and_legalization;
pub mod memory_transform;
pub mod pred_to_case_extraction;
pub mod pred_to_case_to_pred_merging;
pub mod proxy_node_elimination;
pub mod ptr_offset_elaboration;
pub mod ptr_offset_replacement;
pub mod scalar_replacement;
pub mod variable_pointer_emulation;

use crate::rvsdg::transform::dead_connection_elimination::entry_points_eliminate_dead_connections;
use crate::rvsdg::transform::inlining::entry_points_inline_exhaustive;
use crate::rvsdg::transform::memory_transform::entry_points_transform_memory;
use crate::rvsdg::transform::pred_to_case_extraction::entry_points_extract_pred_to_case;
use crate::rvsdg::transform::pred_to_case_to_pred_merging::entry_points_merge_pred_to_case_to_pred;
use crate::rvsdg::transform::ptr_offset_elaboration::entry_points_ptr_offset_elaboration;
use crate::rvsdg::transform::ptr_offset_replacement::entry_points_ptr_offset_replacement;
use crate::rvsdg::Rvsdg;
use crate::Module;

pub fn transform(module: &mut Module, rvsdg: &mut Rvsdg) {
    entry_points_inline_exhaustive(module, rvsdg);
    entry_points_ptr_offset_elaboration(module, rvsdg);
    entry_points_transform_memory(module, rvsdg);
    entry_points_ptr_offset_replacement(module, rvsdg);
    entry_points_extract_pred_to_case(module, rvsdg);
    entry_points_merge_pred_to_case_to_pred(module, rvsdg);
    entry_points_eliminate_dead_connections(module, rvsdg);
}
