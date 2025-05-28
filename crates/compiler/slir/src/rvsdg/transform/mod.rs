pub mod dead_connection_elimination;
pub mod inlining;
pub mod proxy_node_elimination;
pub mod scalar_replacement;

use crate::rvsdg::transform::dead_connection_elimination::entry_points_eliminate_dead_connections;
use crate::rvsdg::transform::inlining::entry_points_inline_exhaustive;
use crate::rvsdg::transform::proxy_node_elimination::entry_points_eliminate_proxy_nodes;
use crate::rvsdg::transform::scalar_replacement::entry_points_scalar_replacement;
use crate::rvsdg::Rvsdg;
use crate::Module;

pub fn transform(module: &mut Module, rvsdg: &mut Rvsdg) {
    entry_points_inline_exhaustive(module, rvsdg);
    entry_points_scalar_replacement(module, rvsdg);
    entry_points_eliminate_proxy_nodes(module, rvsdg);
    entry_points_eliminate_dead_connections(module, rvsdg);
}
