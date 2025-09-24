pub mod resource;
pub mod workgroup;

pub use resl_macros::{compute, fragment, gpu, shader_io, shader_module, vertex};

pub mod prelude {
    pub use super::resource::{resource, Resource, Storage, StorageMut, Uniform};
    pub use super::workgroup::{workgroup_shared, Workgroup};
    pub use super::{compute, fragment, gpu, shader_io, shader_module, vertex};
}
