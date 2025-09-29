#![feature(core_intrinsics)]

pub mod resource;
pub mod workgroup;
pub mod mem;

pub use resl_macros::{compute, fragment, gpu, shader_io, shader_module, vertex};

pub mod prelude {
    pub use super::mem::{Uniform, Storage, StorageMut, Workgroup};
    pub use super::resource::{resource, Resource};
    pub use super::workgroup::{workgroup_shared};
    pub use super::{compute, fragment, gpu, shader_io, shader_module, vertex};
}
