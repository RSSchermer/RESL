#![feature(core_intrinsics)]

pub mod mem;
pub mod primitive;
pub mod resource;
pub mod workgroup;

pub use resl_macros::{compute, fragment, gpu, shader_io, shader_module, vertex};

pub mod prelude {
    pub use super::mem::{Storage, StorageMut, Uniform, Workgroup};
    pub use super::primitive::*;
    pub use super::resource::{Resource, resource};
    pub use super::workgroup::workgroup_shared;
    pub use super::{compute, fragment, gpu, shader_io, shader_module, vertex};
}
