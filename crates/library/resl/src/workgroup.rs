pub use resl_macros::workgroup_shared;

use super::gpu;

#[gpu]
#[cfg_attr(reslc, reslc::mem_resource_ty)]
pub struct Workgroup<T> {
    _init: T,
}

impl<T> Workgroup<T> {
    #[gpu]
    #[cfg_attr(reslc, reslc::intrinsic(mem_resource_as_ref))]
    pub unsafe fn as_ref_unchecked(&self) -> &T {
        #[cfg(reslc)]
        core::intrinsics::abort();

        #[cfg(not(reslc))]
        panic!("workgroup shared memory data is only available in shader running in a GPU context");
    }

    #[gpu]
    #[cfg_attr(reslc, reslc::intrinsic(mem_resource_as_ref))]
    pub unsafe fn as_mut_unchecked(&self) -> &mut T {
        #[cfg(reslc)]
        core::intrinsics::abort();

        #[cfg(not(reslc))]
        panic!("workgroup shared memory data is only available in shader running in a GPU context");
    }
}

/// This is a private helper function. `Workgroup` handles are only meant to be initialized by the
/// compiler.
#[doc(hidden)]
pub const unsafe fn __init_workgroup<T>(init: T) -> Workgroup<T> {
    Workgroup { _init: init }
}
