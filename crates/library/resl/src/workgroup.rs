pub use resl_macros::workgroup_shared;

#[repr(transparent)]
pub struct Workgroup<T> {
    _init: T,
}

impl<T> Workgroup<T> {
    pub unsafe fn as_ref_unchecked(&self) -> &T {
        panic!("workgroup shared memory data is only available in shader running in a GPU context");
    }

    pub unsafe fn as_mut_unchecked(&self) -> &mut T {
        panic!("workgroup shared memory data is only available in shader running in a GPU context");
    }
}

/// This is a private helper function. `Workgroup` handles are only meant to be initialized by the
/// compiler.
#[doc(hidden)]
pub const unsafe fn __init_workgroup<T>(init: T) -> Workgroup<T> {
    Workgroup { _init: init }
}
