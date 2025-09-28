use std::{marker, mem};

pub use resl_macros::resource;

use super::gpu;

#[gpu]
#[cfg_attr(reslc, reslc::mem_resource_ty)]
pub struct Uniform<T> {
    _marker: marker::PhantomData<T>,
}

impl<T> Uniform<T> {
    #[gpu]
    #[cfg_attr(reslc, reslc::intrinsic(mem_resource_as_ref))]
    fn as_ref_intrinsic(&self) -> &T {
        #[cfg(reslc)]
        core::intrinsics::abort();

        #[cfg(not(reslc))]
        panic!("storage memory data is only available in shader running in a GPU context")
    }
}

#[gpu]
impl<T> AsRef<T> for Uniform<T> {
    #[cfg_attr(reslc, reslc::intrinsic(mem_resource_as_ref))]
    fn as_ref(&self) -> &T {
        self.as_ref_intrinsic()
    }
}

#[gpu]
impl<T> std::borrow::Borrow<T> for Uniform<T> {
    fn borrow(&self) -> &T {
        self.as_ref_intrinsic()
    }
}

#[gpu]
impl<T> std::ops::Deref for Uniform<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref_intrinsic()
    }
}

#[gpu]
#[cfg_attr(reslc, reslc::mem_resource_ty)]
pub struct Storage<T>
where
    T: ?Sized,
{
    _marker: marker::PhantomData<T>,
}

impl<T> Storage<T>
where
    T: ?Sized,
{
    #[gpu]
    #[cfg_attr(reslc, reslc::intrinsic(mem_resource_as_ref))]
    fn as_ref_intrinsic(&self) -> &T {
        #[cfg(reslc)]
        core::intrinsics::abort();

        #[cfg(not(reslc))]
        panic!("storage memory data is only available in shader running in a GPU context")
    }
}

#[gpu]
impl<T> AsRef<T> for Storage<T>
where
    T: ?Sized,
{
    fn as_ref(&self) -> &T {
        self.as_ref_intrinsic()
    }
}

#[gpu]
impl<T> std::borrow::Borrow<T> for Storage<T>
where
    T: ?Sized,
{
    fn borrow(&self) -> &T {
        self.as_ref_intrinsic()
    }
}

#[gpu]
impl<T> std::ops::Deref for Storage<T>
where
    T: ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref_intrinsic()
    }
}

#[gpu]
#[cfg_attr(reslc, reslc::mem_resource_ty)]
pub struct StorageMut<T>
where
    T: ?Sized,
{
    _marker: marker::PhantomData<T>,
}

impl<T> StorageMut<T>
where
    T: ?Sized,
{
    #[gpu]
    #[cfg_attr(reslc, reslc::intrinsic(mem_resource_as_ref))]
    pub unsafe fn as_ref_unchecked(&self) -> &T {
        #[cfg(reslc)]
        core::intrinsics::abort();

        #[cfg(not(reslc))]
        panic!("storage memory data is only available in shader running in a GPU context");
    }

    #[gpu]
    #[cfg_attr(reslc, reslc::intrinsic(mem_resource_as_ref))]
    pub unsafe fn as_mut_unchecked(&mut self) -> &mut T {
        #[cfg(reslc)]
        core::intrinsics::abort();

        #[cfg(not(reslc))]
        panic!("storage memory data is only available in shader running in a GPU context");
    }
}

mod seal {
    pub trait Sealed {}
}

pub trait Resource: seal::Sealed {}

impl<T> seal::Sealed for Uniform<T> {}
impl<T> Resource for Uniform<T> {}

impl<T> seal::Sealed for Storage<T> where T: ?Sized {}
impl<T> Resource for Storage<T> where T: ?Sized {}

impl<T> seal::Sealed for StorageMut<T> where T: ?Sized {}
impl<T> Resource for StorageMut<T> where T: ?Sized {}
