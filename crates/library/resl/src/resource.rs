use std::mem;

pub use resl_macros::resource;

macro_rules! immutable_memory_space {
    ($name:ident, $error_name:literal) => {
        pub struct $name<T> {
            _marker: std::marker::PhantomData<T>,
        }

        impl<T> AsRef<T> for $name<T> {
            fn as_ref(&self) -> &T {
                panic!(
                    "{} memory data is only available in shader running in a GPU context",
                    $error_name
                )
            }
        }

        impl<T> std::borrow::Borrow<T> for $name<T> {
            fn borrow(&self) -> &T {
                self.as_ref()
            }
        }

        impl<T> std::ops::Deref for $name<T> {
            type Target = T;

            fn deref(&self) -> &Self::Target {
                self.as_ref()
            }
        }
    };
}

immutable_memory_space!(Uniform, "uniform");
immutable_memory_space!(Storage, "storage");

pub struct StorageMut<T> {
    _marker: std::marker::PhantomData<T>,
}

impl<T> StorageMut<T> {
    pub unsafe fn as_storage_unchecked(&self) -> &Storage<T> {
        mem::transmute(self)
    }

    pub unsafe fn as_ref_unchecked(&self) -> &T {
        panic!("storage memory data is only available in shader running in a GPU context");
    }

    pub unsafe fn as_mut_unchecked(&mut self) -> &mut T {
        panic!("storage memory data is only available in shader running in a GPU context");
    }
}

mod seal {
    pub trait Sealed {}
}

pub trait Resource: seal::Sealed {}

impl<T> seal::Sealed for Uniform<T> {}
impl<T> Resource for Uniform<T> {}

impl<T> seal::Sealed for Storage<T> {}
impl<T> Resource for Storage<T> {}

impl<T> seal::Sealed for StorageMut<T> {}
impl<T> Resource for StorageMut<T> {}
