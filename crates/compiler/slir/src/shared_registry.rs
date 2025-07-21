macro_rules! shared_registry {
    ($registry:ident, $token:ident, $data:ident, $data_ref:ident) => {
        #[derive(Clone, serde::Serialize, serde::Deserialize, Default, Debug)]
        pub struct $registry {
            #[serde(with = "crate::serde::arc_rwlock")]
            store: std::sync::Arc<std::sync::RwLock<indexmap::IndexSet<Box<$data>>>>,
        }
        
        impl $registry {
            pub fn register(&self, data: $data) -> $token {
                let current = self
                    .store
                    .read()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .get_index_of(&data);
        
                let index = if let Some(index) = current {
                    index
                } else {
                    self.store
                        .write()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .insert_full(Box::new(data))
                        .0
                };
        
                $token(index)
            }
        
            pub fn data(&self, token: $token) -> $data_ref {
                let store = self.store.read().unwrap_or_else(std::sync::PoisonError::into_inner);
                let boxed = store.get_index(token.0).expect("unregistered item");
                let ptr = boxed.as_ref() as *const $data;
        
                $data_ref {
                    ptr,
                    _marker: Default::default(),
                }
            }
        }
        
        #[derive(Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize, Debug)]
        pub struct $token(usize);
        
        impl $token {
            pub fn to_usize(&self) -> usize {
                self.0
            }
        }
        
        impl From<usize> for $token {
            fn from(value: usize) -> Self {
                $token(value)
            }
        }
        
        pub struct $data_ref<'a> {
            ptr: *const $data,
            _marker: std::marker::PhantomData<&'a $data>,
        }

        impl $data_ref<'static> {
            fn from_static(kind: &'static $data) -> Self {
                $data_ref {
                    ptr: kind as *const $data,
                    _marker: Default::default(),
                }
            }
        }

        impl Clone for $data_ref<'_> {
            fn clone(&self) -> Self {
                $data_ref {
                    ptr: self.ptr,
                    _marker: Default::default(),
                }
            }
        }

        impl Copy for $data_ref<'_> {}

        impl AsRef<$data> for $data_ref<'_> {
            fn as_ref(&self) -> &$data {
                // SAFETY
                //
                // The ref's lifetime ensures that the store with which it is associated cannot have
                // dropped. As the interface does not expose a mechanism for removing registered
                // items from the store, that implies that the `Box` the ref's pointer points to
                // will also not have dropped. Therefore, the pointer must still be valid.
                unsafe { &*self.ptr }
            }
        }

        impl std::ops::Deref for $data_ref<'_> {
            type Target = $data;

            fn deref(&self) -> &Self::Target {
                self.as_ref()
            }
        }
    };
}

pub use shared_registry;
