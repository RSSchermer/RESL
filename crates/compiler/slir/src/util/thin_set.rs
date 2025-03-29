use std::slice;

use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

/// Set-like that just wraps a `ThinVec`.
///
/// Intended for very small sets only (where linear searching the entire set is cheap), e.g. RVSDG
/// output-value users.
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct ThinSet<T> {
    inner: ThinVec<T>,
}

impl<T> ThinSet<T>
where
    T: PartialEq,
{
    pub fn contains(&self, value: &T) -> bool {
        self.inner.iter().find(|u| *u == value).is_some()
    }

    pub(crate) fn insert(&mut self, value: T) {
        if let Some(i) = self.inner.iter().position(|u| u == &value) {
            self.inner[i] = value;
        } else {
            self.inner.push(value);
        }
    }

    pub(crate) fn remove(&mut self, user: &T) {
        if let Some(i) = self.inner.iter().position(|u| u == user) {
            self.inner.swap_remove(i);
        }
    }
}

impl<T> ThinSet<T> {
    pub fn new() -> Self {
        ThinSet {
            inner: ThinVec::new(),
        }
    }

    pub fn iter(&self) -> slice::Iter<T> {
        self.inner.iter()
    }
}

impl<'a, T> IntoIterator for &'a ThinSet<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> Default for ThinSet<T> {
    fn default() -> Self {
        ThinSet {
            inner: Default::default(),
        }
    }
}
