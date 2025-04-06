//! Locals are in a private module as updating `LocalRef::Operand` has to
//! be careful wrt to subtyping. To deal with this we only allow updates by using
//! `FunctionCx::overwrite_local` which handles it automatically.

use std::ops::{Index, IndexMut};
use index_vec::IndexVec;
use stable_mir::mir;
use tracing::{debug, warn};

use crate::slir_build_2::mir::{FunctionCx, LocalRef};
use crate::slir_build_2::traits::BuilderMethods;

pub(super) struct Locals<V> {
    values: IndexVec<mir::Local, LocalRef<V>>,
}

impl<V> Index<mir::Local> for Locals<V> {
    type Output = LocalRef<V>;

    #[inline]
    fn index(&self, index: mir::Local) -> &LocalRef<V> {
        &self.values[index]
    }
}

impl<V> Locals<V> {
    pub(super) fn empty() -> Locals<V> {
        Locals {
            values: IndexVec::default(),
        }
    }

    pub(super) fn indices(&self) -> impl DoubleEndedIterator<Item = mir::Local> + Clone + '_ {
        self.values.indices()
    }
}

impl<'a, Bx: BuilderMethods<'a>> FunctionCx<'a, Bx> {
    pub(super) fn initialize_locals(&mut self, values: Vec<LocalRef<Bx::Value>>) {
        assert!(self.locals.values.is_empty());

        self.locals.values = IndexVec::from_vec(values);
    }

    pub(super) fn overwrite_local(
        &mut self,
        local: mir::Local,
        mut value: LocalRef<Bx::Value>,
    ) {
        match value {
            LocalRef::Place(_) | LocalRef::UnsizedPlace(_) | LocalRef::PendingOperand => (),
            LocalRef::Operand(ref mut op) => {
                let local_ty = self.mir.locals()[local].ty;

                if local_ty != op.layout.ty {
                    // FIXME(#112651): This can be changed to an ICE afterwards.
                    debug!("updating type of operand due to subtyping");

                    op.layout.ty = local_ty;
                }
            }
        };

        self.locals.values[local] = value;
    }
}
