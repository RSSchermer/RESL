//! An analysis to determine which locals require allocas and
//! which do not.

use std::mem;

use bit_set::BitSet;
use bit_vec::BitVec;
use rustc_data_structures::graph::dominators::Dominators;
use rustc_middle::{bug, span_bug};
use rustc_public::mir::visit::{Location, MirVisitor, PlaceContext, PlaceRef};
use rustc_public::mir::{BasicBlockIdx, Local, LocalDecl, Place, Statement, StatementKind};
use tracing::debug;

use super::FunctionCx;
use crate::slir_build_2::traits::*;

pub(crate) fn non_ssa_locals<'a, 'tcx, Bx: BuilderMethods<'a, 'tcx>>(
    fx: &FunctionCx<'a, 'tcx, Bx>,
    traversal_order: &[BasicBlockIdx],
) -> BitSet<usize> {
    let mir = &fx.mir;
    let dominators = mir.blocks.dominators();
    let locals = mir
        .locals()
        .iter()
        .map(|decl| {
            let ty = decl.ty;
            let layout = ty.layout().unwrap();

            if layout.shape().is_1zst() {
                LocalKind::ZST
            } else {
                LocalKind::Unused
            }
        })
        .collect();

    let mut analyzer = LocalAnalyzer {
        fx,
        dominators,
        locals,
    };

    // Arguments get assigned to by means of the function being called
    for arg in mir.args_iter() {
        analyzer.define(arg, DefLocation::Argument);
    }

    // If there exists a local definition that dominates all uses of that local,
    // the definition should be visited first. Traverse blocks in an order that
    // is a topological sort of dominance partial order.
    for bb in traversal_order.iter().copied() {
        let data = &mir.blocks[bb];

        analyzer.visit_basic_block_data(bb, data);
    }

    let mut non_ssa_locals = BitSet::from_bit_vec(BitVec::from_elem(analyzer.locals.len(), false));

    for (local, kind) in analyzer.locals.iter().enumerate() {
        if matches!(kind, LocalKind::Memory) {
            non_ssa_locals.insert(local);
        }
    }

    non_ssa_locals
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum LocalKind {
    ZST,
    /// A local that requires an alloca.
    Memory,
    /// A scalar or a scalar pair local that is neither defined nor used.
    Unused,
    /// A scalar or a scalar pair local with a single definition that dominates all uses.
    SSA(DefLocation),
}

struct LocalAnalyzer<'a, 'b, 'tcx, Bx: BuilderMethods<'b, 'tcx>> {
    fx: &'a FunctionCx<'b, 'tcx, Bx>,
    dominators: &'a Dominators<mir::BasicBlock>,
    locals: Vec<LocalKind>,
}

impl<'a, 'b, 'tcx, Bx: BuilderMethods<'b, 'tcx>> LocalAnalyzer<'a, 'b, 'tcx, Bx> {
    fn define(&mut self, local: usize, location: DefLocation) {
        let fx = self.fx;
        let kind = &mut self.locals[local];
        let decl = &fx.mir.locals()[local];

        match *kind {
            LocalKind::ZST => {}
            LocalKind::Memory => {}
            LocalKind::Unused => {
                let layout = decl
                    .ty
                    .layout()
                    .expect("layout must be known during codegen");

                *kind =
                    if fx.cx.is_backend_immediate(layout) || fx.cx.is_backend_scalar_pair(layout) {
                        LocalKind::SSA(location)
                    } else {
                        LocalKind::Memory
                    };
            }
            LocalKind::SSA(_) => *kind = LocalKind::Memory,
        }
    }

    fn process_place(
        &mut self,
        place: &Place,
        context: PlaceContext,
        location: Location,
    ) {
        let cx = self.fx.cx;

        if let Some((place_base, elem)) = place.last_projection() {
            let mut base_context = if context.is_mutating_use() {
                PlaceContext::MutatingUse(MutatingUseContext::Projection)
            } else {
                PlaceContext::NonMutatingUse(NonMutatingUseContext::Projection)
            };

            // Allow uses of projections that are ZSTs or from scalar fields.
            let is_consume = matches!(
                context,
                PlaceContext::NonMutatingUse(
                    NonMutatingUseContext::Copy | NonMutatingUseContext::Move,
                )
            );
            if is_consume {
                let base_ty = place_base.ty(self.fx.mir, cx.tcx());
                let base_ty = self.fx.monomorphize(base_ty);

                // ZSTs don't require any actual memory access.
                let elem_ty = base_ty
                    .projection_ty(cx.tcx(), self.fx.monomorphize(elem))
                    .ty;
                let span = self.fx.mir.local_decls[place.local].source_info.span;
                if cx.spanned_layout_of(elem_ty, span).is_zst() {
                    return;
                }

                if let mir::ProjectionElem::Field(..) = elem {
                    let layout = cx.spanned_layout_of(base_ty.ty, span);
                    if cx.is_backend_immediate(layout) || cx.is_backend_scalar_pair(layout) {
                        // Recurse with the same context, instead of `Projection`,
                        // potentially stopping at non-operand projections,
                        // which would trigger `not_ssa` on locals.
                        base_context = context;
                    }
                }
            }

            if let mir::ProjectionElem::Deref = elem {
                // Deref projections typically only read the pointer.
                base_context = PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy);
            }

            self.process_place(&place_base, base_context, location);
            // HACK(eddyb) this emulates the old `visit_projection_elem`, this
            // entire `visit_place`-like `process_place` method should be rewritten,
            // now that we have moved to the "slice of projections" representation.
            if let mir::ProjectionElem::Index(local) = elem {
                self.visit_local(
                    local,
                    PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy),
                    location,
                );
            }
        } else {
            self.visit_local(place.local, context, location);
        }
    }
}

impl<'a, 'b, 'tcx, Bx: BuilderMethods<'b, 'tcx>> MirVisitor<'tcx>
    for LocalAnalyzer<'a, 'b, 'tcx, Bx>
{
    fn visit_statement(&mut self, stmt: &Statement, location: Location) {
        if let StatementKind::Assign(place, rvalue) = &stmt.kind {
            debug!("visit_assign(place={:?}, rvalue={:?})", place, rvalue);

            if place.projection.is_empty() {
                let local = place.local;

                self.define(local, DefLocation::Assignment(location));

                if self.locals[local] != LocalKind::Memory {
                    let decl_span = self.fx.mir.locals()[local].span;

                    if !self.fx.rvalue_creates_operand(rvalue, decl_span) {
                        self.locals[local] = LocalKind::Memory;
                    }
                }
            } else {
                self.visit_place(place, PlaceContext::MutatingUse(MutatingUseContext::Store), location);
            }

            self.visit_rvalue(rvalue, location);
        } else {
            self.super_statement(stmt, location)
        }
    }

    fn visit_place(&mut self, place: &Place, context: PlaceContext, location: Location) {
        debug!("visit_place(place={:?}, context={:?})", place, context);
        self.process_place(&place, context, location);
    }

    fn visit_local(&mut self, local: &Local, context: PlaceContext, location: Location) {
        let local = *local;

        match context {
            PlaceContext::MutatingUse(MutatingUseContext::Call) => {
                let call = location.block;

                let TerminatorKind::Call { target, .. } =
                    self.fx.mir.blocks[call].terminator().kind
                else {
                    bug!()
                };

                self.define(local, DefLocation::CallReturn { call, target });
            }

            PlaceContext::NonUse(_)
            | PlaceContext::NonMutatingUse(NonMutatingUseContext::PlaceMention)
            | PlaceContext::MutatingUse(MutatingUseContext::Retag) => {}

            PlaceContext::NonMutatingUse(
                NonMutatingUseContext::Copy | NonMutatingUseContext::Move,
            ) => match &mut self.locals[local] {
                LocalKind::ZST => {}
                LocalKind::Memory => {}
                LocalKind::SSA(def) if def.dominates(location, self.dominators) => {}
                // Reads from uninitialized variables (e.g., in dead code, after
                // optimizations) require locals to be in (uninitialized) memory.
                // N.B., there can be uninitialized reads of a local visited after
                // an assignment to that local, if they happen on disjoint paths.
                kind @ (LocalKind::Unused | LocalKind::SSA(_)) => {
                    *kind = LocalKind::Memory;
                }
            },

            PlaceContext::MutatingUse(
                MutatingUseContext::Store
                | MutatingUseContext::Deinit
                | MutatingUseContext::SetDiscriminant
                | MutatingUseContext::AsmOutput
                | MutatingUseContext::Borrow
                | MutatingUseContext::RawBorrow
                | MutatingUseContext::Projection,
            )
            | PlaceContext::NonMutatingUse(
                NonMutatingUseContext::Inspect
                | NonMutatingUseContext::SharedBorrow
                | NonMutatingUseContext::FakeBorrow
                | NonMutatingUseContext::RawBorrow
                | NonMutatingUseContext::Projection,
            ) => {
                self.locals[local] = LocalKind::Memory;
            }

            PlaceContext::MutatingUse(MutatingUseContext::Drop) => {
                let kind = &mut self.locals[local];
                if *kind != LocalKind::Memory {
                    let ty = self.fx.mir.locals()[local].ty;

                    let ty = self.fx.monomorphize(ty);
                    if self.fx.cx.type_needs_drop(ty) {
                        // Only need the place if we're actually dropping it.
                        *kind = LocalKind::Memory;
                    }
                }
            }

            PlaceContext::MutatingUse(MutatingUseContext::Yield) => bug!(),
        }
    }
}
