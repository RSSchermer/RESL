use rustc_middle::span_bug;
use stable_mir::mir;
use stable_mir::mir::NonDivergingIntrinsic;
use tracing::instrument;

use super::{FunctionCx, LocalRef};
use crate::slir_build_2::traits::*;

impl<'a, Bx: BuilderMethods<'a>> FunctionCx<'a, Bx> {
    #[instrument(level = "debug", skip(self, bx))]
    pub(crate) fn codegen_statement(&mut self, bx: &mut Bx, statement: &mir::Statement) {
        match &statement.kind {
            mir::StatementKind::Assign(place, rvalue) => {
                if place.projection.is_empty() {
                    match self.locals[place.local] {
                        LocalRef::Place(cg_dest) => self.codegen_rvalue(bx, cg_dest, rvalue),
                        LocalRef::UnsizedPlace(cg_indirect_dest) => {
                            self.codegen_rvalue_unsized(bx, cg_indirect_dest, rvalue)
                        }
                        LocalRef::PendingOperand => {
                            let operand = self.codegen_rvalue_operand(bx, rvalue);
                            self.overwrite_local(place.local, LocalRef::Operand(operand));
                        }
                        LocalRef::Operand(op) => {
                            if !op.layout.layout.shape().is_1zst() {
                                span_bug!(statement.span, "operand {:?} already assigned", rvalue);
                            }

                            // If the type is zero-sized, it's already been set here,
                            // but we still need to make sure we codegen the operand
                            self.codegen_rvalue_operand(bx, rvalue);
                        }
                    }
                } else {
                    let cg_dest = self.codegen_place(
                        bx,
                        mir::visit::PlaceRef {
                            local: place.local,
                            projection: &place.projection,
                        },
                    );

                    self.codegen_rvalue(bx, cg_dest, rvalue);
                }
            }
            mir::StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                todo!()
                // self.codegen_place(bx, place.as_ref())
                //     .codegen_set_discr(bx, variant_index);
            }
            mir::StatementKind::Deinit(..) => {
                // For now, don't codegen this to anything. In the future it may be worth
                // experimenting with what kind of information we can emit to LLVM without hurting
                // perf here
            }
            mir::StatementKind::StorageLive(local) => {
                if let LocalRef::Place(cg_place) = self.locals[local] {
                    cg_place.storage_live(bx);
                } else if let LocalRef::UnsizedPlace(cg_indirect_place) = self.locals[local] {
                    cg_indirect_place.storage_live(bx);
                }
            }
            mir::StatementKind::StorageDead(local) => {
                if let LocalRef::Place(cg_place) = self.locals[local] {
                    cg_place.storage_dead(bx);
                } else if let LocalRef::UnsizedPlace(cg_indirect_place) = self.locals[local] {
                    cg_indirect_place.storage_dead(bx);
                }
            }
            mir::StatementKind::Intrinsic(NonDivergingIntrinsic::Assume(op)) => {
                todo!()
                // let op_val = self.codegen_operand(bx, op);
                // bx.assume(op_val.immediate());
            }
            mir::StatementKind::Intrinsic(NonDivergingIntrinsic::CopyNonOverlapping(
                mir::CopyNonOverlapping { count, src, dst },
            )) => {
                todo!()
                // let dst_val = self.codegen_operand(bx, dst);
                // let src_val = self.codegen_operand(bx, src);
                // let count = self.codegen_operand(bx, count).immediate();
                // let pointee_layout = dst_val
                //     .layout
                //     .pointee_info_at(bx, rustc_abi::Size::ZERO)
                //     .expect("Expected pointer");
                // let bytes = bx.mul(count, bx.const_usize(pointee_layout.size.bytes()));
                //
                // let align = pointee_layout.align;
                // let dst = dst_val.immediate();
                // let src = src_val.immediate();
                // bx.memcpy(dst, align, src, align, bytes, crate::MemFlags::empty());
            }
            mir::StatementKind::Coverage { .. }
            | mir::StatementKind::FakeRead(..)
            | mir::StatementKind::Retag { .. }
            | mir::StatementKind::AscribeUserType(..)
            | mir::StatementKind::ConstEvalCounter
            | mir::StatementKind::PlaceMention(..)
            | mir::StatementKind::Nop => {}
        }
    }
}
