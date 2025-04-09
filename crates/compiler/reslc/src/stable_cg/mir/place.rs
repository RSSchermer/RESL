use rustc_middle::mir::tcx::PlaceTy;
use rustc_middle::{bug, ty};
use stable_mir::abi::{FieldsShape, TyAndLayout};
use stable_mir::mir;
use stable_mir::mir::Mutability;
use stable_mir::target::MachineSize;
use stable_mir::ty::{Align, RigidTy, Size, Ty, TyKind};
use tracing::{debug, instrument};

use super::operand::OperandValue;
use super::{FunctionCx, LocalRef};
use crate::stable_cg::layout::TyAndLayoutExt;
use crate::stable_cg::traits::*;

/// The location and extra runtime properties of the place.
///
/// Typically found in a [`PlaceRef`] or an [`OperandValue::Ref`].
///
/// As a location in memory, this has no specific type. If you want to
/// load or store it using a typed operation, use [`Self::with_type`].
#[derive(Copy, Clone, Debug)]
pub struct PlaceValue<V> {
    /// A pointer to the contents of the place.
    pub llval: V,

    /// This place's extra data if it is unsized, or `None` if null.
    pub llextra: Option<V>,

    /// The alignment we know for this place.
    pub align: Align,
}

impl<V: CodegenObject> PlaceValue<V> {
    /// Constructor for the ordinary case of `Sized` types.
    ///
    /// Sets `llextra` to `None`.
    pub fn new_sized(llval: V, align: Align) -> PlaceValue<V> {
        PlaceValue {
            llval,
            llextra: None,
            align,
        }
    }

    /// Allocates a stack slot in the function for a value
    /// of the specified size and alignment.
    ///
    /// The allocation itself is untyped.
    pub fn alloca<'a, Bx: BuilderMethods<'a, Value = V>>(
        bx: &mut Bx,
        size: MachineSize,
        align: Align,
    ) -> PlaceValue<V> {
        let llval = bx.alloca(size, align);

        PlaceValue::new_sized(llval, align)
    }

    /// Creates a `PlaceRef` to this location with the given type.
    pub fn with_type(self, layout: TyAndLayout) -> PlaceRef<V> {
        PlaceRef { val: self, layout }
    }

    /// Gets the pointer to this place as an [`OperandValue::Immediate`]
    /// or, for those needing metadata, an [`OperandValue::Pair`].
    ///
    /// This is the inverse of [`OperandValue::deref`].
    pub fn address(self) -> OperandValue<V> {
        if let Some(llextra) = self.llextra {
            OperandValue::Pair(self.llval, llextra)
        } else {
            OperandValue::Immediate(self.llval)
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct PlaceRef<V> {
    /// The location and extra runtime properties of the place.
    pub val: PlaceValue<V>,

    /// The monomorphized type of this place, including variant information.
    ///
    /// You probably shouldn't use the alignment from this layout;
    /// rather you should use the `.val.align` of the actual place,
    /// which might be different from the type's normal alignment.
    pub layout: TyAndLayout,
}

impl<'a, V: CodegenObject> PlaceRef<V> {
    pub fn new_sized(llval: V, layout: TyAndLayout) -> PlaceRef<V> {
        PlaceRef::new_sized_aligned(llval, layout, layout.layout.shape().abi_align)
    }

    pub fn new_sized_aligned(llval: V, layout: TyAndLayout, align: Align) -> PlaceRef<V> {
        assert!(layout.layout.shape().is_sized());

        PlaceValue::new_sized(llval, align).with_type(layout)
    }

    // FIXME(eddyb) pass something else for the name so no work is done
    // unless LLVM IR names are turned on (e.g. for `--emit=llvm-ir`).
    pub fn alloca<Bx: BuilderMethods<'a, Value = V>>(bx: &mut Bx, layout: TyAndLayout) -> Self {
        Self::alloca_size(bx, layout.layout.shape().size, layout)
    }

    pub fn alloca_size<Bx: BuilderMethods<'a, Value = V>>(
        bx: &mut Bx,
        size: MachineSize,
        layout: TyAndLayout,
    ) -> Self {
        assert!(
            layout.layout.shape().is_sized(),
            "tried to statically allocate unsized place"
        );

        PlaceValue::alloca(bx, size, layout.layout.shape().abi_align).with_type(layout)
    }

    /// Returns a place for an indirect reference to an unsized place.
    // FIXME(eddyb) pass something else for the name so no work is done
    // unless LLVM IR names are turned on (e.g. for `--emit=llvm-ir`).
    pub fn alloca_unsized_indirect<Bx: BuilderMethods<'a, Value = V>>(
        bx: &mut Bx,
        layout: TyAndLayout,
    ) -> Self {
        assert!(
            layout.layout.shape().is_unsized(),
            "tried to allocate indirect place for sized values"
        );

        let ptr_ty = Ty::new_ptr(layout.ty, Mutability::Mut);
        let ptr_layout = ptr_ty.layout().expect("must have know layout");

        Self::alloca(
            bx,
            TyAndLayout {
                ty: ptr_ty,
                layout: ptr_layout,
            },
        )
    }

    pub fn len<Cx: ConstCodegenMethods<Value = V>>(&self, cx: &Cx) -> V {
        let layout_shape = self.layout.layout.shape();

        if let FieldsShape::Array { count, .. } = &layout_shape.fields {
            if layout_shape.is_unsized() {
                assert_eq!(*count, 0);

                self.val.llextra.unwrap()
            } else {
                cx.const_usize(*count)
            }
        } else {
            bug!("unexpected layout `{:#?}` in PlaceRef::len", self.layout)
        }
    }
}

impl<'a, V: CodegenObject> PlaceRef<V> {
    /// Access a field, at a point when the value's case is known.
    pub fn project_field<Bx: BuilderMethods<'a, Value = V>>(self, bx: &mut Bx, ix: usize) -> Self {
        let field = self.layout.field(ix);
        let ptr = bx.gep(
            bx.backend_type(field),
            self.val.llval,
            &[bx.const_u32(ix as u32)],
        );
        let val = PlaceValue {
            llval: ptr,
            llextra: self.val.llextra,
            align: field.layout.shape().abi_align,
        };

        val.with_type(field)
    }

    pub fn project_index<Bx: BuilderMethods<'a, Value = V>>(
        &self,
        bx: &mut Bx,
        llindex: V,
    ) -> Self {
        let layout = self.layout.field(0);
        let llval = bx.inbounds_gep(bx.backend_type(layout), self.val.llval, &[llindex]);

        PlaceValue::new_sized(llval, layout.layout.shape().abi_align).with_type(layout)
    }

    //
    // /// Obtain the actual discriminant of a value.
    // #[instrument(level = "trace", skip(bx))]
    // pub fn codegen_get_discr<Bx: BuilderMethods<'a, 'tcx, Value = V>>(
    //     self,
    //     bx: &mut Bx,
    //     cast_to: Ty<'tcx>,
    // ) -> V {
    //     let dl = &bx.tcx().data_layout;
    //     let cast_to_layout = bx.cx().layout_of(cast_to);
    //     let cast_to = bx.cx().immediate_backend_type(cast_to_layout);
    //     if self.layout.is_uninhabited() {
    //         return bx.cx().const_poison(cast_to);
    //     }
    //     let (tag_scalar, tag_encoding, tag_field) = match self.layout.variants {
    //         Variants::Empty => unreachable!("we already handled uninhabited types"),
    //         Variants::Single { index } => {
    //             let discr_val = self
    //                 .layout
    //                 .ty
    //                 .discriminant_for_variant(bx.cx().tcx(), index)
    //                 .map_or(index.as_u32() as u128, |discr| discr.val);
    //             return bx.cx().const_uint_big(cast_to, discr_val);
    //         }
    //         Variants::Multiple {
    //             tag,
    //             ref tag_encoding,
    //             tag_field,
    //             ..
    //         } => (tag, tag_encoding, tag_field),
    //     };
    //
    //     // Read the tag/niche-encoded discriminant from memory.
    //     let tag = self.project_field(bx, tag_field);
    //     let tag_op = bx.load_operand(tag);
    //     let tag_imm = tag_op.immediate();
    //
    //     // Decode the discriminant (specifically if it's niche-encoded).
    //     match *tag_encoding {
    //         TagEncoding::Direct => {
    //             let signed = match tag_scalar.primitive() {
    //                 // We use `i1` for bytes that are always `0` or `1`,
    //                 // e.g., `#[repr(i8)] enum E { A, B }`, but we can't
    //                 // let LLVM interpret the `i1` as signed, because
    //                 // then `i1 1` (i.e., `E::B`) is effectively `i8 -1`.
    //                 Int(_, signed) => !tag_scalar.is_bool() && signed,
    //                 _ => false,
    //             };
    //             bx.intcast(tag_imm, cast_to, signed)
    //         }
    //         TagEncoding::Niche {
    //             untagged_variant,
    //             ref niche_variants,
    //             niche_start,
    //         } => {
    //             // Cast to an integer so we don't have to treat a pointer as a
    //             // special case.
    //             let (tag, tag_llty) = match tag_scalar.primitive() {
    //                 // FIXME(erikdesjardins): handle non-default addrspace ptr sizes
    //                 Pointer(_) => {
    //                     let t = bx.type_from_integer(dl.ptr_sized_integer());
    //                     let tag = bx.ptrtoint(tag_imm, t);
    //                     (tag, t)
    //                 }
    //                 _ => (tag_imm, bx.cx().immediate_backend_type(tag_op.layout)),
    //             };
    //
    //             let relative_max = niche_variants.end().as_u32() - niche_variants.start().as_u32();
    //
    //             // We have a subrange `niche_start..=niche_end` inside `range`.
    //             // If the value of the tag is inside this subrange, it's a
    //             // "niche value", an increment of the discriminant. Otherwise it
    //             // indicates the untagged variant.
    //             // A general algorithm to extract the discriminant from the tag
    //             // is:
    //             // relative_tag = tag - niche_start
    //             // is_niche = relative_tag <= (ule) relative_max
    //             // discr = if is_niche {
    //             //     cast(relative_tag) + niche_variants.start()
    //             // } else {
    //             //     untagged_variant
    //             // }
    //             // However, we will likely be able to emit simpler code.
    //             let (is_niche, tagged_discr, delta) = if relative_max == 0 {
    //                 // Best case scenario: only one tagged variant. This will
    //                 // likely become just a comparison and a jump.
    //                 // The algorithm is:
    //                 // is_niche = tag == niche_start
    //                 // discr = if is_niche {
    //                 //     niche_start
    //                 // } else {
    //                 //     untagged_variant
    //                 // }
    //                 let niche_start = bx.cx().const_uint_big(tag_llty, niche_start);
    //                 let is_niche = bx.icmp(IntPredicate::IntEQ, tag, niche_start);
    //                 let tagged_discr = bx
    //                     .cx()
    //                     .const_uint(cast_to, niche_variants.start().as_u32() as u64);
    //                 (is_niche, tagged_discr, 0)
    //             } else {
    //                 // The special cases don't apply, so we'll have to go with
    //                 // the general algorithm.
    //                 let relative_discr = bx.sub(tag, bx.cx().const_uint_big(tag_llty, niche_start));
    //                 let cast_tag = bx.intcast(relative_discr, cast_to, false);
    //                 let is_niche = bx.icmp(
    //                     IntPredicate::IntULE,
    //                     relative_discr,
    //                     bx.cx().const_uint(tag_llty, relative_max as u64),
    //                 );
    //                 (is_niche, cast_tag, niche_variants.start().as_u32() as u128)
    //             };
    //
    //             let tagged_discr = if delta == 0 {
    //                 tagged_discr
    //             } else {
    //                 bx.add(tagged_discr, bx.cx().const_uint_big(cast_to, delta))
    //             };
    //
    //             let discr = bx.select(
    //                 is_niche,
    //                 tagged_discr,
    //                 bx.cx()
    //                     .const_uint(cast_to, untagged_variant.as_u32() as u64),
    //             );
    //
    //             // In principle we could insert assumes on the possible range of `discr`, but
    //             // currently in LLVM this seems to be a pessimization.
    //
    //             discr
    //         }
    //     }
    // }
    //
    // /// Sets the discriminant for a new value of the given case of the given
    // /// representation.
    // pub fn codegen_set_discr<Bx: BuilderMethods<'a, 'tcx, Value = V>>(
    //     &self,
    //     bx: &mut Bx,
    //     variant_index: VariantIdx,
    // ) {
    //     if self
    //         .layout
    //         .for_variant(bx.cx(), variant_index)
    //         .is_uninhabited()
    //     {
    //         // We play it safe by using a well-defined `abort`, but we could go for immediate UB
    //         // if that turns out to be helpful.
    //         bx.abort();
    //         return;
    //     }
    //     match self.layout.variants {
    //         Variants::Empty => unreachable!("we already handled uninhabited types"),
    //         Variants::Single { index } => assert_eq!(index, variant_index),
    //
    //         Variants::Multiple {
    //             tag_encoding: TagEncoding::Direct,
    //             tag_field,
    //             ..
    //         } => {
    //             let ptr = self.project_field(bx, tag_field);
    //             let to = self
    //                 .layout
    //                 .ty
    //                 .discriminant_for_variant(bx.tcx(), variant_index)
    //                 .unwrap()
    //                 .val;
    //             bx.store_to_place(
    //                 bx.cx().const_uint_big(bx.cx().backend_type(ptr.layout), to),
    //                 ptr.val,
    //             );
    //         }
    //         Variants::Multiple {
    //             tag_encoding:
    //                 TagEncoding::Niche {
    //                     untagged_variant,
    //                     ref niche_variants,
    //                     niche_start,
    //                 },
    //             tag_field,
    //             ..
    //         } => {
    //             if variant_index != untagged_variant {
    //                 let niche = self.project_field(bx, tag_field);
    //                 let niche_llty = bx.cx().immediate_backend_type(niche.layout);
    //                 let BackendRepr::Scalar(scalar) = niche.layout.backend_repr else {
    //                     bug!("expected a scalar placeref for the niche");
    //                 };
    //                 // We are supposed to compute `niche_value.wrapping_add(niche_start)` wrapping
    //                 // around the `niche`'s type.
    //                 // The easiest way to do that is to do wrapping arithmetic on `u128` and then
    //                 // masking off any extra bits that occur because we did the arithmetic with too many bits.
    //                 let niche_value = variant_index.as_u32() - niche_variants.start().as_u32();
    //                 let niche_value = (niche_value as u128).wrapping_add(niche_start);
    //                 let niche_value = niche_value & niche.layout.size.unsigned_int_max();
    //
    //                 let niche_llval = bx.cx().scalar_to_backend(
    //                     Scalar::from_uint(niche_value, niche.layout.size),
    //                     scalar,
    //                     niche_llty,
    //                 );
    //                 OperandValue::Immediate(niche_llval).store(bx, niche);
    //             }
    //         }
    //     }
    // }
    //
    //
    // pub fn project_downcast<Bx: BuilderMethods<'a, 'tcx, Value = V>>(
    //     &self,
    //     bx: &mut Bx,
    //     variant_index: VariantIdx,
    // ) -> Self {
    //     let mut downcast = *self;
    //     downcast.layout = self.layout.for_variant(bx.cx(), variant_index);
    //     downcast
    // }
    //
    // pub fn project_type<Bx: BuilderMethods<'a, 'tcx, Value = V>>(
    //     &self,
    //     bx: &mut Bx,
    //     ty: Ty<'tcx>,
    // ) -> Self {
    //     let mut downcast = *self;
    //     downcast.layout = bx.cx().layout_of(ty);
    //     downcast
    // }

    pub fn storage_live<Bx: BuilderMethods<'a, Value = V>>(&self, bx: &mut Bx) {
        bx.lifetime_start(self.val.llval, self.layout.layout.shape().size);
    }

    pub fn storage_dead<Bx: BuilderMethods<'a, Value = V>>(&self, bx: &mut Bx) {
        bx.lifetime_end(self.val.llval, self.layout.layout.shape().size);
    }
}

impl<'a, Bx: BuilderMethods<'a>> FunctionCx<'a, Bx> {
    pub fn codegen_place(
        &mut self,
        bx: &mut Bx,
        place_ref: mir::visit::PlaceRef,
    ) -> PlaceRef<Bx::Value> {
        let cx = self.cx;

        let mut base = 0;

        let mut cg_base = match self.locals[place_ref.local] {
            LocalRef::Place(place) => place,
            LocalRef::UnsizedPlace(place) => bx.load_operand(place).deref(cx),
            LocalRef::Operand(..) => {
                if matches!(
                    place_ref.projection.first(),
                    Some(mir::ProjectionElem::Deref)
                ) {
                    base = 1;

                    let cg_base = self.codegen_consume(
                        bx,
                        mir::visit::PlaceRef {
                            local: place_ref.local,
                            projection: &[],
                        },
                    );

                    cg_base.deref(cx)
                } else {
                    bug!("using operand local as place");
                }
            }
            LocalRef::PendingOperand => {
                bug!("using still-pending operand local as place");
            }
        };

        for elem in place_ref.projection[base..].iter() {
            cg_base = match *elem {
                mir::ProjectionElem::Deref => bx.load_operand(cg_base).deref(cx),
                mir::ProjectionElem::Field(field, _) => cg_base.project_field(bx, field),
                mir::ProjectionElem::OpaqueCast(ty) => {
                    bug!("encountered OpaqueCast({ty}) in codegen")
                }
                mir::ProjectionElem::Subtype(..) => {
                    bug!("RESL does not support type casts")
                }
                mir::ProjectionElem::Index(index) => {
                    let index = &mir::Operand::Copy(mir::Place::from(index));
                    let index = self.codegen_operand(bx, index);
                    let llindex = index.immediate();

                    cg_base.project_index(bx, llindex)
                }
                mir::ProjectionElem::ConstantIndex {
                    offset,
                    from_end: false,
                    min_length: _,
                } => {
                    let lloffset = bx.const_usize(offset);

                    cg_base.project_index(bx, lloffset)
                }
                mir::ProjectionElem::ConstantIndex {
                    offset,
                    from_end: true,
                    min_length: _,
                } => {
                    let lloffset = bx.const_usize(offset);
                    let lllen = cg_base.len(cx);
                    let llindex = bx.sub(lllen, lloffset);

                    cg_base.project_index(bx, llindex)
                }
                mir::ProjectionElem::Subslice { from, to, from_end } => {
                    let mut subslice = cg_base.project_index(bx, bx.const_usize(from));

                    let TyKind::RigidTy(ty) = cg_base.layout.ty.kind() else {
                        bug!("type should be rigid")
                    };

                    let projected_ty = match ty {
                        RigidTy::Slice(..) => cg_base.layout.ty,
                        RigidTy::Array(inner, _) if !from_end => {
                            Ty::try_new_array(inner, to - from).unwrap()
                        }
                        RigidTy::Array(inner, size) if from_end => {
                            let size = size
                                .eval_target_usize()
                                .expect("expected subslice projection on fixed-size array");
                            let len = size - from - to;

                            Ty::try_new_array(inner, len).unwrap()
                        }
                        _ => bug!("cannot subslice non-array type: `{:?}`", cg_base.layout.ty),
                    };

                    subslice.layout = TyAndLayout::expect_from_ty(projected_ty);

                    if subslice.layout.layout.shape().is_unsized() {
                        assert!(from_end, "slice subslices should be `from_end`");

                        subslice.val.llextra =
                            Some(bx.sub(cg_base.val.llextra.unwrap(), bx.const_usize(from + to)));
                    }

                    subslice
                }
                mir::ProjectionElem::Downcast(v) => {
                    todo!()
                    //cg_base.project_downcast(bx, v)
                }
            };
        }

        cg_base
    }
}
