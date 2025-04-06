use rustc_middle::bug;
use stable_mir::abi::{Primitive, Scalar, TyAndLayout, VariantsShape};
use stable_mir::target::{MachineInfo, MachineSize};
use stable_mir::ty::{RigidTy, Ty, TyKind, UintTy};

pub trait TyAndLayoutExt {
    fn expect_from_ty(ty: Ty) -> Self;

    fn field(self, i: usize) -> TyAndLayout;
}

impl TyAndLayoutExt for TyAndLayout {
    fn expect_from_ty(ty: Ty) -> Self {
        let layout = ty.layout().expect("type should have valid layout during codegen");

        TyAndLayout {
            ty,
            layout,
        }
    }

    fn field(self, i: usize) -> TyAndLayout {
        enum TyMaybeWithLayout {
            Ty(Ty),
            TyAndLayout(TyAndLayout),
        }

        fn field_ty_or_layout(this: TyAndLayout, i: usize) -> TyMaybeWithLayout {
            let TyKind::RigidTy(ty) = this.ty.kind() else {
                bug!("only rigid types can project to fields")
            };

            match ty {
                // Types that don't have fields.
                RigidTy::Bool
                | RigidTy::Char
                | RigidTy::Int(..)
                | RigidTy::Uint(..)
                | RigidTy::Float(..)
                | RigidTy::FnPtr(..)
                | RigidTy::Never
                | RigidTy::FnDef(..)
                | RigidTy::CoroutineWitness(..)
                | RigidTy::Foreign(..)
                | RigidTy::Pat(..)
                | RigidTy::Dynamic(..) => {
                    bug!("TyAndLayout::field({:?}): not applicable", this)
                }

                // Types that are not supported by RESL.
                RigidTy::Ref(..)
                | RigidTy::RawPtr(..)
                | RigidTy::Dynamic(..)
                | RigidTy::Coroutine(..)
                | RigidTy::CoroutineClosure(..) => {
                    bug!("Type field projection not supported by RESL for {:?}", ty)
                }

                // Arrays and slices.
                RigidTy::Array(element, _) | RigidTy::Slice(element) => {
                    TyMaybeWithLayout::Ty(element)
                }
                RigidTy::Str => {
                    TyMaybeWithLayout::Ty(Ty::from_rigid_kind(RigidTy::Uint(UintTy::U8)))
                }

                // Closures.
                RigidTy::Closure(_, args) => {
                    // The last closure argument should be a tuple type representing its up-vars
                    let Some(tupled_upvars_ty) = args.0.last().and_then(|arg| arg.ty()) else {
                        bug!(
                            "Closure args ({:?}) should declare a tuple type for its up-vars",
                            args
                        )
                    };

                    field_ty_or_layout(
                        TyAndLayout {
                            ty: *tupled_upvars_ty,
                            layout: this.layout,
                        },
                        i,
                    )
                }

                // Tuples.
                RigidTy::Tuple(tys) => TyMaybeWithLayout::Ty(tys[i]),

                // ADTs.
                RigidTy::Adt(def, args) => {
                    match this.layout.shape().variants {
                        VariantsShape::Single { index } => {
                            let field = def.variant(index).unwrap().fields()[i];

                            TyMaybeWithLayout::Ty(field.ty_with_args(&args))
                        }
                        VariantsShape::Empty => bug!("there is no field in Variants::Empty types"),

                        // Discriminant field for enums (where applicable).
                        VariantsShape::Multiple { tag, .. } => {
                            assert_eq!(i, 0);

                            todo!()
                            //     TyAndLayout {
                            //         layout: tcx.mk_layout(LayoutData::scalar(cx, tag)),
                            //         ty: tag.primitive().to_ty(tcx),
                            //     }
                        }
                    }
                }
            }
        }

        match field_ty_or_layout(self, i) {
            TyMaybeWithLayout::Ty(field_ty) => {
                let Ok(layout) = field_ty.layout() else {
                    bug!(
                        "failed to get layout for `{field_ty}`: {e:?},\n\
                         despite it being a field (#{i}) of an existing layout: {this:#?}",
                    )
                };

                TyAndLayout {
                    ty: field_ty,
                    layout,
                }
            }
            TyMaybeWithLayout::TyAndLayout(field_layout) => field_layout,
        }
    }
}

pub trait ScalarExt {
    fn primitive(&self) -> Primitive;

    fn size(&self, target: &MachineInfo) -> MachineSize;
}

impl ScalarExt for Scalar {
    fn primitive(&self) -> &Primitive {
        match self {
            Scalar::Initialized { value, .. } => value,
            Scalar::Union { value } => value,
        }
    }

    fn size(&self, target: &MachineInfo) -> MachineSize {
        self.primitive().size(target)
    }
}
