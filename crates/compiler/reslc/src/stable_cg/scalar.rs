use std::num::NonZero;

use rustc_middle::bug;
use stable_mir::abi::{FloatLength, IntegerLength, Primitive, ValueAbi};
use stable_mir::mir::alloc::AllocId;
use stable_mir::target::{MachineInfo, MachineSize};
use stable_mir::ty::{Allocation, Prov, Size};
use stable_mir::{abi, Error};

use crate::stable_cg::layout::ScalarExt;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Scalar {
    U32(u32),
    I32(i32),
    F32(f32),
    Pointer(Pointer),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Pointer {
    pub alloc_id: AllocId,
    pub offset: u64,
}

impl Scalar {
    pub fn read_from_alloc(allocation: &Allocation, offset: usize, abi: abi::Scalar) -> Scalar {
        let size = abi.size(&MachineInfo::target());
        let end = offset + size.bytes();

        let data = allocation.read_partial_uint(offset..end).expect(
            "not enough data in allocation to satisfy the size requirements of the scala ABI",
        );

        match abi.primitive() {
            Primitive::Int {
                length: IntegerLength::I32,
                signed: true,
            } => Scalar::I32(i32::from_ne_bytes((data as u32).to_ne_bytes())),
            Primitive::Int {
                length: IntegerLength::I32,
                signed: false,
            } => Scalar::U32(data as u32),
            Primitive::Float {
                length: FloatLength::F32,
            } => Scalar::F32(f32::from_ne_bytes((data as u32).to_ne_bytes())),
            Primitive::Pointer(_) => {
                let prov_index = allocation
                    .provenance
                    .ptrs
                    .binary_search_by(|(offset, _)| offset.cmp(offset))
                    .expect("provenance not found for pointer");
                let (_, Prov(alloc_id)) = allocation.provenance.ptrs[prov_index];

                Scalar::Pointer(Pointer {
                    alloc_id,
                    offset: data as u64,
                })
            }
            _ => bug!("primitive type not supported by RESL"),
        }
    }
}
