use stable_mir::abi::FnAbi;
use stable_mir::mir::mono::Instance;
use stable_mir::ty::Span;

use super::BackendTypes;
use crate::stable_cg::mir::operand::OperandRef;

pub trait IntrinsicCallBuilderMethods: BackendTypes {
    /// Remember to add all intrinsics here, in `compiler/rustc_hir_analysis/src/check/mod.rs`,
    /// and in `library/core/src/intrinsics.rs`; if you need access to any LLVM intrinsics,
    /// add them to `compiler/rustc_codegen_llvm/src/context.rs`.
    /// Returns `Err` if another instance should be called instead. This is used to invoke
    /// intrinsic default bodies in case an intrinsic is not implemented by the backend.
    fn codegen_intrinsic_call(
        &mut self,
        instance: Instance,
        fn_abi: &FnAbi,
        args: &[OperandRef<Self::Value>],
        llresult: Self::Value,
        span: Span,
    ) -> Result<(), Instance>;
}
