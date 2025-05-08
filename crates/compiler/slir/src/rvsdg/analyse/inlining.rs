use rustc_hash::FxHashSet;

use crate::rvsdg::analyse::ptr_in_struct::ptr_in_struct;
use crate::rvsdg::{Node, Rvsdg};
use crate::ty::TypeKind;
use crate::{Function, Module};

pub struct InlineOptions {
    pub inline_ptr_ret: bool,
    pub inline_ptr_in_struct_arg: bool,
    pub inline_ptr_to_ptr_arg: bool,
}

impl Default for InlineOptions {
    fn default() -> Self {
        Self {
            inline_ptr_ret: true,
            inline_ptr_in_struct_arg: true,
            inline_ptr_to_ptr_arg: true,
        }
    }
}

/// Returns the set of functions that must always be inlined, regardless of the context in which a
/// specific function application occurs.
pub fn always_inlined(module: &Module, options: &InlineOptions) -> FxHashSet<Function> {
    let mut always_inlined = FxHashSet::default();

    'outer: for (function, sig) in module.fn_sigs.iter() {
        if options.inline_ptr_ret && sig.ret_ty.map(|ty| module.ty[ty].is_ptr()).unwrap_or(false) {
            always_inlined.insert(*function);

            continue;
        }

        if options.inline_ptr_in_struct_arg {
            for arg in sig.args.iter() {
                if let TypeKind::Struct(struct_handle) = &module.ty[arg.ty]
                    && ptr_in_struct(module, *struct_handle)
                {
                    always_inlined.insert(*function);

                    continue 'outer;
                }
            }
        }

        if options.inline_ptr_to_ptr_arg {
            for arg in sig.args.iter() {
                if let TypeKind::Ptr(pointee_ty) = &module.ty[arg.ty]
                    && module.ty[*pointee_ty].is_ptr()
                {
                    always_inlined.insert(*function);

                    continue 'outer;
                }
            }
        }
    }

    always_inlined
}

pub struct InlineContext<'a> {
    module: &'a Module,
    rvsdg: &'a Rvsdg,
    options: InlineOptions,
    always_inline: FxHashSet<Function>,
}

impl<'a> InlineContext<'a> {
    pub fn new(module: &'a Module, rvsdg: &'a Rvsdg, options: InlineOptions) -> Self {
        Self {
            module,
            rvsdg,
            always_inline: always_inlined(module, &options),
            options,
        }
    }

    pub fn needs_inlining(&self, apply_node: Node) -> bool {
        todo!()
    }
}
