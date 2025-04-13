#![feature(
    assert_matches,
    if_let_guard,
    impl_trait_in_assoc_type,
    let_chains,
    rustc_private,
    trait_alias
)]
#![allow(unused_variables)]

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_codegen_ssa;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_fluent_macro;
extern crate rustc_hir;
extern crate rustc_interface;
#[macro_use]
extern crate rustc_smir;
extern crate core;
extern crate rustc_macros;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_monomorphize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate stable_mir;

mod attr;
mod check;
mod codegen;
mod compiler;
mod context;
mod hir_ext;
mod hir_ext_build;
mod monomorphize;
mod slir_build;
mod stable_cg;

use std::process::ExitCode;
use std::{env, process};

use rustc_driver::catch_with_exit_code;

rustc_fluent_macro::fluent_messages! { "../messages.ftl" }

fn main() -> ExitCode {
    let rustc_args = env::args().into_iter().skip(1).collect();

    let exit_code = catch_with_exit_code(|| {
        compiler::run(rustc_args);

        Ok(())
    });

    process::exit(exit_code)
}
