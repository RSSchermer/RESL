use std::fs::File;

use ar::{GnuBuilder, Header};
use rustc_driver::{run_compiler, Callbacks, Compilation};
use rustc_interface::interface;
use rustc_metadata::fs::encode_and_write_metadata;
use rustc_metadata::METADATA_FILENAME;
use rustc_middle::ty::TyCtxt;
use rustc_session::config::{CrateType, DebugInfo};
use rustc_session::output::out_filename;
use rustc_span::def_id::LOCAL_CRATE;

use crate::codegen::codegen_shader_modules;
use crate::context::ReslContext;

pub const LIB_MODULE_FILENAME: &str = "lib.slir";

pub fn run(args: Vec<String>) {
    let mut compiler = ReslCompiler {};

    run_compiler(&args, &mut compiler);
}

pub struct ReslCompiler {}

impl Callbacks for ReslCompiler {
    fn config(&mut self, config: &mut interface::Config) {
        // Use the `register_tool` feature to register the `resl` tool to make rustc pass through
        // `#[resl::...]` attributes.
        let crate_attr = &mut config.opts.unstable_opts.crate_attr;
        crate_attr.push("feature(register_tool)".to_string());
        crate_attr.push("register_tool(resl)".to_string());

        // With `DebugInfo::Full`, codegen generates a bunch of extra alloca statements for locals
        // into which the function argument values get copied; it does this as a work-around to be
        // able to attach debug-info. For our purposes, this just adds noise, so set the debug-info
        // level to `DebugInfo::Limited`.
        config.opts.debuginfo = DebugInfo::Limited;

        // We never want to generate overflow panics code for the GPU, not even in debug mode, so
        // when compiling RESL we always disable these checks.
        config.opts.cg.overflow_checks = Some(false);

        // Disable certain MIR transformations
        config.opts.unstable_opts.inline_mir = Some(false);
        config.opts.unstable_opts.mir_enable_passes.extend([
            ("GVN".to_string(), false),
            // Disable raw pointer UB checks. We don't currently allow user code to use raw
            // pointers (only references), so this type UB should not be possible. The checks
            // also involve pointer casting operations that the backend cannot support, and if
            // the check were to fail, we would not be able to unwind/abort.
            ("CheckAlignment".to_string(), false),
            ("CheckNull".to_string(), false),
        ]);
    }

    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        compile_resl(tcx);

        Compilation::Stop
    }
}

fn create_rlib(tcx: TyCtxt, lib_module: &(slir::Module, slir::cfg::Cfg)) {
    let filename = out_filename(
        tcx.sess,
        CrateType::Rlib,
        tcx.output_filenames(()),
        tcx.crate_name(LOCAL_CRATE),
    );

    println!("writing archive: {:?}", filename.as_path());

    let out_file = File::create(filename.as_path()).unwrap();

    let mut builder = GnuBuilder::new(
        out_file,
        [METADATA_FILENAME, LIB_MODULE_FILENAME]
            .iter()
            .map(|name| name.as_bytes().to_vec())
            .collect(),
    );

    let (metadata, _) = encode_and_write_metadata(tcx);
    let raw_metadata = metadata.raw_data();

    builder
        .append(
            &Header::new(
                METADATA_FILENAME.as_bytes().to_vec(),
                raw_metadata.len() as u64,
            ),
            raw_metadata,
        )
        .unwrap();

    let raw_lib = bincode::serde::encode_to_vec(lib_module, bincode::config::standard()).unwrap();

    builder
        .append(
            &Header::new(
                LIB_MODULE_FILENAME.as_bytes().to_vec(),
                raw_lib.len() as u64,
            ),
            raw_lib.as_slice(),
        )
        .unwrap();

    builder.into_inner().unwrap();
}

fn compile_resl(tcx: TyCtxt) {
    let mut cx = ReslContext::new(tcx);

    cx.build_hir_ext();

    let lib_module = codegen_shader_modules(&cx);

    create_rlib(tcx, &lib_module);
}
