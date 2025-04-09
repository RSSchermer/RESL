use std::fs::File;
use std::io::Read;

use ar::Archive;
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_session::config::CrateType;
use rustc_session::output::out_filename;
use rustc_smir::rustc_internal::run;
use rustc_span::def_id::{StableCrateId, LOCAL_CRATE};
use rustc_span::Symbol;

use crate::compiler::LIB_MODULE_FILENAME;
use crate::context::{ReslContext as Cx, ReslContext};
use crate::monomorphize::collect_shader_module_codegen_units;
use crate::slir_build2;
use crate::slir_build::build_shader_module;

fn gather_and_import_dependencies(
    rcx: &ReslContext,
    module: &mut slir::Module,
    cfg: &mut slir::cfg::Cfg,
) {
    let dependencies = slir::dependencies::gather_dependencies(cfg);

    import_dependencies(rcx, module, cfg, dependencies);
}

fn import_dependencies(
    rcx: &ReslContext,
    module: &mut slir::Module,
    cfg: &mut slir::cfg::Cfg,
    dependencies: FxHashMap<slir::Symbol, FxHashSet<slir::Function>>,
) {
    for (name, functions) in dependencies.iter() {
        let (dependency_module, dependency_cfg) = load_dependency(rcx, *name, functions);

        for function in functions {
            // Note that this will also automatically import the types and structs used by the
            // imported function, so we don't have to import these separately. Library modules also
            // cannot contain statics, so we don't have to worry about importing those.
            slir::import_cfg::import_fn_cfg(
                (&dependency_module, &dependency_cfg),
                (module, cfg),
                *function,
            );
        }
    }
}

/// Loads a dependency from disk and loads and imports its transitive dependencies.
fn load_dependency(
    rcx: &ReslContext,
    dependency: slir::Symbol,
    functions: &FxHashSet<slir::Function>,
) -> (slir::Module, slir::cfg::Cfg) {
    // We're currently using stable crate names as module names
    let crate_name = Symbol::intern(dependency.as_str());
    let crate_num = rcx.crate_num_for_name(crate_name);
    let filename = &rcx
        .tcx()
        .used_crate_source(crate_num)
        .rlib
        .as_ref()
        .unwrap()
        .0;

    let mut archive = Archive::new(File::open(filename).unwrap());

    while let Some(Ok(mut entry)) = archive.next_entry() {
        if entry.header().identifier() == LIB_MODULE_FILENAME.as_bytes() {
            let mut bytes = Vec::with_capacity(entry.header().size() as usize + 1);

            entry.read_to_end(&mut bytes).unwrap();

            let ((mut module, mut cfg), _) =
                bincode::serde::decode_from_slice::<(slir::Module, slir::cfg::Cfg), _>(
                    bytes.as_slice(),
                    bincode::config::standard(),
                )
                .unwrap();
            let transitive_dependencies = slir::dependencies::gather_dependencies_with_filter(
                &cfg,
                functions.iter().copied(),
            );

            import_dependencies(rcx, &mut module, &mut cfg, transitive_dependencies);

            return (module, cfg);
        }
    }

    panic!("failed to load module dependency: {}", dependency);
}

fn create_slir_artifact(cx: &Cx, module: &slir::Module, cfg: &slir::cfg::Cfg) {
    let mut filename = cx
        .tcx()
        .sess
        .io
        .output_dir
        .clone()
        .expect("not output directory specified");

    filename.push(format!("{}.slir", module.name));

    let mut file = File::create(filename).expect("failed to create slir artifact file");

    bincode::serde::encode_into_std_write((module, cfg), &mut file, bincode::config::standard())
        .expect("failed to encode slir artifact");
}

pub fn codegen_shader_modules(cx: &Cx) -> (slir::Module, slir::cfg::Cfg) {
    let (free_items, shader_modules) = collect_shader_module_codegen_units(cx);
    let crate_name = cx.tcx().crate_name(LOCAL_CRATE);

    run(cx.tcx(), || {
        // We create a separate SLIR artifact for every shader module in the current crate (every
        // `mod` item with a `#[resl::shader_module]` attribute). These artifacts are the basis for the
        // final compilation step (e.g. to WGSL, SPIRV, HLSL, etc.), which is typically done by a macro
        // in the second compilation phase (when the actual non-RESL Rust code gets compiled).
        for shader_module in shader_modules {
            let name = format!("{}-{}", crate_name, shader_module.name);
            let name = slir::Symbol::new(name);
            let (mut module, mut cfg) = slir_build2::build_shader_module(cx, name, &shader_module.items);

            gather_and_import_dependencies(cx, &mut module, &mut cfg);

            create_slir_artifact(cx, &module, &cfg);
        }


        // We also create one additional module for the whole crate for the SLIR of all "free functions"
        // (functions that are not part of a `mod` item with a `#[resl::shader_module]` attribute). This
        // will get stored as part of the crates `rlib`; it is used by `reslc` when compiling dependent
        // crates to import dependencies.
        let lib_name = slir::Symbol::from_ref(crate_name.as_str());

        build_shader_module(cx, lib_name, &free_items)
    }).unwrap()
}
