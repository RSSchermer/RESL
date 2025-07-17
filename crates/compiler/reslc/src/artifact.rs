use std::fs::File;

use ar::{GnuBuilder, Header};
use slir::cfg::Cfg;
use slir::rvsdg::Rvsdg;
use slir::{Module, Symbol};

use crate::context::ReslContext;

pub const MODULE_IDENTIFIER: &'static str = "module";
pub const CFG_IDENTIFIER: &'static str = "cfg";
pub const RVSDG_INITIAL_IDENTIFIER: &'static str = "rvsdg_initial";
pub const RVSDG_TRANSFORMED_IDENTIFIER: &'static str = "rvsdg_transformed";

pub struct SlirArtifactBuilderConfig {
    pub module_name: slir::Symbol,
    pub include_rvsdg_initial: bool,
    pub include_rvsdg_transformed: bool,
}

pub struct SlirArtifactBuilder {
    inner: GnuBuilder<File>,
    module_name: Symbol,
    module_identifier: Vec<u8>,
    cfg_identifier: Vec<u8>,
    rvsdg_initial_identifier: Option<Vec<u8>>,
    rvsdg_transformed_identifier: Option<Vec<u8>>,
}

impl SlirArtifactBuilder {
    pub fn new(cx: &ReslContext, config: SlirArtifactBuilderConfig) -> Self {
        let SlirArtifactBuilderConfig {
            module_name,
            include_rvsdg_initial,
            include_rvsdg_transformed,
        } = config;

        let mut filename = cx
            .tcx()
            .sess
            .io
            .output_dir
            .clone()
            .expect("not output directory specified");

        filename.push(format!("{}.slir", module_name));

        let file = File::create(filename).expect("failed to create slir artifact file");

        let module_identifier = MODULE_IDENTIFIER.as_bytes().to_vec();
        let cfg_identifier = CFG_IDENTIFIER.as_bytes().to_vec();
        let rvsdg_initial_identifier =
            include_rvsdg_initial.then(|| RVSDG_INITIAL_IDENTIFIER.as_bytes().to_vec());
        let rvsdg_transformed_identifier =
            include_rvsdg_transformed.then(|| RVSDG_TRANSFORMED_IDENTIFIER.as_bytes().to_vec());

        let mut identifiers = vec![module_identifier.clone(), cfg_identifier.clone()];

        if let Some(rvsdg_initial_identifier) = &rvsdg_initial_identifier {
            identifiers.push(rvsdg_initial_identifier.clone());
        }

        if let Some(rvsdg_transformed_identifier) = &rvsdg_transformed_identifier {
            identifiers.push(rvsdg_transformed_identifier.clone());
        }

        let inner = GnuBuilder::new(file, identifiers);

        SlirArtifactBuilder {
            inner,
            module_name,
            module_identifier,
            cfg_identifier,
            rvsdg_initial_identifier,
            rvsdg_transformed_identifier,
        }
    }

    pub fn append_cfg(&mut self, cfg: &Cfg) {
        let encoding = bincode::serde::encode_to_vec(cfg, bincode::config::standard())
            .expect("failed to encode SLIR Control-Flow Graph");

        self.inner
            .append(
                &Header::new(self.cfg_identifier.clone(), encoding.len() as u64),
                encoding.as_slice(),
            )
            .expect("failed to append SLIR Control-Flow Graph to SLIR artifact archive");
    }

    pub fn maybe_append_rvsdg_initial(&mut self, rvsdg: &Rvsdg) {
        if let Some(identifier) = self.rvsdg_initial_identifier.clone() {
            let encoding = bincode::serde::encode_to_vec(rvsdg, bincode::config::standard())
                .expect("failed to encode SLIR RVSDG-initial");

            self.inner
                .append(
                    &Header::new(identifier, encoding.len() as u64),
                    encoding.as_slice(),
                )
                .expect("failed to append SLIR RVSDG-initial to SLIR artifact archive");
        }
    }

    pub fn maybe_append_rvsdg_transformed(&mut self, rvsdg: &Rvsdg) {
        if let Some(identifier) = self.rvsdg_transformed_identifier.clone() {
            let encoding = bincode::serde::encode_to_vec(rvsdg, bincode::config::standard())
                .expect("failed to encode SLIR RVSDG-transformed");

            self.inner
                .append(
                    &Header::new(identifier, encoding.len() as u64),
                    encoding.as_slice(),
                )
                .expect("failed to append SLIR RVSDG-transformed to SLIR artifact archive");
        }
    }

    pub fn finish(mut self, module: &Module) {
        assert_eq!(
            module.name, self.module_name,
            "module name must match the module name with which this builder was initialized"
        );

        let encoding = bincode::serde::encode_to_vec(&module, bincode::config::standard())
            .expect("failed to encode SLIR module");

        self.inner
            .append(
                &Header::new(self.module_identifier, encoding.len() as u64),
                encoding.as_slice(),
            )
            .expect("failed to append SLIR module to SLIR artifact archive");
    }
}
