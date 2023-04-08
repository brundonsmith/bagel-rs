use swc::config::{
    Config, ErrorConfig, ExperimentalOptions, JsMinifyFormatOptions, JsMinifyOptions, JscConfig,
    JscExperimental, JscOutputConfig, Options, RootMode,
};
use swc::{BoolConfig, BoolOrDataConfig, Compiler};
use swc_common::errors::{EmitterWriter, Handler};
use swc_common::Globals;
use swc_common::{sync::Lrc, FileName, FilePathMapping, SourceMap, GLOBALS};
use swc_config::config_types::MergingOption;
use swc_ecma_lints::config::LintConfig;
use swc_ecma_minifier::option::terser::TerserEcmaVersion;
use swc_ecma_minifier::option::MangleOptions;
use swc_ecma_parser::{Syntax, TsConfig};
use swc_ecma_transforms::Assumptions;

use crate::model::module::ModulesStore;
use crate::passes::compile::CompileContext;
use std::fmt::Write;
use std::io::Sink;
use std::path::PathBuf;

const CORE_TS: &str = include_str!("../../lib/ts/core.ts");
const REACTIVITY_TS: &str = include_str!("../../lib/ts/reactivity.ts");

impl ModulesStore {
    pub fn bundle<'a>(&self) -> String {
        let mut buf = String::new();

        buf += CORE_TS;
        buf += REACTIVITY_TS;

        let module_id_map = self
            .iter()
            .enumerate()
            .map(|(index, (module_id, _))| (module_id.clone(), index))
            .collect();

        // TODO: Sort by import graph
        // TODO: Helpful error about mutually imported consts

        for module in self.iter().filter_map(|(_, module)| module.as_ref().ok()) {
            module.compile(
                CompileContext {
                    modules: self,
                    current_module: module,
                    include_types: false,
                    qualify_identifiers_with: Some(&module_id_map),
                    qualify_all_identifiers: false,
                },
                &mut buf,
            );
        }

        buf.write_str("\n\nmain();");

        minify(&buf)
    }
}

fn minify(bundle: &str) -> String {
    let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));
    let c = Compiler::new(cm.clone());

    let fm = cm.new_source_file(FileName::Real("input.js".into()), bundle.into());

    let dst: Box<dyn std::io::Write + Send> = Box::new(Sink::default());
    let handler = Handler::with_emitter(
        false,
        false,
        Box::new(EmitterWriter::new(dst, Some(cm), false, true)),
    );

    GLOBALS.set(&Globals::default(), || {
        let mut assumptions = Assumptions::default();
        assumptions.array_like_is_iterable = true;
        assumptions.constant_reexports = true;
        assumptions.constant_super = true;
        assumptions.enumerable_module_meta = true;
        assumptions.ignore_function_length = true;
        assumptions.ignore_function_name = true;
        assumptions.ignore_to_primitive_hint = true;
        assumptions.no_class_calls = true;
        assumptions.no_incomplete_ns_import_detection = true;
        assumptions.no_new_arrows = true;
        assumptions.private_fields_as_properties = true;
        assumptions.pure_getters = true;
        assumptions.set_class_methods = true;
        assumptions.set_computed_properties = true;
        assumptions.set_public_class_fields = true;
        assumptions.set_spread_properties = true;
        assumptions.super_is_callable_constructor = true;
        assumptions.ts_enum_is_readonly = true;

        let s = c.process_js_file(
            fm,
            &handler,
            &Options {
                config: Config {
                    env: None,
                    test: None,
                    exclude: None,
                    jsc: JscConfig {
                        assumptions: Some(assumptions),
                        syntax: Some(Syntax::Typescript(TsConfig::default())),
                        transform: MergingOption::default(),
                        external_helpers: BoolConfig::new(None),
                        target: Some(swc_ecma_ast::EsVersion::EsNext),
                        loose: BoolConfig::new(None),
                        keep_class_names: false.into(),
                        base_url: PathBuf::new(),
                        paths: indexmap::IndexMap::default(),
                        minify: Some(JsMinifyOptions {
                            compress: BoolOrDataConfig::default(),
                            mangle: MangleOptions {
                                props: None,
                                top_level: Some(true),
                                keep_class_names: false,
                                keep_fn_names: false,
                                keep_private_props: false,
                                ie8: false,
                                safari10: false,
                                reserved: Vec::new(),
                            }
                            .into(),
                            format: JsMinifyFormatOptions::default(),
                            ecma: TerserEcmaVersion::Num(2016),
                            keep_classnames: false,
                            keep_fnames: false,
                            module: true,
                            safari10: false,
                            toplevel: true,
                            source_map: BoolOrDataConfig::default(),
                            output_path: None,
                            inline_sources_content: false,
                            emit_source_map_columns: false,
                        }),
                        experimental: JscExperimental::default(),
                        lints: LintConfig::default(),
                        preserve_all_comments: false.into(),
                        output: JscOutputConfig::default(),
                    },
                    module: Some(swc::config::ModuleConfig::Es6),
                    minify: true.into(),
                    input_source_map: None,
                    source_maps: None,
                    inline_sources_content: false.into(),
                    emit_source_map_columns: false.into(),
                    error: ErrorConfig::default(),
                    is_module: Some(swc::config::IsModule::Bool(true)),
                    schema: None,
                },
                skip_helper_injection: true,
                disable_hygiene: true,
                disable_fixer: true,
                top_level_mark: None,
                unresolved_mark: None,
                cwd: PathBuf::new(),
                caller: None,
                filename: "input.js".into(),
                config_file: None,
                root: None,
                root_mode: RootMode::Root,
                swcrc: false,
                swcrc_roots: None,
                env_name: String::new(),
                source_maps: None,
                source_file_name: None,
                source_root: None,
                output_path: None,
                experimental: ExperimentalOptions::default(),
            },
        );

        s.unwrap().code
    })
}
