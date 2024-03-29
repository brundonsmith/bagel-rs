#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(non_snake_case)]
#![macro_use]

mod cli;
mod config;
mod model;
mod passes;
mod tests;
mod utils;

use std::{
    borrow::Borrow, collections::HashMap, ffi::OsStr, ops::Add, path::PathBuf, process::ExitCode,
};

use clap::{command, Parser};
use cli::{Command, ModulesStore};
use colored::Color;
use config::BagelConfig;
use glob::glob;
use model::{
    ast::{
        ArrayLiteral, BooleanLiteral, Declaration, ElementOrSpread, ExactStringLiteral, Expression,
        ImportAllDeclaration, ImportDeclaration, KeyValueOrSpread, Module, NilLiteral,
        NumberLiteral, ObjectLiteral, WithSlice, AST,
    },
    blue_string, ModuleID, ModuleType, ParsedModule, Slice,
};
use passes::{parse, CheckContext, CompileContext};
use serde_json::Value;
use utils::Rcable;

use crate::{model::BagelError, passes::Compilable, utils::cli_label};

pub const DEBUG_MODE: bool = true;

#[derive(Parser, Debug, Clone)]
// #[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

fn main() -> ExitCode {
    let current_dir = std::env::current_dir().unwrap();
    let index_path = current_dir.join("index.bgl");
    let config_path = current_dir.join("bagel.config.json");

    let args = Args::parse();
    let config: Option<BagelConfig> = if config_path.exists() {
        let parsed = serde_json::from_str(&std::fs::read_to_string(&config_path).unwrap());

        match parsed {
            Ok(parsed) => parsed,
            Err(err) => {
                println!(
                    "{} Reading {}:\n  {}",
                    cli_label("Failed", Color::Red),
                    blue_string(config_path.to_string_lossy()),
                    err.to_string()
                );
                return ExitCode::FAILURE;
            }
        }
    } else {
        None
    };

    match args.command {
        Command::New { dir } => {
            let project_path = current_dir.join(dir);

            if project_path.exists() {
                println!(
                    "{} Cannot create project directory {} because it already exists",
                    cli_label("Failed", Color::Red),
                    project_path.to_string_lossy()
                );
                return ExitCode::FAILURE;
            } else {
                std::fs::create_dir_all(project_path.clone()).unwrap();
                std::fs::write(index_path, DEFAULT_INDEX_BGL).unwrap();
                println!(
                    "{} new Bagel project {}",
                    cli_label("Created", Color::Green),
                    project_path.to_string_lossy()
                );
            }
        }
        Command::Init => {
            if index_path.exists() {
                println!(
                    "{} Can't initialize Bagel project here because one already exists",
                    cli_label("Failed", Color::Red)
                );
                return ExitCode::FAILURE;
            } else {
                std::fs::write(index_path, DEFAULT_INDEX_BGL).unwrap();
                println!(
                    "{} Bagel project in current directory",
                    cli_label("Initialized", Color::Green)
                );
            }
        }
        Command::Transpile {
            target,
            watch,
            clean,
        } => {
            let entrypoints = get_all_entrypoints(target.as_str()).collect::<Vec<_>>();
            let modules_store = load_and_parse(entrypoints.iter(), clean);
            let errors = gather_errors(&modules_store);
            print_error_results(&errors);

            if errors.values().any(|errors| errors.len() > 0) {
                return ExitCode::FAILURE;
            }

            for (module_id, module) in modules_store.iter() {
                if let ModuleID::Local(module_path) = module_id {
                    if let Ok(module) = module {
                        println!("entrypoints: {:?}", entrypoints);
                        if entrypoints
                            .iter()
                            .map(|entry| ModuleID::try_from(entry.as_path()).unwrap())
                            .any(|entry| &entry == module_id)
                        {
                            println!("stuff");
                            let mut compiled = String::new();
                            module
                                .compile(
                                    CompileContext {
                                        modules: &modules_store,
                                        current_module: &module,
                                        include_types: true,
                                        qualify_identifiers_with: None,
                                        qualify_all_identifiers: false,
                                    },
                                    &mut compiled,
                                )
                                .unwrap();

                            std::fs::write(module_path.with_extension("bgl.ts"), compiled).unwrap();
                        }
                    }
                }
            }
        }
        Command::Build {
            target,
            watch,
            clean,
        } => {
            let entry_path = if let Some(target) = target {
                target
            } else if let Some(config) = config {
                config.entry
            } else {
                println!(
                    "{} No entry module specified, and current directory doesn't have a {}",
                    cli_label("Failed", Color::Red),
                    blue_string("bagel.config.json"),
                );
                return ExitCode::FAILURE;
            };
            let bundle_path = bundle(entry_path.as_str(), watch, clean);

            if bundle_path.is_err() {
                return ExitCode::FAILURE;
            }
        }
        Command::Run {
            target,
            node,
            deno,
            bun,
            clean,
        } => {
            let entry_path = if let Some(target) = target {
                target
            } else if let Some(config) = config {
                config.entry
            } else {
                println!(
                    "{} No entry module specified, and current directory doesn't have a {}",
                    cli_label("Failed", Color::Red),
                    blue_string("bagel.config.json"),
                );
                return ExitCode::FAILURE;
            };
            let bundle_path = bundle(entry_path.as_str(), false, clean);

            match bundle_path {
                Ok(bundle_path) => {
                    // let platforms = config.platforms;

                    if node {
                        let command = std::env::var("BAGEL_NODE_BIN").unwrap_or("node".to_owned());

                        println!(
                            "{} {}",
                            cli_label(&format!("Running ({})", command), Color::Green),
                            bundle_path.to_string_lossy()
                        );

                        std::process::Command::new(&command)
                            .arg(bundle_path.canonicalize().unwrap())
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap()
                            .code()
                            .unwrap();
                    } else if deno {
                        let command = std::env::var("BAGEL_DENO_BIN").unwrap_or("deno".to_owned());

                        println!(
                            "{} {}",
                            cli_label(&format!("Running ({})", command), Color::Green),
                            bundle_path.to_string_lossy()
                        );

                        std::process::Command::new(&command)
                            .arg("run")
                            .arg("--unstable")
                            .arg("--allow-all")
                            .arg(bundle_path.canonicalize().unwrap())
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap()
                            .code()
                            .unwrap();
                    } else if bun {
                        let command = std::env::var("BAGEL_BUN_BIN").unwrap_or("bun".to_owned());

                        println!(
                            "{} {}",
                            cli_label(&format!("Running ({})", command), Color::Green),
                            bundle_path.to_string_lossy()
                        );

                        std::process::Command::new(&command)
                            .arg("run")
                            .arg(bundle_path.canonicalize().unwrap())
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap()
                            .code()
                            .unwrap();
                    } else {
                        todo!()
                    }
                }
                Err(_) => return ExitCode::FAILURE,
            }
        }
        Command::Check {
            target,
            watch,
            clean,
        } => {
            let modules_store = if let Some(target) = target {
                load_and_parse(get_all_entrypoints(target.as_str()), clean)
            } else if let Some(config) = config {
                load_and_parse(std::iter::once(&config.entry.into()), clean)
            } else {
                println!(
                    "{} No module or directory specified, and current directory doesn't have a {}",
                    cli_label("Failed", Color::Red),
                    blue_string("bagel.config.json"),
                );
                return ExitCode::FAILURE;
            };

            let errors = gather_errors(&modules_store);
            print_error_results(&errors);

            if errors.values().any(|errors| errors.len() > 0) {
                return ExitCode::FAILURE;
            }
        }
        Command::Test {
            target,
            test_filter,
            watch,
            clean,
        } => todo!(),
        Command::Clean { target } => todo!(),
    };

    ExitCode::SUCCESS
}

const DEFAULT_INDEX_BGL: &'static str = "
export const config: BagelConfig = {

    // Remove entries from this list to enable different platform-specific APIs
    platforms: ['node', 'deno', 'browser'],

    // You can override individual rules here, or leave empty for the default linter behavior
    lintRules: { },
}

proc main() {

}
";

fn s_or_none(n: usize) -> &'static str {
    if n == 1 {
        ""
    } else {
        "s"
    }
}

fn pretty_size(bytes: u64) -> String {
    if bytes < 1_000 {
        format!("{} bytes", bytes)
    } else if bytes < 1_000_000 {
        format!("{:.1}KB", bytes as f64 / 1_000.0)
    } else if bytes < 1_000_000_000 {
        format!("{:.1}MB", bytes as f64 / 1_000_000.0)
    } else {
        format!("{:.1}GB", bytes as f64 / 1_000_000_000.0)
    }
}

pub const MINIFY: bool = false;

fn bundle(entrypoint: &str, watch: bool, clean: bool) -> Result<PathBuf, ()> {
    let entrypoint_path_buf = get_single_entrypoint(entrypoint);

    match entrypoint_path_buf {
        Ok(entrypoint) => {
            let modules_store = load_and_parse(std::iter::once(&entrypoint), clean);
            let errors = gather_errors(&modules_store);
            print_error_results(&errors);

            if !DEBUG_MODE && errors.values().any(|errors| errors.len() > 0) {
                return Err(());
            }

            let bundle_path =
                entrypoint.with_extension(if MINIFY { "bundle.js" } else { "bundle.ts" });

            std::fs::write(&bundle_path, crate::passes::bundle(&modules_store)).map_err(|_| ())?;

            let bundle_size = std::fs::metadata(&bundle_path).unwrap().len();

            println!(
                "{} {} ({})",
                cli_label("Bundled", Color::Green),
                (&bundle_path).to_string_lossy(),
                pretty_size(bundle_size)
            );

            Ok(bundle_path)
        }
        Err(err) => {
            match err {
                SingleEntrypointError::NotFound => println!(
                    "{} Couldn't find module {}",
                    cli_label("Failed", Color::Red),
                    blue_string(entrypoint)
                ),
                SingleEntrypointError::Multi => println!(
                    "{} This command expects a single bagel module",
                    cli_label("Failed", Color::Red),
                ),
            };

            Err(())
        }
    }
}

fn get_all_entrypoints(target: &str) -> impl Iterator<Item = PathBuf> {
    let paths = glob(target).unwrap();

    paths.filter_map(|path| {
        let path = path.unwrap();

        if path.is_dir() {
            todo!() // recurse
        } else if path.extension() == Some(OsStr::new("bgl")) {
            Some(path)
        } else {
            None
        }
    })
}

fn get_single_entrypoint(target: &str) -> Result<PathBuf, SingleEntrypointError> {
    let mut paths = glob(target).map_err(|_| SingleEntrypointError::NotFound)?;

    let result = paths
        .next()
        .map(Result::ok)
        .flatten()
        .ok_or(SingleEntrypointError::NotFound);

    if paths.next().is_some() {
        Err(SingleEntrypointError::Multi)
    } else {
        result
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SingleEntrypointError {
    NotFound,
    Multi,
}

fn load_and_parse<'a, P: Borrow<PathBuf>, I: 'a + Iterator<Item = P>>(
    entrypoints: I,
    clean: bool,
) -> ModulesStore {
    let mut modules_store = HashMap::new();

    for path in entrypoints {
        let module_id = ModuleID::try_from(path.borrow().as_path()).unwrap();
        load_module_and_dependencies(&mut modules_store, module_id, clean);
    }

    modules_store
}

pub fn gather_errors(modules_store: &ModulesStore) -> HashMap<ModuleID, Vec<BagelError>> {
    let mut errors = HashMap::new();

    for (module_id, module) in modules_store.iter() {
        let mut module_errors = Vec::new();
        let report_error = &mut |error: BagelError| {
            module_errors.push(error);
        };

        match module {
            Ok(module) => {
                let mut context = CheckContext {
                    modules: &modules_store,
                    current_module: &module,
                    nearest_func_or_proc: None,
                    in_expression_context: false,
                };

                module.check(&mut context, report_error);
            }
            Err(error) => {
                module_errors.push(BagelError::from(error.clone()));
            }
        }

        errors.insert(module_id.clone(), module_errors);
    }

    errors
}

fn print_error_results(errors: &HashMap<ModuleID, Vec<BagelError>>) {
    let modules_checked = errors.len();
    let modules_with_errors = errors.values().filter(|errors| errors.len() > 0).count();
    let total_errors = errors.values().map(|errors| errors.len()).fold(0, Add::add);

    if errors.values().any(|errors| errors.len() > 0) {
        print_errors(errors);
        println!(
            "Found {} problem{} across {} module{} ({} module{} checked)",
            total_errors,
            s_or_none(total_errors),
            modules_with_errors,
            s_or_none(modules_with_errors),
            modules_checked,
            s_or_none(modules_checked)
        );
    } else {
        println!(
            "{} {} module{} and found no problems",
            cli_label("Checked", Color::Green),
            modules_checked,
            s_or_none(modules_checked)
        );
    }
}

pub fn print_errors(errors: &HashMap<ModuleID, Vec<BagelError>>) {
    if errors.len() > 0 {
        let mut error_output_buf = String::new();
        let error_output_buf_ref = &mut error_output_buf;

        for (_, errors) in errors {
            for error in errors {
                error.pretty_print(error_output_buf_ref, true).unwrap();
                error_output_buf_ref.push('\n');
                error_output_buf_ref.push('\n');
            }
        }

        print!("{}", error_output_buf);
    }
}
fn load_module_and_dependencies(
    modules_store: &mut ModulesStore,
    module_id: ModuleID,
    clean: bool,
) {
    let module_type = ModuleType::from(&module_id);

    if module_type == ModuleType::Npm {
        modules_store.insert(
            module_id.clone(),
            Ok(ParsedModule::JavaScript {
                module_id: module_id.clone(),
            }),
        );
    }

    if let Some(mut module_src) = module_id.load(clean) {
        module_src.push('\n'); // https://github.com/Geal/nom/issues/1573
        let module_src = module_src.rc();

        match module_type {
            ModuleType::JavaScript => {
                modules_store.insert(
                    module_id.clone(),
                    Ok(ParsedModule::JavaScript {
                        module_id: module_id.clone(),
                    }),
                );
            }
            ModuleType::Npm => {}
            ModuleType::JSON => {
                let module_src = Slice::new(module_src);

                let parsed: Value = serde_json::from_str(module_src.as_str()).unwrap();
                let contents = json_value_to_ast(module_src, parsed);

                modules_store.insert(
                    module_id.clone(),
                    Ok(ParsedModule::Singleton {
                        module_id: module_id.clone(),
                        contents,
                    }),
                );
            }
            ModuleType::Raw => {
                let module_src = Slice::new(module_src);

                modules_store.insert(
                    module_id.clone(),
                    Ok(ParsedModule::Singleton {
                        module_id: module_id.clone(),
                        contents: ExactStringLiteral {
                            tag: None,
                            value: module_src.clone(),
                        }
                        .as_ast(module_src)
                        .recast::<Expression>(),
                    }),
                );
            }
            ModuleType::Bagel => {
                let parsed = parse(module_id.clone(), module_src.clone());
                modules_store.insert(
                    module_id.clone(),
                    parsed.map(|ast| ParsedModule::Bagel {
                        module_id: module_id.clone(),
                        ast,
                    }),
                );

                if let Some(Module {
                    module_id: _,
                    declarations,
                }) = modules_store
                    .get(&module_id)
                    .map(|res| {
                        res.as_ref().ok().map(|module| match module {
                            ParsedModule::Bagel { module_id: _, ast } => Some(ast.downcast()),
                            ParsedModule::JavaScript { module_id: _ } => None,
                            ParsedModule::Singleton {
                                module_id: _,
                                contents: _,
                            } => None,
                        })
                    })
                    .flatten()
                    .flatten()
                {
                    let imported = declarations
                        .iter()
                        .filter_map(|decl| match decl.downcast() {
                            Declaration::ImportAllDeclaration(ImportAllDeclaration {
                                platforms: _,
                                name: _,
                                path,
                            }) => Some(path.downcast().value.as_str().to_owned()),
                            Declaration::ImportDeclaration(ImportDeclaration {
                                platforms: _,
                                imports: _,
                                path,
                            }) => Some(path.downcast().value.as_str().to_owned()),
                            _ => None,
                        });

                    for path in imported {
                        let other_module_id = module_id.imported(&path);

                        if let Some(other_module_id) = other_module_id {
                            if !modules_store.contains_key(&other_module_id) {
                                load_module_and_dependencies(modules_store, other_module_id, clean);
                            }
                        } else {
                            // TODO: Improve this, make it a proper error
                            println!("ERROR: Malformed import path {:?}", path);
                        }
                    }
                }
            }
        }
    }
}

fn json_value_to_ast(module_src: Slice, value: Value) -> AST<Expression> {
    match value {
        Value::Null => NilLiteral.as_ast(module_src).recast::<Expression>(),
        Value::Bool(value) => BooleanLiteral(value)
            .as_ast(module_src)
            .recast::<Expression>(),
        Value::Number(value) => {
            NumberLiteral(Slice::new(value.to_string().rc())) // HACK: Not shared with module_src
                .as_ast(module_src)
                .recast::<Expression>()
        }
        Value::String(value) => ExactStringLiteral {
            tag: None,
            value: Slice::new(value.rc()), // HACK: Not shared with module_src
        }
        .as_ast(module_src)
        .recast::<Expression>(),
        Value::Array(members) => ArrayLiteral(
            members
                .into_iter()
                .map(|member| {
                    ElementOrSpread::Element(json_value_to_ast(module_src.clone(), member))
                })
                .collect(),
        )
        .as_ast(module_src)
        .recast::<Expression>(),
        Value::Object(entries) => ObjectLiteral(
            entries
                .into_iter()
                .map(|(key, value)| {
                    KeyValueOrSpread::KeyValue(
                        ExactStringLiteral {
                            tag: None,
                            value: Slice::new(key.rc()),
                        }
                        .as_ast(module_src.clone())
                        .recast::<Expression>(),
                        json_value_to_ast(module_src.clone(), value),
                        false,
                    )
                })
                .collect(),
        )
        .as_ast(module_src)
        .recast::<Expression>(),
    }
}
