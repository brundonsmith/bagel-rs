// #![allow(dead_code)]
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
use cli::Command;
use colored::Color;
use config::BagelConfig;
use glob::glob;
use model::{
    errors::blue_string,
    module::{ModuleID, ModulesStore},
};
use passes::{check::CheckContext, compile::CompileContext};

use crate::{model::errors::BagelError, utils::cli_label};

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
                if let Ok(module) = module {
                    if let ModuleID::Local(path) = module_id {
                        if entrypoints.iter().any(|entry| path.as_ref() == entry) {
                            let mut compiled = String::new();
                            module
                                .compile(
                                    CompileContext {
                                        include_types: true,
                                    },
                                    &mut compiled,
                                )
                                .unwrap();

                            std::fs::write(path.with_extension("bgl.ts"), compiled).unwrap();
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
                            .code();
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
                            .code();
                    } else if bun {
                        todo!()
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

fn bundle(entrypoint: &str, watch: bool, clean: bool) -> Result<PathBuf, ()> {
    let entrypoint_path_buf = get_single_entrypoint(entrypoint);

    match entrypoint_path_buf {
        Ok(entrypoint) => {
            let modules_store = load_and_parse(std::iter::once(&entrypoint), clean);
            let errors = gather_errors(&modules_store);
            print_error_results(&errors);

            if errors.values().any(|errors| errors.len() > 0) {
                return Err(());
            }

            let bundle_path = entrypoint.with_extension("bundle.js");

            std::fs::write(&bundle_path, modules_store.bundle());

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
    let mut modules_store = ModulesStore::new();

    for path in entrypoints {
        let module_id = ModuleID::try_from(path.borrow().as_path()).unwrap();
        modules_store.load_module_and_dependencies(module_id, clean);
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

    if errors.len() > 0 {
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
                error.pretty_print(error_output_buf_ref, true);
                error_output_buf_ref.push('\n');
                error_output_buf_ref.push('\n');
            }
        }

        print!("{}", error_output_buf);
    }
}
