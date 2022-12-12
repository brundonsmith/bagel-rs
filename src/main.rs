#![allow(dead_code)]
#![macro_use]

mod cli;
mod model;
mod passes;
mod tests;
mod utils;

use std::{collections::HashMap, ffi::OsStr, ops::Add, path::PathBuf, process::ExitCode};

use clap::{command, Parser};
use cli::Command;
use colored::Color;
use glob::glob;
use model::module::{ModuleID, ModulesStore};
use passes::check::CheckContext;

use crate::{model::errors::BagelError, utils::cli_label};

pub const DEBUG_MODE: bool = true;

#[derive(Parser, Debug, Clone)]
// #[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

fn main() -> ExitCode {
    let args = Args::parse();

    match args.command {
        Command::New { dir } => {
            let project_path = std::env::current_dir().unwrap().join(dir);
            let index_path = project_path.clone().join("index.bgl");

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
            let index_path = std::env::current_dir().unwrap().join("index.bgl");

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
        Command::Build {
            target,
            watch,
            clean,
        } => {
            let modules_store = load_and_parse(get_all_entrypoints(target), clean);
            let errors = gather_errors(&modules_store);
            print_error_results(&errors);

            if errors.values().any(|errors| errors.len() > 0) {
                return ExitCode::FAILURE;
            }

            // if no errors,
            //  transpile
            //  then bundle
        }
        Command::Run {
            target,
            node,
            deno,
            clean,
        } => {
            let modules_store = load_and_parse(get_all_entrypoints(target), clean);
            let errors = gather_errors(&modules_store);
            print_error_results(&errors);

            if errors.values().any(|errors| errors.len() > 0) {
                return ExitCode::FAILURE;
            }

            // if no errors,
            //  transpile
            //  then bundle
            //  then run
        }
        Command::Transpile {
            target,
            watch,
            clean,
        } => {
            let modules_store = load_and_parse(get_all_entrypoints(target), clean);
            let errors = gather_errors(&modules_store);
            print_error_results(&errors);

            if errors.values().any(|errors| errors.len() > 0) {
                return ExitCode::FAILURE;
            }

            // if no errors,
            //  transpile
        }
        Command::Check {
            target,
            watch,
            clean,
        } => {
            let modules_store = load_and_parse(get_all_entrypoints(target), clean);
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

fn get_all_entrypoints(target: String) -> impl Iterator<Item = PathBuf> {
    let paths = glob(target.as_str()).unwrap();

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

fn load_and_parse<I: Iterator<Item = PathBuf>>(entrypoints: I, clean: bool) -> ModulesStore {
    let mut modules_store = ModulesStore::new();

    for path in entrypoints {
        let module_id = ModuleID::try_from(path.as_path()).unwrap();
        modules_store.load_module_and_dependencies(module_id, clean);
    }

    modules_store
}

pub fn gather_errors(modules_store: &ModulesStore) -> HashMap<ModuleID, Vec<BagelError>> {
    let mut errors = HashMap::new();

    for (module_id, module) in modules_store.iter() {
        let mut module_errors = Vec::new();

        match module {
            Ok(module) => {
                module.check(
                    CheckContext {
                        modules: &modules_store,
                        current_module: &module,
                    },
                    &mut |error: BagelError| {
                        module_errors.push(error);
                    },
                );
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

// #[test]
// fn dfsjlkghdsfg() {
//     let code = "nil + 2";
//     let module = &Module {
//         module_id: "MyModule.bgl".to_owned().into(),
//         declarations: vec![],
//     };

//     let parsed = parse_2::expression(Slice::new(code)).unwrap().1;

//     parsed.check(CheckContext { module }, &mut |error| {
//         println!("{:?}", error);
//     });
// }

// #[test]
// fn foo() {
//     match parse(Slice::new("[1, 2")).map_err(|(index, message)| BagelError::ParseError {
//         p: PhantomData,
//         index,
//         module_id: "FooBar.bgl".to_owned().into(),
//         message,
//     }) {
//         Ok(parsed) => println!("{}", parsed),
//         Err(err) => println!("{}", err),
//     };
// }

// fn main() {
//     let bgl = "3 * (nil + 2";
//     let mut errors: Vec<Error> = vec![];

//     {
//         let all_modules = HashMap::new();
//         let (ast, parse_errors) = parser().parse_recovery(bgl);

//         for error in parse_errors {
//             let msg = if let chumsky::error::SimpleReason::Custom(msg) = error.reason() {
//                 msg.clone()
//             } else {
//                 format!(
//                     "{}{}, expected {}",
//                     if error.found().is_some() {
//                         "Unexpected token"
//                     } else {
//                         "Unexpected end of input"
//                     },
//                     if let Some(label) = error.label() {
//                         format!(" while parsing {}", label)
//                     } else {
//                         String::new()
//                     },
//                     if error.expected().len() == 0 {
//                         "something else".to_string()
//                     } else {
//                         error
//                             .expected()
//                             .map(|expected| match expected {
//                                 Some(expected) => expected.to_string(),
//                                 None => "end of input".to_string(),
//                             })
//                             .collect::<Vec<_>>()
//                             .join(", ")
//                     },
//                 )
//             };

//             let report = Report::build(ReportKind::Error, (), error.src().start)
//                 .with_message(msg)
//                 // .with_label(Label::new(error.src()).with_message("Failed to parse!"))
//                 .with_label(
//                     Label::new(error.src())
//                         .with_message(match error.reason() {
//                             chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
//                             _ => format!(
//                                 "Unexpected {}",
//                                 error
//                                     .found()
//                                     .map(|c| format!("token {}", c.fg(Color::Red)))
//                                     .unwrap_or_else(|| "end of input".to_string())
//                             ),
//                         })
//                         .with_color(Color::Red),
//                 );

//             let report = match error.reason() {
//                 chumsky::error::SimpleReason::Unclosed {  delimiter } => report.with_label(
//                     Label::new(src.clone())
//                         .with_message(format!(
//                             "Unclosed delimiter {}",
//                             delimiter.fg(Color::Yellow)
//                         ))
//                         .with_color(Color::Yellow),
//                 ),
//                 chumsky::error::SimpleReason::Unexpected => report,
//                 chumsky::error::SimpleReason::Custom(_) => report,
//             };

//             report.finish().print(Source::from(bgl)).unwrap();
//         }

//         if let Some(ast) = ast {
//             println!("Parsed: {}", ast);

//             if let ASTEnum::Expression(AST::Ok(expr)) = &ast {
//                 println!(
//                     "AST: {}\nInferred type: {:?}",
//                     &expr.node,
//                     infer_type(
//                         &Context {
//                             all_modules: &all_modules,
//                             report_error: &mut |error| errors.push(error),
//                         },
//                         &expr.node
//                     )
//                 );
//             }

//             typecheck(
//                 &mut Context {
//                     all_modules: &all_modules,
//                     report_error: &mut |error| errors.push(error),
//                 },
//                 &ast,
//             );
//             println!("Type errors: {:?}", errors);

//             let compiled = compile(
//                 &Context {
//                     all_modules: &all_modules,
//                     report_error: &mut |error| errors.push(error),
//                 },
//                 &ast,
//             );

//             if let Ok(compiled) = compiled {
//                 let js_ast = boa::parse(compiled, false).unwrap();

//                 let mut js_ctx = boa::context::Context::new();
//                 println!("Result: {:?}", js_ast.run(&mut js_ctx));
//             }
//         }
//     }
// }
