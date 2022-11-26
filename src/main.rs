#![allow(dead_code)]

mod cli;
mod model;
mod passes;
mod tests;
mod utils;

use std::{
    collections::HashMap,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use clap::{command, Parser};
use cli::Command;
use glob::{glob, Paths};

use crate::{
    model::ast::{Declaration, Module, ModuleID},
    model::errors::{pretty_print_parse_error, BagelError, ParseError},
    passes::check::{Check, CheckContext},
    passes::parse::parse,
};

#[derive(Parser, Debug, Clone)]
// #[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

fn main() {
    let args = Args::parse();
    println!("{:?}", args);

    let mut modules_store: ModulesStore = HashMap::new();

    match args.command {
        Command::New { dir } => todo!(),
        Command::Init => todo!(),
        Command::Build { target, watch } => todo!(),
        Command::Run { target, node, deno } => todo!(),
        Command::Transpile { target, watch } => todo!(),
        Command::Check { target, watch } => {
            let entrypoints = get_all_entrypoints(glob(target.as_str()).unwrap());

            for path in entrypoints {
                let module_id = ModuleID::try_from(path.as_path()).unwrap();
                load_module_and_dependencies(&mut modules_store, module_id);
            }

            let mut error_output_buf = String::new();
            let error_output_buf = &mut error_output_buf;
            for module in modules_store.values() {
                match module {
                    Ok(module) => {
                        module.check(
                            CheckContext {
                                modules: &modules_store,
                                current_module: &module,
                            },
                            &mut |error: BagelError| {
                                error.pretty_print(error_output_buf, &module.src, true);
                            },
                        );
                    }
                    Err(error) => {
                        pretty_print_parse_error(error, error_output_buf, true);
                    }
                }
            }

            // write buffer to console
            println!("{}", error_output_buf);
        }
        Command::Test {
            target,
            test_filter,
            watch,
        } => todo!(),
    }
}

pub type ModulesStore = HashMap<ModuleID, Result<Module, ParseError>>;

fn get_all_entrypoints(paths: Paths) -> impl Iterator<Item = PathBuf> {
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

fn load_module_and_dependencies(
    modules_store: &mut HashMap<ModuleID, Result<Module, ParseError>>,
    module_id: ModuleID,
) {
    if let Some(mut bgl) = module_id.load() {
        bgl.push('\n'); // https://github.com/Geal/nom/issues/1573

        let parsed = parse(module_id.clone(), bgl); // TODO
        modules_store.insert(module_id.clone(), parsed);

        if let Some(Ok(parsed)) = modules_store.get(&module_id) {
            let imported: Vec<String> = parsed
                .declarations
                .iter()
                .filter_map(|decl| {
                    if let Declaration::ImportAllDeclaration { name: _, path } = &decl.node {
                        Some(path.node.value.clone())
                    } else if let Declaration::ImportDeclaration { imports: _, path } = &decl.node {
                        Some(path.node.value.clone())
                    } else {
                        None
                    }
                })
                .collect();

            for path in imported {
                let path = Path::new(&path);
                let full_path = path.to_owned(); //module_id.as_path().join(path);
                let other_module_id = ModuleID::try_from(full_path.as_path()).unwrap();
                // TODO: Handle https modules

                if !modules_store.contains_key(&other_module_id) {
                    load_module_and_dependencies(modules_store, other_module_id);
                }
            }
        }
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
