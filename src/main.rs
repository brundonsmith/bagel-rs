#![allow(dead_code)]

use std::marker::PhantomData;

use clap::{command, Parser};
use cli::Command;
use enum_variant_type::EnumVariantType;

mod ast;
mod check;
mod cli;
mod compile;
mod errors;
mod parse;
mod parse_utils;
mod resolve;
mod typeinfer;

#[derive(Parser, Debug, Clone)]
// #[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

fn main() {
    let args = Args::parse();

    println!("{:?}", args);
}

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
//                 chumsky::error::SimpleReason::Unclosed { src, delimiter } => report.with_label(
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
