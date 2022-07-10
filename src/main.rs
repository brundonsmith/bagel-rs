#![allow(dead_code)]

use boa::exec::Executable;
use chumsky::prelude::*;
use compile::compile;
use model::ast::ASTEnum;
use std::collections::HashMap;

use crate::{
    model::misc::{Context, Error},
    parse::parser,
    typecheck::typecheck,
    typeinfer::infer_type,
};

mod compile;
mod model;
mod parse;
mod typecheck;
mod typeinfer;

fn main() {
    let mut errors: Vec<Error> = vec![];
    {
        let all_modules = HashMap::new();
        let mut report_error = |error| errors.push(error);

        let mut ctx = Context {
            all_modules: &all_modules,
            report_error: &mut report_error,
        };
        let ast = parser().parse("3 * (nil + 2)").unwrap();

        if let ASTEnum::Expression(expr) = &ast.node {
            println!(
                "AST: {}\nInferred type: {:?}",
                &expr,
                infer_type(&ctx, &expr)
            );
        }

        typecheck(&mut ctx, &ast);

        let compiled = compile(&ctx, &ast);

        let js_ast = boa::parse(compiled, false).unwrap();

        let mut js_ctx = boa::context::Context::new();
        println!("Result: {:?}", js_ast.run(&mut js_ctx));
    }

    println!("{:?}", errors);
}
