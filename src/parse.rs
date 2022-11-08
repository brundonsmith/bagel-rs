use crate::{
    ast::{Declaration, Module, ModuleName},
    errors::BagelError,
    parse_utils::{parse_chain, ParseChain, ParseSeriesOptions},
};

pub fn bagel_module<'a>(module_name: ModuleName, code: &'a str) -> Result<Module, BagelError> {
    let declarations =
        parse_chain(code).parse_series(declaration, ParseSeriesOptions { delimiter: "" });

    match declarations {
        ParseChain::Parsing {
            code,
            index,
            expecting,
            collected: ((), declarations),
        } => Ok(Module {
            module_name,
            declarations,
        }),
        ParseChain::None { index } => Err(BagelError::ParseError {
            index,
            module_name,
            message: format!("Failed to consume entire input"),
        }),
        ParseChain::Error(error) => Err(BagelError::from(error)),
    }
}

fn declaration<'a>(code: &'a str) -> ParseChain<'a, Declaration> {
    todo!()
}

// fn invocation<'a>(code: &'a str) -> ParseChain<'a, HList!(Invocation)> {
//     parse_chain(code)
//         .parse(identifier)
//         .consume_whitespace()
//         .consume("(")
//         .expect()
//         .consume_whitespace()
//         .parse_series(expression, ParseSeriesOptions { delimiter: "," })
//         .consume_whitespace()
//         .consume(")")
//         .collect(|collected| {
//             let (arguments, subject) = collected.into_tuple2();
//             Invocation { subject, arguments }
//         })
// }

// fn expression(code: &str) -> ParseResult<Expression> {
//     unimplemented!()
// }

// fn identifier(code: &str) -> ParseResult<String> {
//     ParseResult::Success {
//         parsed: String::from("foo"),
//         code: &code[3..],
//     }
// }
