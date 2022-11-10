use std::borrow::Cow;

use crate::{
    ast::{BooleanLiteral, Declaration, Expression, Module, ModuleName, NilLiteral, NumberLiteral},
    errors::BagelError,
    parse_utils::{consume, one_of, parse_series, ParseResult, PartialParseError},
};

pub fn bagel_module<'a>(module_name: ModuleName, src: &'a str) -> Result<Module, BagelError> {
    match parse_series(src, declaration) {
        Ok((_, declarations)) => Ok(Module {
            module_name,
            declarations,
        }),
        Err(Some(PartialParseError { src, message })) => Err(BagelError::ParseError {
            src,
            module_name,
            message,
        }),
        Err(None) => Err(BagelError::ParseError {
            src,
            module_name,
            message: format!("Failed to consume entire input"),
        }),
    }
}

fn declaration<'a>(start_src: &'a str) -> ParseResult<'a, Declaration> {
    todo!()
}

#[test]
fn foo() {
    println!("{:?}", expression("false"));
}

fn expression<'a>(start_src: &'a str) -> ParseResult<'a, Expression> {
    one_of(
        start_src,
        [
            |src| number_literal(src).map(|(src, parsed)| (src, Expression::from(parsed))),
            |src| boolean_literal(src).map(|(src, parsed)| (src, Expression::from(parsed))),
            |src| nil_literal(src).map(|(src, parsed)| (src, Expression::from(parsed))),
        ],
    )
}

fn number_literal<'a>(start_src: &'a str) -> ParseResult<'a, NumberLiteral> {
    let src = start_src.trim_start_matches(char::is_numeric);
    let parsed_chars = start_src.len() - src.len();

    if parsed_chars > 0 {
        let num_slice = &start_src[0..parsed_chars];
        Ok((
            src,
            NumberLiteral {
                src: Some(num_slice),
                value: Cow::from(num_slice),
            },
        ))
    } else {
        Err(None)
    }
}

fn boolean_literal<'a>(start_src: &'a str) -> ParseResult<'a, BooleanLiteral> {
    one_of(
        start_src,
        [|src| consume(src, "true"), |src| consume(src, "false")],
    )
    .map(|(src, segment)| {
        (
            src,
            BooleanLiteral {
                src: Some(segment),
                value: segment == "true",
            },
        )
    })
}

fn nil_literal<'a>(start_src: &'a str) -> ParseResult<'a, NilLiteral> {
    consume(start_src, "nil").map(|(src, segment)| (src, NilLiteral { src: Some(segment) }))
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

// fn local_identifier<'a>(code: &'a str) -> ParseChain<'a, LocalIdentifier> {
//     // ch != null && (isAlpha(ch) || ch === "_" || (!isFirstCharacter && (isNumeric(ch) || ch === "$")));
//     // let ident = &code[]
//     ParseResult::Success {
//         parsed: String::from("foo"),
//         code: &code[3..],
//     }
// }
