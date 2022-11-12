use core::num;

use lazy_static::lazy_static;

use crate::{
    ast::{
        BinaryOperation, BinaryOperator, BooleanLiteral, Declaration, Expression, Module,
        ModuleName, NilLiteral, NumberLiteral, Sourced,
    },
    errors::BagelError,
    parse_utils::{
        boxed_parse_function, one_of, parse_series, parse_str, BoxedParseFunction, ParseFunction,
        ParseResult, ParseSeriesOptions, ParsedSeries, PartialParseError,
        RequiredOptionalForbidden,
    },
    slice::Slice,
};

pub fn bagel_module<'a>(module_name: ModuleName, src: &'a str) -> Result<Module, BagelError> {
    match parse_series(
        src.into(),
        declaration,
        &[""],
        ParseSeriesOptions {
            leading_delimiter: RequiredOptionalForbidden::Forbidden,
            trailing_delimiter: RequiredOptionalForbidden::Forbidden,
            whitespace_forbidden: false,
            min_items: 0,
        },
    ) {
        Ok((
            _,
            ParsedSeries {
                leading_delimiter: _,
                items,
            },
        )) => Ok(Module {
            module_name,
            declarations: items.into_iter().map(|(item, _)| item).collect(),
        }),
        Err(Some(PartialParseError { src, message })) => Err(BagelError::ParseError {
            src,
            module_name,
            message,
        }),
        Err(None) => Err(BagelError::ParseError {
            src: src.into(),
            module_name,
            message: format!("Failed to consume entire input"),
        }),
    }
}

fn declaration<'a>(start_src: Slice<'a>) -> ParseResult<'a, Declaration> {
    todo!()
}

fn binary_operation<'a, 'b>(
    start_src: Slice<'a>,
    operators: &'b [&'static str],
    current_tier: usize,
) -> ParseResult<'a, BinaryOperation<'a>> {
    let (src, series) = parse_series(
        start_src,
        |src| expression(src, current_tier + 1),
        operators,
        ParseSeriesOptions {
            leading_delimiter: RequiredOptionalForbidden::Forbidden,
            trailing_delimiter: RequiredOptionalForbidden::Forbidden,
            whitespace_forbidden: false,
            min_items: 2,
        },
    )?;

    let mut current: Option<(Expression<'a>, BinaryOperator)> = None;

    for (current_operand, current_operator) in series.items {
        if let Some(current_operator) = current_operator {
            match current {
                None => {
                    current = Some((
                        current_operand,
                        BinaryOperator::from_symbol(current_operator.as_str()),
                    ));
                }
                Some((left, previous_operator)) => {
                    current = Some((
                        Expression::BinaryOperation {
                            src: left.spanning(&current_operand),
                            left: Box::new(left),
                            op: previous_operator,
                            right: Box::new(current_operand),
                        },
                        BinaryOperator::from_symbol(current_operator.as_str()),
                    ));
                }
            };
        } else {
            let (left, operator) = current.unwrap();
            return Ok((
                src,
                BinaryOperation {
                    src: left.spanning(&current_operand),
                    left: Box::new(left),
                    op: operator,
                    right: Box::new(current_operand),
                },
            ));
        }
    }

    Err(Some(PartialParseError {
        src,
        message: format!("Unknown error parsing binary operation"),
    }))
}

// const binaryOperator = memo((tier: number): ParseFunction<BinaryOperator> => memo((module, code, startIndex) =>
//     given(parseSeries(module, code, startIndex,
//         EXPRESSION_PARSER.beneath(binaryOperator(tier)),
//         _binaryOperatorSymbol(tier),
//         { leadingDelimiter: "forbidden", trailingDelimiter: "forbidden" }
//     ), ({ parsed: segments, index }) =>
//         segments.length >= 3 ?
//             given(_segmentsToOps(segments), parsed => ({
//                 parsed,
//                 index,
//             }))
//         : undefined
//     )))

#[test]
fn dfjskh() {
    println!("{:?}", expression("12 + 3 * 4".into(), 0));
}

fn expression<'a, 'b>(start_src: Slice<'a>, current_tier: usize) -> ParseResult<'a, Expression> {
    let parse_next_tier = Box::new(move |src| expression(src, current_tier + 1))
        as BoxedParseFunction<'a, Expression<'a>>;

    let op_tiers: [&[&str]; 7] = [
        &[BinaryOperator::NullishCoalescing.symbol()],
        &[BinaryOperator::Or.symbol()],
        &[BinaryOperator::And.symbol()],
        &[
            BinaryOperator::Equals.symbol(),
            BinaryOperator::NotEquals.symbol(),
        ],
        &[
            BinaryOperator::LessEqual.symbol(),
            BinaryOperator::GreaterEqual.symbol(),
            BinaryOperator::Less.symbol(),
            BinaryOperator::Greater.symbol(),
        ],
        &[
            BinaryOperator::Plus.symbol(),
            BinaryOperator::Minus.symbol(),
        ],
        &[
            BinaryOperator::Times.symbol(),
            BinaryOperator::Divide.symbol(),
        ],
    ];

    let parse_fn_for_operators = |tier: usize| -> BoxedParseFunction<'a, Expression<'a>> {
        Box::new(move |src| binary_operation(src, op_tiers[tier], current_tier).map(to_expression))
            as BoxedParseFunction<'a, Expression<'a>>
    };

    match &current_tier {
        0 => one_of(
            start_src,
            [parse_fn_for_operators(0), parse_next_tier].into_iter(),
        ),
        1 => one_of(
            start_src,
            [parse_fn_for_operators(1), parse_next_tier].into_iter(),
        ),
        2 => one_of(
            start_src,
            [parse_fn_for_operators(2), parse_next_tier].into_iter(),
        ),
        3 => one_of(
            start_src,
            [parse_fn_for_operators(3), parse_next_tier].into_iter(),
        ),
        4 => one_of(
            start_src,
            [parse_fn_for_operators(4), parse_next_tier].into_iter(),
        ),
        5 => one_of(
            start_src,
            [parse_fn_for_operators(5), parse_next_tier].into_iter(),
        ),
        6 => one_of(
            start_src,
            [parse_fn_for_operators(6), parse_next_tier].into_iter(),
        ),
        6 => one_of(
            start_src,
            [
                boxed_expression_parse_fn(boolean_literal, current_tier),
                boxed_expression_parse_fn(nil_literal, current_tier),
                boxed_expression_parse_fn(number_literal, current_tier),
                parse_next_tier,
            ]
            .into_iter(),
        ),
        _ => Err(None),
    }
}

fn number_literal<'a>(start_src: Slice<'a>, current_tier: usize) -> ParseResult<'a, NumberLiteral> {
    let src = start_src.trim_start_matches(char::is_numeric);
    let parsed_chars = start_src.len() - src.len();

    if parsed_chars > 0 {
        let num_slice = &start_src.slice_range(0, Some(parsed_chars));
        Ok((
            src,
            NumberLiteral {
                src: Some(*num_slice),
                value: *num_slice,
            },
        ))
    } else {
        Err(None)
    }
}

fn boolean_literal<'a>(
    start_src: Slice<'a>,
    current_tier: usize,
) -> ParseResult<'a, BooleanLiteral> {
    one_of(
        start_src,
        [
            boxed_parse_function(|src| parse_str(src, "true")),
            boxed_parse_function(|src| parse_str(src, "false")),
        ]
        .into_iter(),
    )
    .map(|(src, segment)| {
        (
            src,
            BooleanLiteral {
                src: Some(segment),
                value: segment.as_str() == "true",
            },
        )
    })
}

fn nil_literal<'a>(start_src: Slice<'a>, current_tier: usize) -> ParseResult<'a, NilLiteral> {
    parse_str(start_src, "nil").map(|(src, segment)| (src, NilLiteral { src: Some(segment) }))
}

fn boxed_expression_parse_fn<'a, T: 'a, F: 'static + Fn(Slice<'a>, usize) -> ParseResult<'a, T>>(
    f: F,
    current_tier: usize,
) -> BoxedParseFunction<'a, Expression<'a>>
where
    Expression<'a>: From<T>,
{
    Box::new(move |src| f(src, current_tier).map(to_expression))
        as BoxedParseFunction<'a, Expression<'a>>
}

fn to_expression<'a, T>((src, parsed): (Slice<'a>, T)) -> (Slice<'a>, Expression<'a>)
where
    Expression<'a>: From<T>,
{
    (src, Expression::from(parsed))
}

// const EXPRESSION_PARSER = new TieredParser<Expression>([
//     [ debug, javascriptEscape, elementTag ],
//     [ func, proc ],
//     [ binaryOperator(0) ],
//     [ binaryOperator(1) ],
//     [ binaryOperator(2) ],
//     [ binaryOperator(3) ],
//     [ binaryOperator(4) ],
//     [ binaryOperator(5) ],
//     [ binaryOperator(6) ],
//     [ asCast, instanceOf ],
//     [ negationOperator ],
//     [ indexer ],
//     [ error ],
//     [ invocationAccessorChain ],
//     [ range ],
//     [ parenthesized ],
//     [ ifElseExpression, switchExpression, inlineConstGroup, booleanLiteral, nilLiteral, objectLiteral, arrayLiteral,
//         stringLiteral, numberLiteral ],
//     [ localIdentifier, regExp ],
// ])

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
