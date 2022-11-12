use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while},
    character::complete::{alphanumeric1 as alphanumeric, char, digit1, one_of},
    combinator::{cut, fail, map, map_res, opt, value},
    error::{context, convert_error, ContextError, ErrorKind, ParseError, VerboseError},
    multi::separated_list0,
    number::complete::{double, recognize_float},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    Err, IResult,
};

use crate::{
    ast::*,
    precedence::{binary_op, precedence, unary_op, Assoc, Operation, Operator},
    slice::Slice,
};

fn expression<'a, E: ParseError<Slice<'a>> + ContextError<Slice<'a>>>(
    i: Slice<'a>,
) -> IResult<Slice<'a>, Expression<'a>, E> {
    preceded(
        whitespace,
        alt((
            map(object_literal, Expression::from),
            map(array_literal, Expression::from),
            map(string_literal, |p| Expression::LocalIdentifier {
                src: p.src,
                name: p.name,
            }),
            map(recognize_float, |value: Slice| Expression::NumberLiteral {
                src: Some(value),
                value,
            }),
            map(boolean_literal, Expression::from),
            map(nil, Expression::from),
            map(local_identifier, Expression::from),
        )),
    )(i)
}

fn expression_2<'a>(i: Slice<'a>) -> IResult<Slice<'a>, i64> {
    precedence(
        unary_op(1, tag("-")),
        fail,
        alt((
            binary_op(2, Assoc::Left, tag("*")),
            binary_op(2, Assoc::Left, tag("/")),
            binary_op(3, Assoc::Left, tag("+")),
            binary_op(3, Assoc::Left, tag("-")),
        )),
        alt((
            map_res(digit1, |s: &str| s.parse::<i64>()),
            delimited(tag("("), expression_2, tag(")")),
        )),
        |op: Operation<&str, &str, &str, i64>| match op {
            Operator::Prefix("-", o) => Ok(-o),
            Operator::Binary(lhs, "*", rhs) => Ok(lhs * rhs),
            Operator::Binary(lhs, "/", rhs) => Ok(lhs / rhs),
            Operator::Binary(lhs, "+", rhs) => Ok(lhs + rhs),
            Operator::Binary(lhs, "-", rhs) => Ok(lhs - rhs),
            _ => Err("Invalid combination"),
        },
    )(i)
}

fn local_identifier<'a, E: ParseError<Slice<'a>> + ContextError<Slice<'a>>>(
    i: Slice<'a>,
) -> IResult<Slice<'a>, LocalIdentifier<'a>, E> {
    context(
        "local identifier",
        map(identifier_like, |name| LocalIdentifier {
            src: Some(name),
            name,
        }),
    )(i)
}

fn object_literal<'a, E: ParseError<Slice<'a>> + ContextError<Slice<'a>>>(
    i: Slice<'a>,
) -> IResult<Slice<'a>, ObjectLiteral<'a>, E> {
    context(
        "object",
        map(
            pair(
                tag("{"),
                cut(pair(
                    separated_list0(preceded(whitespace, char(',')), key_value),
                    preceded(whitespace, tag("}")),
                )),
            ),
            |(open_bracket, (entries, close_bracket))| ObjectLiteral {
                src: Some(open_bracket.spanning(close_bracket)),
                entries: entries
                    .into_iter()
                    .map(|(key, value)| {
                        ObjectLiteralEntry::KeyValue(
                            PlainIdentifier {
                                src: Some(key),
                                name: key,
                            },
                            value,
                        )
                    })
                    .collect(),
            },
        ),
    )(i)
}

fn key_value<'a, E: ParseError<Slice<'a>> + ContextError<Slice<'a>>>(
    i: Slice<'a>,
) -> IResult<Slice<'a>, (Slice<'a>, Expression<'a>), E> {
    separated_pair(
        preceded(whitespace, identifier_like),
        cut(preceded(whitespace, char(':'))),
        expression,
    )(i)
}

fn array_literal<'a, E: ParseError<Slice<'a>> + ContextError<Slice<'a>>>(
    i: Slice<'a>,
) -> IResult<Slice<'a>, ArrayLiteral<'a>, E> {
    context(
        "array",
        map(
            pair(
                tag("["),
                cut(pair(
                    separated_list0(preceded(whitespace, char(',')), expression),
                    preceded(whitespace, tag("]")),
                )),
            ),
            |(open_bracket, (entries, close_bracket))| ArrayLiteral {
                src: Some(open_bracket.spanning(close_bracket)),
                entries: entries
                    .into_iter()
                    .map(ArrayLiteralEntry::Element)
                    .collect(),
            },
        ),
    )(i)
}

fn string_literal<'a, E: ParseError<Slice<'a>> + ContextError<Slice<'a>>>(
    i: Slice<'a>,
) -> IResult<Slice<'a>, PlainIdentifier<'a>, E> {
    context(
        "string",
        map(
            pair(tag("\""), cut(pair(string_contents, tag("\"")))),
            |(open_quote, (contents, close_quote))| PlainIdentifier {
                src: Some(open_quote.spanning(close_quote)),
                name: contents,
            },
        ),
    )(i)
}

fn string_contents<'a, E: ParseError<Slice<'a>>>(i: Slice<'a>) -> IResult<Slice<'a>, Slice<'a>, E> {
    escaped(alphanumeric, '\\', one_of("\"n\\"))(i)
}

fn boolean_literal<'a, E: ParseError<Slice<'a>>>(
    input: Slice<'a>,
) -> IResult<Slice<'a>, BooleanLiteral<'a>, E> {
    let parse_true = map(tag("true"), |src| BooleanLiteral {
        src: Some(src),
        value: true,
    });

    let parse_false = map(tag("false"), |src| BooleanLiteral {
        src: Some(src),
        value: false,
    });

    alt((parse_true, parse_false))(input)
}

fn nil<'a, E: ParseError<Slice<'a>>>(input: Slice<'a>) -> IResult<Slice<'a>, NilLiteral<'a>, E> {
    map(tag("nil"), |src| NilLiteral { src: Some(src) })(input)
}

fn identifier_like<'a, E: ParseError<Slice<'a>>>(i: Slice<'a>) -> IResult<Slice<'a>, Slice<'a>, E> {
    take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$')(i)
}

fn whitespace<'a, E: ParseError<Slice<'a>>>(i: Slice<'a>) -> IResult<Slice<'a>, Slice<'a>, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

#[test]
fn foo() {
    let data = "  { a\t: 42,
  b: [ \"x\", \"y\", 12 ] ,
  c: { hello : \"world\"
  }
  } ";

    println!(
        "will try to parse valid JSON data:\n\n**********\n{}\n**********\n",
        data
    );

    println!(
        "parsing a valid file:\n{:#?}\n",
        expression::<(Slice, ErrorKind)>(Slice::new(data))
    );

    let data = "  { a\t: 42,
  b: [ \"x\", \"y\", 12 ] ,
  c: { \"hello\" : \"world\"
  }
  } ";

    println!(
        "will try to parse invalid JSON data:\n\n**********\n{}\n**********\n",
        data
    );

    println!(
        "basic errors - `root::<(Slice, ErrorKind)>(data)`:\n{:#?}\n",
        expression::<(Slice, ErrorKind)>(Slice::new(data))
    );

    println!(
        "parsed verbose: {:#?}",
        expression::<VerboseError<Slice>>(Slice::new(data))
    );

    match expression::<VerboseError<Slice>>(Slice::new(data)) {
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            println!(
                "verbose errors - `root::<VerboseError>(data)`:\n{}",
                convert_error(
                    data,
                    VerboseError {
                        errors: e
                            .errors
                            .into_iter()
                            .map(|(src, kind)| (src.as_str(), kind))
                            .collect()
                    }
                )
            );
        }
        _ => {}
    }
}

#[test]
fn nil_test() {
    println!("{:?}", Slice::new("nil"));
    println!("{:?}", expression::<(Slice, ErrorKind)>(Slice::new("nil")));
    assert!(expression::<(Slice, ErrorKind)>(Slice::new("nil")).is_ok());
}
