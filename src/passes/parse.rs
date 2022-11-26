use std::{rc::Rc, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{alphanumeric1, char, one_of},
    combinator::{complete, cut, fail, map, opt},
    error::context,
    multi::{many0, separated_list0},
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    model::ast::*,
    model::errors::ParseError,
    model::slice::Slice,
    passes::precedence::{binary_op, precedence, Assoc, Operation},
};

pub fn parse(module_id: ModuleID, module_src: Rc<String>) -> Result<Module, ParseError> {
    let bgl = Slice::new(module_src.clone());

    let res = complete(terminated(
        separated_list0(whitespace, declaration),
        whitespace,
    ))(bgl);

    match res {
        Ok((i, declarations)) => {
            if i.len() > 0 {
                Err(ParseError {
                    module_id,
                    src: i,
                    message: "Failed to parse entire input".to_owned(),
                })
            } else {
                Ok(Module {
                    module_id,
                    src: module_src,
                    declarations,
                })
            }
        }
        Err(error) => match error {
            nom::Err::Error(RawParseError { src, details }) => Err(ParseError {
                module_id: module_id.clone(),
                src,
                message: match details {
                    RawParseErrorDetails::Kind(kind) => kind.description().to_owned(),
                    RawParseErrorDetails::Char(ch) => format!("Expected '{}'", ch),
                },
            }),
            nom::Err::Failure(RawParseError { src, details }) => Err(ParseError {
                module_id: module_id.clone(),
                src,
                message: match details {
                    RawParseErrorDetails::Kind(kind) => kind.description().to_owned(),
                    RawParseErrorDetails::Char(ch) => format!("Expected '{}'", ch),
                },
            }),
            nom::Err::Incomplete(_) => unreachable!(),
        },
    }
}

fn declaration<'a>(i: Slice) -> ParseResult<'a, Src<Declaration>> {
    preceded(
        whitespace,
        alt((
            map(import_all_declaration, |x| x.map(Declaration::from)),
            map(import_declaration, |x| x.map(Declaration::from)),
            map(type_declaration, |x| x.map(Declaration::from)),
            map(func_declaration, |x| x.map(Declaration::from)),
            map(proc_declaration, |x| x.map(Declaration::from)),
            map(value_declaration, |x| x.map(Declaration::from)),
            // map(test_expr_declaration, |x| x.map(Declaration::from)),
            // map(test_block_declaration, |x| x.map(Declaration::from)),
            // map(test_type_declaration, |x| x.map(Declaration::from)),
        )),
    )(i)
}

fn import_all_declaration<'a>(i: Slice) -> ParseResult<'a, Src<ImportAllDeclaration>> {
    context(
        "import-all declaration",
        map(
            tuple((
                tag("import"),
                preceded(whitespace, exact_string_literal),
                preceded(whitespace, tag("as")),
                preceded(whitespace, plain_identifier),
            )),
            |(start, path, _, name)| {
                let src = start.spanning(&name.0);

                ImportAllDeclaration { path, name }.with_src(src)
            },
        ),
    )(i)
}

fn import_declaration<'a>(i: Slice) -> ParseResult<'a, Src<ImportDeclaration>> {
    context(
        "import declaration",
        map(
            tuple((
                tag("from"),
                preceded(whitespace, exact_string_literal),
                preceded(whitespace, tag("import")),
                preceded(whitespace, tag("{")),
                separated_list0(
                    preceded(whitespace, tag(",")),
                    pair(
                        preceded(whitespace, plain_identifier),
                        opt(map(
                            pair(
                                preceded(whitespace, tag("as")),
                                preceded(whitespace, plain_identifier),
                            ),
                            |(_, alias)| alias,
                        )),
                    ),
                ),
                preceded(whitespace, tag("}")),
            )),
            |(start, path, _, _, imports, end)| {
                ImportDeclaration { path, imports }.with_src(start.spanning(&end))
            },
        ),
    )(i)
}

fn type_declaration<'a>(i: Slice) -> ParseResult<'a, Src<TypeDeclaration>> {
    context(
        "type declaration",
        map(
            tuple((
                opt(tag("export")),
                preceded(whitespace, tag("type")),
                preceded(whitespace, plain_identifier),
                preceded(whitespace, tag("=")),
                preceded(whitespace, type_expression),
            )),
            |(export, keyword, name, _, declared_type)| {
                let exported = export.is_some();
                let src = export.unwrap_or(keyword).spanning(&declared_type.src);

                TypeDeclaration {
                    name,
                    declared_type,
                    exported,
                }
                .with_src(src)
            },
        ),
    )(i)
}

fn func_declaration<'a>(i: Slice) -> ParseResult<'a, Src<FuncDeclaration>> {
    context(
        "func declaration",
        map(
            tuple((
                opt(tag("export")),
                opt(preceded(whitespace, tag("pure"))),
                opt(preceded(whitespace, tag("async"))),
                preceded(whitespace, tag("func")),
                preceded(whitespace, plain_identifier),
                preceded(whitespace, tag("(")),
                preceded(whitespace, args),
                preceded(whitespace, tag(")")),
                preceded(whitespace, opt(type_annotation)),
                preceded(whitespace, tag("=>")),
                preceded(whitespace, expression),
            )),
            |(export, pure, asyn, keyword, name, open_paren, args, _, return_type, _, body)| {
                let exported = export.is_some();
                let is_async = asyn.is_some();
                let is_pure = pure.is_some();
                let src = export
                    .unwrap_or(pure.unwrap_or(asyn.unwrap_or(keyword)))
                    .spanning(&body.src);

                FuncDeclaration {
                    name,
                    func: Func {
                        type_annotation: FuncType {
                            args,
                            args_spread: None, // TODO
                            is_pure,
                            returns: return_type.map(Box::new),
                        }
                        .with_src(open_paren.spanning(&body.src)),

                        is_async,
                        is_pure,
                        body: FuncBody::Expression(Box::new(body)),
                    }
                    .with_src(src.clone()),
                    exported,
                    platforms: PlatformSet::all(), // TODO
                    decorators: vec![],            // TODO
                }
                .with_src(src)
            },
        ),
    )(i)
}

fn args<'a>(i: Slice) -> ParseResult<'a, Vec<Src<Arg>>> {
    map(
        separated_list0(
            preceded(whitespace, tag(",")),
            preceded(whitespace, pair(plain_identifier, opt(type_annotation))),
        ),
        |args| {
            args.into_iter()
                .map(|(name, type_annotation)| {
                    let src = name.0.clone().spanning(
                        &type_annotation
                            .as_ref()
                            .map(|t| t.src.clone())
                            .unwrap_or(name.0.clone()),
                    );

                    Arg {
                        name,
                        type_annotation,
                        optional: false, // TODO
                    }
                    .with_src(src)
                })
                .collect()
        },
    )(i)
}

fn proc_declaration<'a>(i: Slice) -> ParseResult<'a, Src<ProcDeclaration>> {
    context(
        "proc declaration",
        map(
            tuple((
                opt(tag("export")),
                opt(preceded(whitespace, tag("pure"))),
                opt(preceded(whitespace, tag("async"))),
                preceded(whitespace, tag("proc")),
                preceded(whitespace, plain_identifier),
                preceded(whitespace, tag("(")),
                preceded(whitespace, args),
                preceded(whitespace, tag(")")),
                preceded(whitespace, tag("{")),
                many0(preceded(whitespace, statement)),
                preceded(whitespace, tag("}")),
            )),
            |(export, pure, asyn, keyword, name, open_paren, args, _, _, body, closing_brace)| {
                let exported = export.is_some();
                let is_async = asyn.is_some();
                let is_pure = pure.is_some();
                let src = export
                    .unwrap_or(pure.unwrap_or(asyn.unwrap_or(keyword)))
                    .spanning(&closing_brace);

                ProcDeclaration {
                    name,
                    proc: Proc {
                        type_annotation: ProcType {
                            args,
                            args_spread: None, // TODO
                            is_pure,
                            is_async,
                            throws: None, // TODO
                        }
                        .with_src(open_paren.spanning(&closing_brace)),

                        is_async,
                        is_pure,
                        body: ProcBody::Statements(body),
                    }
                    .with_src(src.clone()),
                    exported,
                    platforms: PlatformSet::all(), // TODO
                    decorators: vec![],            // TODO
                }
                .with_src(src)
            },
        ),
    )(i)
}

fn value_declaration<'a>(i: Slice) -> ParseResult<'a, Src<ValueDeclaration>> {
    context(
        "value declaration",
        map(
            tuple((
                opt(tag("export")),
                preceded(whitespace, alt((tag("const"), tag("let")))),
                preceded(whitespace, plain_identifier),
                opt(preceded(whitespace, type_annotation)),
                preceded(whitespace, tag("=")),
                preceded(whitespace, expression),
            )),
            |(export, keyword, name, type_annotation, _, value)| {
                let exported = export.is_some();
                let is_const = keyword.as_str() == "const";
                let src = export.unwrap_or(keyword).spanning(&value.src);

                ValueDeclaration {
                    name,
                    type_annotation,
                    value,
                    is_const,
                    exported,
                    platforms: PlatformSet::all(), // TODO
                }
                .with_src(src)
            },
        ),
    )(i)
}

fn test_expr_declaration<'a>(i: Slice) -> ParseResult<'a, Src<TestExprDeclaration>> {
    todo!()
}

fn test_block_declaration<'a>(i: Slice) -> ParseResult<'a, Src<TestBlockDeclaration>> {
    todo!()
}

fn test_type_declaration<'a>(i: Slice) -> ParseResult<'a, Src<TestTypeDeclaration>> {
    todo!()
}

fn type_annotation<'a>(i: Slice) -> ParseResult<'a, Src<TypeExpression>> {
    preceded(tag(":"), preceded(whitespace, type_expression))(i)
}

fn type_expression<'a>(i: Slice) -> ParseResult<'a, Src<TypeExpression>> {
    alt((
        map(exact_string_literal, |s| {
            s.map(|s| TypeExpression::LiteralType {
                value: LiteralTypeValue::ExactString(s),
            })
        }),
        map(number_literal, |s| {
            s.map(|s| TypeExpression::LiteralType {
                value: LiteralTypeValue::NumberLiteral(s),
            })
        }),
        map(boolean_literal, |s| {
            s.map(|s| TypeExpression::LiteralType {
                value: LiteralTypeValue::BooleanLiteral(s),
            })
        }),
        map(tag("string"), |s: Slice| {
            TypeExpression::StringType.with_src(s)
        }),
        map(tag("number"), |s: Slice| {
            TypeExpression::NumberType.with_src(s)
        }),
        map(tag("boolean"), |s: Slice| {
            TypeExpression::BooleanType.with_src(s)
        }),
        map(tag("nil"), |s: Slice| TypeExpression::NilType.with_src(s)),
    ))(i)
}

fn statement<'a>(i: Slice) -> ParseResult<'a, Src<Statement>> {
    todo!()
}

fn expression<'a>(i: Slice) -> ParseResult<'a, Src<Expression>> {
    preceded(whitespace, alt((binary_operation, atomic_expression)))(i)
}

fn atomic_expression<'a>(i: Slice) -> ParseResult<'a, Src<Expression>> {
    preceded(
        whitespace,
        alt((
            map(parenthesis, |x| x.map(Expression::from)),
            map(object_literal, |x| x.map(Expression::from)),
            map(array_literal, |x| x.map(Expression::from)),
            map(exact_string_literal, |x| x.map(Expression::from)),
            map(number_literal, |x| x.map(Expression::from)),
            map(boolean_literal, |x| x.map(Expression::from)),
            map(nil_literal, |x| x.map(Expression::from)),
            map(local_identifier, |x| {
                Expression::from(x.clone()).with_src(x.0)
            }),
        )),
    )(i)
}

fn binary_operation<'a>(i: Slice) -> ParseResult<'a, Src<Expression>> {
    context(
        "binary operation",
        precedence(
            fail,
            // unary_op(1, tag("-")),
            fail,
            alt((
                binary_op(
                    2,
                    Assoc::Left,
                    map(preceded(whitespace, tag("*")), |op| {
                        BinaryOperator::from_str(op.as_str()).unwrap().with_src(op)
                    }),
                ),
                binary_op(
                    2,
                    Assoc::Left,
                    map(preceded(whitespace, tag("/")), |op| {
                        BinaryOperator::from_str(op.as_str()).unwrap().with_src(op)
                    }),
                ),
                binary_op(
                    3,
                    Assoc::Left,
                    map(preceded(whitespace, tag("+")), |op| {
                        BinaryOperator::from_str(op.as_str()).unwrap().with_src(op)
                    }),
                ),
                binary_op(
                    3,
                    Assoc::Left,
                    map(preceded(whitespace, tag("-")), |op| {
                        BinaryOperator::from_str(op.as_str()).unwrap().with_src(op)
                    }),
                ),
            )),
            atomic_expression,
            |op: Operation<
                Src<BinaryOperator>,
                Src<BinaryOperator>,
                Src<BinaryOperator>,
                Src<Expression>,
            >| {
                match op {
                    // Operation::Prefix("-", o) => Ok(-o),
                    Operation::Binary(left, op, right) => {
                        let src = left.spanning(&right);

                        Ok(Expression::BinaryOperation {
                            left: Box::new(left),
                            op,
                            right: Box::new(right),
                        }
                        .with_src(src))
                    }
                    // Operation::Binary(lhs, "*", rhs) => Ok(lhs * rhs),
                    // Operation::Binary(lhs, "/", rhs) => Ok(lhs / rhs),
                    // Operation::Binary(lhs, "+", rhs) => Ok(lhs + rhs),
                    // Operation::Binary(lhs, "-", rhs) => Ok(lhs - rhs),
                    _ => Err("Invalid combination"),
                }
            },
        ),
    )(i)
}

/// fn parser(i: &str) -> IResult<&str, i64> {
///   precedence(
///     unary_op(1, tag("-")),
///     fail,
///     alt((
///       binary_op(2, Assoc::Left, tag("*")),
///       binary_op(2, Assoc::Left, tag("/")),
///       binary_op(3, Assoc::Left, tag("+")),
///       binary_op(3, Assoc::Left, tag("-")),
///     )),
///     alt((
///       map_res(digit1, |s: &str| s.parse::<i64>()),
///       delimited(tag("("), parser, tag(")")),
///     )),
///     |op: Operation<&str, &str, &str, i64>| {
///       use nom_7_precedence::Operation::*;
///       match op {
///         Prefix("-", o) => Ok(-o),
///         Binary(lhs, "*", rhs) => Ok(lhs * rhs),
///         Binary(lhs, "/", rhs) => Ok(lhs / rhs),
///         Binary(lhs, "+", rhs) => Ok(lhs + rhs),
///         Binary(lhs, "-", rhs) => Ok(lhs - rhs),
///         _ => Err("Invalid combination"),
///       }
///     }
///   )(i)
/// }

// + ContextError<Slice>
fn parenthesis<'a>(i: Slice) -> ParseResult<'a, Src<Parenthesis>> {
    context(
        "parenthesized expression",
        map(
            pair(
                tag("("),
                cut(pair(
                    preceded(whitespace, expression),
                    preceded(whitespace, tag(")")),
                )),
            ),
            |(open_paren, (inner, close_paren))| {
                Parenthesis(Box::new(inner)).with_src(open_paren.spanning(&close_paren))
            },
        ),
    )(i)
}

fn object_literal<'a>(i: Slice) -> ParseResult<'a, Src<ObjectLiteral>> {
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
            |(open_bracket, (entries, close_bracket))| {
                ObjectLiteral {
                    entries: entries
                        .into_iter()
                        .map(|(key, value)| {
                            let src = key.clone().spanning(&value.src);

                            ObjectLiteralEntry::KeyAndValue(
                                IdentifierOrExpression::PlainIdentifier(PlainIdentifier(key)),
                                value,
                            )
                            .with_src(src)
                        })
                        .collect(),
                }
                .with_src(open_bracket.spanning(&close_bracket))
            },
        ),
    )(i)
}

fn key_value<'a>(i: Slice) -> ParseResult<'a, (Slice, Src<Expression>)> {
    separated_pair(
        preceded(whitespace, identifier_like),
        cut(preceded(whitespace, char(':'))),
        preceded(whitespace, expression),
    )(i)
}

fn array_literal<'a>(i: Slice) -> ParseResult<'a, Src<ArrayLiteral>> {
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
            |(open_bracket, (entries, close_bracket))| {
                ArrayLiteral {
                    entries: entries
                        .into_iter()
                        .map(|x| x.map(ArrayLiteralEntry::Element))
                        .collect(),
                }
                .with_src(open_bracket.spanning(&close_bracket))
            },
        ),
    )(i)
}

fn exact_string_literal<'a>(i: Slice) -> ParseResult<'a, Src<ExactStringLiteral>> {
    context(
        "string",
        map(
            pair(tag("\'"), cut(pair(string_contents, tag("\'")))),
            |(open_quote, (contents, close_quote))| {
                ExactStringLiteral {
                    tag: None, // TODO
                    value: contents,
                }
                .with_src(open_quote.spanning(&close_quote))
            },
        ),
    )(i)
}

fn string_contents<'a>(i: Slice) -> ParseResult<'a, Slice> {
    escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

fn number_literal<'a>(i: Slice) -> ParseResult<'a, Src<NumberLiteral>> {
    map(
        tuple((opt(tag("-")), numeric, opt(tuple((tag("."), cut(numeric)))))),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int.clone());
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            NumberLiteral {
                value: full.clone(),
            }
            .with_src(full)
        },
    )(i)
}

fn boolean_literal<'a>(input: Slice) -> ParseResult<'a, Src<BooleanLiteral>> {
    let parse_true = map(tag("true"), |src: Slice| {
        BooleanLiteral { value: true }.with_src(src)
    });

    let parse_false = map(tag("false"), |src: Slice| {
        BooleanLiteral { value: false }.with_src(src)
    });

    alt((parse_true, parse_false))(input)
}

fn nil_literal<'a>(input: Slice) -> ParseResult<'a, Src<NilLiteral>> {
    map(tag("nil"), |src: Slice| NilLiteral.with_src(src))(input)
}

fn local_identifier<'a>(i: Slice) -> ParseResult<'a, LocalIdentifier> {
    context(
        "identifier",
        map(identifier_like, |name| LocalIdentifier(name)),
    )(i)
}

fn plain_identifier<'a>(i: Slice) -> ParseResult<'a, PlainIdentifier> {
    context(
        "identifier",
        map(identifier_like, |name| PlainIdentifier(name)),
    )(i)
}

// utils
fn identifier_like<'a>(i: Slice) -> ParseResult<'a, Slice> {
    take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$')(i)
}

fn numeric<'a>(i: Slice) -> ParseResult<'a, Slice> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace<'a>(i: Slice) -> ParseResult<'a, Slice> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i)
}

type ParseResult<'a, T> = IResult<Slice, T, RawParseError>;

#[derive(Debug, Clone, PartialEq)]
struct RawParseError {
    src: Slice,
    details: RawParseErrorDetails,
}

#[derive(Debug, Clone, PartialEq)]
enum RawParseErrorDetails {
    Kind(nom::error::ErrorKind),
    Char(char),
}

impl<'a> nom::error::ParseError<Slice> for RawParseError {
    fn from_error_kind(input: Slice, kind: nom::error::ErrorKind) -> Self {
        Self {
            src: input,
            details: RawParseErrorDetails::Kind(kind),
        }
    }

    fn append(_input: Slice, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: Slice, ch: char) -> Self {
        Self {
            src: input,
            details: RawParseErrorDetails::Char(ch),
        }
    }
}

impl<'a> nom::error::ContextError<Slice> for RawParseError {
    fn add_context(_input: Slice, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<'a, E> nom::error::FromExternalError<Slice, E> for RawParseError {
    fn from_external_error(input: Slice, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self {
            src: input,
            details: RawParseErrorDetails::Kind(kind),
        }
    }
}
