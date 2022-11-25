use std::collections::HashSet;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{alphanumeric1, char, one_of},
    combinator::{cut, fail, map, opt},
    error::context,
    multi::separated_list0,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    ast::*,
    errors::ParseError,
    precedence::{binary_op, precedence, Assoc, Operation},
    slice::Slice,
    string_and_slice::StringAndSlice,
};

pub fn parse(module_id: ModuleID, module_src: String) -> Result<Module, ParseError> {
    let bgl = module_src.clone();
    let bgl_and_slice = StringAndSlice {
        string: &bgl,
        slice: Slice::new(&bgl),
    };
    let res = terminated(separated_list0(whitespace, declaration), whitespace)(bgl_and_slice);

    match res {
        Ok((i, declarations)) => {
            if i.len() > 0 {
                Err(ParseError {
                    module_id,
                    index: Some(i.slice.start - bgl_and_slice.slice.start),
                    module_src,
                    message: "Failed to parse entire input".to_owned(),
                })
            } else {
                Ok(Module {
                    module_id,
                    src: module_src.clone(),
                    declarations,
                })
            }
        }
        Err(error) => match error {
            nom::Err::Error(RawParseError {
                module_src,
                index,
                details,
            }) => Err(ParseError {
                module_id: module_id.clone(),
                index,
                module_src,
                message: match details {
                    RawParseErrorDetails::Kind(kind) => kind.description().to_owned(),
                    RawParseErrorDetails::Char(ch) => format!("Expected '{}'", ch),
                },
            }),
            nom::Err::Failure(RawParseError {
                module_src,
                index,
                details,
            }) => Err(ParseError {
                module_id: module_id.clone(),
                index,
                module_src,
                message: match details {
                    RawParseErrorDetails::Kind(kind) => kind.description().to_owned(),
                    RawParseErrorDetails::Char(ch) => format!("Expected '{}'", ch),
                },
            }),
            nom::Err::Incomplete(_) => Err(ParseError {
                module_id: module_id.clone(),
                index: None,
                module_src: module_src.clone(),
                message: "Incomplete".to_owned(),
            }),
        },
    }
}

fn declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<Declaration>> {
    preceded(
        whitespace,
        alt((
            map(import_all_declaration, |x| x.map(Declaration::from)),
            map(import_declaration, |x| x.map(Declaration::from)),
            map(type_declaration, |x| x.map(Declaration::from)),
            map(func_declaration, |x| x.map(Declaration::from)),
            // map(proc_declaration, |x| x.map(Declaration::from)),
            map(value_declaration, |x| x.map(Declaration::from)),
            // map(test_expr_declaration, |x| x.map(Declaration::from)),
            // map(test_block_declaration, |x| x.map(Declaration::from)),
            // map(test_type_declaration, |x| x.map(Declaration::from)),
        )),
    )(i)
}

fn import_all_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<ImportAllDeclaration>> {
    context(
        "import-all declaration",
        map(
            tuple((
                tag("import"),
                preceded(whitespace, exact_string_literal),
                preceded(whitespace, tag("as")),
                preceded(whitespace, plain_identifier),
            )),
            |(start, path, _, name)| Src {
                src: name.src.map(|name| start.slice.spanning(&name)),
                node: ImportAllDeclaration { path, name },
            },
        ),
    )(i)
}

fn import_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<ImportDeclaration>> {
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
            |(start, path, _, _, imports, end)| Src {
                src: Some(start.slice.spanning(&end.slice)),
                node: ImportDeclaration { path, imports },
            },
        ),
    )(i)
}

fn type_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<TypeDeclaration>> {
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
            |(export, keyword, name, _, declared_type)| Src {
                src: declared_type
                    .src
                    .map(|decl| export.unwrap_or(keyword).slice.spanning(&decl)),
                node: TypeDeclaration {
                    name,
                    declared_type,
                    exported: export.is_some(),
                },
            },
        ),
    )(i)
}

fn func_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<FuncDeclaration>> {
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
            |(export, pure, asyn, keyword, name, _, args, _, return_type, _, body)| {
                let src = body.src.map(|decl| {
                    export
                        .unwrap_or(pure.unwrap_or(asyn.unwrap_or(keyword)))
                        .slice
                        .spanning(&decl)
                });

                Src {
                    src,
                    node: FuncDeclaration {
                        name,
                        func: Src {
                            src,
                            node: Func {
                                type_annotation: Src {
                                    src: None,
                                    node: FuncType {
                                        args,
                                        is_pure: pure.is_some(),
                                        returns: return_type.map(Box::new),
                                    },
                                },
                                is_async: asyn.is_some(),
                                is_pure: pure.is_some(),
                                body: Box::new(body),
                            },
                        },
                        exported: export.is_some(),
                        platforms: HashSet::new(), // TODO
                        decorators: vec![],        // TODO
                    },
                }
            },
        ),
    )(i)
}

fn args<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<Args>> {
    map(
        separated_list0(
            preceded(whitespace, tag(",")),
            preceded(whitespace, pair(plain_identifier, opt(type_annotation))),
        ),
        |args| Src {
            src: None,
            node: Args::Individual(
                args.into_iter()
                    .map(|(name, type_annotation)| {
                        Arg {
                            name,
                            type_annotation,
                            optional: false, // TODO
                        }
                    })
                    .collect(),
            ),
        },
    )(i)
}

fn proc_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<ProcDeclaration>> {
    todo!()
}

fn value_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<ValueDeclaration>> {
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
                Src {
                    src: value
                        .src
                        .map(|end| export.unwrap_or(keyword).slice.spanning(&end)),
                    node: ValueDeclaration {
                        name,
                        type_annotation,
                        value,
                        is_const: keyword.as_str() == "const",
                        exported: export.is_some(),
                        platforms: HashSet::new(), // TODO
                    },
                }
            },
        ),
    )(i)
}

fn test_expr_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<TestExprDeclaration>> {
    todo!()
}

fn test_block_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<TestBlockDeclaration>> {
    todo!()
}

fn test_type_declaration<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<TestTypeDeclaration>> {
    todo!()
}

fn type_annotation<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<TypeExpression>> {
    preceded(tag(":"), preceded(whitespace, type_expression))(i)
}

fn type_expression<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<TypeExpression>> {
    alt((
        map(tag("string"), |s: StringAndSlice<'a>| {
            TypeExpression::StringType.with_src(s.slice)
        }),
        map(tag("number"), |s: StringAndSlice<'a>| {
            TypeExpression::NumberType.with_src(s.slice)
        }),
        map(tag("boolean"), |s: StringAndSlice<'a>| {
            TypeExpression::BooleanType.with_src(s.slice)
        }),
        map(tag("nil"), |s: StringAndSlice<'a>| {
            TypeExpression::NilType.with_src(s.slice)
        }),
    ))(i)
}

fn statement<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<Statement>> {
    todo!()
}

fn expression<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<Expression>> {
    preceded(whitespace, alt((binary_operation, atomic_expression)))(i)
}

fn atomic_expression<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<Expression>> {
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
            map(local_identifier, |x| x.map(Expression::from)),
        )),
    )(i)
}

fn binary_operation<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<Expression>> {
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
                    map(preceded(whitespace, tag("*")), |op| Src {
                        src: Some(op.slice),
                        node: BinaryOperator::from_symbol(op.as_str()),
                    }),
                ),
                binary_op(
                    2,
                    Assoc::Left,
                    map(preceded(whitespace, tag("/")), |op| Src {
                        src: Some(op.slice),
                        node: BinaryOperator::from_symbol(op.as_str()),
                    }),
                ),
                binary_op(
                    3,
                    Assoc::Left,
                    map(preceded(whitespace, tag("+")), |op| Src {
                        src: Some(op.slice),
                        node: BinaryOperator::from_symbol(op.as_str()),
                    }),
                ),
                binary_op(
                    3,
                    Assoc::Left,
                    map(preceded(whitespace, tag("-")), |op| Src {
                        src: Some(op.slice),
                        node: BinaryOperator::from_symbol(op.as_str()),
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
                    Operation::Binary(left, op, right) => Ok(Src {
                        src: left.spanning(&right),
                        node: Expression::BinaryOperation {
                            left: Box::new(left),
                            op,
                            right: Box::new(right),
                        },
                    }),
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
fn parenthesis<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<Parenthesis>> {
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
            |(open_paren, (inner, close_paren))| Src {
                src: Some(open_paren.slice.spanning(&close_paren.slice)),
                node: Parenthesis {
                    inner: Box::new(inner),
                },
            },
        ),
    )(i)
}

fn object_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<ObjectLiteral>> {
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
            |(open_bracket, (entries, close_bracket))| Src {
                src: Some(open_bracket.slice.spanning(&close_bracket.slice)),
                node: ObjectLiteral {
                    entries: entries
                        .into_iter()
                        .map(|(key, value)| Src {
                            src: None,
                            node: ObjectLiteralEntry::KeyValue(
                                PlainIdentifier {
                                    name: key.as_str().to_owned(),
                                },
                                value,
                            ),
                        })
                        .collect(),
                },
            },
        ),
    )(i)
}

fn key_value<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, (StringAndSlice<'a>, Expression)> {
    separated_pair(
        preceded(whitespace, identifier_like),
        cut(preceded(whitespace, char(':'))),
        map(expression, |x| x.node),
    )(i)
}

fn array_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<ArrayLiteral>> {
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
            |(open_bracket, (entries, close_bracket))| Src {
                src: Some(open_bracket.slice.spanning(&close_bracket.slice)),
                node: ArrayLiteral {
                    entries: entries
                        .into_iter()
                        .map(|x| x.map(ArrayLiteralEntry::Element))
                        .collect(),
                },
            },
        ),
    )(i)
}

fn exact_string_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<ExactStringLiteral>> {
    context(
        "string",
        map(
            pair(tag("\'"), cut(pair(string_contents, tag("\'")))),
            |(open_quote, (contents, close_quote))| Src {
                src: Some(open_quote.slice.spanning(&close_quote.slice)),
                node: ExactStringLiteral {
                    tag: None, // TODO
                    value: contents.as_str().to_owned(),
                },
            },
        ),
    )(i)
}

fn string_contents<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

fn number_literal<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<NumberLiteral>> {
    map(
        tuple((opt(tag("-")), numeric, opt(tuple((tag("."), cut(numeric)))))),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int);
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            Src {
                src: Some(full.slice),
                node: NumberLiteral {
                    value: full.as_str().to_owned(),
                },
            }
        },
    )(i)
}

fn boolean_literal<'a>(input: StringAndSlice<'a>) -> ParseResult<'a, Src<BooleanLiteral>> {
    let parse_true = map(tag("true"), |src: StringAndSlice<'a>| Src {
        src: Some(src.slice),
        node: BooleanLiteral { value: true },
    });

    let parse_false = map(tag("false"), |src: StringAndSlice<'a>| Src {
        src: Some(src.slice),
        node: BooleanLiteral { value: false },
    });

    alt((parse_true, parse_false))(input)
}

fn nil_literal<'a>(input: StringAndSlice<'a>) -> ParseResult<'a, Src<NilLiteral>> {
    map(tag("nil"), |src: StringAndSlice<'a>| Src {
        src: Some(src.slice),
        node: NilLiteral,
    })(input)
}

fn local_identifier<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<LocalIdentifier>> {
    context(
        "identifier",
        map(identifier_like, |name| Src {
            src: Some(name.slice),
            node: LocalIdentifier {
                name: name.as_str().to_owned(),
            },
        }),
    )(i)
}

fn plain_identifier<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, Src<PlainIdentifier>> {
    context(
        "identifier",
        map(identifier_like, |name| Src {
            src: Some(name.slice),
            node: PlainIdentifier {
                name: name.as_str().to_owned(),
            },
        }),
    )(i)
}

// utils
fn identifier_like<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$')(i)
}

fn numeric<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace<'a>(i: StringAndSlice<'a>) -> ParseResult<'a, StringAndSlice<'a>> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i)
}

type ParseResult<'a, T> = IResult<StringAndSlice<'a>, T, RawParseError>;

#[derive(Debug, Clone, PartialEq)]
struct RawParseError {
    module_src: String,
    index: Option<usize>,
    details: RawParseErrorDetails,
}

#[derive(Debug, Clone, PartialEq)]
enum RawParseErrorDetails {
    Kind(nom::error::ErrorKind),
    Char(char),
}

impl<'a> nom::error::ParseError<StringAndSlice<'a>> for RawParseError {
    fn from_error_kind(input: StringAndSlice<'a>, kind: nom::error::ErrorKind) -> Self {
        Self {
            module_src: input.string.clone(),
            index: Some(input.slice.start),
            details: RawParseErrorDetails::Kind(kind),
        }
    }

    fn append(_input: StringAndSlice<'a>, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: StringAndSlice<'a>, ch: char) -> Self {
        Self {
            module_src: input.string.clone(),
            index: Some(input.slice.start),
            details: RawParseErrorDetails::Char(ch),
        }
    }
}

impl<'a> nom::error::ContextError<StringAndSlice<'a>> for RawParseError {
    fn add_context(_input: StringAndSlice<'a>, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<'a, E> nom::error::FromExternalError<StringAndSlice<'a>, E> for RawParseError {
    fn from_external_error(input: StringAndSlice<'a>, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self {
            module_src: input.string.clone(),
            index: Some(input.slice.start),
            details: RawParseErrorDetails::Kind(kind),
        }
    }
}
