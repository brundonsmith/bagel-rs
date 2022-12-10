use std::{rc::Rc, str::FromStr, time::SystemTime};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{char, one_of},
    combinator::{complete, cut, map, opt},
    error::{context, ErrorKind},
    multi::{many0, separated_list0, separated_list1},
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};

use crate::{
    model::ast::*,
    model::{errors::ParseError, module::ModuleID},
    model::{module::Module, slice::Slice},
    DEBUG_MODE,
};

use crate::utils::Loggable;
use memoize::memoize;

pub fn parse(module_id: ModuleID, module_src: Rc<String>) -> Result<Module, ParseError> {
    let bgl = Slice::new(module_src.clone());

    let start = SystemTime::now();
    let res = complete(terminated(
        separated_list0(whitespace, declaration),
        whitespace,
    ))(bgl);

    if DEBUG_MODE {
        println!(
            "* Parsing  {:?} took {}ms",
            module_id,
            start.elapsed().unwrap().as_millis()
        );
    }

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

// --- Declaration

fn declaration(i: Slice) -> ParseResult<Node<Declaration>> {
    preceded(
        whitespace,
        alt((
            map(import_all_declaration, |x| x.map_deep(Declaration::from)),
            map(import_declaration, |x| x.map_deep(Declaration::from)),
            map(type_declaration, |x| x.map_deep(Declaration::from)),
            map(func_declaration, |x| x.map_deep(Declaration::from)),
            map(proc_declaration, |x| x.map_deep(Declaration::from)),
            map(value_declaration, |x| x.map_deep(Declaration::from)),
            map(test_expr_declaration, |x| x.map_deep(Declaration::from)),
            map(test_block_declaration, |x| x.map_deep(Declaration::from)),
            // map(test_type_declaration, |x| x.map_deep(Declaration::from)),
        )),
    )(i)
}

fn import_all_declaration(i: Slice) -> ParseResult<Node<ImportAllDeclaration>> {
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

                ImportAllDeclaration { path, name }.with_slice(src)
            },
        ),
    )(i)
}

fn import_declaration(i: Slice) -> ParseResult<Node<ImportDeclaration>> {
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
                ImportDeclaration { path, imports }.with_slice(start.spanning(&end))
            },
        ),
    )(i)
}

fn type_declaration(i: Slice) -> ParseResult<Node<TypeDeclaration>> {
    context(
        "type declaration",
        map(
            tuple((
                opt(tag("export")),
                preceded(whitespace, tag("type")),
                preceded(whitespace, plain_identifier),
                preceded(whitespace, tag("=")),
                preceded(whitespace, type_expression(0)),
            )),
            |(export, keyword, name, _, declared_type)| {
                let exported = export.is_some();
                let src = export.unwrap_or(keyword).spanning(&declared_type.slice);

                TypeDeclaration {
                    name,
                    declared_type,
                    exported,
                }
                .with_slice(src)
            },
        ),
    )(i)
}

fn func_declaration(i: Slice) -> ParseResult<Node<FuncDeclaration>> {
    context(
        "func declaration",
        map(
            tuple((
                opt(tag("export")),
                opt(preceded(whitespace, tag("pure"))),
                opt(preceded(whitespace, tag("async"))),
                preceded(whitespace, tag("func")),
                preceded(whitespace, plain_identifier),
                preceded(whitespace, args),
                preceded(whitespace, opt(type_annotation)),
                preceded(whitespace, tag("=>")),
                preceded(whitespace, expression(0)),
            )),
            |(export, pure, asyn, keyword, name, args, returns, _, body)| {
                let exported = export.is_some();
                let is_async = asyn.is_some();
                let is_pure = pure.is_some();
                let src = export
                    .unwrap_or(pure.unwrap_or(asyn.unwrap_or(keyword)))
                    .spanning(&body.slice);
                let args_start = args[0].slice.clone();

                FuncDeclaration {
                    name,
                    func: Func {
                        type_annotation: FuncType {
                            args,
                            args_spread: None, // TODO
                            is_pure,
                            returns,
                        }
                        .with_slice(args_start.spanning(&body.slice)),

                        is_async,
                        is_pure,
                        body: FuncBody::Expression(body),
                    }
                    .with_slice(src.clone()),
                    exported,
                    platforms: PlatformSet::all(), // TODO
                    decorators: vec![],            // TODO
                }
                .with_slice(src)
            },
        ),
    )(i)
}

fn args(i: Slice) -> ParseResult<Vec<Node<Arg>>> {
    alt((
        map(plain_identifier, |name| {
            vec![Arg {
                name: name.clone(),
                type_annotation: None,
                optional: false,
            }
            .with_slice(name.0)]
        }),
        map(
            tuple((
                tag("("),
                preceded(
                    whitespace,
                    separated_list0(
                        preceded(whitespace, tag(",")),
                        preceded(whitespace, pair(plain_identifier, opt(type_annotation))),
                    ),
                ),
                preceded(whitespace, tag(")")),
            )),
            |(_, args, _)| {
                args.into_iter()
                    .map(|(name, type_annotation)| {
                        let src = name.0.clone().spanning(
                            &type_annotation
                                .as_ref()
                                .map(|t| t.slice.clone())
                                .unwrap_or(name.0.clone()),
                        );

                        Arg {
                            name,
                            type_annotation,
                            optional: false, // TODO
                        }
                        .with_slice(src)
                    })
                    .collect()
            },
        ),
    ))(i)
}

fn proc_declaration(i: Slice) -> ParseResult<Node<ProcDeclaration>> {
    context(
        "proc declaration",
        map(
            tuple((
                opt(tag("export")),
                opt(preceded(whitespace, tag("pure"))),
                opt(preceded(whitespace, tag("async"))),
                preceded(whitespace, tag("proc")),
                preceded(whitespace, plain_identifier),
                preceded(whitespace, args),
                preceded(whitespace, tag("{")),
                many0(preceded(whitespace, statement)),
                preceded(whitespace, tag("}")),
            )),
            |(export, pure, asyn, keyword, name, args, _, body, closing_brace)| {
                let exported = export.is_some();
                let is_async = asyn.is_some();
                let is_pure = pure.is_some();
                let src = export
                    .unwrap_or(pure.unwrap_or(asyn.unwrap_or(keyword)))
                    .spanning(&closing_brace);
                let args_start = args[0].slice.clone();

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
                        .with_slice(args_start.spanning(&closing_brace)),

                        is_async,
                        is_pure,
                        body: ProcBody::Statements(body),
                    }
                    .with_slice(src.clone()),
                    exported,
                    platforms: PlatformSet::all(), // TODO
                    decorators: vec![],            // TODO
                }
                .with_slice(src)
            },
        ),
    )(i)
}

fn value_declaration(i: Slice) -> ParseResult<Node<ValueDeclaration>> {
    context(
        "value declaration",
        map(
            tuple((
                opt(tag("export")),
                preceded(whitespace, alt((tag("const"), tag("let")))),
                preceded(whitespace, plain_identifier),
                opt(preceded(whitespace, type_annotation)),
                preceded(whitespace, tag("=")),
                preceded(whitespace, expression(0)),
            )),
            |(export, keyword, name, type_annotation, _, value)| {
                let exported = export.is_some();
                let is_const = keyword.as_str() == "const";
                let src = export.unwrap_or(keyword).spanning(&value.slice);

                ValueDeclaration {
                    name,
                    type_annotation,
                    value,
                    is_const,
                    exported,
                    platforms: PlatformSet::all(), // TODO
                }
                .with_slice(src)
            },
        ),
    )(i)
}

fn test_expr_declaration(i: Slice) -> ParseResult<Node<TestExprDeclaration>> {
    map(
        tuple((
            tag("test"),
            preceded(whitespace, tag("expr")),
            preceded(whitespace, exact_string_literal),
            preceded(whitespace, tag("=>")),
            preceded(whitespace, expression(0)),
        )),
        |(start, _, name, _, expr)| {
            let src = start.spanning(&expr.slice);

            TestExprDeclaration { name, expr }.with_slice(src)
        },
    )(i)
}

fn test_block_declaration(i: Slice) -> ParseResult<Node<TestBlockDeclaration>> {
    map(
        tuple((
            tag("test"),
            preceded(whitespace, tag("block")),
            preceded(whitespace, exact_string_literal),
            preceded(whitespace, tag("{")),
            many0(preceded(whitespace, statement)),
            preceded(whitespace, tag("}")),
        )),
        |(start, _, name, _, block, end)| {
            TestBlockDeclaration { name, block }.with_slice(start.spanning(&end))
        },
    )(i)
}

fn test_type_declaration(i: Slice) -> ParseResult<Node<TestTypeDeclaration>> {
    todo!()
}

fn type_annotation(i: Slice) -> ParseResult<Node<TypeExpression>> {
    preceded(tag(":"), preceded(whitespace, type_expression(0)))(i)
}

// --- TypeExpression ---

macro_rules! type_expr_level {
    ($level:expr, $this_level:expr, $i:expr, $a:expr) => {
        $this_level += 1;
        if $level <= $this_level {
            let res = map($a, |x| x.map_deep(TypeExpression::from))($i.clone());

            if res.is_ok() {
                return res;
            }
        }
    };
}

fn type_expression(l: usize) -> impl Fn(Slice) -> ParseResult<Node<TypeExpression>> {
    move |i: Slice| -> ParseResult<Node<TypeExpression>> {
        let mut tl = 0;

        type_expr_level!(
            l,
            tl,
            i,
            map(
                tuple((tag("typeof"), preceded(whitespace, expression(0)))),
                |(keyword, expr)| {
                    let src = keyword.clone().spanning(&expr.slice);

                    TypeExpression::TypeofType(expr).with_slice(src)
                }
            )
        );

        type_expr_level!(
            l,
            tl,
            i,
            map(
                tuple((
                    alt((
                        tag("keyof"),
                        tag("valueof"),
                        tag("elementof"),
                        tag("readonly")
                    )),
                    preceded(whitespace, type_expression(0))
                )),
                |(keyword, inner)| {
                    let src = keyword.clone().spanning(&inner.slice);

                    match keyword.as_str() {
                        "keyof" => TypeExpression::KeyofType(inner),
                        "valueof" => TypeExpression::ValueofType(inner),
                        "elementof" => TypeExpression::ElementofType(inner),
                        "readonly" => TypeExpression::ReadonlyType(inner),
                        _ => unreachable!(),
                    }
                    .with_slice(src)
                }
            )
        );

        // generic type

        type_expr_level!(
            l,
            tl,
            i,
            map(
                separated_list2(
                    preceded(whitespace, tag("|")),
                    preceded(whitespace, type_expression(tl + 1))
                ),
                |members| {
                    let src = members[0]
                        .slice
                        .clone()
                        .spanning(&members[members.len() - 1].slice);

                    TypeExpression::UnionType(members).with_slice(src)
                }
            )
        );

        type_expr_level!(
            l,
            tl,
            i,
            map(
                tuple((type_expression(tl + 1), tag("?"))),
                |(inner, end)| {
                    let src = inner.slice.clone().spanning(&end);

                    TypeExpression::MaybeType(inner).with_slice(src)
                }
            )
        );

        // boundGenericType

        type_expr_level!(
            l,
            tl,
            i,
            map(
                tuple((type_expression(tl + 1), tag("[]"))),
                |(element, end)| {
                    let src = element.slice.clone().spanning(&end);

                    TypeExpression::ArrayType(element).with_slice(src)
                }
            )
        );

        type_expr_level!(
            l,
            tl,
            i,
            alt((
                map(func_type, |s| s.map_deep(TypeExpression::from)),
                map(proc_type, |s| s.map_deep(TypeExpression::from)),
                map(record_type, |s| s.map_deep(TypeExpression::from)),
                object_or_interface_type,
                map(tuple_type, |s| s.map_deep(TypeExpression::from)),
                map(
                    tuple((
                        tag("("),
                        preceded(whitespace, type_expression(tl + 1)),
                        preceded(whitespace, tag(")"))
                    )),
                    |(open, inner, close)| {
                        TypeExpression::ParenthesizedType(inner).with_slice(open.spanning(&close))
                    }
                ),
                map(exact_string_literal, |s| {
                    s.map_deep(|s| TypeExpression::LiteralType(LiteralTypeValue::ExactString(s)))
                }),
                map(number_literal, |s| {
                    s.map_deep(|s| TypeExpression::LiteralType(LiteralTypeValue::NumberLiteral(s)))
                }),
                map(boolean_literal, |s| {
                    s.map_deep(|s| TypeExpression::LiteralType(LiteralTypeValue::BooleanLiteral(s)))
                }),
                map(tag("string"), |s: Slice| {
                    TypeExpression::StringType.with_slice(s)
                }),
                map(tag("number"), |s: Slice| {
                    TypeExpression::NumberType.with_slice(s)
                }),
                map(tag("boolean"), |s: Slice| {
                    TypeExpression::BooleanType.with_slice(s)
                }),
                map(tag("unknown"), |s: Slice| TypeExpression::UnknownType
                    .with_slice(s)),
                map(tag("nil"), |s: Slice| TypeExpression::NilType.with_slice(s)),
            ))
        );

        type_expr_level!(
            l,
            tl,
            i,
            map(plain_identifier, |name| {
                let src = name.0.clone();

                TypeExpression::NamedType(name).with_slice(src)
            })
        );

        Err(nom::Err::Error(RawParseError {
            src: i,
            details: RawParseErrorDetails::Kind(ErrorKind::Fail),
        }))
    }
}

#[memoize]
fn func_type(i: Slice) -> ParseResult<Node<FuncType>> {
    map(
        tuple((
            args,
            preceded(whitespace, tag("=>")),
            preceded(whitespace, type_expression(0)),
        )),
        |(args, _, returns)| {
            let src = args // TODO: func type with 0 arguments will have weird src
                .get(0)
                .map(|a| a.slice.clone())
                .unwrap_or_else(|| returns.slice.clone())
                .spanning(&returns.slice);

            FuncType {
                args,
                args_spread: None, // TODO
                is_pure: false,    // TODO
                returns: Some(returns),
            }
            .with_slice(src)
        },
    )(i)
}

#[memoize]
fn proc_type(i: Slice) -> ParseResult<Node<ProcType>> {
    map(
        tuple((
            args,
            preceded(whitespace, tag("(")),
            preceded(whitespace, tag(")")),
            preceded(whitespace, tag("{")),
            preceded(whitespace, tag("}")),
        )),
        |(args, _, _, _, close)| {
            let src = args // TODO: proc type with 0 arguments will have weird src
                .get(0)
                .map(|a| a.slice.clone())
                .unwrap_or_else(|| close.clone())
                .spanning(&close);

            ProcType {
                args,
                args_spread: None, // TODO
                is_pure: false,    // TODO
                is_async: false,   // TODO
                throws: None,      // TODO
            }
            .with_slice(src)
        },
    )(i)
}

#[memoize]
fn record_type(i: Slice) -> ParseResult<Node<RecordType>> {
    map(
        tuple((
            tag("{"),
            preceded(whitespace, tag("[")),
            preceded(whitespace, type_expression(0)),
            preceded(whitespace, tag("]")),
            preceded(whitespace, tag(":")),
            preceded(whitespace, type_expression(0)),
            preceded(whitespace, tag("}")),
        )),
        |(open, _, key_type, _, _, value_type, close)| {
            RecordType {
                key_type: key_type,
                value_type: value_type,
            }
            .with_slice(open.spanning(&close))
        },
    )(i)
}

#[memoize]
fn object_or_interface_type(i: Slice) -> ParseResult<Node<TypeExpression>> {
    map(
        tuple((
            opt(tag("interface")),
            preceded(whitespace, tag("{")),
            separated_list0(
                preceded(whitespace, tag(",")),
                preceded(whitespace, key_value_type_expression),
            ),
            preceded(whitespace, tag("}")),
        )),
        |(interface, open, entries, close)| {
            if let Some(interface) = interface {
                TypeExpression::InterfaceType(entries).with_slice(interface.spanning(&close))
            } else {
                TypeExpression::ObjectType(
                    entries
                        .into_iter()
                        .map(|(key, value)| ObjectTypeEntry::KeyValue(key, value))
                        .collect(),
                )
                .with_slice(open.spanning(&close))
            }
        },
    )(i)
}

fn key_value_type_expression(i: Slice) -> ParseResult<(PlainIdentifier, Node<TypeExpression>)> {
    separated_pair(
        preceded(whitespace, plain_identifier),
        cut(preceded(whitespace, char(':'))),
        preceded(whitespace, type_expression(0)),
    )(i)
}

fn tuple_type(i: Slice) -> ParseResult<Node<TupleType>> {
    map(
        tuple((
            tag("["),
            separated_list0(
                preceded(whitespace, tag(",")),
                preceded(whitespace, type_expression(0)),
            ),
            preceded(whitespace, tag("]")),
        )),
        |(open, members, close)| TupleType(members).with_slice(open.spanning(&close)),
    )(i)
}

// --- Statement ---

fn statement(i: Slice) -> ParseResult<Node<Statement>> {
    alt((
        // map(declaration_statement, |x| x.map(Statement::from)),
        map(if_else_statement, |x| x.map_deep(Statement::from)),
        map(for_loop, |x| x.map_deep(Statement::from)),
        map(while_loop, |x| x.map_deep(Statement::from)),
        // map(assignment, |x| x.map_deep(Statement::from)),
        // map(try_catch, |x| x.map_deep(Statement::from)),
        map(throw_statement, |x| x.map_deep(Statement::from)),
        map(autorun, |x| x.map_deep(Statement::from)),
        // map(invocation_statement, |x| x.map_deep(Statement::from)),
    ))(i)
}

fn declaration_statement(i: Slice) -> ParseResult<Node<DeclarationStatement>> {
    todo!()
}

fn if_else_statement(i: Slice) -> ParseResult<Node<IfElseStatement>> {
    map(
        tuple((
            separated_list1(preceded(whitespace, tag("else")), if_else_statement_case),
            opt(preceded(
                whitespace,
                tuple((
                    tag("else"),
                    preceded(whitespace, tag("{")),
                    many0(preceded(whitespace, statement)),
                    preceded(whitespace, tag("}")),
                )),
            )),
        )),
        |(cases, default_case)| {
            let src = cases[0].slice.clone().spanning(
                &default_case
                    .as_ref()
                    .map(|(start, _, _, end)| start.clone().spanning(end))
                    .unwrap_or_else(|| cases[cases.len() - 1].slice.clone()),
            );

            IfElseStatement {
                cases,
                default_case: default_case.map(|(_, _, statements, _)| statements),
            }
            .with_slice(src)
        },
    )(i)
}

fn if_else_statement_case(i: Slice) -> ParseResult<Node<(Node<Expression>, Vec<Node<Statement>>)>> {
    map(
        tuple((
            tag("if"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("{")),
            many0(preceded(whitespace, statement)),
            preceded(whitespace, tag("}")),
        )),
        |(start, condition, _, outcome, end)| (condition, outcome).with_slice(start.spanning(&end)),
    )(i)
}

fn for_loop(i: Slice) -> ParseResult<Node<ForLoop>> {
    map(
        tuple((
            tag("for"),
            preceded(whitespace, plain_identifier),
            preceded(whitespace, tag("in")),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("{")),
            many0(preceded(whitespace, statement)),
            preceded(whitespace, tag("}")),
        )),
        |(start, item_identifier, _, iterator, _, body, end)| {
            ForLoop {
                item_identifier,
                iterator,
                body,
            }
            .with_slice(start.spanning(&end))
        },
    )(i)
}

fn while_loop(i: Slice) -> ParseResult<Node<WhileLoop>> {
    map(
        tuple((
            tag("while"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("{")),
            many0(preceded(whitespace, statement)),
            preceded(whitespace, tag("}")),
        )),
        |(start, condition, _, body, end)| {
            WhileLoop { condition, body }.with_slice(start.spanning(&end))
        },
    )(i)
}

fn assignment(i: Slice) -> ParseResult<Node<Assignment>> {
    todo!()
}

fn try_catch(i: Slice) -> ParseResult<Node<TryCatch>> {
    todo!()
}

fn throw_statement(i: Slice) -> ParseResult<Node<ThrowStatement>> {
    map(
        tuple((
            tag("throw"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag(";")),
        )),
        |(start, error_expression, end)| {
            ThrowStatement { error_expression }.with_slice(start.spanning(&end))
        },
    )(i)
}

#[memoize]
fn autorun(i: Slice) -> ParseResult<Node<Autorun>> {
    map(
        tuple((
            tag("autorun"),
            preceded(whitespace, tag("{")),
            many0(preceded(whitespace, statement)),
            preceded(whitespace, tag("}")),
            preceded(
                whitespace,
                alt((
                    map(tag("forever"), |_| None),
                    map(
                        tuple((
                            tag("until"),
                            preceded(whitespace, tag("=>")),
                            preceded(whitespace, expression(0)),
                        )),
                        |(_, _, until)| Some(until),
                    ),
                )),
            ),
            preceded(whitespace, tag(";")),
        )),
        |(start, _, effect, _, until, end)| {
            Autorun { effect, until }.with_slice(start.spanning(&end))
        },
    )(i)
}

fn invocation_statement(i: Slice) -> ParseResult<Node<InvocationStatement>> {
    todo!()
}

// --- Expression ---

macro_rules! expr_level {
    ($level:expr, $this_level:expr, $i:expr, $a:expr) => {
        $this_level += 1;
        if $level <= $this_level {
            let res = map($a, |x| x.map_deep(Expression::from))($i.clone());

            if res.is_ok() {
                return res;
            }
        }
    };
}

fn expression(l: usize) -> impl Fn(Slice) -> ParseResult<Node<Expression>> {
    move |i: Slice| -> ParseResult<Node<Expression>> {
        let mut tl = 0;

        // expr_level!(level, this_level, i, ); // debug, javascriptEscape, elementTag
        expr_level!(
            l,
            tl,
            i,
            alt((
                map(func, |x| x.map_deep(Expression::from)),
                // map(proc, |x| x.map(Expression::from)),
            ))
        );
        expr_level!(l, tl, i, binary_operator_1(tl, "??"));
        expr_level!(l, tl, i, binary_operator_1(tl, "||"));
        expr_level!(l, tl, i, binary_operator_1(tl, "&&"));
        expr_level!(l, tl, i, binary_operator_2(tl, "==", "!="));
        expr_level!(l, tl, i, binary_operator_4(tl, "<=", ">=", "<", ">"));
        expr_level!(l, tl, i, binary_operator_2(tl, "+", "-"));
        expr_level!(l, tl, i, binary_operator_2(tl, "*", "/"));
        expr_level!(
            l,
            tl,
            i,
            alt((
                map(as_cast(tl), |x| x.map_deep(Expression::from)),
                map(instance_of(tl), |x| x.map_deep(Expression::from)),
            ))
        );
        expr_level!(l, tl, i, negation_operation(tl));

        // indexer

        // expr_level!(l, tl, i, error_expression);

        // invocationAccessorChain

        expr_level!(l, tl, i, range_expression);

        expr_level!(l, tl, i, parenthesis(tl));

        expr_level!(
            l,
            tl,
            i,
            alt((
                map(if_else_expression, |x| x.map_deep(Expression::from)),
                map(switch_expression, |x| x.map_deep(Expression::from)),
                // map(inline_const_group, |x| x.map_deep(Expression::from)),
                map(object_literal, |x| x.map_deep(Expression::from)),
                map(array_literal, |x| x.map_deep(Expression::from)),
                map(exact_string_literal, |x| x.map_deep(Expression::from)),
                map(number_literal, |x| x.map_deep(Expression::from)),
                map(boolean_literal, |x| x.map_deep(Expression::from)),
                map(nil_literal, |x| x.map_deep(Expression::from)),
            ))
        );
        expr_level!(
            l,
            tl,
            i,
            alt((
                map(local_identifier, |x| {
                    Expression::from(x.clone()).with_slice(x.0)
                }),
                // localIdentifier, regExp
            ))
        );

        Err(nom::Err::Error(RawParseError {
            src: i,
            details: RawParseErrorDetails::Kind(ErrorKind::Fail),
        }))
    }
}

fn binary_operator_1(
    level: usize,
    a: &'static str,
) -> impl Fn(Slice) -> ParseResult<Node<Expression>> {
    move |i: Slice| -> ParseResult<Node<Expression>> {
        map(
            tuple((
                expression(level + 1),
                preceded(whitespace, tag(a)),
                preceded(whitespace, expression(level + 1)),
            )),
            |(left, op, right)| {
                let src = left.slice.clone().spanning(&right.slice);

                Expression::BinaryOperation {
                    left: left,
                    op: BinaryOperator::from_str(op.as_str())
                        .unwrap()
                        .with_slice(op),
                    right: right,
                }
                .with_slice(src)
            },
        )(i.clone())
    }
}

fn binary_operator_2(
    level: usize,
    a: &'static str,
    b: &'static str,
) -> impl Fn(Slice) -> ParseResult<Node<Expression>> {
    move |i: Slice| -> ParseResult<Node<Expression>> {
        map(
            tuple((
                expression(level + 1),
                preceded(whitespace, alt((tag(a), tag(b)))),
                preceded(whitespace, expression(level + 1)),
            )),
            |(left, op, right)| {
                let src = left.slice.clone().spanning(&right.slice);

                Expression::BinaryOperation {
                    left: left,
                    op: BinaryOperator::from_str(op.as_str())
                        .unwrap()
                        .with_slice(op),
                    right: right,
                }
                .with_slice(src)
            },
        )(i.clone())
    }
}

fn binary_operator_4(
    level: usize,
    a: &'static str,
    b: &'static str,
    c: &'static str,
    d: &'static str,
) -> impl Fn(Slice) -> ParseResult<Node<Expression>> {
    move |i: Slice| -> ParseResult<Node<Expression>> {
        map(
            tuple((
                expression(level + 1),
                preceded(whitespace, alt((tag(a), tag(b), tag(c), tag(d)))),
                preceded(whitespace, expression(level + 1)),
            )),
            |(left, op, right)| {
                let src = left.slice.clone().spanning(&right.slice);

                Expression::BinaryOperation {
                    left: left,
                    op: BinaryOperator::from_str(op.as_str())
                        .unwrap()
                        .with_slice(op),
                    right: right,
                }
                .with_slice(src)
            },
        )(i.clone())
    }
}

fn regular_expression(i: Slice) -> ParseResult<Node<RegularExpression>> {
    todo!()
}

fn error_expression(i: Slice) -> ParseResult<Node<ErrorExpression>> {
    todo!()
}

fn instance_of(level: usize) -> impl Parser<Slice, Node<InstanceOf>, RawParseError> {
    move |i: Slice| -> ParseResult<Node<InstanceOf>> {
        map(
            tuple((
                expression(level + 1),
                preceded(whitespace, tag("instanceof")),
                type_expression(0),
            )),
            |(inner, _, possible_type)| {
                let src = inner.slice.clone().spanning(&possible_type.slice);

                InstanceOf {
                    inner: inner,
                    possible_type: possible_type,
                }
                .with_slice(src)
            },
        )(i)
    }
}

fn as_cast(level: usize) -> impl Parser<Slice, Node<AsCast>, RawParseError> {
    move |i: Slice| -> ParseResult<Node<AsCast>> {
        map(
            tuple((
                expression(level + 1),
                preceded(whitespace, tag("as")),
                type_expression(0),
            )),
            |(inner, _, as_type)| {
                let src = inner.slice.clone().spanning(&as_type.slice);

                AsCast {
                    inner: inner,
                    as_type: as_type,
                }
                .with_slice(src)
            },
        )(i)
    }
}

fn element_tag(i: Slice) -> ParseResult<Node<ElementTag>> {
    todo!()
}

#[memoize]
fn if_else_expression(i: Slice) -> ParseResult<Node<IfElseExpression>> {
    map(
        tuple((
            separated_list1(
                preceded(whitespace, tag("else")),
                preceded(whitespace, if_else_expression_case),
            ),
            opt(preceded(
                whitespace,
                tuple((
                    tag("else"),
                    preceded(whitespace, tag("{")),
                    preceded(whitespace, expression(0)),
                    preceded(whitespace, tag("}")),
                )),
            )),
        )),
        |(cases, default_case)| {
            let src = cases[0].slice.clone().spanning(
                &default_case
                    .as_ref()
                    .map(|(start, _, _, end)| start.clone().spanning(end))
                    .unwrap_or_else(|| cases[cases.len() - 1].slice.clone()),
            );

            IfElseExpression {
                cases,
                default_case: default_case.map(|(_, _, expr, _)| expr),
            }
            .with_slice(src)
        },
    )(i)
}

fn if_else_expression_case(i: Slice) -> ParseResult<Node<(Node<Expression>, Node<Expression>)>> {
    map(
        tuple((
            tag("if"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("{")),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("}")),
        )),
        |(start, condition, _, outcome, end)| (condition, outcome).with_slice(start.spanning(&end)),
    )(i)
}

#[memoize]
fn switch_expression(i: Slice) -> ParseResult<Node<SwitchExpression>> {
    map(
        tuple((
            tag("switch"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("{")),
            preceded(
                whitespace,
                separated_list1(
                    preceded(whitespace, tag(",")),
                    preceded(
                        whitespace,
                        map(
                            tuple((
                                tag("case"),
                                preceded(whitespace, type_expression(0)),
                                preceded(whitespace, tag(":")),
                                preceded(whitespace, expression(0)),
                            )),
                            |(keyword, typ, _, expr)| (typ, expr),
                        ),
                    ),
                ),
            ),
            opt(preceded(
                whitespace,
                map(
                    tuple((tag("default"), preceded(whitespace, expression(0)))),
                    |(_, expr)| expr,
                ),
            )),
            preceded(whitespace, tag("}")),
        )),
        |(start, value, _, cases, default_case, end)| {
            SwitchExpression {
                value,
                cases,
                default_case,
            }
            .with_slice(start.spanning(&end))
        },
    )(i)
}

fn range_expression(i: Slice) -> ParseResult<Node<RangeExpression>> {
    map(
        tuple((number_literal, tag(".."), number_literal)),
        |(start, _, end)| {
            let src = start.slice.clone().spanning(&end.slice);

            RangeExpression {
                start: start.map_deep(Expression::from),
                end: end.map_deep(Expression::from),
            }
            .with_slice(src)
        },
    )(i)
}

fn javascript_escape_expression(i: Slice) -> ParseResult<Node<JavascriptEscapeExpression>> {
    todo!()
}

fn proc(i: Slice) -> ParseResult<Node<Proc>> {
    todo!()
}

#[memoize]
fn func(i: Slice) -> ParseResult<Node<Func>> {
    map(
        tuple((
            opt(tag("pure")),
            opt(preceded(whitespace, tag("async"))),
            preceded(whitespace, args),
            preceded(whitespace, opt(type_annotation)),
            preceded(whitespace, tag("=>")),
            preceded(whitespace, expression(0)),
        )),
        |(pure, asyn, args, returns, _, body)| {
            let is_async = asyn.is_some();
            let is_pure = pure.is_some();
            let src = pure
                .unwrap_or(asyn.unwrap_or(args[0].slice.clone()))
                .spanning(&body.slice);

            Func {
                type_annotation: FuncType {
                    args,
                    args_spread: None, // TODO
                    is_pure,
                    returns,
                }
                .with_slice(src.clone()),

                is_async,
                is_pure,
                body: FuncBody::Expression(body),
            }
            .with_slice(src.clone())
        },
    )(i)
}

fn inline_const_group(i: Slice) -> ParseResult<Node<InlineConstGroup>> {
    todo!()
}

fn negation_operation(level: usize) -> impl Parser<Slice, Node<NegationOperation>, RawParseError> {
    move |i: Slice| -> ParseResult<Node<NegationOperation>> {
        map(
            tuple((tag("!"), preceded(whitespace, expression(level + 1)))),
            |(start, expr)| {
                let src = start.spanning(&expr.slice);

                NegationOperation(expr).with_slice(src)
            },
        )(i)
    }
}

fn parenthesis(level: usize) -> impl Parser<Slice, Node<Parenthesis>, RawParseError> {
    move |i: Slice| -> ParseResult<Node<Parenthesis>> {
        context(
            "parenthesized expression",
            map(
                pair(
                    tag("("),
                    cut(pair(
                        preceded(whitespace, expression(level + 1)),
                        preceded(whitespace, tag(")")),
                    )),
                ),
                |(open_paren, (inner, close_paren))| {
                    Parenthesis(inner).with_slice(open_paren.spanning(&close_paren))
                },
            ),
        )(i)
    }
}

#[memoize]
fn object_literal(i: Slice) -> ParseResult<Node<ObjectLiteral>> {
    context(
        "object",
        map(
            pair(
                tag("{"),
                cut(pair(
                    separated_list0(preceded(whitespace, char(',')), key_value_expression),
                    preceded(whitespace, tag("}")),
                )),
            ),
            |(open_bracket, (entries, close_bracket))| {
                ObjectLiteral(
                    entries
                        .into_iter()
                        .map(|(key, value)| {
                            let src = key.clone().spanning(&value.slice);

                            ObjectLiteralEntry::KeyAndValue(
                                IdentifierOrExpression::PlainIdentifier(PlainIdentifier(key)),
                                value,
                            )
                            .with_slice(src)
                        })
                        .collect(),
                )
                .with_slice(open_bracket.spanning(&close_bracket))
            },
        ),
    )(i)
}

fn key_value_expression(i: Slice) -> ParseResult<(Slice, Node<Expression>)> {
    separated_pair(
        preceded(whitespace, identifier_like),
        cut(preceded(whitespace, char(':'))),
        preceded(whitespace, expression(0)),
    )(i)
}

#[memoize]
fn array_literal(i: Slice) -> ParseResult<Node<ArrayLiteral>> {
    context(
        "array",
        map(
            pair(
                tag("["),
                cut(pair(
                    separated_list0(preceded(whitespace, char(',')), expression(0)),
                    preceded(whitespace, tag("]")),
                )),
            ),
            |(open_bracket, (entries, close_bracket))| {
                ArrayLiteral(
                    entries
                        .into_iter()
                        .map(|x| x.map_deep(ArrayLiteralEntry::Element))
                        .collect(),
                )
                .with_slice(open_bracket.spanning(&close_bracket))
            },
        ),
    )(i)
}

#[memoize]
fn string_literal(i: Slice) -> ParseResult<Node<StringLiteral>> {
    context(
        "string",
        map(
            tuple((
                tag("\'"),
                cut(tuple((many0(string_literal_segment), tag("\'")))),
            )),
            |(open_quote, (segments, close_quote))| {
                StringLiteral {
                    tag: None, // TODO
                    segments,
                }
                .with_slice(open_quote.spanning(&close_quote))
            },
        ),
    )(i)
}

#[memoize]
fn exact_string_literal(i: Slice) -> ParseResult<Node<ExactStringLiteral>> {
    context(
        "string",
        map(
            tuple((tag("\'"), string_contents, tag("\'"))),
            |(open_quote, contents, close_quote)| {
                ExactStringLiteral {
                    tag: None, // TODO
                    value: contents,
                }
                .with_slice(open_quote.spanning(&close_quote))
            },
        ),
    )(i)
}

fn string_literal_segment(i: Slice) -> ParseResult<Node<StringLiteralSegment>> {
    alt((
        map(
            tuple((
                tag("${"),
                preceded(whitespace, expression(0)),
                preceded(whitespace, tag("}")),
            )),
            |(open, expr, close)| {
                StringLiteralSegment::Expression(expr).with_slice(open.spanning(&close))
            },
        ),
        map(string_contents, |s| {
            StringLiteralSegment::String(s.clone()).with_slice(s)
        }),
    ))(i)
}

fn string_contents(i: Slice) -> ParseResult<Slice> {
    escaped(take_while(|ch: char| ch != '\''), '\\', one_of("$\'n\\"))(i)
}

fn number_literal(i: Slice) -> ParseResult<Node<NumberLiteral>> {
    map(
        tuple((opt(tag("-")), numeric, opt(tuple((tag("."), cut(numeric)))))),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int.clone());
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            NumberLiteral(full.clone()).with_slice(full)
        },
    )(i)
}

fn boolean_literal(input: Slice) -> ParseResult<Node<BooleanLiteral>> {
    let parse_true = map(tag("true"), |src: Slice| {
        BooleanLiteral(true).with_slice(src)
    });

    let parse_false = map(tag("false"), |src: Slice| {
        BooleanLiteral(false).with_slice(src)
    });

    alt((parse_true, parse_false))(input)
}

fn nil_literal(input: Slice) -> ParseResult<Node<NilLiteral>> {
    map(tag("nil"), |src: Slice| NilLiteral.with_slice(src))(input)
}

fn local_identifier(i: Slice) -> ParseResult<LocalIdentifier> {
    context(
        "identifier",
        map(identifier_like, |name| LocalIdentifier(name)),
    )(i)
}

fn plain_identifier(i: Slice) -> ParseResult<PlainIdentifier> {
    context(
        "identifier",
        map(identifier_like, |name| PlainIdentifier(name)),
    )(i)
}

// --- Util parsers ---

fn identifier_like(i: Slice) -> ParseResult<Slice> {
    take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$')(i)
}

fn numeric(i: Slice) -> ParseResult<Slice> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace(i: Slice) -> ParseResult<Slice> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i)
}

fn separated_list2<O, O2, F, G>(sep: G, f: F) -> impl FnMut(Slice) -> ParseResult<Vec<O>>
where
    F: Parser<Slice, O, RawParseError>,
    G: Parser<Slice, O2, RawParseError>,
{
    let mut parser = separated_list1(sep, f);

    move |i: Slice| -> ParseResult<Vec<O>> {
        let res = parser(i)?;

        if res.1.len() < 2 {
            Err(nom::Err::Error(RawParseError {
                src: res.0,
                details: RawParseErrorDetails::Kind(ErrorKind::SeparatedList),
            }))
        } else {
            Ok(res)
        }
    }
}

// --- Util types ---

type ParseResult<T> = IResult<Slice, T, RawParseError>;

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

impl nom::error::ParseError<Slice> for RawParseError {
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

impl nom::error::ContextError<Slice> for RawParseError {
    fn add_context(_input: Slice, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<E> nom::error::FromExternalError<Slice, E> for RawParseError {
    fn from_external_error(input: Slice, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self {
            src: input,
            details: RawParseErrorDetails::Kind(kind),
        }
    }
}
