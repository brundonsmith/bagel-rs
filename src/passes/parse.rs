use crate::{
    model::ast::*,
    model::slice::Slice,
    model::{
        ast::{covering, ASTDetails, AST},
        errors::ParseError,
        module::ModuleID,
    },
    utils::Loggable,
    DEBUG_MODE,
};
use memoize::memoize;
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
use std::str::FromStr;
use std::{rc::Rc, time::SystemTime};

pub fn parse(module_id: ModuleID, module_src: Rc<String>) -> Result<AST, ParseError> {
    let bgl = Slice::new(module_src.clone());

    let start = SystemTime::now();
    let res = module(bgl.clone());

    if DEBUG_MODE {
        println!(
            "* Parsing  {:?} took {}ms",
            module_id,
            start.elapsed().unwrap().as_millis()
        );
    }

    match res {
        Ok((remaining_i, module)) => {
            if remaining_i.len() > 0 {
                Err(ParseError {
                    module_id,
                    src: remaining_i,
                    message: "Failed to parse entire input".to_owned(),
                })
            } else {
                Ok(module)
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

macro_rules! parse_level {
    ($level:expr, $this_level:expr, $i:expr, $a:expr) => {
        $this_level += 1;
        if $level <= $this_level {
            let res = $a($i.clone());

            if res.is_ok() {
                return res;
            }
        }
    };
}

fn module(i: Slice) -> ParseResult {
    map(
        complete(terminated(
            separated_list0(whitespace, declaration),
            whitespace,
        )),
        |mut declarations| {
            let src = covering(&declarations).unwrap_or(i.clone().slice_range(0, Some(0)));

            let this = ASTDetails::Module {
                declarations: declarations.clone(),
            }
            .with_slice(src);

            for declaration in declarations.iter_mut() {
                declaration.set_parent(&this);
            }

            this
        },
    )(i.clone())
}

// --- Declaration

fn declaration(i: Slice) -> ParseResult {
    preceded(
        whitespace,
        alt((
            import_all_declaration,
            import_declaration,
            type_declaration,
            func_declaration,
            proc_declaration,
            value_declaration,
            test_expr_declaration,
            test_block_declaration,
            // test_type_declaration,
        )),
    )(i)
}

fn import_all_declaration(i: Slice) -> ParseResult {
    context(
        "import-all declaration",
        map(
            tuple((
                tag("import"),
                preceded(whitespace, exact_string_literal),
                preceded(whitespace, tag("as")),
                preceded(whitespace, plain_identifier),
            )),
            |(start, mut path, _, mut name)| {
                let this = ASTDetails::ImportAllDeclaration {
                    path: path.clone(),
                    name: name.clone(),
                }
                .with_slice(start.spanning(name.slice()));

                path.set_parent(&this);
                name.set_parent(&this);

                this
            },
        ),
    )(i)
}

fn import_declaration(i: Slice) -> ParseResult {
    context(
        "import declaration",
        map(
            tuple((
                tag("from"),
                preceded(whitespace, exact_string_literal),
                preceded(whitespace, tag("import")),
                preceded(whitespace, tag("{")),
                separated_list0(preceded(whitespace, tag(",")), import_item),
                preceded(whitespace, tag("}")),
            )),
            |(start, mut path, _, _, mut imports, end)| {
                let this = ASTDetails::ImportDeclaration {
                    path: path.clone(),
                    imports: imports.clone(),
                }
                .with_slice(start.spanning(&end));

                path.set_parent(&this);
                for import in imports.iter_mut() {
                    import.set_parent(&this);
                }

                this
            },
        ),
    )(i)
}

fn import_item(i: Slice) -> ParseResult {
    map(
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
        |(mut name, mut alias)| {
            let this = ASTDetails::ImportItem {
                name: name.clone(),
                alias: alias.clone(),
            }
            .with_slice(
                name.slice().clone().spanning(
                    alias
                        .as_ref()
                        .map(|alias| alias.slice())
                        .unwrap_or(name.slice()),
                ),
            );

            name.set_parent(&this);
            if let Some(alias) = &mut alias {
                alias.set_parent(&this);
            }

            this
        },
    )(i)
}

fn type_declaration(i: Slice) -> ParseResult {
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
            |(export, keyword, mut name, _, mut declared_type)| {
                let this = ASTDetails::TypeDeclaration {
                    name: name.clone(),
                    declared_type: declared_type.clone(),
                    exported: export.is_some(),
                }
                .with_slice(export.unwrap_or(keyword).spanning(&declared_type.slice()));

                name.set_parent(&this);
                declared_type.set_parent(&this);

                this
            },
        ),
    )(i)
}

fn func_declaration(i: Slice) -> ParseResult {
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
            |(export, pure, asyn, keyword, mut name, mut args, mut returns, _, mut body)| {
                let src = export
                    .as_ref()
                    .unwrap_or(pure.as_ref().unwrap_or(asyn.as_ref().unwrap_or(&keyword)))
                    .clone()
                    .spanning(body.slice());

                let mut type_annotation = ASTDetails::FuncType {
                    args: args.clone(),
                    args_spread: None, // TODO
                    is_pure: pure.is_some(),
                    returns: returns.clone(),
                }
                .with_slice(args.slice().clone().spanning(body.slice()));

                args.set_parent(&type_annotation);
                if let Some(returns) = &mut returns {
                    returns.set_parent(&type_annotation);
                }

                let mut func = ASTDetails::Func {
                    type_annotation: type_annotation.clone(),
                    is_async: asyn.is_some(),
                    is_pure: pure.is_some(),
                    body,
                }
                .with_slice(src.clone());

                type_annotation.set_parent(&func);

                let this = ASTDetails::FuncDeclaration {
                    name: name.clone(),
                    func: func.clone(),
                    exported: export.is_some(),
                    platforms: PlatformSet::all(), // TODO
                    decorators: vec![],            // TODO
                }
                .with_slice(src);

                name.set_parent(&this);
                func.set_parent(&this);

                this
            },
        ),
    )(i)
}

fn args(i: Slice) -> ParseResult {
    alt((
        map(plain_identifier, |name| {
            ASTDetails::ArgsSeries(vec![ASTDetails::Arg {
                name: name.clone(),
                type_annotation: None,
                optional: false,
            }
            .with_slice(name.slice().clone())])
            .with_slice(name.slice().clone())
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
            |(start, args, end)| {
                ASTDetails::ArgsSeries(
                    args.into_iter()
                        .map(|(name, type_annotation)| {
                            let src = name.slice().clone().spanning(
                                &type_annotation
                                    .as_ref()
                                    .map(|t| t.slice())
                                    .unwrap_or(name.slice()),
                            );

                            ASTDetails::Arg {
                                name,
                                type_annotation,
                                optional: false, // TODO
                            }
                            .with_slice(src)
                        })
                        .collect(),
                )
                .with_slice(start.spanning(&end))
            },
        ),
    ))(i)
}

fn proc_declaration(i: Slice) -> ParseResult {
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
                preceded(whitespace, block),
            )),
            |(export, pure, asyn, keyword, name, args, body)| {
                let exported = export.is_some();
                let is_async = asyn.is_some();
                let is_pure = pure.is_some();
                let src = export
                    .unwrap_or(pure.unwrap_or(asyn.unwrap_or(keyword)))
                    .spanning(body.slice());
                let args_start = args.slice();

                ASTDetails::ProcDeclaration {
                    name,
                    proc: ASTDetails::Proc {
                        type_annotation: ASTDetails::ProcType {
                            args: args.clone(),
                            args_spread: None, // TODO
                            is_pure,
                            is_async,
                            throws: None, // TODO
                        }
                        .with_slice(args_start.clone().spanning(body.slice())),

                        is_async,
                        is_pure,
                        body,
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

fn block(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("{"),
            many0(preceded(whitespace, statement)),
            preceded(whitespace, tag("}")),
        )),
        |(open, statements, close)| ASTDetails::Block(statements).with_slice(open.spanning(&close)),
    )(i)
}

fn value_declaration(i: Slice) -> ParseResult {
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
            |(export, keyword, mut name, mut type_annotation, _, mut value)| {
                let exported = export.is_some();
                let is_const = keyword.as_str() == "const";
                let src = export.unwrap_or(keyword).spanning(value.slice());

                let this = ASTDetails::ValueDeclaration {
                    name: name.clone(),
                    type_annotation: type_annotation.clone(),
                    value: value.clone(),
                    is_const,
                    exported,
                    platforms: PlatformSet::all(), // TODO
                }
                .with_slice(src);

                name.set_parent(&this);
                if let Some(type_annotation) = &mut type_annotation {
                    type_annotation.set_parent(&this);
                }
                value.set_parent(&this);

                this
            },
        ),
    )(i)
}

fn test_expr_declaration(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("test"),
            preceded(whitespace, tag("expr")),
            preceded(whitespace, exact_string_literal),
            preceded(whitespace, tag("=>")),
            preceded(whitespace, expression(0)),
        )),
        |(start, _, name, _, expr)| {
            let src = start.spanning(expr.slice());

            ASTDetails::TestExprDeclaration { name, expr }.with_slice(src)
        },
    )(i)
}

fn test_block_declaration(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("test"),
            preceded(whitespace, tag("block")),
            preceded(whitespace, exact_string_literal),
            preceded(whitespace, block),
        )),
        |(start, _, name, block)| {
            ASTDetails::TestBlockDeclaration {
                name: name.clone(),
                block: block.clone(),
            }
            .with_slice(start.spanning(block.slice()))
        },
    )(i)
}

fn test_type_declaration(i: Slice) -> ParseResult {
    todo!()
}

fn type_annotation(i: Slice) -> ParseResult {
    preceded(tag(":"), preceded(whitespace, type_expression(0)))(i)
}

// --- TypeExpression ---

fn type_expression(l: usize) -> impl Fn(Slice) -> ParseResult {
    move |i: Slice| -> ParseResult {
        let mut tl = 0;

        parse_level!(
            l,
            tl,
            i,
            map(
                tuple((tag("typeof"), preceded(whitespace, expression(0)))),
                |(keyword, expr)| {
                    let src = keyword.clone().spanning(expr.slice());

                    ASTDetails::TypeofType(expr).with_slice(src)
                }
            )
        );

        parse_level!(
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
                    let src = keyword.clone().spanning(inner.slice());

                    match keyword.as_str() {
                        "keyof" => ASTDetails::KeyofType(inner),
                        "valueof" => ASTDetails::ValueofType(inner),
                        "elementof" => ASTDetails::ElementofType(inner),
                        "readonly" => ASTDetails::ReadonlyType(inner),
                        _ => unreachable!(),
                    }
                    .with_slice(src)
                }
            )
        );

        // generic type

        parse_level!(
            l,
            tl,
            i,
            map(
                separated_list2(
                    preceded(whitespace, tag("|")),
                    preceded(whitespace, type_expression(tl + 1))
                ),
                |members| {
                    let src = covering(&members).unwrap();

                    ASTDetails::UnionType(members).with_slice(src)
                }
            )
        );

        parse_level!(
            l,
            tl,
            i,
            map(
                tuple((type_expression(tl + 1), tag("?"))),
                |(inner, end)| {
                    let src = inner.slice().clone().spanning(&end);

                    ASTDetails::MaybeType(inner).with_slice(src)
                }
            )
        );

        // boundGenericType

        parse_level!(
            l,
            tl,
            i,
            map(
                tuple((type_expression(tl + 1), tag("[]"))),
                |(element, end)| {
                    let src = element.slice().clone().spanning(&end);

                    ASTDetails::ArrayType(element).with_slice(src)
                }
            )
        );

        parse_level!(
            l,
            tl,
            i,
            alt((
                func_type,
                proc_type,
                record_type,
                object_or_interface_type,
                tuple_type,
                map(
                    tuple((
                        tag("("),
                        preceded(whitespace, type_expression(tl + 1)),
                        preceded(whitespace, tag(")"))
                    )),
                    |(open, inner, close)| {
                        ASTDetails::ParenthesizedType(inner).with_slice(open.spanning(&close))
                    }
                ),
                map(
                    tuple((tag("\'"), string_contents, tag("\'"))),
                    |(open_quote, value, close_quote)| {
                        ASTDetails::StringLiteralType(value)
                            .with_slice(open_quote.spanning(&close_quote))
                    },
                ),
                map(tuple((opt(tag("-")), numeric)), |(neg, int)| {
                    let src = neg.unwrap_or_else(|| int.clone()).spanning(&int);
                    ASTDetails::NumberLiteralType(src.clone()).with_slice(src)
                }),
                map(alt((tag("true"), tag("false"))), |s: Slice| {
                    ASTDetails::BooleanLiteralType(s.as_str() == "true").with_slice(s)
                }),
                map(tag("string"), |s: Slice| {
                    ASTDetails::StringType.with_slice(s)
                }),
                map(tag("number"), |s: Slice| {
                    ASTDetails::NumberType.with_slice(s)
                }),
                map(tag("boolean"), |s: Slice| {
                    ASTDetails::BooleanType.with_slice(s)
                }),
                map(tag("unknown"), |s: Slice| ASTDetails::UnknownType
                    .with_slice(s)),
                map(tag("nil"), |s: Slice| ASTDetails::NilType.with_slice(s)),
            ))
        );

        parse_level!(
            l,
            tl,
            i,
            map(plain_identifier, |name| {
                let src = name.slice().clone();

                ASTDetails::NamedType(name).with_slice(src)
            })
        );

        Err(nom::Err::Error(RawParseError {
            src: i,
            details: RawParseErrorDetails::Kind(ErrorKind::Fail),
        }))
    }
}

#[memoize]
fn func_type(i: Slice) -> ParseResult {
    map(
        tuple((
            args,
            preceded(whitespace, tag("=>")),
            preceded(whitespace, type_expression(0)),
        )),
        |(args, _, returns)| {
            // TODO: func type with 0 arguments will have weird src
            let src = args.slice().clone().spanning(returns.slice());

            ASTDetails::FuncType {
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
fn proc_type(i: Slice) -> ParseResult {
    map(
        tuple((
            args,
            preceded(whitespace, tag("(")),
            preceded(whitespace, tag(")")),
            preceded(whitespace, tag("{")),
            preceded(whitespace, tag("}")),
        )),
        |(args, _, _, _, close)| {
            // TODO: proc type with 0 arguments will have weird src
            let src = args.slice().clone().spanning(&close);

            ASTDetails::ProcType {
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
fn record_type(i: Slice) -> ParseResult {
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
            ASTDetails::RecordType {
                key_type,
                value_type,
            }
            .with_slice(open.spanning(&close))
        },
    )(i)
}

#[memoize]
fn object_or_interface_type(i: Slice) -> ParseResult {
    map(
        tuple((
            opt(tag("interface")),
            preceded(whitespace, tag("{")),
            separated_list0(
                preceded(whitespace, tag(",")),
                preceded(whitespace, key_value_type),
            ),
            preceded(whitespace, tag("}")),
        )),
        |(interface, open, entries, close)| {
            if let Some(interface) = interface {
                ASTDetails::InterfaceType(entries).with_slice(interface.spanning(&close))
            } else {
                ASTDetails::ObjectType(entries).with_slice(open.spanning(&close))
            }
        },
    )(i)
}

fn key_value_type(i: Slice) -> ParseResult {
    map(
        separated_pair(
            preceded(whitespace, plain_identifier),
            cut(preceded(whitespace, char(':'))),
            preceded(whitespace, type_expression(0)),
        ),
        |(key, value)| {
            ASTDetails::KeyValueType {
                key: key.clone(),
                value: value.clone(),
            }
            .with_slice(key.slice().clone().spanning(value.slice()))
        },
    )(i)
}

fn tuple_type(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("["),
            separated_list0(
                preceded(whitespace, tag(",")),
                preceded(whitespace, type_expression(0)),
            ),
            preceded(whitespace, tag("]")),
        )),
        |(open, members, close)| ASTDetails::TupleType(members).with_slice(open.spanning(&close)),
    )(i)
}

// --- Statement ---

fn statement(i: Slice) -> ParseResult {
    alt((
        // map(declaration_statement, |x| x.map(ASTDetails::from)),
        if_else_statement,
        for_loop,
        while_loop,
        // assignment,
        // try_catch,
        throw_statement,
        autorun,
        // invocation_statement,
    ))(i)
}

fn declaration_statement(i: Slice) -> ParseResult {
    todo!()
}

fn if_else_statement(i: Slice) -> ParseResult {
    map(
        tuple((
            separated_list1(preceded(whitespace, tag("else")), if_else_statement_case),
            opt(map(
                preceded(
                    whitespace,
                    tuple((tag("else"), preceded(whitespace, block))),
                ),
                |(_, default_case)| default_case,
            )),
        )),
        |(mut cases, mut default_case)| {
            let src = covering(&cases).unwrap();

            let this = ASTDetails::IfElseStatement {
                cases: cases.clone(),
                default_case: default_case.clone(),
            }
            .with_slice(src);

            for case in cases.iter_mut() {
                case.set_parent(&this);
            }
            if let Some(default_case) = &mut default_case {
                default_case.set_parent(&this);
            }

            this
        },
    )(i)
}

fn if_else_statement_case(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("if"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, block),
        )),
        |(start, condition, outcome)| {
            let src = start.spanning(outcome.slice());

            ASTDetails::IfElseExpressionCase { condition, outcome }.with_slice(src)
        },
    )(i)
}

fn for_loop(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("for"),
            preceded(whitespace, plain_identifier),
            preceded(whitespace, tag("in")),
            preceded(whitespace, expression(0)),
            preceded(whitespace, block),
        )),
        |(start, mut item_identifier, _, mut iterator, mut body)| {
            let this = ASTDetails::ForLoop {
                item_identifier: item_identifier.clone(),
                iterator: iterator.clone(),
                body: body.clone(),
            }
            .with_slice(start.spanning(body.slice()));

            item_identifier.set_parent(&this);
            iterator.set_parent(&this);
            body.set_parent(&this);

            this
        },
    )(i)
}

fn while_loop(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("while"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, block),
        )),
        |(start, mut condition, mut body)| {
            let this = ASTDetails::WhileLoop {
                condition: condition.clone(),
                body: body.clone(),
            }
            .with_slice(start.spanning(body.slice()));

            condition.set_parent(&this);
            body.set_parent(&this);

            this
        },
    )(i)
}

fn assignment(i: Slice) -> ParseResult {
    todo!()
}

fn try_catch(i: Slice) -> ParseResult {
    todo!()
}

fn throw_statement(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("throw"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag(";")),
        )),
        |(start, mut error_expression, end)| {
            let this = ASTDetails::ThrowStatement {
                error_expression: error_expression.clone(),
            }
            .with_slice(start.spanning(&end));

            error_expression.set_parent(&this);

            this
        },
    )(i)
}

#[memoize]
fn autorun(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("autorun"),
            preceded(whitespace, block),
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
        |(start, mut effect_block, mut until, end)| {
            let this = ASTDetails::Autorun {
                effect_block: effect_block.clone(),
                until: until.clone(),
            }
            .with_slice(start.spanning(&end));

            effect_block.set_parent(&this);
            if let Some(until) = &mut until {
                until.set_parent(&this);
            }

            this
        },
    )(i)
}

fn invocation_statement(i: Slice) -> ParseResult {
    todo!()
}

// --- Expression ---

fn expression(l: usize) -> impl Fn(Slice) -> ParseResult {
    move |i: Slice| -> ParseResult {
        let mut tl = 0;

        // parse_level!(level, this_level, i, ); // debug, javascriptEscape, elementTag
        parse_level!(
            l,
            tl,
            i,
            alt((
                func,
                // map(proc, |x| x.map(ASTDetails::from)),
            ))
        );
        parse_level!(l, tl, i, binary_operator_1(tl, "??"));
        parse_level!(l, tl, i, binary_operator_1(tl, "||"));
        parse_level!(l, tl, i, binary_operator_1(tl, "&&"));
        parse_level!(l, tl, i, binary_operator_2(tl, "==", "!="));
        parse_level!(l, tl, i, binary_operator_4(tl, "<=", ">=", "<", ">"));
        parse_level!(l, tl, i, binary_operator_2(tl, "+", "-"));
        parse_level!(l, tl, i, binary_operator_2(tl, "*", "/"));
        parse_level!(l, tl, i, alt((as_cast(tl), instance_of(tl))));
        parse_level!(l, tl, i, negation_operation(tl));

        // indexer

        // parse_level!(l, tl, i, error_expression);

        // invocationAccessorChain

        parse_level!(l, tl, i, range_expression);

        parse_level!(l, tl, i, parenthesis(tl));

        parse_level!(
            l,
            tl,
            i,
            alt((
                if_else_expression,
                switch_expression,
                // inline_const_group,
                object_literal,
                array_literal,
                exact_string_literal,
                number_literal,
                boolean_literal,
                nil_literal,
            ))
        );
        parse_level!(
            l,
            tl,
            i,
            alt((
                local_identifier,
                // localIdentifier, regExp
            ))
        );

        Err(nom::Err::Error(RawParseError {
            src: i,
            details: RawParseErrorDetails::Kind(ErrorKind::Fail),
        }))
    }
}

macro_rules! parse_binary_operation {
    ($level:expr, $parser:expr) => {
        move |i: Slice| -> ParseResult {
            map(
                tuple((
                    expression($level + 1),
                    preceded(whitespace, $parser),
                    preceded(whitespace, expression($level + 1)),
                )),
                |(mut left, op, mut right)| {
                    let src = left.slice().clone().spanning(right.slice());

                    let mut op = ASTDetails::BinaryOperator(
                        BinaryOperatorOp::from_str(op.as_str()).unwrap(),
                    )
                    .with_slice(op);

                    let this = ASTDetails::BinaryOperation {
                        left: left.clone(),
                        op: op.clone(),
                        right: right.clone(),
                    }
                    .with_slice(src);

                    left.set_parent(&this);
                    op.set_parent(&this);
                    right.set_parent(&this);

                    this
                },
            )(i.clone())
        }
    };
}

fn binary_operator_1(level: usize, a: &'static str) -> impl Fn(Slice) -> ParseResult {
    parse_binary_operation!(level, tag(a))
}

fn binary_operator_2(
    level: usize,
    a: &'static str,
    b: &'static str,
) -> impl Fn(Slice) -> ParseResult {
    parse_binary_operation!(level, alt((tag(a), tag(b))))
}

fn binary_operator_4(
    level: usize,
    a: &'static str,
    b: &'static str,
    c: &'static str,
    d: &'static str,
) -> impl Fn(Slice) -> ParseResult {
    parse_binary_operation!(level, alt((tag(a), tag(b), tag(c), tag(d))))
}

fn regular_expression(i: Slice) -> ParseResult {
    todo!()
}

fn error_expression(i: Slice) -> ParseResult {
    todo!()
}

fn instance_of(level: usize) -> impl Fn(Slice) -> ParseResult {
    move |i: Slice| -> ParseResult {
        map(
            tuple((
                expression(level + 1),
                preceded(whitespace, tag("instanceof")),
                type_expression(0),
            )),
            |(mut inner, _, mut possible_type)| {
                let src = inner.slice().clone().spanning(possible_type.slice());

                let this = ASTDetails::InstanceOf {
                    inner: inner.clone(),
                    possible_type: possible_type.clone(),
                }
                .with_slice(src);

                inner.set_parent(&this);
                possible_type.set_parent(&this);

                this
            },
        )(i)
    }
}

fn as_cast(level: usize) -> impl Fn(Slice) -> ParseResult {
    move |i: Slice| -> ParseResult {
        map(
            tuple((
                expression(level + 1),
                preceded(whitespace, tag("as")),
                type_expression(0),
            )),
            |(mut inner, _, mut as_type)| {
                let src = inner.slice().clone().spanning(as_type.slice());

                let this = ASTDetails::AsCast {
                    inner: inner.clone(),
                    as_type: as_type.clone(),
                }
                .with_slice(src);

                inner.set_parent(&this);
                as_type.set_parent(&this);

                this
            },
        )(i)
    }
}

fn element_tag(i: Slice) -> ParseResult {
    todo!()
}

#[memoize]
fn if_else_expression(i: Slice) -> ParseResult {
    map(
        tuple((
            separated_list1(
                preceded(whitespace, tag("else")),
                preceded(whitespace, if_else_expression_case),
            ),
            opt(map(
                preceded(
                    whitespace,
                    tuple((
                        tag("else"),
                        preceded(whitespace, tag("{")),
                        preceded(whitespace, expression(0)),
                        preceded(whitespace, tag("}")),
                    )),
                ),
                |(_, _, outcome, _)| outcome,
            )),
        )),
        |(mut cases, mut default_case)| {
            let src = covering(&cases).unwrap();

            let this = ASTDetails::IfElseExpression {
                cases: cases.clone(),
                default_case: default_case.clone(),
            }
            .with_slice(src);

            for case in cases.iter_mut() {
                case.set_parent(&this);
            }
            if let Some(default_case) = &mut default_case {
                default_case.set_parent(&this);
            }

            this
        },
    )(i)
}

fn if_else_expression_case(i: Slice) -> ParseResult {
    map(
        tuple((
            tag("if"),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("{")),
            preceded(whitespace, expression(0)),
            preceded(whitespace, tag("}")),
        )),
        |(start, condition, _, outcome, end)| {
            ASTDetails::IfElseExpressionCase { condition, outcome }.with_slice(start.spanning(&end))
        },
    )(i)
}

#[memoize]
fn switch_expression(i: Slice) -> ParseResult {
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
                            |(keyword, mut type_filter, _, mut outcome)| {
                                ASTDetails::SwitchExpressionCase {
                                    type_filter: type_filter.clone(),
                                    outcome: outcome.clone(),
                                }
                                .with_slice(keyword.spanning(outcome.slice()))
                            },
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
        |(start, mut value, _, mut cases, mut default_case, end)| {
            let this = ASTDetails::SwitchExpression {
                value: value.clone(),
                cases: cases.clone(),
                default_case: default_case.clone(),
            }
            .with_slice(start.spanning(&end));

            value.set_parent(&this);
            for case in cases.iter_mut() {
                case.set_parent(&this);
            }
            if let Some(default_case) = &mut default_case {
                default_case.set_parent(&this);
            }

            this
        },
    )(i)
}

fn range_expression(i: Slice) -> ParseResult {
    map(
        tuple((number_literal, tag(".."), number_literal)),
        |(mut start, _, mut end)| {
            let src = start.slice().clone().spanning(end.slice());

            let this = ASTDetails::RangeExpression {
                start: start.clone(),
                end: end.clone(),
            }
            .with_slice(src);

            start.set_parent(&this);
            end.set_parent(&this);

            this
        },
    )(i)
}

fn javascript_escape_expression(i: Slice) -> ParseResult {
    todo!()
}

fn proc(i: Slice) -> ParseResult {
    todo!()
}

#[memoize]
fn func(i: Slice) -> ParseResult {
    map(
        tuple((
            opt(tag("pure")),
            opt(preceded(whitespace, tag("async"))),
            preceded(whitespace, args),
            preceded(whitespace, opt(type_annotation)),
            preceded(whitespace, tag("=>")),
            preceded(whitespace, expression(0)),
        )),
        |(pure, asyn, args, returns, _, mut body)| {
            let is_async = asyn.is_some();
            let is_pure = pure.is_some();
            let src = pure
                .unwrap_or(asyn.unwrap_or_else(|| args.slice().clone()))
                .spanning(body.slice());

            let mut type_annotation = ASTDetails::FuncType {
                args,
                args_spread: None, // TODO
                is_pure,
                returns,
            }
            .with_slice(src.clone());

            let this = ASTDetails::Func {
                type_annotation: type_annotation.clone(),
                is_async,
                is_pure,
                body: body.clone(),
            }
            .with_slice(src.clone());

            type_annotation.set_parent(&this);
            body.set_parent(&this);

            this
        },
    )(i)
}

fn inline_const_group(i: Slice) -> ParseResult {
    todo!()
}

fn negation_operation(level: usize) -> impl Fn(Slice) -> ParseResult {
    move |i: Slice| -> ParseResult {
        map(
            tuple((tag("!"), preceded(whitespace, expression(level + 1)))),
            |(start, mut expr)| {
                let src = start.spanning(expr.slice());
                let this = ASTDetails::NegationOperation(expr.clone()).with_slice(src);

                expr.set_parent(&this);

                this
            },
        )(i)
    }
}

fn parenthesis(level: usize) -> impl Fn(Slice) -> ParseResult {
    move |i: Slice| -> ParseResult {
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
                |(open_paren, (mut inner, close_paren))| {
                    let this = ASTDetails::Parenthesis(inner.clone())
                        .with_slice(open_paren.spanning(&close_paren));

                    inner.set_parent(&this);

                    this
                },
            ),
        )(i)
    }
}

#[memoize]
fn object_literal(i: Slice) -> ParseResult {
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
            |(open_bracket, (mut entries, close_bracket))| {
                let this = ASTDetails::ObjectLiteral(entries.clone())
                    .with_slice(open_bracket.spanning(&close_bracket));

                for entry in entries.iter_mut() {
                    entry.set_parent(&this);
                }

                this
            },
        ),
    )(i)
}

fn key_value_expression(i: Slice) -> ParseResult {
    map(
        separated_pair(
            preceded(whitespace, plain_identifier),
            cut(preceded(whitespace, char(':'))),
            preceded(whitespace, expression(0)),
        ),
        |(mut key, mut value)| {
            ASTDetails::KeyValue {
                key: key.clone(),
                value: value.clone(),
            }
            .with_slice(key.slice().clone().spanning(value.slice()))
        },
    )(i)
}

#[memoize]
fn array_literal(i: Slice) -> ParseResult {
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
            |(open_bracket, (mut entries, close_bracket))| {
                let this = ASTDetails::ArrayLiteral(entries.clone())
                    .with_slice(open_bracket.spanning(&close_bracket));

                for entry in entries.iter_mut() {
                    entry.set_parent(&this);
                }

                this
            },
        ),
    )(i)
}

#[memoize]
fn string_literal(i: Slice) -> ParseResult {
    context(
        "string",
        map(
            tuple((
                tag("\'"),
                cut(tuple((many0(string_literal_segment), tag("\'")))),
            )),
            |(open_quote, (mut segments, close_quote))| {
                let this = ASTDetails::StringLiteral {
                    tag: None, // TODO
                    segments: segments.clone(),
                }
                .with_slice(open_quote.spanning(&close_quote));

                for segment in segments.iter_mut() {
                    segment.set_parent(&this);
                }

                this
            },
        ),
    )(i)
}

#[memoize]
fn exact_string_literal(i: Slice) -> ParseResult {
    context(
        "string",
        map(
            tuple((tag("\'"), string_contents, tag("\'"))),
            |(open_quote, value, close_quote)| {
                ASTDetails::ExactStringLiteral {
                    tag: None, // TODO
                    value,
                }
                .with_slice(open_quote.spanning(&close_quote))
            },
        ),
    )(i)
}

fn string_literal_segment(i: Slice) -> ParseResult {
    alt((
        map(
            tuple((
                tag("${"),
                preceded(whitespace, expression(0)),
                preceded(whitespace, tag("}")),
            )),
            |(_, expr, _)| expr,
        ),
        map(string_contents, |s| {
            ASTDetails::StringLiteralRawSegment(s.clone()).with_slice(s)
        }),
    ))(i)
}

fn number_literal(i: Slice) -> ParseResult {
    map(
        tuple((opt(tag("-")), numeric, opt(tuple((tag("."), cut(numeric)))))),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int.clone());
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            ASTDetails::NumberLiteral(full.clone()).with_slice(full)
        },
    )(i)
}

fn boolean_literal(input: Slice) -> ParseResult {
    let parse_true = map(tag("true"), |src: Slice| {
        ASTDetails::BooleanLiteral(true).with_slice(src)
    });

    let parse_false = map(tag("false"), |src: Slice| {
        ASTDetails::BooleanLiteral(false).with_slice(src)
    });

    alt((parse_true, parse_false))(input)
}

fn nil_literal(input: Slice) -> ParseResult {
    map(tag("nil"), |src: Slice| {
        ASTDetails::NilLiteral.with_slice(src)
    })(input)
}

fn local_identifier(i: Slice) -> ParseResult {
    context(
        "identifier",
        map(identifier_like, |name| {
            ASTDetails::LocalIdentifier(name.clone()).with_slice(name)
        }),
    )(i)
}

fn plain_identifier(i: Slice) -> ParseResult {
    context(
        "identifier",
        map(identifier_like, |name| {
            ASTDetails::PlainIdentifier(name.clone()).with_slice(name)
        }),
    )(i)
}

// --- Util parsers ---

fn string_contents(i: Slice) -> IResult<Slice, Slice, RawParseError> {
    escaped(take_while(|ch: char| ch != '\''), '\\', one_of("$\'n\\"))(i)
}

fn identifier_like(i: Slice) -> IResult<Slice, Slice, RawParseError> {
    take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$')(i)
}

fn numeric(i: Slice) -> IResult<Slice, Slice, RawParseError> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace(i: Slice) -> IResult<Slice, Slice, RawParseError> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i)
}

fn separated_list2<O2, F, G>(
    sep: G,
    f: F,
) -> impl FnMut(Slice) -> IResult<Slice, Vec<AST>, RawParseError>
where
    F: Parser<Slice, AST, RawParseError>,
    G: Parser<Slice, O2, RawParseError>,
{
    let mut parser = separated_list1(sep, f);

    move |i: Slice| -> IResult<Slice, Vec<AST>, RawParseError> {
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

type ParseResult = IResult<Slice, AST, RawParseError>;

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
