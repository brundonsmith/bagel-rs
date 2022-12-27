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
    combinator::{complete, map, opt},
    error::ErrorKind,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};
use std::str::FromStr;
use std::{rc::Rc, time::SystemTime};

macro_rules! seq {
    ($( $s:expr ),* $(,)?) => {
        tuple(( $(preceded(whitespace, $s)),* ))
    };
}

macro_rules! make_node {
    ($kind:ident, $src:expr, $( $prop:ident ),* $(,)?) => {
        {
            let this = ASTDetails::$kind {
                $($prop: $prop.clone(),)*
            }
            .with_slice($src);

            $($prop.set_parent(&this);)*

            this
        }
    };
}

macro_rules! make_node_typed {
    ($kind:ident, $src:expr, $( $prop:ident ),* $(,)?) => {
        {
            let this = $kind {
                $($prop: $prop.clone(),)*
            }
            .with_slice($src);

            $($prop.set_parent(&this);)*

            this
        }
    };
}

macro_rules! make_node_tuple {
    ($kind:ident, $src:expr, $( $prop:ident ),* $(,)?) => {
        {
            let this = ASTDetails::$kind(
                $($prop.clone()),*
            )
            .with_slice($src);

            $($prop.set_parent(&this);)*

            this
        }
    };
}

macro_rules! make_node_tuple_typed {
    ($kind:ident, $src:expr, $( $prop:ident ),* $(,)?) => {
        {
            let this = $kind(
                $($prop.clone()),*
            )
            .with_slice($src);

            $($prop.set_parent(&this);)*

            this
        }
    };
}

pub fn parse(module_id: ModuleID, module_src: Rc<String>) -> Result<AST<Module>, ParseError> {
    let bgl = Slice::new(module_src.clone());

    let start = SystemTime::now();
    let res = module(bgl.clone());

    if DEBUG_MODE {
        println!(
            "* Parsing  {} took {}ms",
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
            let res = map($a, AST::upcast)($i.clone());

            if res.is_ok() {
                return res;
            }
        }
    };
}

fn module(i: Slice) -> ParseResult<AST<Module>> {
    map(
        complete(terminated(many0(w(declaration)), whitespace)),
        |mut declarations| {
            let src = covering(&declarations).unwrap_or(i.clone().slice_range(0, Some(0)));

            let this = Module {
                declarations: declarations.clone(),
            }
            .with_slice(src);

            declarations.set_parent(&this);

            this
        },
    )(i.clone())
}

// --- Declaration

#[memoize]
fn declaration(i: Slice) -> ParseResult<ASTAny> {
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
    ))(i)
}

#[memoize]
fn import_all_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("import"),
            exact_string_literal,
            tag("as"),
            plain_identifier,
        ),
        |(start, mut path, _, mut name)| {
            make_node!(
                ImportAllDeclaration,
                start.spanning(name.slice()),
                path,
                name
            )
        },
    )(i)
}

#[memoize]
fn import_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("from"),
            exact_string_literal,
            tag("import"),
            tag("{"),
            separated_list0(w(tag(",")), import_item),
            tag("}"),
        ),
        |(start, mut path, _, _, mut imports, end)| {
            make_node!(ImportDeclaration, start.spanning(&end), path, imports)
        },
    )(i)
}

#[memoize]
fn import_item(i: Slice) -> ParseResult<AST<ImportItem>> {
    map(
        pair(
            plain_identifier,
            opt(map(seq!(tag("as"), plain_identifier), |(_, alias)| alias)),
        ),
        |(mut name, mut alias)| {
            make_node_typed!(
                ImportItem,
                name.slice().clone().spanning(
                    alias
                        .as_ref()
                        .map(|alias| alias.slice())
                        .unwrap_or(name.slice()),
                ),
                name,
                alias
            )
        },
    )(i)
}

#[memoize]
fn type_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            opt(tag("export")),
            tag("type"),
            plain_identifier,
            tag("="),
            type_expression(0),
        ),
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
    )(i)
}

#[memoize]
fn func_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            opt(tag("export")),
            opt(tag("pure")),
            opt(tag("async")),
            tag("func"),
            plain_identifier,
            alt((args_parenthesized, arg_singleton)),
            opt(type_annotation),
            tag("=>"),
            expression(0),
        ),
        |(export, pure, asyn, keyword, mut name, mut args, mut returns, arrow, mut body)| {
            let src = export
                .as_ref()
                .unwrap_or(pure.as_ref().unwrap_or(asyn.as_ref().unwrap_or(&keyword)))
                .clone()
                .spanning(body.slice());

            let mut type_annotation = FuncType {
                args: args.clone(),
                args_spread: None, // TODO
                is_pure: pure.is_some(),
                is_async: asyn.is_some(),
                returns: returns.clone(),
            }
            .with_slice(
                args.get(0)
                    .map(|arg| arg.downcast().name.slice().clone())
                    .unwrap_or(arrow)
                    .spanning(body.slice()),
            );

            args.set_parent(&type_annotation);
            returns.set_parent(&type_annotation);

            let mut func = Func {
                type_annotation: type_annotation.clone(),
                is_async: asyn.is_some(),
                is_pure: pure.is_some(),
                body: body.clone(),
            }
            .with_slice(src.clone());

            type_annotation.set_parent(&func);
            body.set_parent(&func);

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
    )(i)
}

#[memoize]
fn arg_singleton(i: Slice) -> ParseResult<Vec<AST<Arg>>> {
    map(plain_identifier, |name| {
        vec![Arg {
            name: name.clone(),
            type_annotation: None,
            optional: false,
        }
        .with_slice(name.slice().clone())]
    })(i)
}

#[memoize]
fn args_parenthesized(i: Slice) -> ParseResult<Vec<AST<Arg>>> {
    map(
        seq!(
            tag("("),
            preceded(
                whitespace,
                separated_list0(w(tag(",")), w(seq!(plain_identifier, opt(type_annotation)))),
            ),
            tag(")"),
        ),
        |(start, args, end)| {
            args.into_iter()
                .map(|(mut name, mut type_annotation)| {
                    let this = Arg {
                        name: name.clone(),
                        type_annotation: type_annotation.clone(),
                        optional: false, // TODO
                    }
                    .with_slice(
                        name.slice().clone().spanning(
                            type_annotation
                                .as_ref()
                                .map(|a| a.slice())
                                .unwrap_or(name.slice()),
                        ),
                    );

                    name.set_parent(&this);
                    type_annotation.set_parent(&this);

                    this
                })
                .collect()
        },
    )(i)
}

#[memoize]
fn proc_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            opt(tag("export")),
            opt(tag("pure")),
            opt(tag("async")),
            tag("proc"),
            plain_identifier,
            alt((args_parenthesized, arg_singleton)),
            block,
        ),
        |(export, pure, asyn, keyword, name, args, body)| {
            let exported = export.is_some();
            let is_async = asyn.is_some();
            let is_pure = pure.is_some();
            let src = export
                .unwrap_or(pure.unwrap_or(asyn.unwrap_or(keyword)))
                .spanning(body.slice());

            ASTDetails::ProcDeclaration {
                name,
                proc: Proc {
                    type_annotation: ProcType {
                        args: args.clone(),
                        args_spread: None, // TODO
                        is_pure,
                        is_async,
                        throws: None, // TODO
                    }
                    .with_slice(
                        args.get(0)
                            .map(|arg| arg.downcast().name.slice().clone())
                            .unwrap_or(body.slice().clone())
                            .spanning(body.slice()),
                    ),

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
    )(i)
}

#[memoize]
fn block(i: Slice) -> ParseResult<AST<Block>> {
    map(
        seq!(tag("{"), many0(statement), tag("}")),
        |(open, mut statements, close)| {
            make_node_tuple_typed!(Block, open.spanning(&close), statements)
        },
    )(i)
}

#[memoize]
fn value_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            opt(tag("export")),
            alt((tag("const"), tag("let"))),
            plain_identifier,
            opt(type_annotation),
            tag("="),
            expression(0),
        ),
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
            type_annotation.set_parent(&this);
            value.set_parent(&this);

            this
        },
    )(i)
}

#[memoize]
fn test_expr_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("test"),
            tag("expr"),
            exact_string_literal,
            tag("=>"),
            expression(0),
        ),
        |(start, _, mut name, _, mut expr)| {
            make_node!(
                TestExprDeclaration,
                start.spanning(expr.slice()),
                name,
                expr
            )
        },
    )(i)
}

#[memoize]
fn test_block_declaration(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(tag("test"), tag("block"), exact_string_literal, block),
        |(start, _, mut name, mut block)| {
            make_node!(
                TestBlockDeclaration,
                start.spanning(block.slice()),
                name,
                block
            )
        },
    )(i)
}

#[memoize]
fn test_type_declaration(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

#[memoize]
fn type_annotation(i: Slice) -> ParseResult<ASTAny> {
    preceded(tag(":"), w(type_expression(0)))(i)
}

// --- TypeExpression ---

fn type_expression(l: usize) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    move |i: Slice| -> ParseResult<ASTAny> { type_expression_inner(l, i) }
}

fn type_expression_inner(l: usize, i: Slice) -> ParseResult<ASTAny> {
    let mut tl = 0;

    parse_level!(
        l,
        tl,
        i,
        map(seq!(tag("typeof"), expression(0)), |(keyword, mut expr)| {
            make_node_tuple!(TypeofType, keyword.clone().spanning(expr.slice()), expr)
        })
    );

    parse_level!(
        l,
        tl,
        i,
        map(
            seq!(
                alt((
                    tag("keyof"),
                    tag("valueof"),
                    tag("elementof"),
                    tag("readonly")
                )),
                type_expression(0)
            ),
            |(keyword, mut inner)| {
                let src = keyword.clone().spanning(inner.slice());

                let this = ASTDetails::ModifierType {
                    kind: keyword.as_str().try_into().unwrap(),
                    inner: inner.clone(),
                }
                .with_slice(src);

                inner.set_parent(&this);

                this
            }
        )
    );

    // generic type

    parse_level!(
        l,
        tl,
        i,
        map(
            separated_list2(w(tag("|")), w(type_expression(tl + 1))),
            |mut members| { make_node_tuple!(UnionType, covering(&members).unwrap(), members) }
        )
    );

    parse_level!(
        l,
        tl,
        i,
        map(
            seq!(type_expression(tl + 1), tag("?")),
            |(mut inner, end)| {
                make_node_tuple!(MaybeType, inner.slice().clone().spanning(&end), inner)
            }
        )
    );

    // boundGenericType

    parse_level!(
        l,
        tl,
        i,
        map(
            seq!(type_expression(tl + 1), tag("[]")),
            |(mut element, end)| {
                make_node_tuple!(ArrayType, element.slice().clone().spanning(&end), element)
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
                seq!(tag("("), type_expression(tl + 1), tag(")")),
                |(open, mut inner, close)| {
                    make_node_tuple!(ParenthesizedType, open.spanning(&close), inner)
                }
            ),
            map(
                seq!(tag("\'"), string_contents, tag("\'")),
                |(open_quote, value, close_quote)| {
                    ASTDetails::StringLiteralType(value)
                        .with_slice(open_quote.spanning(&close_quote))
                },
            ),
            map(seq!(opt(tag("-")), numeric), |(neg, int)| {
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
        map(plain_identifier, |mut name| {
            make_node_tuple!(NamedType, name.slice().clone(), name)
        })
    );

    Err(nom::Err::Error(RawParseError {
        src: i,
        details: RawParseErrorDetails::Kind(ErrorKind::Fail),
    }))
}

#[memoize]
fn func_type(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(args_parenthesized, tag("=>"), type_expression(0)),
        |(args, _, returns)| {
            // TODO: func type with 0 arguments will have weird src
            let src = args
                .get(0)
                .map(|arg| arg.downcast().name.slice().clone())
                .unwrap_or(returns.slice().clone())
                .spanning(returns.slice());

            ASTDetails::FuncType {
                args,
                args_spread: None, // TODO
                is_pure: false,    // TODO
                is_async: false,   // TODO
                returns: Some(returns),
            }
            .with_slice(src)
        },
    )(i)
}

#[memoize]
fn proc_type(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(args_parenthesized, tag("{"), tag("}")),
        |(args, open_brace, close)| {
            // TODO: proc type with 0 arguments will have weird src
            let src = args
                .get(0)
                .map(|arg| arg.downcast().name.slice().clone())
                .unwrap_or(open_brace.clone())
                .spanning(&close);

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
fn record_type(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("{"),
            tag("["),
            type_expression(0),
            tag("]"),
            tag(":"),
            type_expression(0),
            tag("}"),
        ),
        |(open, _, mut key_type, _, _, mut value_type, close)| {
            make_node!(RecordType, open.spanning(&close), key_type, value_type)
        },
    )(i)
}

#[memoize]
fn object_or_interface_type(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            opt(tag("interface")),
            tag("{"),
            separated_list0(w(tag(",")), w(key_value_type)),
            tag("}"),
        ),
        |(interface, open, mut entries, close)| {
            let this = ASTDetails::ObjectType {
                entries: entries.clone(),
                is_interface: interface.is_some(),
            }
            .with_slice(interface.unwrap_or(open).spanning(&close));

            entries.set_parent(&this);

            this
        },
    )(i)
}

#[memoize]
fn key_value_type(i: Slice) -> ParseResult<ObjectTypeEntry> {
    map(
        separated_pair(plain_identifier, tag(":"), type_expression(0)),
        |(mut key, mut value)| {
            KeyValueType {
                key: key.upcast(),
                value,
            }
            .into()
        },
    )(i)
}

#[memoize]
fn tuple_type(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("["),
            separated_list0(w(tag(",")), w(type_expression(0))),
            tag("]"),
        ),
        |(open, mut members, close)| make_node_tuple!(TupleType, open.spanning(&close), members),
    )(i)
}

// --- Statement ---

#[memoize]
fn statement(i: Slice) -> ParseResult<ASTAny> {
    alt((
        // map(declaration_statement, |x| x.map(ASTDetails::from)),
        if_else_statement,
        for_loop,
        while_loop,
        // assignment,
        // try_catch,
        throw_statement,
        autorun,
        map(
            seq!(
                invocation_accessor_chain(11), // HACK: Has to be kept in sync with expression() function!
                tag(";")
            ),
            |(invocation, _)| invocation,
        ),
    ))(i)
}

#[memoize]
fn declaration_statement(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

#[memoize]
fn if_else_statement(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            separated_list1(w(tag("else")), w(if_else_statement_case)),
            opt(map(w(seq!(tag("else"), block)), |(_, default_case)| {
                default_case
            },)),
        ),
        |(mut cases, mut default_case)| {
            make_node!(
                IfElseStatement,
                covering(&cases).unwrap(),
                cases,
                default_case
            )
        },
    )(i)
}

#[memoize]
fn if_else_statement_case(i: Slice) -> ParseResult<AST<IfElseStatementCase>> {
    map(
        seq!(tag("if"), expression(0), block),
        |(start, mut condition, mut outcome)| {
            make_node_typed!(
                IfElseStatementCase,
                start.spanning(outcome.slice()),
                condition,
                outcome
            )
        },
    )(i)
}

#[memoize]
fn for_loop(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("for"),
            plain_identifier,
            tag("in"),
            expression(0),
            block,
        ),
        |(start, mut item_identifier, _, mut iterator, mut body)| {
            make_node!(
                ForLoop,
                start.spanning(body.slice()),
                item_identifier,
                iterator,
                body
            )
        },
    )(i)
}

#[memoize]
fn while_loop(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(tag("while"), expression(0), block),
        |(start, mut condition, mut body)| {
            make_node!(WhileLoop, start.spanning(body.slice()), condition, body)
        },
    )(i)
}

#[memoize]
fn assignment(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

#[memoize]
fn try_catch(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

#[memoize]
fn throw_statement(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(tag("throw"), expression(0), tag(";")),
        |(start, mut error_expression, end)| {
            make_node!(ThrowStatement, start.spanning(&end), error_expression)
        },
    )(i)
}

#[memoize]
fn autorun(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("autorun"),
            block,
            alt((
                map(tag("forever"), |_| None),
                map(
                    seq!(tag("until"), tag("=>"), expression(0)),
                    |(_, _, until)| Some(until),
                ),
            )),
            tag(";"),
        ),
        |(start, mut effect_block, mut until, end)| {
            make_node!(Autorun, start.spanning(&end), effect_block, until)
        },
    )(i)
}

// --- Expression ---

fn expression(l: usize) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    move |i: Slice| -> ParseResult<ASTAny> { expression_inner(l, i) }
}

#[memoize]
fn expression_inner(l: usize, i: Slice) -> ParseResult<ASTAny> {
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
    parse_level!(l, tl, i, binary_operation_1(tl, "??"));
    parse_level!(l, tl, i, binary_operation_1(tl, "||"));
    parse_level!(l, tl, i, binary_operation_1(tl, "&&"));
    parse_level!(l, tl, i, binary_operation_2(tl, "==", "!="));
    parse_level!(l, tl, i, binary_operation_4(tl, "<=", ">=", "<", ">"));
    parse_level!(l, tl, i, binary_operation_2(tl, "+", "-"));
    parse_level!(l, tl, i, binary_operation_2(tl, "*", "/"));
    parse_level!(l, tl, i, alt((as_cast(tl), instance_of(tl))));
    parse_level!(l, tl, i, negation_operation(tl));

    // indexer

    // parse_level!(l, tl, i, error_expression);

    parse_level!(l, tl, i, invocation_accessor_chain(tl));

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
            map(exact_string_literal, AST::upcast),
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

fn invocation_accessor_chain(level: usize) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    move |i: Slice| -> ParseResult<ASTAny> { invocation_accessor_chain_inner(level, i) }
}

#[memoize]
fn invocation_accessor_chain_inner(level: usize, i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            expression(level + 1),
            many1(alt((
                invocation_args,
                indexer_expression,
                dot_property_access,
            )))
        ),
        |(mut base, clauses)| {
            let mut subject = base;

            for clause in clauses {
                let mut old_subject = subject;

                match clause {
                    InvocationOrPropertyAccess::InvokingWith(mut args, args_slice) => {
                        subject = ASTDetails::Invocation {
                            subject: old_subject.clone(),
                            args: args.clone(),
                            spread_args: None,         // TODO
                            type_args: Vec::new(),     // TODO
                            bubbles: false,            // TODO
                            awaited_or_detached: None, // TODO
                        }
                        .with_slice(old_subject.slice().clone().spanning(&args_slice));

                        old_subject.set_parent(&subject);
                        args.set_parent(&subject);
                    }
                    InvocationOrPropertyAccess::Accessing(mut property, indexer_slice) => {
                        subject = ASTDetails::PropertyAccessor {
                            subject: old_subject.clone(),
                            property: property.clone(),
                            optional: false, // TODO
                        }
                        .with_slice(old_subject.slice().clone().spanning(&indexer_slice));

                        old_subject.set_parent(&subject);
                        property.set_parent(&subject);
                    }
                }
            }

            subject
        },
    )(i)
}

#[memoize]
fn invocation_args(i: Slice) -> ParseResult<InvocationOrPropertyAccess> {
    map(
        seq!(
            tag("("),
            separated_list0(w(tag(",")), w(expression(0))),
            tag(")")
        ),
        |(open, args, close)| InvocationOrPropertyAccess::InvokingWith(args, open.spanning(&close)),
    )(i)
}

#[memoize]
fn indexer_expression(i: Slice) -> ParseResult<InvocationOrPropertyAccess> {
    map(
        seq!(tag("["), expression(0), tag("]")),
        |(open, property, close)| {
            InvocationOrPropertyAccess::Accessing(property.upcast(), open.spanning(&close))
        },
    )(i)
}

#[memoize]
fn dot_property_access(i: Slice) -> ParseResult<InvocationOrPropertyAccess> {
    map(seq!(tag("."), plain_identifier), |(dot, property)| {
        let src = dot.spanning(property.slice());

        InvocationOrPropertyAccess::Accessing(property.upcast(), src)
    })(i)
}

#[derive(Debug, Clone)]
enum InvocationOrPropertyAccess {
    InvokingWith(Vec<ASTAny>, Slice),
    Accessing(ASTAny, Slice),
}

macro_rules! parse_binary_operation {
    ($level:expr, $operator:expr) => {
        move |i: Slice| -> ParseResult<ASTAny> {
            map(
                tuple((
                    expression($level + 1),
                    many1(pair(w($operator), w(expression($level + 1)))),
                )),
                |(base, clauses)| {
                    let mut left = base;

                    for (op, mut right) in clauses {
                        let mut old_left = left.clone();
                        let mut op =
                            BinaryOperator(BinaryOperatorOp::from_str(op.as_str()).unwrap())
                                .with_slice(op);

                        left = ASTDetails::BinaryOperation {
                            left: old_left.clone(),
                            op: op.clone(),
                            right: right.clone(),
                        }
                        .with_slice(old_left.slice().clone().spanning(right.slice()));

                        old_left.set_parent(&left);
                        op.set_parent(&left);
                        right.set_parent(&left);
                    }

                    left
                },
            )(i.clone())
        }
    };
}

fn binary_operation_1(level: usize, a: &'static str) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    parse_binary_operation!(level, tag(a))
}

fn binary_operation_2(
    level: usize,
    a: &'static str,
    b: &'static str,
) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    parse_binary_operation!(level, alt((tag(a), tag(b))))
}

fn binary_operation_4(
    level: usize,
    a: &'static str,
    b: &'static str,
    c: &'static str,
    d: &'static str,
) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    parse_binary_operation!(level, alt((tag(a), tag(b), tag(c), tag(d))))
}

fn regular_expression(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

fn error_expression(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

fn instance_of(level: usize) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    move |i: Slice| -> ParseResult<ASTAny> {
        map(
            seq!(expression(level + 1), tag("instanceof"), type_expression(0)),
            |(mut inner, _, mut possible_type)| {
                make_node!(
                    InstanceOf,
                    inner.slice().clone().spanning(possible_type.slice()),
                    inner,
                    possible_type
                )
            },
        )(i)
    }
}

fn as_cast(level: usize) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    move |i: Slice| -> ParseResult<ASTAny> {
        map(
            seq!(expression(level + 1), tag("as"), type_expression(0)),
            |(mut inner, _, mut as_type)| {
                make_node!(
                    AsCast,
                    inner.slice().clone().spanning(as_type.slice()),
                    inner,
                    as_type
                )
            },
        )(i)
    }
}

#[memoize]
fn element_tag(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

#[memoize]
fn if_else_expression(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            separated_list1(w(tag("else")), w(if_else_expression_case)),
            opt(map(
                seq!(tag("else"), tag("{"), expression(0), tag("}")),
                |(_, _, outcome, _)| outcome,
            )),
        ),
        |(mut cases, mut default_case)| {
            make_node!(
                IfElseExpression,
                covering(&cases).unwrap(),
                cases,
                default_case
            )
        },
    )(i)
}

#[memoize]
fn if_else_expression_case(i: Slice) -> ParseResult<AST<IfElseExpressionCase>> {
    map(
        seq!(tag("if"), expression(0), tag("{"), expression(0), tag("}"),),
        |(start, mut condition, _, mut outcome, end)| {
            make_node_typed!(
                IfElseExpressionCase,
                start.spanning(&end),
                condition,
                outcome
            )
        },
    )(i)
}

#[memoize]
fn switch_expression(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("switch"),
            expression(0),
            tag("{"),
            separated_list1(
                w(tag(",")),
                w(map(
                    seq!(tag("case"), type_expression(0), tag(":"), expression(0)),
                    |(keyword, mut type_filter, _, mut outcome)| {
                        SwitchExpressionCase {
                            type_filter: type_filter.clone(),
                            outcome: outcome.clone(),
                        }
                        .with_slice(keyword.spanning(outcome.slice()))
                    },
                ),),
            ),
            opt(preceded(
                whitespace,
                map(seq!(tag("default"), expression(0)), |(_, expr)| expr),
            )),
            tag("}"),
        ),
        |(start, mut value, _, mut cases, mut default_case, end)| {
            make_node!(
                SwitchExpression,
                start.spanning(&end),
                value,
                cases,
                default_case
            )
        },
    )(i)
}

#[memoize]
fn range_expression(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(number_literal, tag(".."), number_literal),
        |(mut start, _, mut end)| {
            make_node!(
                RangeExpression,
                start.slice().clone().spanning(end.slice()),
                start,
                end
            )
        },
    )(i)
}

#[memoize]
fn javascript_escape_expression(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

#[memoize]
fn proc(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

#[memoize]
fn func(i: Slice) -> ParseResult<AST<Func>> {
    map(
        seq!(
            opt(tag("pure")),
            opt(tag("async")),
            alt((args_parenthesized, arg_singleton)),
            opt(type_annotation),
            tag("=>"),
            expression(0),
        ),
        |(pure, asyn, mut args, mut returns, _, mut body)| {
            let is_async = asyn.is_some();
            let is_pure = pure.is_some();
            let src = pure
                .unwrap_or(asyn.unwrap_or_else(|| {
                    args.get(0)
                        .map(|arg| arg.downcast().name.slice().clone())
                        .unwrap_or(body.slice().clone())
                }))
                .spanning(body.slice());

            let mut type_annotation = FuncType {
                args: args.clone(),
                args_spread: None, // TODO
                is_pure,
                is_async,
                returns: returns.clone(),
            }
            .with_slice(src.clone());

            args.set_parent(&type_annotation);
            returns.set_parent(&type_annotation);

            let this = Func {
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

#[memoize]
fn inline_const_group(i: Slice) -> ParseResult<ASTAny> {
    todo!()
}

fn negation_operation(level: usize) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    move |i: Slice| -> ParseResult<ASTAny> {
        map(
            seq!(tag("!"), expression(level + 1)),
            |(start, mut expr)| {
                make_node_tuple!(NegationOperation, start.spanning(expr.slice()), expr)
            },
        )(i)
    }
}

fn parenthesis(level: usize) -> impl Fn(Slice) -> ParseResult<ASTAny> {
    move |i: Slice| -> ParseResult<ASTAny> {
        map(
            seq!(tag("("), expression(level + 1), tag(")")),
            |(open_paren, mut inner, close_paren)| {
                make_node_tuple!(Parenthesis, open_paren.spanning(&close_paren), inner)
            },
        )(i)
    }
}

#[memoize]
fn object_literal(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("{"),
            separated_list0(w(char(',')), w(key_value_expression)),
            tag("}"),
        ),
        |(open_bracket, mut entries, close_bracket)| {
            let this = ASTDetails::ObjectLiteral(entries.clone())
                .with_slice(open_bracket.spanning(&close_bracket));

            for entry in entries.iter_mut() {
                match entry {
                    ObjectLiteralEntry::KeyValue(KeyValue { key, value }) => {
                        key.set_parent(&this);
                        value.set_parent(&this);
                    }
                    ObjectLiteralEntry::Spread(Spread(expr)) => {
                        expr.set_parent(&this);
                    }
                }
            }

            this
        },
    )(i)
}

fn key_value_expression(i: Slice) -> ParseResult<ObjectLiteralEntry> {
    map(
        seq!(plain_identifier, tag(":"), expression(0)),
        |(mut key, _, mut value)| {
            KeyValue {
                key: key.upcast(),
                value,
            }
            .into()
        },
    )(i)
}

#[memoize]
fn array_literal(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(
            tag("["),
            separated_list0(w(char(',')), w(expression(0))),
            tag("]"),
        ),
        |(open_bracket, mut entries, close_bracket)| {
            make_node_tuple!(ArrayLiteral, open_bracket.spanning(&close_bracket), entries)
        },
    )(i)
}

#[memoize]
fn string_literal(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(tag("\'"), many0(string_literal_segment), tag("\'")),
        |(open_quote, mut segments, close_quote)| {
            let this = ASTDetails::StringLiteral {
                tag: None, // TODO
                segments: segments.clone(),
            }
            .with_slice(open_quote.spanning(&close_quote));

            for segment in segments.iter_mut() {
                if let StringLiteralSegment::AST(ast) = segment {
                    ast.set_parent(&this);
                }
            }

            this
        },
    )(i)
}

#[memoize]
fn exact_string_literal(i: Slice) -> ParseResult<AST<ExactStringLiteral>> {
    map(
        seq!(tag("\'"), string_contents, tag("\'")),
        |(open_quote, value, close_quote)| {
            ExactStringLiteral {
                tag: None, // TODO
                value,
            }
            .with_slice(open_quote.spanning(&close_quote))
        },
    )(i)
}

#[memoize]
fn string_literal_segment(i: Slice) -> ParseResult<StringLiteralSegment> {
    alt((
        map(seq!(tag("${"), expression(0), tag("}")), |(_, expr, _)| {
            expr.into()
        }),
        map(string_contents, |s| s.into()),
    ))(i)
}

#[memoize]
fn number_literal(i: Slice) -> ParseResult<ASTAny> {
    map(
        seq!(opt(tag("-")), numeric, opt(seq!(tag("."), numeric))),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int.clone());
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            ASTDetails::NumberLiteral(full.clone()).with_slice(full)
        },
    )(i)
}

#[memoize]
fn boolean_literal(input: Slice) -> ParseResult<ASTAny> {
    let parse_true = map(tag("true"), |src: Slice| {
        ASTDetails::BooleanLiteral(true).with_slice(src)
    });

    let parse_false = map(tag("false"), |src: Slice| {
        ASTDetails::BooleanLiteral(false).with_slice(src)
    });

    alt((parse_true, parse_false))(input)
}

#[memoize]
fn nil_literal(input: Slice) -> ParseResult<ASTAny> {
    map(tag("nil"), |src: Slice| {
        ASTDetails::NilLiteral.with_slice(src)
    })(input)
}

#[memoize]
fn local_identifier(i: Slice) -> ParseResult<ASTAny> {
    map(identifier_like, |name| {
        ASTDetails::LocalIdentifier(name.clone()).with_slice(name)
    })(i)
}

fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    map(identifier_like, |name| {
        PlainIdentifier(name.clone()).with_slice(name)
    })(i)
}

// --- Util parsers ---

fn w<O, G>(parser: G) -> impl FnMut(Slice) -> ParseResult<O>
where
    G: Parser<Slice, O, RawParseError>,
{
    preceded(whitespace, parser)
}

fn string_contents(i: Slice) -> ParseResult<Slice> {
    escaped(take_while(|ch: char| ch != '\''), '\\', one_of("$\'n\\"))(i)
}

fn identifier_like(i: Slice) -> ParseResult<Slice> {
    take_while1(|ch: char| ch.is_alphabetic() || ch == '_' || ch == '$')(i) // TODO: numeric
}

fn numeric(i: Slice) -> ParseResult<Slice> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace(i: Slice) -> ParseResult<Slice> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i) // TODO: comments
}

fn separated_list2<O2, F, G>(
    sep: G,
    f: F,
) -> impl FnMut(Slice) -> IResult<Slice, Vec<ASTAny>, RawParseError>
where
    F: Parser<Slice, ASTAny, RawParseError>,
    G: Parser<Slice, O2, RawParseError>,
{
    let mut parser = separated_list1(sep, f);

    move |i: Slice| -> IResult<Slice, Vec<ASTAny>, RawParseError> {
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
pub struct RawParseError {
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
