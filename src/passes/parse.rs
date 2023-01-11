use crate::{
    model::ast::*,
    model::slice::Slice,
    model::{
        ast::{covering, Any, AST},
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
            let src = $src;
            let this = $kind {
                $($prop: $prop.clone(),)*
            }
            .as_ast(src);

            $($prop.set_parent(&this);)*

            this
        }
    };
}

macro_rules! make_node_tuple {
    ($kind:ident, $src:expr, $( $prop:ident ),* $(,)?) => {
        {
            let src = $src;
            let this = $kind(
                $($prop.clone()),*
            )
            .as_ast(src);

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
            let res = $a($i.clone());

            if res.is_ok() {
                return res;
            }
        }
    };
}

macro_rules! parse_level_expression {
    ($level:expr, $this_level:expr, $i:expr, $a:expr) => {
        $this_level += 1;
        if $level <= $this_level {
            let res = map($a, AST::recast::<Expression>)($i.clone());

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
            make_node!(
                Module,
                covering(&declarations).unwrap_or(i.clone().slice_range(0, Some(0))),
                declarations
            )
        },
    )(i.clone())
}

// --- Declaration

#[memoize]
fn declaration(i: Slice) -> ParseResult<AST<Declaration>> {
    alt((
        map(import_all_declaration, AST::recast::<Declaration>),
        map(import_declaration, AST::recast::<Declaration>),
        map(type_declaration, AST::recast::<Declaration>),
        map(func_declaration, AST::recast::<Declaration>),
        map(proc_declaration, AST::recast::<Declaration>),
        map(value_declaration, AST::recast::<Declaration>),
        map(test_expr_declaration, AST::recast::<Declaration>),
        map(test_block_declaration, AST::recast::<Declaration>),
        // test_type_declaration,
    ))(i)
}

#[memoize]
fn import_all_declaration(i: Slice) -> ParseResult<AST<ImportAllDeclaration>> {
    map(
        seq!(
            tag("import"),
            exact_string_literal,
            tag("as"),
            plain_identifier,
        ),
        |(start, mut path, _, mut name)| {
            make_node!(ImportAllDeclaration, start.spanning(&name), path, name)
        },
    )(i)
}

#[memoize]
fn import_declaration(i: Slice) -> ParseResult<AST<ImportDeclaration>> {
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
            make_node!(
                ImportItem,
                name.spanning(alias.as_ref().unwrap_or(&name),),
                name,
                alias
            )
        },
    )(i)
}

#[memoize]
fn type_declaration(i: Slice) -> ParseResult<AST<TypeDeclaration>> {
    map(
        seq!(
            opt(tag("export")),
            tag("type"),
            plain_identifier,
            tag("="),
            type_expression(0),
        ),
        |(export, keyword, mut name, _, mut declared_type)| {
            let mut exported = export.is_some();

            make_node!(
                TypeDeclaration,
                export.unwrap_or(keyword).spanning(&declared_type),
                name,
                declared_type,
                exported
            )
        },
    )(i)
}

#[memoize]
fn func_declaration(i: Slice) -> ParseResult<AST<FuncDeclaration>> {
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
                .spanning(&body);
            let mut exported = export.is_some();
            let mut is_pure = pure.is_some();
            let mut is_async = asyn.is_some();
            let mut platforms = PlatformSet::all(); // TODO
            let mut decorators = vec![]; // TODO

            let mut args_spread = None;
            let mut type_annotation = make_node!(
                FuncType,
                args.get(0)
                    .map(|arg| arg.downcast().name.slice().clone())
                    .unwrap_or(arrow)
                    .spanning(&body),
                args,
                args_spread,
                is_pure,
                is_async,
                returns
            );

            let mut func = make_node!(Func, src.clone(), type_annotation, is_async, is_pure, body);

            make_node!(
                FuncDeclaration,
                src,
                name,
                func,
                exported,
                platforms,
                decorators
            )
        },
    )(i)
}

#[memoize]
fn arg_singleton(i: Slice) -> ParseResult<Vec<AST<Arg>>> {
    map(plain_identifier, |mut name| {
        let mut type_annotation = None;
        let mut optional = false;

        vec![make_node!(
            Arg,
            name.slice().clone(),
            name,
            type_annotation,
            optional
        )]
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
                    let mut optional = false;

                    make_node!(
                        Arg,
                        (&name).spanning(
                            type_annotation
                                .as_ref()
                                .map(|a| a.slice())
                                .unwrap_or(name.slice()),
                        ),
                        name,
                        type_annotation,
                        optional
                    )
                })
                .collect()
        },
    )(i)
}

#[memoize]
fn proc_declaration(i: Slice) -> ParseResult<AST<ProcDeclaration>> {
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
        |(export, pure, asyn, keyword, mut name, mut args, mut body)| {
            let src = export
                .clone()
                .unwrap_or(pure.clone().unwrap_or(asyn.clone().unwrap_or(keyword)))
                .spanning(&body);
            let mut exported = export.is_some();
            let mut is_async = asyn.is_some();
            let mut is_pure = pure.is_some();
            let mut platforms = PlatformSet::all(); // TODO
            let mut decorators = vec![]; // TODO

            let mut args_spread = None;
            let mut throws = None; // TODO
            let mut type_annotation = make_node!(
                ProcType,
                args.get(0)
                    .map(|arg| arg.downcast().name.slice().clone())
                    .unwrap_or(body.slice().clone())
                    .spanning(&body),
                args,
                args_spread,
                is_pure,
                is_async,
                throws
            );

            let mut proc = make_node!(Proc, src.clone(), type_annotation, is_async, is_pure, body);

            make_node!(
                ProcDeclaration,
                src,
                name,
                proc,
                exported,
                platforms,
                decorators
            )
        },
    )(i)
}

#[memoize]
fn block(i: Slice) -> ParseResult<AST<Block>> {
    map(
        seq!(tag("{"), many0(statement), tag("}")),
        |(open, mut statements, close)| make_node_tuple!(Block, open.spanning(&close), statements),
    )(i)
}

#[memoize]
fn value_declaration(i: Slice) -> ParseResult<AST<ValueDeclaration>> {
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
            let mut exported = export.is_some();
            let mut is_const = keyword.as_str() == "const";
            let mut platforms = PlatformSet::all(); // TODO
            let src = export.unwrap_or(keyword).spanning(&value);

            make_node!(
                ValueDeclaration,
                src,
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms
            )
        },
    )(i)
}

#[memoize]
fn test_expr_declaration(i: Slice) -> ParseResult<AST<TestExprDeclaration>> {
    map(
        seq!(
            tag("test"),
            tag("expr"),
            exact_string_literal,
            tag("=>"),
            expression(0),
        ),
        |(start, _, mut name, _, mut expr)| {
            make_node!(TestExprDeclaration, start.spanning(&expr), name, expr)
        },
    )(i)
}

#[memoize]
fn test_block_declaration(i: Slice) -> ParseResult<AST<TestBlockDeclaration>> {
    map(
        seq!(tag("test"), tag("block"), exact_string_literal, block),
        |(start, _, mut name, mut block)| {
            make_node!(TestBlockDeclaration, start.spanning(&block), name, block)
        },
    )(i)
}

#[memoize]
fn test_type_declaration(i: Slice) -> ParseResult<AST<TestTypeDeclaration>> {
    todo!()
}

#[memoize]
fn type_annotation(i: Slice) -> ParseResult<AST<TypeExpression>> {
    preceded(tag(":"), w(type_expression(0)))(i)
}

// --- TypeExpression ---

fn type_expression(l: usize) -> impl Fn(Slice) -> ParseResult<AST<TypeExpression>> {
    move |i: Slice| -> ParseResult<AST<TypeExpression>> { type_expression_inner(l, i) }
}

fn type_expression_inner(l: usize, i: Slice) -> ParseResult<AST<TypeExpression>> {
    let mut tl = 0;

    parse_level!(
        l,
        tl,
        i,
        map(seq!(tag("typeof"), expression(0)), |(keyword, mut expr)| {
            make_node_tuple!(TypeofType, keyword.spanning(&expr), expr).recast::<TypeExpression>()
        })
    );

    parse_level!(
        l,
        tl,
        i,
        map(
            seq!(
                alt((
                    tag(ModifierTypeKind::Keyof.into()),
                    tag(ModifierTypeKind::Valueof.into()),
                    tag(ModifierTypeKind::Elementof.into()),
                    tag(ModifierTypeKind::Readonly.into())
                )),
                type_expression(0)
            ),
            |(keyword, mut inner)| {
                let mut kind: ModifierTypeKind = keyword.as_str().try_into().unwrap();
                let src = keyword.spanning(&inner);

                make_node!(ModifierType, src, kind, inner).recast::<TypeExpression>()
            }
        )
    );

    parse_level!(
        l,
        tl,
        i,
        map(
            seq!(
                named_type,
                tag("<"),
                separated_list1(w(tag(",")), w(type_expression(0))),
                tag(">"),
            ),
            |(mut generic, _, mut type_args, end)| {
                let mut generic = generic.recast::<TypeExpression>();
                let src = generic.spanning(&end);

                if let Some(name) = generic.try_downcast::<NamedType>() {
                    if type_args.len() == 1 {
                        let kind = match name.0.downcast().0.as_str() {
                            "Plan" => Some(SpecialTypeKind::Plan),
                            "Iterator" => Some(SpecialTypeKind::Iterator),
                            "Error" => Some(SpecialTypeKind::Error),
                            _ => None,
                        };

                        if let Some(mut kind) = kind {
                            let mut inner = type_args.into_iter().next().unwrap();

                            return make_node!(SpecialType, src, kind, inner)
                                .recast::<TypeExpression>();
                        }
                    }
                }

                return make_node!(BoundGenericType, src, generic, type_args)
                    .recast::<TypeExpression>();
            }
        )
    );

    parse_level!(
        l,
        tl,
        i,
        map(
            separated_list2(w(tag("|")), w(type_expression(tl + 1))),
            |mut members| {
                make_node_tuple!(UnionType, covering(&members).unwrap(), members)
                    .recast::<TypeExpression>()
            }
        )
    );

    parse_level!(
        l,
        tl,
        i,
        map(
            seq!(type_expression(tl + 1), tag("?")),
            |(mut inner, end)| {
                make_node_tuple!(MaybeType, inner.spanning(&end), inner).recast::<TypeExpression>()
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
                make_node_tuple!(ArrayType, element.spanning(&end), element)
                    .recast::<TypeExpression>()
            }
        )
    );

    parse_level!(
        l,
        tl,
        i,
        alt((
            map(func_type, AST::recast::<TypeExpression>),
            map(proc_type, AST::recast::<TypeExpression>),
            map(record_type, AST::recast::<TypeExpression>),
            map(object_or_interface_type, AST::recast::<TypeExpression>),
            map(tuple_type, AST::recast::<TypeExpression>),
            map(
                seq!(tag("("), type_expression(0), tag(")")),
                |(open, mut inner, close)| {
                    make_node_tuple!(ParenthesizedType, open.spanning(&close), inner)
                        .recast::<TypeExpression>()
                }
            ),
            map(
                seq!(tag("\'"), string_contents, tag("\'")),
                |(open_quote, value, close_quote)| {
                    StringLiteralType(value)
                        .as_ast(open_quote.spanning(&close_quote))
                        .recast::<TypeExpression>()
                },
            ),
            map(seq!(opt(tag("-")), numeric), |(neg, int)| {
                let src = neg.as_ref().unwrap_or(&int).spanning(&int);
                NumberLiteralType(src.clone())
                    .as_ast(src)
                    .recast::<TypeExpression>()
            }),
            map(alt((tag("true"), tag("false"))), |s: Slice| {
                BooleanLiteralType(s.as_str() == "true")
                    .as_ast(s)
                    .recast::<TypeExpression>()
            }),
            map(tag("string"), |s: Slice| StringType
                .as_ast(s)
                .recast::<TypeExpression>()),
            map(tag("number"), |s: Slice| NumberType
                .as_ast(s)
                .recast::<TypeExpression>()),
            map(tag("boolean"), |s: Slice| BooleanType
                .as_ast(s)
                .recast::<TypeExpression>()),
            map(tag("unknown"), |s: Slice| UnknownType
                .as_ast(s)
                .recast::<TypeExpression>()),
            map(tag("nil"), |s: Slice| NilType
                .as_ast(s)
                .recast::<TypeExpression>()),
        ))
    );

    parse_level!(l, tl, i, map(named_type, AST::recast::<TypeExpression>));

    Err(nom::Err::Error(RawParseError {
        src: i,
        details: RawParseErrorDetails::Kind(ErrorKind::Fail),
    }))
}

#[memoize]
fn func_type(i: Slice) -> ParseResult<AST<FuncType>> {
    map(
        seq!(args_parenthesized, tag("=>"), type_expression(0)),
        |(mut args, _, mut returns)| {
            // TODO: func type with 0 arguments will have weird src
            let src = args
                .get(0)
                .map(|arg| arg.downcast().name.slice().clone())
                .unwrap_or(returns.slice().clone())
                .spanning(&returns);

            let mut args_spread = None; // TODO
            let mut is_pure = false; // TODO
            let mut is_async = false; // TODO
            let mut returns = Some(returns);

            make_node!(FuncType, src, args, args_spread, is_pure, is_async, returns)
        },
    )(i)
}

#[memoize]
fn proc_type(i: Slice) -> ParseResult<AST<ProcType>> {
    map(
        seq!(args_parenthesized, tag("{"), tag("}")),
        |(mut args, open_brace, close)| {
            // TODO: proc type with 0 arguments will have weird src
            let src = args
                .get(0)
                .map(|arg| arg.downcast().name.slice().clone())
                .unwrap_or(open_brace.clone())
                .spanning(&close);

            let mut args_spread = None; // TODO
            let mut is_pure = false; // TODO
            let mut is_async = false; // TODO
            let mut throws = None; // TODO

            make_node!(ProcType, src, args, args_spread, is_pure, is_async, throws)
        },
    )(i)
}

#[memoize]
fn record_type(i: Slice) -> ParseResult<AST<RecordType>> {
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
fn object_or_interface_type(i: Slice) -> ParseResult<AST<ObjectType>> {
    map(
        seq!(
            opt(tag("interface")),
            tag("{"),
            separated_list0(
                w(tag(",")),
                alt((
                    map(
                        preceded(tag("..."), type_expression(0)),
                        KeyValueOrSpread::Spread
                    ),
                    key_value_type,
                ))
            ),
            tag("}"),
        ),
        |(interface, open, mut entries, close)| {
            let mut is_interface = interface.is_some();

            make_node!(
                ObjectType,
                interface.unwrap_or(open).spanning(&close),
                entries,
                is_interface
            )
        },
    )(i)
}

#[memoize]
fn key_value_type(i: Slice) -> ParseResult<KeyValueOrSpread<AST<TypeExpression>>> {
    map(
        seq!(plain_identifier, tag(":"), type_expression(0)),
        |(key, _, value)| {
            KeyValueOrSpread::KeyValue(
                identifier_to_string_type(key).recast::<TypeExpression>(),
                value,
            )
        },
    )(i)
}

#[memoize]
fn tuple_type(i: Slice) -> ParseResult<AST<TupleType>> {
    map(
        seq!(
            tag("["),
            separated_list0(
                w(tag(",")),
                w(alt((
                    map(
                        preceded(tag("..."), type_expression(0)),
                        ElementOrSpread::Spread
                    ),
                    map(type_expression(0), ElementOrSpread::Element),
                )))
            ),
            tag("]"),
        ),
        |(open, mut members, close)| make_node_tuple!(TupleType, open.spanning(&close), members),
    )(i)
}

#[memoize]
fn named_type(i: Slice) -> ParseResult<AST<NamedType>> {
    map(local_identifier, |mut ident| {
        make_node_tuple!(NamedType, ident.slice().clone(), ident)
    })(i)
}

// --- Statement ---

#[memoize]
fn statement(i: Slice) -> ParseResult<AST<Statement>> {
    alt((
        map(declaration_statement, AST::recast::<Statement>),
        map(if_else_statement, AST::recast::<Statement>),
        map(for_loop, AST::recast::<Statement>),
        map(while_loop, AST::recast::<Statement>),
        map(assignment, AST::recast::<Statement>),
        map(try_catch, AST::recast::<Statement>),
        map(throw_statement, AST::recast::<Statement>),
        map(autorun, AST::recast::<Statement>),
        map(
            seq!(
                invocation_accessor_chain(13), // HACK: Has to be kept in sync with expression() function!
                tag(";")
            ),
            |(invocation, _)| {
                invocation
                    .try_recast::<Invocation>()
                    .unwrap()
                    .recast::<Statement>()
            },
        ),
    ))(i)
}

#[memoize]
fn declaration_statement(i: Slice) -> ParseResult<AST<DeclarationStatement>> {
    map(
        seq!(
            alt((tag("let"), tag("const"))),
            declaration_destination,
            tag("="),
            expression(0),
            tag(";")
        ),
        |(keyword, mut destination, _, mut value, end)| {
            let mut is_const = keyword.as_str() == "const";

            make_node!(
                DeclarationStatement,
                keyword.spanning(&end),
                destination,
                value,
                is_const
            )
        },
    )(i)
}

#[memoize]
fn if_else_statement(i: Slice) -> ParseResult<AST<IfElseStatement>> {
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
            make_node!(
                IfElseStatementCase,
                start.spanning(&outcome),
                condition,
                outcome
            )
        },
    )(i)
}

#[memoize]
fn for_loop(i: Slice) -> ParseResult<AST<ForLoop>> {
    map(
        seq!(
            tag("for"),
            plain_identifier,
            tag("of"),
            expression(0),
            block,
        ),
        |(start, mut item_identifier, _, mut iterator, mut body)| {
            make_node!(
                ForLoop,
                start.spanning(&body),
                item_identifier,
                iterator,
                body
            )
        },
    )(i)
}

#[memoize]
fn while_loop(i: Slice) -> ParseResult<AST<WhileLoop>> {
    map(
        seq!(tag("while"), expression(0), block),
        |(start, mut condition, mut body)| {
            make_node!(WhileLoop, start.spanning(&body), condition, body)
        },
    )(i)
}

#[memoize]
fn assignment(i: Slice) -> ParseResult<AST<Assignment>> {
    map(
        seq!(
            alt((
                map(invocation_accessor_chain(13), AST::recast::<Expression>),
                map(local_identifier, AST::recast::<Expression>)
            )),
            pair(
                opt(map(
                    alt((
                        tag("??"),
                        tag("||"),
                        tag("&&"),
                        tag("+"),
                        tag("-"),
                        tag("*"),
                        tag("/"),
                    )),
                    |slice: Slice| BinaryOperator(
                        BinaryOperatorOp::from_str(slice.as_str()).unwrap()
                    )
                    .as_ast(slice)
                )),
                tag("=")
            ),
            expression(0),
            tag(";")
        ),
        |(mut target, (mut operator, _), mut value, end)| {
            make_node!(Assignment, target.spanning(&end), target, value, operator)
        },
    )(i)
}

#[memoize]
fn try_catch(i: Slice) -> ParseResult<AST<TryCatch>> {
    map(
        seq!(tag("try"), block, tag("catch"), plain_identifier, block,),
        |(start, mut try_block, _, mut error_identifier, mut catch_block)| {
            make_node!(
                TryCatch,
                start.spanning(&catch_block),
                try_block,
                error_identifier,
                catch_block
            )
        },
    )(i)
}

#[memoize]
fn throw_statement(i: Slice) -> ParseResult<AST<ThrowStatement>> {
    map(
        seq!(tag("throw"), expression(0), tag(";")),
        |(start, mut error_expression, end)| {
            make_node!(ThrowStatement, start.spanning(&end), error_expression)
        },
    )(i)
}

#[memoize]
fn autorun(i: Slice) -> ParseResult<AST<Autorun>> {
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

fn expression(l: usize) -> impl Fn(Slice) -> ParseResult<AST<Expression>> {
    move |i: Slice| -> ParseResult<AST<Expression>> { expression_inner(l, i) }
}

#[memoize]
fn expression_inner(l: usize, i: Slice) -> ParseResult<AST<Expression>> {
    let mut tl = 0;

    // parse_level_expression!(level, this_level, i, ); // debug, javascriptEscape, elementTag
    parse_level_expression!(
        l,
        tl,
        i,
        alt((
            map(func, AST::recast::<Expression>),
            // map(proc, AST::recast::<Expression>)
        ))
    );
    parse_level_expression!(l, tl, i, binary_operation_1(tl, "??"));
    parse_level_expression!(l, tl, i, binary_operation_1(tl, "||"));
    parse_level_expression!(l, tl, i, binary_operation_1(tl, "&&"));
    parse_level_expression!(l, tl, i, binary_operation_2(tl, "==", "!="));
    parse_level_expression!(l, tl, i, binary_operation_4(tl, "<=", ">=", "<", ">"));
    parse_level_expression!(l, tl, i, binary_operation_2(tl, "+", "-"));
    parse_level_expression!(l, tl, i, binary_operation_2(tl, "*", "/"));
    parse_level_expression!(
        l,
        tl,
        i,
        alt((
            map(as_cast(tl), AST::recast::<Expression>),
            map(instance_of(tl), AST::recast::<Expression>)
        ))
    );

    parse_level_expression!(
        l,
        tl,
        i,
        map(seq!(tag("await"), expression(tl)), |(keyword, mut expr)| {
            make_node_tuple!(AwaitExpression, keyword.spanning(&expr), expr).recast::<Expression>()
        })
    );

    parse_level_expression!(l, tl, i, negation_operation(tl));

    // indexer

    parse_level_expression!(l, tl, i, error_expression);

    parse_level_expression!(l, tl, i, invocation_accessor_chain(tl));

    parse_level_expression!(l, tl, i, range_expression);

    parse_level_expression!(l, tl, i, parenthesis(tl));

    parse_level_expression!(
        l,
        tl,
        i,
        alt((
            map(if_else_expression, AST::recast::<Expression>),
            map(switch_expression, AST::recast::<Expression>),
            map(inline_const_group, AST::recast::<Expression>),
            map(object_literal, AST::recast::<Expression>),
            map(array_literal, AST::recast::<Expression>),
            map(exact_string_literal, AST::recast::<Expression>),
            map(string_literal, AST::recast::<Expression>),
            map(number_literal, AST::recast::<Expression>),
            map(boolean_literal, AST::recast::<Expression>),
            map(nil_literal, AST::recast::<Expression>),
        ),)
    );
    parse_level_expression!(
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

fn invocation_accessor_chain(level: usize) -> impl Fn(Slice) -> ParseResult<AST<Expression>> {
    move |i: Slice| -> ParseResult<AST<Expression>> { invocation_accessor_chain_inner(level, i) }
}

#[memoize]
fn invocation_accessor_chain_inner(level: usize, i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            await_or_detach,
            expression(level + 1),
            many1(alt((
                invocation_args,
                indexer_expression,
                dot_property_access,
            )))
        ),
        |(mut awaited_or_detached, base, clauses)| {
            let mut next_subject = base;

            for clause in clauses {
                let mut subject = next_subject;

                match clause {
                    InvocationOrPropertyAccess::InvokingWith(mut args, args_slice) => {
                        let mut spread_args = None; // TODO
                        let mut type_args = Vec::new(); // TODO
                        let mut bubbles = false; // TODO

                        next_subject = make_node!(
                            Invocation,
                            subject.spanning(&args_slice),
                            subject,
                            args,
                            spread_args,
                            type_args,
                            bubbles,
                            awaited_or_detached
                        )
                        .recast::<Expression>();
                    }
                    InvocationOrPropertyAccess::Accessing(mut property, indexer_slice) => {
                        let mut optional = false;

                        next_subject = make_node!(
                            PropertyAccessor,
                            subject.spanning(&indexer_slice),
                            subject,
                            property,
                            optional,
                        )
                        .recast::<Expression>();
                    }
                }
            }

            next_subject
        },
    )(i)
}

#[memoize]
fn await_or_detach(i: Slice) -> ParseResult<Option<AwaitOrDetach>> {
    map(opt(alt((tag("await"), tag("detach")))), |keyword| {
        keyword.map(|s: Slice| match s.as_str() {
            "await" => AwaitOrDetach::Await,
            "detach" => AwaitOrDetach::Detach,
            _ => unreachable!(),
        })
    })(i)
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
            InvocationOrPropertyAccess::Accessing(
                Property::Expression(property),
                open.spanning(&close),
            )
        },
    )(i)
}

#[memoize]
fn dot_property_access(i: Slice) -> ParseResult<InvocationOrPropertyAccess> {
    map(seq!(tag("."), plain_identifier), |(dot, property)| {
        let src = dot.spanning(&property);

        InvocationOrPropertyAccess::Accessing(Property::PlainIdentifier(property), src)
    })(i)
}

#[derive(Debug, Clone)]
enum InvocationOrPropertyAccess {
    InvokingWith(Vec<AST<Expression>>, Slice),
    Accessing(Property, Slice),
}

macro_rules! parse_binary_operation {
    ($level:expr, $operator:expr) => {
        move |i: Slice| -> ParseResult<AST<Expression>> {
            map(
                tuple((
                    expression($level + 1),
                    many1(pair(w($operator), w(expression($level + 1)))),
                )),
                |(base, clauses)| {
                    let mut next_left: AST<Expression> = base;

                    for (op, mut right) in clauses {
                        let mut left = next_left.clone();
                        let mut op =
                            BinaryOperator(BinaryOperatorOp::from_str(op.as_str()).unwrap())
                                .as_ast(op);

                        next_left = make_node!(
                            BinaryOperation,
                            left.slice().clone().spanning(right.slice()),
                            left,
                            op,
                            right
                        )
                        .recast::<Expression>();
                    }

                    next_left
                },
            )(i.clone())
        }
    };
}

fn binary_operation_1(
    level: usize,
    a: &'static str,
) -> impl Fn(Slice) -> ParseResult<AST<Expression>> {
    parse_binary_operation!(level, tag(a))
}

fn binary_operation_2(
    level: usize,
    a: &'static str,
    b: &'static str,
) -> impl Fn(Slice) -> ParseResult<AST<Expression>> {
    parse_binary_operation!(level, alt((tag(a), tag(b))))
}

fn binary_operation_4(
    level: usize,
    a: &'static str,
    b: &'static str,
    c: &'static str,
    d: &'static str,
) -> impl Fn(Slice) -> ParseResult<AST<Expression>> {
    parse_binary_operation!(level, alt((tag(a), tag(b), tag(c), tag(d))))
}

fn regular_expression(i: Slice) -> ParseResult<AST<RegularExpression>> {
    todo!()
}

fn error_expression(i: Slice) -> ParseResult<AST<ErrorExpression>> {
    map(
        seq!(tag("Error"), tag("("), expression(0), tag(")")),
        |(start, _, mut inner, end)| make_node_tuple!(ErrorExpression, start.spanning(&end), inner,),
    )(i)
}

fn instance_of(level: usize) -> impl Fn(Slice) -> ParseResult<AST<InstanceOf>> {
    move |i: Slice| -> ParseResult<AST<InstanceOf>> {
        map(
            seq!(expression(level + 1), tag("instanceof"), type_expression(0)),
            |(mut inner, _, mut possible_type)| {
                make_node!(
                    InstanceOf,
                    inner.spanning(&possible_type),
                    inner,
                    possible_type
                )
            },
        )(i)
    }
}

fn as_cast(level: usize) -> impl Fn(Slice) -> ParseResult<AST<AsCast>> {
    move |i: Slice| -> ParseResult<AST<AsCast>> {
        map(
            seq!(expression(level + 1), tag("as"), type_expression(0)),
            |(mut inner, _, mut as_type)| {
                make_node!(AsCast, inner.spanning(&as_type), inner, as_type)
            },
        )(i)
    }
}

#[memoize]
fn element_tag(i: Slice) -> ParseResult<AST<ElementTag>> {
    todo!()
}

#[memoize]
fn if_else_expression(i: Slice) -> ParseResult<AST<IfElseExpression>> {
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
            make_node!(
                IfElseExpressionCase,
                start.spanning(&end),
                condition,
                outcome
            )
        },
    )(i)
}

#[memoize]
fn switch_expression(i: Slice) -> ParseResult<AST<SwitchExpression>> {
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
                        make_node!(
                            SwitchExpressionCase,
                            keyword.spanning(&outcome),
                            type_filter,
                            outcome
                        )
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
fn range_expression(i: Slice) -> ParseResult<AST<RangeExpression>> {
    map(
        seq!(number_literal, tag(".."), number_literal),
        |(mut start, _, mut end)| {
            let mut start = start.recast::<Expression>();
            let mut end = end.recast::<Expression>();

            make_node!(RangeExpression, start.spanning(&end), start, end)
        },
    )(i)
}

#[memoize]
fn javascript_escape_expression(i: Slice) -> ParseResult<AST<JavascriptEscape>> {
    todo!()
}

#[memoize]
fn proc(i: Slice) -> ParseResult<AST<Proc>> {
    map(
        seq!(
            opt(tag("pure")),
            opt(tag("async")),
            alt((args_parenthesized, arg_singleton)),
            opt(map(
                seq!(tag("throws"), type_expression(0)),
                |(_, throws)| throws
            )),
            block,
        ),
        |(pure, asyn, mut args, mut throws, mut body)| {
            let mut is_async = asyn.is_some();
            let mut is_pure = pure.is_some();
            let src = pure
                .unwrap_or(asyn.unwrap_or_else(|| {
                    args.get(0)
                        .map(|arg| arg.downcast().name.slice().clone())
                        .unwrap_or(body.slice().clone())
                }))
                .spanning(&body);

            let mut args_spread = None; // TODO
            let mut type_annotation = make_node!(
                ProcType,
                src.clone(),
                args,
                args_spread,
                is_pure,
                is_async,
                throws
            );

            make_node!(Proc, src, type_annotation, is_async, is_pure, body)
        },
    )(i)
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
            let mut is_async = asyn.is_some();
            let mut is_pure = pure.is_some();
            let src = pure
                .unwrap_or(asyn.unwrap_or_else(|| {
                    args.get(0)
                        .map(|arg| arg.downcast().name.slice().clone())
                        .unwrap_or(body.slice().clone())
                }))
                .spanning(&body);

            let mut args_spread = None; // TODO
            let mut type_annotation = make_node!(
                FuncType,
                src.clone(),
                args,
                args_spread,
                is_pure,
                is_async,
                returns
            );

            make_node!(Func, src, type_annotation, is_async, is_pure, body)
        },
    )(i)
}

#[memoize]
fn inline_const_group(i: Slice) -> ParseResult<AST<InlineConstGroup>> {
    map(
        seq!(many1(inline_declaration), expression(0)),
        |(mut declarations, mut inner)| {
            make_node!(
                InlineConstGroup,
                declarations[0].spanning(&inner),
                declarations,
                inner
            )
        },
    )(i)
}

#[memoize]
fn inline_declaration(i: Slice) -> ParseResult<AST<InlineDeclaration>> {
    map(
        seq!(
            tag("const"),
            declaration_destination,
            tag("="),
            expression(0),
            tag(",")
        ),
        |(start, mut destination, _, mut value, end)| {
            make_node!(InlineDeclaration, start.spanning(&end), destination, value)
        },
    )(i)
}

#[memoize]
fn declaration_destination(i: Slice) -> ParseResult<DeclarationDestination> {
    // TODO: Destructuring
    map(
        seq!(
            plain_identifier,
            opt(map(
                seq!(tag(":"), type_expression(0)),
                |(_, type_annotation)| type_annotation
            ))
        ),
        |(name, type_annotation)| {
            DeclarationDestination::NameAndType(NameAndType {
                name,
                type_annotation,
            })
        },
    )(i)
}

fn negation_operation(level: usize) -> impl Fn(Slice) -> ParseResult<AST<NegationOperation>> {
    move |i: Slice| -> ParseResult<AST<NegationOperation>> {
        map(
            seq!(tag("!"), expression(level + 1)),
            |(start, mut expr)| make_node_tuple!(NegationOperation, start.spanning(&expr), expr),
        )(i)
    }
}

fn parenthesis(level: usize) -> impl Fn(Slice) -> ParseResult<AST<Parenthesis>> {
    move |i: Slice| -> ParseResult<AST<Parenthesis>> {
        map(
            seq!(tag("("), expression(level + 1), tag(")")),
            |(open_paren, mut inner, close_paren)| {
                make_node_tuple!(Parenthesis, open_paren.spanning(&close_paren), inner)
            },
        )(i)
    }
}

#[memoize]
fn object_literal(i: Slice) -> ParseResult<AST<ObjectLiteral>> {
    map(
        seq!(
            tag("{"),
            separated_list0(
                w(char(',')),
                w(alt((
                    map(
                        preceded(tag("..."), expression(0)),
                        KeyValueOrSpread::Spread
                    ),
                    key_value_expression,
                )))
            ),
            tag("}"),
        ),
        |(open_bracket, mut entries, close_bracket)| {
            make_node_tuple!(
                ObjectLiteral,
                open_bracket.spanning(&close_bracket),
                entries
            )
        },
    )(i)
}

fn key_value_expression(i: Slice) -> ParseResult<KeyValueOrSpread<AST<Expression>>> {
    map(
        seq!(plain_identifier, tag(":"), expression(0)),
        |(key, _, value)| {
            KeyValueOrSpread::KeyValue(identifier_to_string(key).recast::<Expression>(), value)
        },
    )(i)
}

#[memoize]
fn array_literal(i: Slice) -> ParseResult<AST<ArrayLiteral>> {
    map(
        seq!(
            tag("["),
            separated_list0(
                w(char(',')),
                w(alt((
                    map(preceded(tag("..."), expression(0)), ElementOrSpread::Spread),
                    map(expression(0), ElementOrSpread::Element),
                )))
            ),
            tag("]"),
        ),
        |(open_bracket, mut entries, close_bracket)| {
            make_node_tuple!(ArrayLiteral, open_bracket.spanning(&close_bracket), entries)
        },
    )(i)
}

#[memoize]
fn string_literal(i: Slice) -> ParseResult<AST<StringLiteral>> {
    map(
        pair(
            opt(plain_identifier),
            seq!(tag("\'"), many0(string_literal_segment), tag("\'")),
        ),
        |(mut tag, (open_quote, mut segments, close_quote))| {
            make_node!(
                StringLiteral,
                tag.as_ref()
                    .map(|tag| tag.slice())
                    .unwrap_or(&open_quote)
                    .spanning(&close_quote),
                tag,
                segments
            )
        },
    )(i)
}

#[memoize]
fn exact_string_literal(i: Slice) -> ParseResult<AST<ExactStringLiteral>> {
    map(
        pair(
            opt(plain_identifier),
            seq!(tag("\'"), string_contents, tag("\'")),
        ),
        |(mut tag, (open_quote, mut value, close_quote))| {
            make_node!(
                ExactStringLiteral,
                tag.as_ref()
                    .map(|tag| tag.slice())
                    .unwrap_or(&open_quote)
                    .spanning(&close_quote),
                tag,
                value
            )
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
fn number_literal(i: Slice) -> ParseResult<AST<NumberLiteral>> {
    map(
        seq!(opt(tag("-")), numeric, opt(seq!(tag("."), numeric))),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int.clone());
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            NumberLiteral(full.clone()).as_ast(full)
        },
    )(i)
}

#[memoize]
fn boolean_literal(input: Slice) -> ParseResult<AST<BooleanLiteral>> {
    alt((
        map(tag("true"), |src: Slice| BooleanLiteral(true).as_ast(src)),
        map(tag("false"), |src: Slice| BooleanLiteral(false).as_ast(src)),
    ))(input)
}

#[memoize]
fn nil_literal(input: Slice) -> ParseResult<AST<NilLiteral>> {
    map(tag("nil"), |src: Slice| NilLiteral.as_ast(src))(input)
}

#[memoize]
fn local_identifier(i: Slice) -> ParseResult<AST<LocalIdentifier>> {
    map(identifier_like, |name| {
        LocalIdentifier(name.clone()).as_ast(name)
    })(i)
}

fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    map(identifier_like, |name| {
        PlainIdentifier(name.clone()).as_ast(name)
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
    map(
        tuple((
            take_while1(|ch: char| ch.is_alphabetic() || ch == '_' || ch == '$'),
            take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$'),
        )),
        |(a, b): (Slice, Slice)| a.spanning(&b),
    )(i)
}

fn numeric(i: Slice) -> ParseResult<Slice> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace(i: Slice) -> ParseResult<Slice> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i) // TODO: comments
}

fn separated_list2<TKind, O2, F, G>(
    sep: G,
    f: F,
) -> impl FnMut(Slice) -> IResult<Slice, Vec<AST<TKind>>, RawParseError>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
    F: Parser<Slice, AST<TKind>, RawParseError>,
    G: Parser<Slice, O2, RawParseError>,
{
    let mut parser = separated_list1(sep, f);

    move |i: Slice| -> IResult<Slice, Vec<AST<TKind>>, RawParseError> {
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
