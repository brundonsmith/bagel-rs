#[allow(unused_imports)]
use crate::utils::Loggable;
use crate::{
    model::{ast::*, ModuleID, ParseError, Slice},
    utils::cli_label,
    DEBUG_MODE,
};
use colored::Color;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{char, one_of},
    combinator::{complete, map, opt, verify},
    error::ErrorKind,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{pair, preceded, terminated, tuple},
    IResult, Parser,
};
use std::{rc::Rc, str::FromStr, time::SystemTime};
use swc_common::util::take::Take;

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
    let res = module(bgl.clone(), module_id.clone());

    if DEBUG_MODE {
        println!(
            "{} Parsing {} took {}ms",
            cli_label("Debug", Color::Blue),
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
        Err(error) => {
            let RawParseError { src, details } = match error {
                nom::Err::Error(raw) => raw,
                nom::Err::Failure(raw) => raw,
                nom::Err::Incomplete(_) => unreachable!(),
            };

            Err(ParseError {
                module_id: module_id.clone(),
                src,
                message: match details {
                    RawParseErrorDetails::Expected(description) => {
                        format!("Expected {}", description)
                    }
                    RawParseErrorDetails::Kind(kind) => kind.description().to_owned(),
                    RawParseErrorDetails::Char(ch) => format!("Expected '{}'", ch),
                },
            })
        }
    }
}

fn module(i: Slice, module_id: ModuleID) -> ParseResult<AST<Module>> {
    let i_clone = i.clone();
    map(
        complete(terminated(many0(w(declaration)), whitespace)),
        move |mut declarations| {
            let module = Module {
                module_id: module_id.clone(),
                declarations: declarations.clone(),
            }
            .as_ast(covering(&declarations).unwrap_or(i_clone.clone().slice_range(0, Some(0))));

            declarations.set_parent(&module);

            module
        },
    )(i.clone())
}

// --- Declaration

fn declaration(i: Slice) -> ParseResult<AST<Declaration>> {
    alt((
        map(import_all_declaration, AST::recast::<Declaration>),
        map(import_declaration, AST::recast::<Declaration>),
        map(type_declaration, AST::recast::<Declaration>),
        map(func_declaration, AST::recast::<Declaration>),
        map(proc_declaration, AST::recast::<Declaration>),
        map(value_declaration, AST::recast::<Declaration>),
        map(symbol_declaration, AST::recast::<Declaration>),
        map(test_expr_declaration, AST::recast::<Declaration>),
        map(test_block_declaration, AST::recast::<Declaration>),
        // test_type_declaration,
    ))(i)
}

fn import_all_declaration(i: Slice) -> ParseResult<AST<ImportAllDeclaration>> {
    map(
        seq!(
            opt(declaration_platforms),
            tag("import"),
            expect(exact_string_literal, "import path"),
            expect_tag("as"),
            expect(plain_identifier, "import name"),
        ),
        |(mut platforms, start, mut path, _, mut name)| {
            make_node!(
                ImportAllDeclaration,
                start.spanning(&name),
                platforms,
                path,
                name
            )
        },
    )(i)
}

fn import_declaration(i: Slice) -> ParseResult<AST<ImportDeclaration>> {
    map(
        seq!(
            opt(declaration_platforms),
            tag("from"),
            expect(exact_string_literal, "import path"),
            expect_tag("import"),
            expect_tag("{"),
            terminated(
                separated_list0(w(tag(",")), w(import_item)),
                opt(w(tag(",")))
            ),
            expect_tag("}"),
        ),
        |(mut platforms, start, mut path, _, _, mut imports, end)| {
            make_node!(
                ImportDeclaration,
                start.spanning(&end),
                platforms,
                path,
                imports
            )
        },
    )(i)
}

fn import_item(i: Slice) -> ParseResult<AST<ImportItem>> {
    map(
        pair(
            plain_identifier,
            opt(map(
                seq!(tag("as"), expect(plain_identifier, "import alias")),
                |(_, alias)| alias,
            )),
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

fn type_declaration(i: Slice) -> ParseResult<AST<TypeDeclaration>> {
    map(
        seq!(
            opt(tag("export")),
            tag("type"),
            expect(plain_identifier, "type name"),
            expect_tag("="),
            expect(type_expression(None), "type definition"),
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

fn func_declaration(i: Slice) -> ParseResult<AST<FuncDeclaration>> {
    map(
        seq!(
            opt(declaration_platforms),
            many0(terminated(decorator, whitespace_required)),
            opt(terminated(tag("export"), whitespace_required)),
            opt(terminated(tag("async"), whitespace_required)),
            terminated(tag("func"), whitespace_required),
            expect(plain_identifier, "function name"),
            type_params,
            expect(args_parenthesized, "function parameters"),
            opt(type_annotation),
            expect_tag("=>"),
            expect(expression(None), "function body"),
        ),
        |(
            mut platforms,
            mut decorators,
            export,
            asyn,
            keyword,
            mut name,
            mut type_params,
            (mut args, mut args_spread),
            mut returns,
            arrow,
            mut body,
        )| {
            let src = export
                .as_ref()
                .unwrap_or(asyn.as_ref().unwrap_or(&keyword))
                .clone()
                .spanning(&body);
            let mut exported = export.is_some();
            let mut is_async = asyn.is_some();

            let mut type_annotation = make_node!(
                FuncType,
                args.get(0)
                    .map(|arg| arg.downcast().name.slice().clone())
                    .unwrap_or(arrow)
                    .spanning(&body),
                type_params,
                args,
                args_spread,
                returns
            );

            let mut func = make_node!(Func, src.clone(), type_annotation, is_async, body);

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

fn arg_singleton(i: Slice) -> ParseResult<(Vec<AST<Arg>>, Option<AST<Arg>>)> {
    map(plain_identifier, |mut name| {
        let mut type_annotation = None;
        let mut optional = false;

        (
            vec![make_node!(
                Arg,
                name.slice().clone(),
                name,
                type_annotation,
                optional
            )],
            None,
        )
    })(i)
}

fn args_parenthesized(i: Slice) -> ParseResult<(Vec<AST<Arg>>, Option<AST<Arg>>)> {
    map(
        seq!(
            tag("("),
            separated_list0(
                w(tag(",")),
                w(seq!(
                    plain_identifier,
                    opt(map(
                        tuple((opt(tag("?")), type_annotation)),
                        |(question, type_annotation)| (question.is_some(), type_annotation)
                    ))
                ))
            ),
            opt(map(
                seq!(
                    tag("..."),
                    expect(plain_identifier, "spread parameters name"),
                    opt(type_annotation)
                ),
                |(_, mut name, mut type_annotation)| {
                    let mut optional = false;

                    make_node!(
                        Arg,
                        name.spanning(
                            type_annotation
                                .as_ref()
                                .map(|a| a.slice())
                                .unwrap_or(name.slice()),
                        ),
                        name,
                        type_annotation,
                        optional
                    )
                }
            )),
            expect_tag(")"),
        ),
        |(start, args, spread, end)| {
            (
                args.into_iter()
                    .map(|(mut name, annotation)| {
                        let (mut optional, mut type_annotation) = annotation
                            .map(|(optional, type_annotation)| (optional, Some(type_annotation)))
                            .unwrap_or((false, None));

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
                    .collect(),
                spread,
            )
        },
    )(i)
}

fn proc_declaration(i: Slice) -> ParseResult<AST<ProcDeclaration>> {
    map(
        seq!(
            opt(declaration_platforms),
            many0(terminated(decorator, whitespace_required)),
            opt(terminated(tag("export"), whitespace_required)),
            opt(terminated(tag("async"), whitespace_required)),
            terminated(tag("proc"), whitespace_required),
            expect(plain_identifier, "proc name"),
            type_params,
            expect(args_parenthesized, "proc parameters"),
            opt(throws_clause),
            expect_tag("|>"),
            expect(statement, "proc body"),
        ),
        |(
            mut platforms,
            mut decorators,
            export,
            asyn,
            keyword,
            mut name,
            mut type_params,
            (mut args, mut args_spread),
            mut throws,
            _,
            mut body,
        )| {
            let src = export
                .clone()
                .unwrap_or(asyn.clone().unwrap_or(keyword))
                .spanning(&body);
            let mut exported = export.is_some();
            let mut is_async = asyn.is_some();

            let mut type_annotation = make_node!(
                ProcType,
                args.get(0)
                    .map(|arg| arg.downcast().name.slice().clone())
                    .unwrap_or(body.slice().clone())
                    .spanning(&body),
                type_params,
                args,
                args_spread,
                is_async,
                throws
            );

            let mut proc = make_node!(Proc, src.clone(), type_annotation, is_async, body);

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

fn type_params(i: Slice) -> ParseResult<Vec<AST<TypeParam>>> {
    map(
        opt(seq!(
            tag("<"),
            separated_list1(w(tag(",")), w(type_param)),
            expect_tag(">")
        )),
        |res| res.map(|(_, params, _)| params).unwrap_or(Vec::new()),
    )(i)
}

fn type_param(i: Slice) -> ParseResult<AST<TypeParam>> {
    map(
        tuple((
            plain_identifier,
            opt(tuple((
                whitespace_required,
                tag("extends"),
                whitespace_required,
                expect(type_expression(None), "type"),
            ))),
        )),
        |(mut name, extends)| {
            let mut extends = extends.map(|(_, _, _, extends)| extends);

            make_node!(
                TypeParam,
                name.spanning(extends.as_ref().map(|e| e.slice()).unwrap_or(name.slice())),
                name,
                extends
            )
        },
    )(i)
}

fn decorator(i: Slice) -> ParseResult<AST<Decorator>> {
    map(
        tuple((
            tag("@"),
            plain_identifier,
            opt(seq!(
                tag("("),
                separated_list0(w(tag(",")), w(expression(None))),
                expect_tag(")")
            )),
        )),
        |(start, mut name, arguments)| {
            let arguments_end = arguments.as_ref().map(|a| a.2.clone());
            let mut arguments = arguments.map(|(_, arguments, _)| arguments);

            make_node!(
                Decorator,
                start.spanning(arguments_end.as_ref().unwrap_or(name.slice())),
                name,
                arguments
            )
        },
    )(i)
}

fn block(i: Slice) -> ParseResult<AST<Block>> {
    map(
        seq!(tag("{"), many0(statement), expect_tag("}")),
        |(open, mut statements, close)| make_node_tuple!(Block, open.spanning(&close), statements),
    )(i)
}

fn value_declaration(i: Slice) -> ParseResult<AST<ValueDeclaration>> {
    map(
        seq!(
            opt(declaration_platforms),
            opt(terminated(tag("export"), whitespace_required)),
            terminated(alt((tag("const"), tag("let"))), whitespace_required),
            expect(declaration_destination, "name or destructure"),
            expect_tag("="),
            expect(expression(None), "value"),
        ),
        |(mut platforms, export, keyword, mut destination, _, mut value)| {
            let mut exported = export.is_some();
            let mut is_const = keyword == "const";
            let src = export.unwrap_or(keyword).spanning(&value);

            make_node!(
                ValueDeclaration,
                src,
                platforms,
                destination,
                value,
                is_const,
                exported
            )
        },
    )(i)
}

fn symbol_declaration(i: Slice) -> ParseResult<AST<SymbolDeclaration>> {
    map(
        seq!(
            opt(terminated(tag("export"), whitespace_required)),
            terminated(tag("symbol"), whitespace_required),
            expect(plain_identifier, "symbol name")
        ),
        |(export, keyword, mut name)| {
            let mut exported = export.is_some();
            let src = export.unwrap_or(keyword).spanning(&name);

            make_node!(SymbolDeclaration, src, name, exported)
        },
    )(i)
}

fn test_expr_declaration(i: Slice) -> ParseResult<AST<TestExprDeclaration>> {
    map(
        seq!(
            opt(declaration_platforms),
            break_after(tag("test")),
            tag("expr"),
            expect(exact_string_literal, "test description string"),
            expect_tag("=>"),
            expect(expression(None), "test expression"),
        ),
        |(mut platforms, start, _, mut name, _, mut expr)| {
            make_node!(
                TestExprDeclaration,
                start.spanning(&expr),
                platforms,
                name,
                expr
            )
        },
    )(i)
}

fn test_block_declaration(i: Slice) -> ParseResult<AST<TestBlockDeclaration>> {
    map(
        seq!(
            opt(declaration_platforms),
            break_after(tag("test")),
            tag("block"),
            expect(exact_string_literal, "test description string"),
            expect(block, "test block")
        ),
        |(mut platforms, start, _, mut name, mut block)| {
            make_node!(
                TestBlockDeclaration,
                start.spanning(&block),
                platforms,
                name,
                block
            )
        },
    )(i)
}

fn test_type_declaration(i: Slice) -> ParseResult<AST<TestTypeDeclaration>> {
    todo!()
}

fn declaration_platforms(i: Slice) -> ParseResult<AST<DeclarationPlatforms>> {
    map(
        seq!(
            tag("["),
            separated_list0(w(tag(",")), w(plain_identifier)),
            expect_tag("]"),
        ),
        |(start, mut platforms, end)| {
            make_node!(DeclarationPlatforms, start.spanning(&end), platforms)
        },
    )(i)
}

fn type_annotation(i: Slice) -> ParseResult<AST<TypeExpression>> {
    preceded(tag(":"), w(expect(type_expression(None), "type")))(i)
}

// --- TypeExpression ---

fn type_expression(
    after: Option<fn(Slice) -> ParseResult<AST<TypeExpression>>>,
) -> impl Fn(Slice) -> ParseResult<AST<TypeExpression>> {
    move |i: Slice| {
        precedence(
            &[
                &[typeof_type],
                &[modifier_type],
                &[union_type],
                &[maybe_type],
                &[array_type],
                &[bound_generic_type],
                &[
                    func_type,
                    proc_type,
                    record_type,
                    object_or_interface_type,
                    tuple_type,
                    parenthesized_type,
                    string_literal_type,
                    number_literal_type,
                ],
                &[named_type],
            ],
            after,
            i,
        )
    }
}

fn typeof_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            break_after(tag("typeof")),
            expect(expression(None), "expression")
        ),
        |(keyword, mut expr)| {
            make_node_tuple!(TypeofType, keyword.spanning(&expr), expr).recast::<TypeExpression>()
        },
    )(i)
}

fn modifier_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            break_after(alt((
                tag(ModifierTypeKind::Keyof.into()),
                tag(ModifierTypeKind::Valueof.into()),
                tag(ModifierTypeKind::Elementof.into()),
                tag(ModifierTypeKind::Readonly.into())
            ))),
            expect(type_expression(None), "type")
        ),
        |(keyword, mut inner)| {
            let mut kind: ModifierTypeKind = keyword.as_str().try_into().unwrap();
            let src = keyword.spanning(&inner);

            make_node!(ModifierType, src, kind, inner).recast::<TypeExpression>()
        },
    )(i)
}

fn union_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        preceded(
            opt(w(tag("|"))),
            separated_list1(w(tag("|")), w(type_expression(Some(union_type)))),
        ),
        |mut members| match members.len() {
            1 => members.pop().unwrap(),
            _ => make_node_tuple!(UnionType, covering(&members).unwrap(), members)
                .recast::<TypeExpression>(),
        },
    )(i)
}

fn maybe_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(type_expression(Some(maybe_type)), opt(tag("?"))),
        |(mut inner, question)| match question {
            Some(question) => make_node_tuple!(MaybeType, inner.spanning(&question), inner)
                .recast::<TypeExpression>(),
            None => inner,
        },
    )(i)
}

fn array_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(type_expression(Some(array_type)), opt(tag("[]"))),
        |(mut element, brackets)| match brackets {
            Some(brackets) => make_node_tuple!(ArrayType, element.spanning(&brackets), element)
                .recast::<TypeExpression>(),
            None => element,
        },
    )(i)
}

fn bound_generic_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            type_expression(Some(bound_generic_type)),
            opt(seq!(
                tag("<"),
                separated_list1(w(tag(",")), w(type_expression(None))),
                expect_tag(">"),
            ))
        ),
        |(generic, bindings)| match bindings {
            Some((_, mut type_args, end)) => {
                let mut generic = generic.recast::<TypeExpression>();
                let src = generic.spanning(&end);

                if let Some(name) = generic.try_downcast::<NamedType>() {
                    if type_args.len() == 1 {
                        let kind = match name.0.downcast().0.as_str() {
                            "Plan" => Some(SpecialTypeKind::Plan),
                            "Iterable" => Some(SpecialTypeKind::Iterable),
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
            None => generic,
        },
    )(i)
}

fn func_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            type_params,
            args_parenthesized,
            tag("=>"),
            expect(type_expression(None), "return type")
        ),
        |(mut type_params, (mut args, mut args_spread), _, returns)| {
            // TODO: func type with 0 arguments will have weird src
            let src = args
                .get(0)
                .map(|a| a.slice().clone())
                .unwrap_or(returns.slice().clone())
                .spanning(&returns);

            let mut returns = Some(returns);

            make_node!(FuncType, src, type_params, args, args_spread, returns)
                .recast::<TypeExpression>()
        },
    )(i)
}

fn proc_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            opt(terminated(tag("async"), whitespace_required)),
            type_params,
            args_parenthesized,
            opt(throws_clause),
            tag("|>"),
            expect_tag("{"),
            expect_tag("}")
        ),
        |(asyn, mut type_params, (mut args, mut args_spread), mut throws, arrow, _, end)| {
            // TODO: proc type with 0 arguments will have weird src
            let src = args
                .get(0)
                .map(|a| a.slice().clone())
                .unwrap_or(arrow)
                .spanning(&end);

            let mut is_async = asyn.is_some();

            make_node!(
                ProcType,
                src,
                type_params,
                args,
                args_spread,
                is_async,
                throws
            )
            .recast::<TypeExpression>()
        },
    )(i)
}

fn throws_clause(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            break_after(tag("throws")),
            expect(type_expression(None), "type thrown")
        ),
        |(_, throws)| throws,
    )(i)
}

fn record_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            tag("{"),
            tag("["),
            expect(type_expression(None), "key type"),
            expect_tag("]"),
            expect_tag(":"),
            expect(type_expression(None), "value type"),
            expect_tag("}"),
        ),
        |(open, _, mut key_type, _, _, mut value_type, close)| {
            make_node!(RecordType, open.spanning(&close), key_type, value_type)
                .recast::<TypeExpression>()
        },
    )(i)
}

fn object_or_interface_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            opt(tag("interface")),
            tag("{"),
            terminated(
                separated_list0(
                    w(tag(",")),
                    w(alt((
                        map(
                            preceded(tag("..."), expect(type_expression(None), "spread type")),
                            KeyValueOrSpread::Spread
                        ),
                        key_value_type,
                    )))
                ),
                opt(w(tag(","))),
            ),
            expect_tag("}"),
        ),
        |(interface, open, mut entries, close)| {
            let mut is_interface = interface.is_some();

            make_node!(
                ObjectType,
                interface.unwrap_or(open).spanning(&close),
                entries,
                is_interface
            )
            .recast::<TypeExpression>()
        },
    )(i)
}

fn key_value_type(i: Slice) -> ParseResult<KeyValueOrSpread<AST<TypeExpression>>> {
    map(
        seq!(
            plain_identifier,
            opt(tag("?")),
            tag(":"),
            expect(type_expression(None), "value type")
        ),
        |(key, optional, _, value)| {
            KeyValueOrSpread::KeyValue(
                identifier_to_string_type(key).recast::<TypeExpression>(),
                value,
                optional.is_some(),
            )
        },
    )(i)
}

fn tuple_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            tag("["),
            terminated(
                separated_list0(
                    w(tag(",")),
                    w(alt((
                        map(
                            preceded(tag("..."), expect(type_expression(None), "spread type")),
                            ElementOrSpread::Spread
                        ),
                        map(type_expression(None), ElementOrSpread::Element),
                    )))
                ),
                opt(w(tag(",")))
            ),
            expect_tag("]"),
        ),
        |(open, mut members, close)| {
            make_node_tuple!(TupleType, open.spanning(&close), members).recast::<TypeExpression>()
        },
    )(i)
}

fn parenthesized_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        seq!(
            tag("("),
            expect(type_expression(None), "type"),
            expect_tag(")")
        ),
        |(open, mut inner, close)| {
            make_node_tuple!(ParenthesizedType, open.spanning(&close), inner)
                .recast::<TypeExpression>()
        },
    )(i)
}

fn string_literal_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(
        tuple((tag("\'"), string_contents, expect_tag("\'"))),
        |(open_quote, value, close_quote)| {
            StringLiteralType(value)
                .as_ast(open_quote.spanning(&close_quote))
                .recast::<TypeExpression>()
        },
    )(i)
}

fn number_literal_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(seq!(opt(tag("-")), numeric), |(neg, int)| {
        let src = neg.as_ref().unwrap_or(&int).spanning(&int);
        NumberLiteralType(src.clone())
            .as_ast(src)
            .recast::<TypeExpression>()
    })(i)
}

fn named_type(i: Slice) -> ParseResult<AST<TypeExpression>> {
    map(local_identifier, |mut ident| {
        let s = ident.slice().clone();

        match s.as_str() {
            "RegExp" => RegularExpressionType.as_ast(s).recast::<TypeExpression>(),
            "true" => BooleanLiteralType(true)
                .as_ast(s)
                .recast::<TypeExpression>(),
            "false" => BooleanLiteralType(false)
                .as_ast(s)
                .recast::<TypeExpression>(),
            "string" => StringType.as_ast(s).recast::<TypeExpression>(),
            "number" => NumberType.as_ast(s).recast::<TypeExpression>(),
            "boolean" => BooleanType.as_ast(s).recast::<TypeExpression>(),
            "unknown" => UnknownType.as_ast(s).recast::<TypeExpression>(),
            "nil" => NilType.as_ast(s).recast::<TypeExpression>(),
            _ => {
                make_node_tuple!(NamedType, ident.slice().clone(), ident).recast::<TypeExpression>()
            }
        }
    })(i)
}

// --- Statement ---

fn statement(i: Slice) -> ParseResult<AST<Statement>> {
    alt((
        map(
            terminated(value_declaration, w(tag(";"))),
            AST::recast::<Statement>,
        ),
        map(if_else_statement, AST::recast::<Statement>),
        map(for_loop, AST::recast::<Statement>),
        map(while_loop, AST::recast::<Statement>),
        map(assignment, AST::recast::<Statement>),
        map(try_catch, AST::recast::<Statement>),
        map(throw_statement, AST::recast::<Statement>),
        map(autorun, AST::recast::<Statement>),
        map(block, AST::recast::<Statement>),
        map(
            seq!(invocation_accessor_chain, expect_tag(";")),
            |(invocation, _)| {
                invocation
                    .try_recast::<Invocation>()
                    .unwrap()
                    .recast::<Statement>()
            },
        ),
    ))(i)
}

fn if_else_statement(i: Slice) -> ParseResult<AST<IfElseStatement>> {
    map(
        seq!(
            separated_list1(w(tag("else")), w(if_else_statement_case)),
            opt(map(
                w(seq!(tag("else"), expect(block, "else block"))),
                |(_, default_case)| { default_case },
            )),
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

fn if_else_statement_case(i: Slice) -> ParseResult<AST<IfElseStatementCase>> {
    map(
        seq!(
            tag("if"),
            expect(expression(None), "condition"),
            expect(block, "block")
        ),
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

fn for_loop(i: Slice) -> ParseResult<AST<ForLoop>> {
    map(
        seq!(
            tag("for"),
            expect(plain_identifier, "item name"),
            expect_tag("of"),
            expect(expression(None), "iterable"),
            expect(block, "loop block"),
        ),
        |(start, mut item_identifier, _, mut iterable, mut body)| {
            make_node!(
                ForLoop,
                start.spanning(&body),
                item_identifier,
                iterable,
                body
            )
        },
    )(i)
}

fn while_loop(i: Slice) -> ParseResult<AST<WhileLoop>> {
    map(
        seq!(
            tag("while"),
            expect(expression(None), "loop condition"),
            expect(block, "loop block")
        ),
        |(start, mut condition, mut body)| {
            make_node!(WhileLoop, start.spanning(&body), condition, body)
        },
    )(i)
}

fn assignment(i: Slice) -> ParseResult<AST<Assignment>> {
    map(
        seq!(
            alt((
                map(invocation_accessor_chain, AST::recast::<Expression>),
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
            expect(expression(None), "value for assignment"),
            expect_tag(";")
        ),
        |(mut target, (mut operator, _), mut value, end)| {
            make_node!(Assignment, target.spanning(&end), target, value, operator)
        },
    )(i)
}

fn try_catch(i: Slice) -> ParseResult<AST<TryCatch>> {
    map(
        seq!(
            tag("try"),
            expect(block, "try block"),
            expect_tag("catch"),
            expect(plain_identifier, "error name"),
            expect(block, "catch block"),
        ),
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

fn throw_statement(i: Slice) -> ParseResult<AST<ThrowStatement>> {
    map(
        seq!(
            tag("throw"),
            expect(expression(None), "value to be thrown"),
            expect_tag(";")
        ),
        |(start, mut error_expression, end)| {
            make_node!(ThrowStatement, start.spanning(&end), error_expression)
        },
    )(i)
}

fn autorun(i: Slice) -> ParseResult<AST<Autorun>> {
    map(
        seq!(
            tag("autorun"),
            expect(block, "autorun block"),
            expect(
                alt((
                    map(tag("forever"), |_| None),
                    map(
                        seq!(
                            tag("until"),
                            expect_tag("=>"),
                            expect(expression(None), "until-condition")
                        ),
                        |(_, _, until)| Some(until),
                    ),
                )),
                "autorun duration (either 'forever' or 'until => _')"
            ),
            expect_tag(";"),
        ),
        |(start, mut effect_block, mut until, end)| {
            make_node!(Autorun, start.spanning(&end), effect_block, until)
        },
    )(i)
}

// --- Expression ---

fn expression(
    after: Option<fn(Slice) -> ParseResult<AST<Expression>>>,
) -> impl Fn(Slice) -> ParseResult<AST<Expression>> {
    move |i: Slice| {
        precedence(
            &[
                &[element_tag],
                &[func, proc],
                &[binary_operation_1],
                &[binary_operation_2],
                &[binary_operation_3],
                &[binary_operation_4],
                &[binary_operation_5],
                &[binary_operation_6],
                &[binary_operation_7],
                &[as_cast, instance_of],
                // TODO: await expression
                &[negation_operation],
                &[error_expression],
                &[invocation_accessor_chain],
                &[range_expression],
                &[parenthesis],
                &[
                    if_else_expression,
                    switch_expression,
                    inline_const_group,
                    object_literal,
                    array_literal,
                    exact_string_literal_expr,
                    string_literal,
                    number_literal,
                    boolean_literal,
                    nil_literal,
                ],
                &[local_identifier_expr, regular_expression],
            ],
            after,
            i,
        )
    }
}

fn invocation_accessor_chain(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            await_or_detach,
            expression(Some(invocation_accessor_chain)),
            many0(alt((
                invocation_args,
                indexer_expression,
                w(dot_property_access),
            )))
        ),
        |(mut awaited_or_detached, base, clauses)| {
            let mut next_subject = base;

            for clause in clauses {
                let mut subject = next_subject;

                match clause {
                    InvocationOrPropertyAccess::InvokingWith(
                        mut type_args,
                        mut args,
                        args_slice,
                    ) => {
                        let mut spread_args = None; // TODO
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
                    InvocationOrPropertyAccess::Accessing {
                        mut property,
                        mut optional,
                        slice,
                    } => {
                        next_subject = make_node!(
                            PropertyAccessor,
                            subject.spanning(&slice),
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

fn await_or_detach(i: Slice) -> ParseResult<Option<AwaitOrDetach>> {
    map(opt(alt((tag("await"), tag("detach")))), |keyword| {
        keyword.map(|s: Slice| s.as_str().try_into().unwrap())
    })(i)
}

fn invocation_args(i: Slice) -> ParseResult<InvocationOrPropertyAccess> {
    map(
        seq!(
            opt(map(
                seq!(
                    tag("<"),
                    separated_list0(w(tag(",")), w(type_expression(None))),
                    expect_tag(">")
                ),
                |(_, type_args, _)| type_args
            )),
            tag("("),
            separated_list0(w(tag(",")), w(expression(None))),
            expect_tag(")")
        ),
        |(type_args, open, args, close)| {
            InvocationOrPropertyAccess::InvokingWith(
                type_args.unwrap_or(Vec::new()),
                args,
                open.spanning(&close),
            )
        },
    )(i)
}

fn indexer_expression(i: Slice) -> ParseResult<InvocationOrPropertyAccess> {
    map(
        tuple((
            opt(tag("?.")),
            tag("["),
            w(expect(expression(None), "index or property")),
            w(expect_tag("]")),
        )),
        |(question_dot, open, property, close)| InvocationOrPropertyAccess::Accessing {
            property: Property::Expression(property),
            optional: question_dot.is_some(),
            slice: question_dot
                .as_ref()
                .unwrap_or(&open)
                .clone()
                .spanning(&close),
        },
    )(i)
}

fn dot_property_access(i: Slice) -> ParseResult<InvocationOrPropertyAccess> {
    map(
        tuple((
            opt(tag("?")),
            tag("."),
            expect(plain_identifier, "property name"),
        )),
        |(question, dot, property)| {
            let src = question
                .as_ref()
                .unwrap_or(&dot)
                .clone()
                .spanning(&property);

            InvocationOrPropertyAccess::Accessing {
                property: Property::PlainIdentifier(property),
                optional: question.is_some(),
                slice: src,
            }
        },
    )(i)
}

#[derive(Debug, Clone)]
enum InvocationOrPropertyAccess {
    InvokingWith(Vec<AST<TypeExpression>>, Vec<AST<Expression>>, Slice),
    Accessing {
        property: Property,
        optional: bool,
        slice: Slice,
    },
}

macro_rules! binary_operation {
    ($name:ident, $( $operator:expr ),* $(,)?) => {
        fn $name(i: Slice) -> ParseResult<AST<Expression>> {
            map(
                tuple((
                    expression(Some($name)),
                    many0(pair(
                        w(alt(($(tag($operator),)*))),
                        w(expression(Some($name))),
                    )),
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
            )(i)
        }
    };
}

binary_operation!(binary_operation_1, "??");
binary_operation!(binary_operation_2, "||");
binary_operation!(binary_operation_3, "&&");
binary_operation!(binary_operation_4, "==", "!=");
binary_operation!(binary_operation_5, "<=", ">=", "<", ">");
binary_operation!(binary_operation_6, "+", "-");
binary_operation!(binary_operation_7, "*", "/");

fn regular_expression(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        tuple((
            tag("/"),
            escaped(
                take_while1(|ch: char| ch != '/' && ch != '\\'),
                '\\',
                one_of("/\\"),
            ),
            expect_tag("/"),
            many0(regular_expression_flag),
        )),
        |(start, mut expr, end, mut flags)| {
            make_node!(RegularExpression, start.spanning(&end), expr, flags).recast::<Expression>()
        },
    )(i)
}

fn regular_expression_flag(i: Slice) -> ParseResult<RegularExpressionFlag> {
    map(
        alt((
            tag("d"),
            tag("g"),
            tag("i"),
            tag("m"),
            tag("s"),
            tag("u"),
            tag("y"),
        )),
        |letter: Slice| letter.as_str().try_into().unwrap(),
    )(i)
}

fn error_expression(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            tag("Error"),
            tag("("),
            expect(expression(None), "error value"),
            expect_tag(")")
        ),
        |(start, _, mut inner, end)| {
            make_node_tuple!(ErrorExpression, start.spanning(&end), inner,).recast::<Expression>()
        },
    )(i)
}

fn instance_of(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            expression(Some(instance_of)),
            opt(seq!(
                tag("instanceof"),
                expect(type_expression(None), "type")
            ))
        ),
        |(mut inner, instanceof_clause)| match instanceof_clause {
            Some((_, mut possible_type)) => make_node!(
                InstanceOf,
                inner.spanning(&possible_type),
                inner,
                possible_type
            )
            .recast::<Expression>(),
            None => inner,
        },
    )(i)
}

fn as_cast(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            expression(Some(as_cast)),
            opt(seq!(tag("as"), expect(type_expression(None), "type")))
        ),
        |(mut inner, as_clause)| match as_clause {
            Some((_, mut as_type)) => {
                make_node!(AsCast, inner.spanning(&as_type), inner, as_type).recast::<Expression>()
            }
            None => inner,
        },
    )(i)
}

fn if_else_expression(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            separated_list1(w(tag("else")), w(if_else_expression_case)),
            opt(map(
                seq!(
                    tag("else"),
                    expect_tag("{"),
                    expect(expression(None), "expression"),
                    expect_tag("}")
                ),
                |(_, _, outcome, end)| (outcome, end),
            )),
        ),
        |(mut cases, default_case)| {
            let cases_src = covering(&cases).unwrap();

            let end = default_case
                .as_ref()
                .map(|(_, end)| end)
                .unwrap_or(&cases_src);
            let mut default_case = default_case.clone().map(|(default_case, _)| default_case);

            make_node!(
                IfElseExpression,
                cases_src.clone().join(&end),
                cases,
                default_case
            )
            .recast::<Expression>()
        },
    )(i)
}

fn if_else_expression_case(i: Slice) -> ParseResult<AST<IfElseExpressionCase>> {
    map(
        seq!(
            tag("if"),
            expect(expression(None), "condition"),
            expect_tag("{"),
            expect(expression(None), "expression"),
            expect_tag("}"),
        ),
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

fn switch_expression(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            tag("switch"),
            expect(expression(None), "value to switch over"),
            expect_tag("{"),
            separated_list1(
                w(tag(",")),
                w(map(
                    seq!(
                        tag("case"),
                        expect(type_expression(None), "type to match against"),
                        expect_tag(":"),
                        expect(expression(None), "result value")
                    ),
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
                map(
                    seq!(
                        tag(","),
                        tag("default"),
                        expect_tag(":"),
                        expect(expression(None), "default value")
                    ),
                    |(_, _, _, expr)| expr
                ),
            )),
            opt(w(tag(","))),
            expect_tag("}"),
        ),
        |(start, mut value, _, mut cases, mut default_case, _, end)| {
            make_node!(
                SwitchExpression,
                start.spanning(&end),
                value,
                cases,
                default_case
            )
            .recast::<Expression>()
        },
    )(i)
}

fn range_expression(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            number_literal,
            tag(".."),
            expect(number_literal, "range end")
        ),
        |(start, _, end)| {
            let mut start = start.recast::<Expression>();
            let mut end = end.recast::<Expression>();

            make_node!(RangeExpression, start.spanning(&end), start, end).recast::<Expression>()
        },
    )(i)
}

fn proc(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            opt(tag("async")),
            type_params,
            alt((args_parenthesized, arg_singleton)),
            opt(throws_clause),
            tag("|>"),
            expect(statement, "proc body"),
        ),
        |(asyn, mut type_params, (mut args, mut args_spread), mut throws, _, mut body)| {
            let mut is_async = asyn.is_some();
            let src = asyn
                .unwrap_or(
                    args.get(0)
                        .map(|a| a.slice().clone())
                        .unwrap_or(body.slice().clone()),
                )
                .spanning(&body);

            let mut type_annotation = make_node!(
                ProcType,
                src.clone(),
                type_params,
                args,
                args_spread,
                is_async,
                throws
            );

            make_node!(Proc, src, type_annotation, is_async, body).recast::<Expression>()
        },
    )(i)
}

fn element_tag(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        tuple((
            tag("<"),
            plain_identifier,
            many0(preceded(
                whitespace_required,
                map(
                    tuple((
                        plain_identifier,
                        tag("="),
                        expect_tag("{"),
                        expect(expression(None), "value"),
                        expect_tag("}"),
                    )),
                    |(key, _, _, value, _)| (key, value),
                ),
            )),
            expect(
                alt((
                    map(
                        tuple((
                            w(tag(">")),
                            many0(w(alt((
                                map(element_tag, |el| el.recast::<Expression>()),
                                map(
                                    seq!(
                                        tag("{"),
                                        expect(expression(None), "value"),
                                        expect_tag("}")
                                    ),
                                    |(_, expr, _)| expr,
                                ),
                            )))),
                            w(tag("</")),
                            plain_identifier,
                            w(expect_tag(">")),
                        )),
                        |(_, children, _, closing_tag, end)| (children, Some(closing_tag), end),
                    ),
                    map(w(tag("/>")), |end| (Vec::new(), None, end)),
                )),
                "closing tag",
            ),
        )),
        |(start, mut tag_name, mut attributes, (mut children, closing_tag, end))| {
            let src = start.spanning(&end);

            make_node!(ElementTag, src, tag_name, attributes, children).recast::<Expression>()
        },
    )(i)
}

fn func(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            opt(tag("async")),
            type_params,
            alt((args_parenthesized, arg_singleton)),
            opt(type_annotation),
            tag("=>"),
            expect(expression(None), "function body"),
        ),
        |(asyn, mut type_params, (mut args, mut args_spread), mut returns, _, mut body)| {
            let mut is_async = asyn.is_some();
            let src = asyn
                .unwrap_or(
                    args.get(0)
                        .map(|a| a.slice().clone())
                        .unwrap_or(body.slice().clone()),
                )
                .spanning(&body);

            let mut type_annotation = make_node!(
                FuncType,
                src.clone(),
                type_params,
                args,
                args_spread,
                returns
            );

            make_node!(Func, src, type_annotation, is_async, body).recast::<Expression>()
        },
    )(i)
}

fn inline_const_group(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(many1(inline_declaration), expression(None)),
        |(mut declarations, mut inner)| {
            make_node!(
                InlineConstGroup,
                declarations[0].spanning(&inner),
                declarations,
                inner
            )
            .recast::<Expression>()
        },
    )(i)
}

fn inline_declaration(i: Slice) -> ParseResult<AST<InlineDeclaration>> {
    map(
        seq!(
            tag("const"),
            expect(declaration_destination, "name or destructure"),
            expect_tag("="),
            expect(expression(None), "value"),
            expect_tag(",")
        ),
        |(start, mut destination, _, mut value, end)| {
            make_node!(InlineDeclaration, start.spanning(&end), destination, value)
        },
    )(i)
}

fn declaration_destination(i: Slice) -> ParseResult<DeclarationDestination> {
    alt((
        map(
            seq!(
                tag("{"),
                separated_list0(w(char(',')), w(plain_identifier)),
                opt(tag(",")),
                expect_tag("}"),
            ),
            |(_, properties, _, _)| {
                DeclarationDestination::Destructure(Destructure {
                    properties,
                    spread: None, // TODO
                    destructure_kind: DestructureKind::Object,
                })
            },
        ),
        map(
            seq!(
                tag("["),
                separated_list0(w(char(',')), w(plain_identifier)),
                opt(w(tag(","))),
                expect_tag("]"),
            ),
            |(_, properties, _, _)| {
                DeclarationDestination::Destructure(Destructure {
                    properties,
                    spread: None, // TODO
                    destructure_kind: DestructureKind::Array,
                })
            },
        ),
        map(
            seq!(plain_identifier, opt(type_annotation)),
            |(name, type_annotation)| {
                DeclarationDestination::NameAndType(NameAndType {
                    name,
                    type_annotation,
                })
            },
        ),
    ))(i)
}

fn negation_operation(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        pair(tag("!"), expression(Some(negation_operation))),
        |(start, mut expr)| {
            make_node_tuple!(NegationOperation, start.spanning(&expr), expr).recast::<Expression>()
        },
    )(i)
}

fn parenthesis(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            tag("("),
            expect(expression(None), "parenthesized expression"),
            expect_tag(")")
        ),
        |(open_paren, mut inner, close_paren)| {
            make_node_tuple!(Parenthesis, open_paren.spanning(&close_paren), inner)
                .recast::<Expression>()
        },
    )(i)
}

fn object_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            tag("{"),
            terminated(
                separated_list0(
                    w(char(',')),
                    w(alt((
                        map(
                            preceded(tag("..."), expect(expression(None), "spread expression")),
                            KeyValueOrSpread::Spread
                        ),
                        key_value_expression,
                    )))
                ),
                opt(w(tag(",")))
            ),
            expect_tag("}"),
        ),
        |(open_bracket, mut entries, close_bracket)| {
            make_node_tuple!(
                ObjectLiteral,
                open_bracket.spanning(&close_bracket),
                entries
            )
            .recast::<Expression>()
        },
    )(i)
}

fn key_value_expression(i: Slice) -> ParseResult<KeyValueOrSpread<AST<Expression>>> {
    map(
        seq!(
            plain_identifier,
            tag(":"),
            expect(expression(None), "value")
        ),
        |(key, _, value)| {
            KeyValueOrSpread::KeyValue(
                identifier_to_string(key).recast::<Expression>(),
                value,
                false,
            )
        },
    )(i)
}

fn array_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        seq!(
            tag("["),
            terminated(
                separated_list0(
                    w(char(',')),
                    w(alt((
                        map(
                            preceded(tag("..."), expect(expression(None), "spread expression")),
                            ElementOrSpread::Spread
                        ),
                        map(expression(None), ElementOrSpread::Element),
                    )))
                ),
                opt(w(tag(",")))
            ),
            expect_tag("]"),
        ),
        |(open_bracket, mut entries, close_bracket)| {
            make_node_tuple!(ArrayLiteral, open_bracket.spanning(&close_bracket), entries)
                .recast::<Expression>()
        },
    )(i)
}

fn string_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        pair(
            opt(plain_identifier),
            tuple((tag("\'"), many0(string_literal_segment), expect_tag("\'"))),
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
            .recast::<Expression>()
        },
    )(i)
}

fn exact_string_literal_expr(i: Slice) -> ParseResult<AST<Expression>> {
    map(exact_string_literal, AST::recast::<Expression>)(i)
}

fn exact_string_literal(i: Slice) -> ParseResult<AST<ExactStringLiteral>> {
    map(
        pair(
            opt(plain_identifier),
            tuple((tag("\'"), string_contents, expect_tag("\'"))),
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

fn string_literal_segment(i: Slice) -> ParseResult<StringLiteralSegment> {
    alt((
        map(
            tuple((tag("${"), w(expression(None)), w(expect_tag("}")))),
            |(_, expr, _)| expr.into(),
        ),
        map(string_contents, |s| s.into()),
    ))(i)
}

fn number_literal(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        tuple((
            opt(tag("-")),
            numeric,
            opt(tuple((tag("."), expect(numeric, "decimal")))),
        )),
        |(neg, int, tail)| {
            let front = neg.unwrap_or(int.clone());
            let back = tail.map(|(_, decimal)| decimal).unwrap_or(int);
            let full = front.spanning(&back);

            NumberLiteral(full.clone())
                .as_ast(full)
                .recast::<Expression>()
        },
    )(i)
}

fn boolean_literal(input: Slice) -> ParseResult<AST<Expression>> {
    alt((
        map(tag("true"), |src: Slice| {
            BooleanLiteral(true).as_ast(src).recast::<Expression>()
        }),
        map(tag("false"), |src: Slice| {
            BooleanLiteral(false).as_ast(src).recast::<Expression>()
        }),
    ))(input)
}

fn nil_literal(input: Slice) -> ParseResult<AST<Expression>> {
    map(tag("nil"), |src: Slice| {
        NilLiteral.as_ast(src).recast::<Expression>()
    })(input)
}

fn local_identifier_expr(i: Slice) -> ParseResult<AST<Expression>> {
    map(local_identifier, AST::recast::<Expression>)(i)
}

fn local_identifier(i: Slice) -> ParseResult<AST<LocalIdentifier>> {
    map(identifier_like, |name| {
        LocalIdentifier(name.clone()).as_ast(name)
    })(i)
}

fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    verify(
        map(identifier_like, |name| {
            PlainIdentifier(name.clone()).as_ast(name)
        }),
        |ident| !RESERVED_IDENTIFIERS.contains(&ident.downcast().0.as_str()),
    )(i)
}

// --- Util parsers ---

fn w<O, G>(parser: G) -> impl FnMut(Slice) -> ParseResult<O>
where
    G: Parser<Slice, O, RawParseError>,
{
    preceded(whitespace, parser)
}

fn break_after<O, G>(parser: G) -> impl FnMut(Slice) -> ParseResult<O>
where
    G: Parser<Slice, O, RawParseError>,
{
    terminated(parser, whitespace_required)
}

fn string_contents(i: Slice) -> ParseResult<Slice> {
    escaped(
        take_while1(|ch: char| ch != '\'' && ch != '$' && ch != '\\'),
        '\\',
        one_of("$\'\\"),
    )(i)
}

fn identifier_like(i: Slice) -> ParseResult<Slice> {
    verify(
        map(
            tuple((
                take_while1(|ch: char| ch.is_alphabetic() || ch == '_' || ch == '$'),
                take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '$'),
            )),
            |(a, b): (Slice, Slice)| a.spanning(&b),
        ),
        |s| is_valid_identifier(s.as_str()),
    )(i)
}

fn is_valid_identifier(s: &str) -> bool {
    s.char_indices().all(|(index, ch)| {
        ch == '_'
            || ch == '$'
            || if index == 0 {
                ch.is_alphabetic()
            } else {
                ch.is_alphanumeric()
            }
    }) && !INVALID_IDENTIFIERS.contains(&s)
}

const INVALID_IDENTIFIERS: [&'static str; 0] = [];
const RESERVED_IDENTIFIERS: [&'static str; 1] = [JS_GLOBAL_IDENTIFIER];

fn numeric(i: Slice) -> ParseResult<Slice> {
    take_while1(|c: char| c.is_numeric())(i)
}

fn whitespace(i: Slice) -> ParseResult<Slice> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i) // TODO: comments
}

fn whitespace_required(i: Slice) -> ParseResult<Slice> {
    take_while1(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i) // TODO: comments
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
    Expected(String),
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

fn expect<TResult, F: FnMut(Slice) -> ParseResult<TResult>>(
    f: F,
    description: &'static str,
) -> impl FnMut(Slice) -> ParseResult<TResult> {
    expect_inner(f, description, false)
}

fn expect_tag(t: &'static str) -> impl FnMut(Slice) -> ParseResult<Slice> {
    expect_inner(tag(t), t, true)
}

fn expect_inner<TResult, F: FnMut(Slice) -> ParseResult<TResult>>(
    mut f: F,
    description: &'static str,
    quoted: bool,
) -> impl FnMut(Slice) -> ParseResult<TResult> {
    move |i: Slice| {
        let res = f(i.clone());

        if matches!(res, Err(nom::Err::Error(_))) {
            let details = if quoted {
                format!("'{}'", description)
            } else {
                description.to_owned()
            };

            Err(nom::Err::Failure(RawParseError {
                src: i,
                details: RawParseErrorDetails::Expected(details),
            }))
        } else {
            res
        }
    }
}

use precedence::precedence;
mod precedence {
    use nom::error::ErrorKind;

    use crate::model::{
        ast::{ASTAny, Any, AST},
        Slice,
    };

    use super::{precedence_index, ParseResult, RawParseError, RawParseErrorDetails};

    fn precedence_inner<TKind>(
        levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
        after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
        i: Slice,
    ) -> ParseResult<AST<TKind>>
    where
        TKind: 'static + Clone + TryFrom<Any>,
        Any: From<TKind>,
    {
        let start_index = precedence_index(levels, after);
        for level in &levels[start_index..] {
            for f in *level {
                let res = f(i.clone());

                if res.is_ok() {
                    return res;
                }
            }
        }

        Err(nom::Err::Error(RawParseError {
            src: i,
            details: RawParseErrorDetails::Kind(ErrorKind::Fail),
        }))
    }

    std::thread_local! {
      static MEMOIZED_MAPPING_PRECEDENCE: std::cell::RefCell<std::collections::HashMap<(*const u8, Option<*const u8>, Slice), ParseResult<ASTAny>>>  = std::cell::RefCell::new(std::collections::HashMap::new());
    }

    #[allow(unused_variables)]
    pub fn precedence<TKind>(
        levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
        after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
        i: Slice,
    ) -> ParseResult<AST<TKind>>
    where
        TKind: 'static + Clone + TryFrom<Any>,
        Any: From<TKind>,
    {
        let levels_ptr = levels.as_ptr().cast::<u8>();
        let after_ptr = after.map(|after| after as *const u8);

        let r = MEMOIZED_MAPPING_PRECEDENCE.with(|hm| {
            let hm = hm.borrow_mut();
            hm.get(&(levels_ptr, after_ptr, i.clone())).cloned()
        });
        if let Some(r) = r {
            return r.map(|(s, ast)| (s, ast.try_recast::<TKind>().unwrap()));
        }
        let r = precedence_inner(levels, after, i.clone());
        MEMOIZED_MAPPING_PRECEDENCE.with(|hm| {
            let mut hm: std::cell::RefMut<
                std::collections::HashMap<
                    (*const u8, Option<*const u8>, Slice),
                    ParseResult<ASTAny>,
                >,
            > = hm.borrow_mut();
            hm.insert(
                (levels_ptr, after_ptr, i),
                r.clone().map(|(s, ast)| (s, ast.upcast())),
            );
        });
        r
    }
}

use precedence_index::precedence_index;
mod precedence_index {
    use crate::model::{
        ast::{Any, AST},
        Slice,
    };

    use super::ParseResult;

    fn precedence_index_inner<TKind>(
        levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
        after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
    ) -> usize
    where
        TKind: 'static + Clone + TryFrom<Any>,
        Any: From<TKind>,
    {
        match after {
            Some(after) => {
                levels
                    .iter()
                    .take_while(|level| !level.contains(&after))
                    .count()
                    + 1
            }
            None => 0,
        }
    }

    std::thread_local! {
      static MEMOIZED_MAPPING_PRECEDENCE_INDEX: std::cell::RefCell<std::collections::HashMap<(*const u8, Option<*const u8>), usize>>  = std::cell::RefCell::new(std::collections::HashMap::new());
    }

    #[allow(unused_variables)]
    pub fn precedence_index<TKind>(
        levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
        after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
    ) -> usize
    where
        TKind: 'static + Clone + TryFrom<Any>,
        Any: From<TKind>,
    {
        let levels_ptr = levels.as_ptr().cast::<u8>();
        let after_ptr = after.map(|after| after as *const u8);

        let r = MEMOIZED_MAPPING_PRECEDENCE_INDEX.with(|hm| {
            let hm = hm.borrow_mut();
            hm.get(&(levels_ptr, after_ptr)).cloned()
        });
        if let Some(r) = r {
            return r;
        }
        let r = precedence_index_inner(levels, after);
        MEMOIZED_MAPPING_PRECEDENCE_INDEX.with(|hm| {
            let mut hm: std::cell::RefMut<
                std::collections::HashMap<(*const u8, Option<*const u8>), usize>,
            > = hm.borrow_mut();
            hm.insert((levels_ptr, after_ptr), r);
        });
        r
    }
}

fn log<T: std::fmt::Debug, F: FnMut(Slice) -> ParseResult<T>>(
    f: F,
) -> impl FnMut(Slice) -> ParseResult<T> {
    map(f, |r| {
        println!("{:?}", r);
        r
    })
}
