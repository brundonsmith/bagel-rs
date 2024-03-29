use crate::{
    cli::ModulesStore,
    model::{ast::*, ModuleID, ParsedModule, Slice},
};
use std::{
    collections::HashMap,
    fmt::{Result, Write},
};

use super::ResolveSymbolContext;

#[derive(Debug, Clone, Copy)]
pub struct CompileContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a ParsedModule,
    pub include_types: bool,
    pub qualify_identifiers_with: Option<&'a HashMap<ModuleID, usize>>,
    pub qualify_all_identifiers: bool,
}

impl<'a> CompileContext<'a> {
    pub fn current_module_qualifier(&self) -> Option<usize> {
        self.qualify_identifiers_with
            .map(|qualify_identifiers_with| {
                qualify_identifiers_with
                    .get(self.current_module.module_id())
                    .cloned()
            })
            .flatten()
    }

    pub fn qualifying_all_identifiers(self) -> Self {
        CompileContext {
            modules: self.modules,
            current_module: self.current_module,
            include_types: self.include_types,
            qualify_identifiers_with: self.qualify_identifiers_with,
            qualify_all_identifiers: true,
        }
    }
}

pub trait Compilable {
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> Result;
}

impl Compilable for ParsedModule {
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> Result {
        match self {
            ParsedModule::Bagel { module_id: _, ast } => ast.compile(ctx, f),
            ParsedModule::JavaScript { module_id: _ } => Ok(()),
            ParsedModule::Singleton {
                module_id: _,
                contents,
            } => {
                f.write_str("\nexport default ")?;
                contents.compile(ctx, f)
            }
        }
    }
}

impl<TKind> Compilable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> Result {
        match self.details() {
            Any::Module(Module {
                module_id: _,
                declarations,
            }) => {
                for decl in declarations {
                    decl.compile(ctx, f)?;
                    f.write_str(";\n\n")?;
                }

                Ok(())
            }
            Any::ImportAllDeclaration(ImportAllDeclaration {
                platforms,
                name,
                path,
            }) => {
                if ctx.qualify_identifiers_with.is_none() {
                    f.write_str("import * as ")?;
                    name.compile(ctx, f)?;
                    f.write_str(" from \"")?;
                    f.write_str(path.downcast().value.as_str())?;
                    f.write_str(".ts")?;
                    f.write_str("\"")
                } else {
                    Ok(())
                }
            }
            Any::ImportDeclaration(ImportDeclaration {
                platforms,
                imports,
                path,
            }) => {
                if ctx.qualify_identifiers_with.is_none() {
                    f.write_str("import { ")?;
                    for (index, import) in imports.iter().enumerate() {
                        if index > 0 {
                            f.write_str(", ")?;
                        }

                        import.compile(ctx, f)?;
                    }
                    f.write_str(" } from \"")?;
                    f.write_str(path.downcast().value.as_str())?;
                    f.write_str(".ts")?;
                    f.write_str("\"")
                } else {
                    Ok(())
                }
            }
            Any::ImportItem(ImportItem { name, alias }) => {
                name.compile(ctx, f)?;

                if let Some(alias) = alias {
                    f.write_str(" as ")?;
                    alias.compile(ctx, f)?;
                }

                Ok(())
            }
            Any::TypeDeclaration(TypeDeclaration {
                name,
                declared_type,
                exported,
            }) => {
                if ctx.include_types {
                    if *exported {
                        f.write_str("\nexport ")?;
                    }

                    f.write_str("type ")?;
                    name.compile(ctx, f)?;
                    if let Some(id) = ctx.current_module_qualifier() {
                        f.write_fmt(format_args!("_{}", id))?;
                    }
                    f.write_str(" = ")?;
                    declared_type.compile(ctx, f)?;
                }

                Ok(())
            }
            Any::FuncDeclaration(FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            }) => {
                let func = func.downcast();
                let type_annotation = func.type_annotation.downcast();

                if *exported {
                    f.write_str("\nexport ")?;
                }
                f.write_str("const ")?;
                name.compile(ctx, f)?;
                if let Some(id) = ctx.current_module_qualifier() {
                    f.write_fmt(format_args!("_{}", id))?;
                }
                f.write_str(" = ")?;
                compile_function(
                    ctx,
                    f,
                    Some(name.slice().as_str()),
                    &type_annotation.args,
                    false,
                    type_annotation.returns.as_ref(),
                    &func.body.upcast(),
                )
            }
            Any::ProcDeclaration(ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            }) => {
                let proc = proc.downcast();
                let type_annotation = proc.type_annotation.downcast();

                if *exported {
                    f.write_str("\nexport ")?;
                }
                f.write_str("const ")?;
                name.compile(ctx, f)?;
                if name.slice().as_str() != "main" {
                    if let Some(id) = ctx.current_module_qualifier() {
                        f.write_fmt(format_args!("_{}", id))?;
                    }
                }
                f.write_str(" = ")?;
                compile_function(
                    ctx,
                    f,
                    Some(name.slice().as_str()),
                    &type_annotation.args,
                    true,
                    None,
                    &proc.body.upcast(),
                )
            }
            Any::Decorator(Decorator { name, arguments }) => todo!(),
            Any::ValueDeclaration(ValueDeclaration {
                destination,
                value,
                is_const,
                exported,
                platforms,
            }) => {
                if *exported {
                    f.write_str("\nexport ")?;
                }
                f.write_str("const ")?;
                destination.compile(ctx.qualifying_all_identifiers(), f)?;
                f.write_str(" = ")?;
                if *is_const {
                    value.compile(ctx, f)?;
                } else {
                    f.write_str("{ value: ")?;
                    value.compile(ctx, f)?;
                    f.write_str(" }")?;
                }

                Ok(())
            }
            Any::SymbolDeclaration(SymbolDeclaration { name, exported }) => {
                if *exported {
                    f.write_str("\nexport ")?;
                }
                f.write_str("const ")?;
                name.compile(ctx, f)?;
                if let Some(id) = ctx.current_module_qualifier() {
                    f.write_fmt(format_args!("_{}", id))?;
                }
                f.write_str(" = Symbol('")?;
                name.compile(ctx, f)?;
                f.write_str("');")?;

                Ok(())
            }
            Any::TestExprDeclaration(TestExprDeclaration {
                platforms,
                name,
                expr,
            }) => Ok(()),
            Any::TestBlockDeclaration(TestBlockDeclaration {
                platforms,
                name,
                block,
            }) => todo!(),
            Any::TestTypeDeclaration(TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            }) => todo!(),
            Any::NilLiteral(_) => f.write_str(NIL),
            Any::BooleanLiteral(BooleanLiteral(value)) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            Any::NumberLiteral(NumberLiteral(value)) => f.write_str(value.as_str()),
            Any::StringLiteral(StringLiteral { tag: _, segments }) => {
                f.write_char('`')?;
                for segment in segments {
                    match segment {
                        StringLiteralSegment::Slice(s) => compile_string_contents(f, s)?,
                        StringLiteralSegment::AST(insert) => {
                            f.write_str("${")?;
                            insert.compile(ctx, f)?;
                            f.write_str("}")?;
                        }
                    };
                }
                f.write_char('`')
            }
            Any::ExactStringLiteral(ExactStringLiteral { tag, value }) => {
                f.write_char('`')?;
                compile_string_contents(f, value)?;
                f.write_char('`')
            }
            Any::ArrayLiteral(ArrayLiteral(entries)) => {
                f.write_char('[')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    match entry {
                        ElementOrSpread::Element(element) => {
                            element.compile(ctx, f)?;
                        }
                        ElementOrSpread::Spread(spread) => {
                            f.write_str("...")?;
                            spread.compile(ctx, f)?;
                        }
                    }
                }
                f.write_char(']')
            }
            Any::ObjectLiteral(ObjectLiteral(entries)) => {
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }
                    f.write_char(' ')?;

                    match entry {
                        KeyValueOrSpread::KeyValue(key, value, _) => {
                            key.compile(ctx, f)?;
                            f.write_str(": ")?;
                            value.compile(ctx, f)?;
                        }
                        KeyValueOrSpread::Spread(expr) => {
                            f.write_str("...")?;
                            expr.compile(ctx, f)?;
                        }
                    }
                }
                f.write_str(" }")
            }
            Any::SpreadExpression(SpreadExpression(inner)) => {
                f.write_str("...")?;
                inner.compile(ctx, f)
            }
            Any::BinaryOperation(BinaryOperation { left, op, right }) => {
                f.write_char('(')?;
                left.compile(ctx, f)?;
                f.write_char(' ')?;
                op.compile(ctx, f)?;
                f.write_char(' ')?;
                right.compile(ctx, f)?;
                f.write_char(')')
            }
            Any::BinaryOperator(BinaryOperator(op)) => f.write_str(op.into()),
            Any::NegationOperation(NegationOperation(inner)) => {
                f.write_char('!')?;
                inner.compile(ctx, f)
            }
            Any::Parenthesis(Parenthesis(inner)) => {
                f.write_char('(')?;
                inner.compile(ctx, f)?;
                f.write_char(')')
            }
            Any::LocalIdentifier(LocalIdentifier(name)) => {
                if name == JS_GLOBAL_IDENTIFIER {
                    f.write_str("globalThis")
                } else {
                    let resolved = self.resolve_symbol(
                        ResolveSymbolContext {
                            modules: ctx.modules,
                            current_module: ctx.current_module,
                            follow_imports: false,
                        },
                        name.as_str(),
                    );

                    match resolved.as_ref().map(|r| r.details()) {
                        Some(Any::ImportDeclaration(ImportDeclaration {
                            platforms: _,
                            imports: _,
                            path,
                        })) => {
                            f.write_str(name.as_str())?;

                            if let Some(id) = ctx
                                .qualify_identifiers_with
                                .map(|qualify_identifiers_with| {
                                    ctx.current_module
                                        .module_id()
                                        .imported(path.downcast().value.as_str())
                                        .map(|other_module_id| {
                                            qualify_identifiers_with.get(&other_module_id)
                                        })
                                })
                                .flatten()
                                .flatten()
                            {
                                f.write_fmt(format_args!("_{}", id))?;
                            }

                            Ok(())
                        }
                        Some(Any::TypeDeclaration(TypeDeclaration {
                            name,
                            declared_type: _,
                            exported: _,
                        })) => {
                            name.compile(ctx, f)?;
                            if let Some(id) = ctx.current_module_qualifier() {
                                f.write_fmt(format_args!("_{}", id))?;
                            }
                            Ok(())
                        }
                        Some(Any::FuncDeclaration(FuncDeclaration {
                            platforms: _,
                            name,
                            func: _,
                            exported: _,
                            decorators: _,
                        })) => {
                            name.compile(ctx, f)?;
                            if let Some(id) = ctx.current_module_qualifier() {
                                f.write_fmt(format_args!("_{}", id))?;
                            }
                            Ok(())
                        }
                        Some(Any::ProcDeclaration(ProcDeclaration {
                            platforms: _,
                            name,
                            proc: _,
                            exported: _,
                            decorators: _,
                        })) => {
                            name.compile(ctx, f)?;
                            if name.slice().as_str() != "main" {
                                if let Some(id) = ctx.current_module_qualifier() {
                                    f.write_fmt(format_args!("_{}", id))?;
                                }
                            }
                            Ok(())
                        }
                        Some(Any::SymbolDeclaration(SymbolDeclaration { name, exported })) => {
                            name.compile(ctx, f)?;
                            if let Some(id) = ctx.current_module_qualifier() {
                                f.write_fmt(format_args!("_{}", id))?;
                            }
                            Ok(())
                        }
                        Some(Any::ValueDeclaration(ValueDeclaration {
                            destination: _,
                            value: _,
                            is_const,
                            exported: _,
                            platforms: _,
                        })) => {
                            if !*is_const {
                                f.write_str(INT)?;
                                f.write_str("observe(")?;
                                f.write_str(name.as_str())?;
                                if let Some(id) = ctx.current_module_qualifier() {
                                    f.write_fmt(format_args!("_{}", id))?;
                                }
                                f.write_str(", 'value')")
                            } else {
                                f.write_str(name.as_str())?;
                                if let Some(id) = ctx.current_module_qualifier() {
                                    f.write_fmt(format_args!("_{}", id))?;
                                }
                                Ok(())
                            }
                        }

                        _ => f.write_str(name.as_str()),
                    }
                }
            }
            Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner,
            }) => {
                f.write_str("(() => {\n")?;
                for decl in declarations {
                    decl.compile(ctx, f)?;
                    f.write_char('\n')?;
                }
                f.write_str("return ")?;
                inner.compile(ctx, f)?;
                f.write_str(";\n})()")
            }
            Any::InlineDeclaration(InlineDeclaration { destination, value }) => {
                f.write_str("const ")?;
                destination.compile(ctx, f)?;
                f.write_str(" = ")?;
                value.compile(ctx, f)?;
                f.write_char(';')
            }
            Any::Func(Func {
                type_annotation,
                is_async,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                compile_function(
                    ctx,
                    f,
                    None,
                    &type_annotation.args,
                    false,
                    type_annotation.returns.as_ref(),
                    &body.clone().upcast(),
                )
            }
            Any::Proc(Proc {
                type_annotation,
                is_async,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                compile_function(
                    ctx,
                    f,
                    None,
                    &type_annotation.args,
                    true,
                    None,
                    &body.clone().upcast(),
                )
            }
            Any::Block(Block(statements)) => {
                f.write_str("{\n")?;
                for stmt in statements {
                    stmt.compile(ctx, f)?;
                    f.write_char(';')?;
                    f.write_char('\n')?;
                }
                f.write_str("\n}")
            }
            Any::RangeExpression(RangeExpression { start, end }) => {
                f.write_str(INT)?;
                f.write_str("range(")?;
                start.compile(ctx, f)?;
                f.write_char(',')?;
                end.compile(ctx, f)?;
                f.write_char(')')
            }
            Any::AwaitExpression(AwaitExpression(inner)) => todo!(),
            Any::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            }) => {
                if let Some(inv) = method_call_as_invocation(
                    ctx.into(),
                    self.clone().upcast().try_recast::<Invocation>().unwrap(),
                ) {
                    inv.compile(ctx, f)?;
                } else {
                    subject.compile(ctx, f)?;

                    f.write_char('(')?;
                    for (index, arg) in args.iter().enumerate() {
                        if index > 0 {
                            f.write_str(", ")?;
                        }

                        arg.compile(ctx, f)?;
                    }
                    f.write_char(')')?;
                }

                Ok(())
            }
            Any::PropertyAccessor(PropertyAccessor {
                subject,
                property,
                optional,
            }) => {
                f.write_str(INT)?;
                f.write_str("observe(")?;
                subject.compile(ctx, f)?;
                f.write_str(", ")?;

                match property {
                    Property::Expression(expr) => expr.compile(ctx, f)?,
                    Property::PlainIdentifier(ident) => {
                        f.write_char('\'')?;
                        ident.compile(ctx, f)?;
                        f.write_char('\'')?;
                    }
                }

                f.write_str(")")?;

                Ok(())
            }
            Any::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            }) => {
                f.write_char('(')?;
                for case in cases {
                    case.compile(ctx, f)?;
                }
                if let Some(default_case) = default_case {
                    default_case.compile(ctx, f)?;
                } else {
                    f.write_str(NIL)?;
                }
                f.write_char(')')
            }
            Any::IfElseExpressionCase(IfElseExpressionCase { condition, outcome }) => {
                condition.compile(ctx, f)?;
                f.write_str(" ? ")?;
                outcome.compile(ctx, f)?;
                f.write_str(" : ")
            }
            Any::SwitchExpression(SwitchExpression {
                value,
                cases,
                default_case,
            }) => {
                f.write_str("(() => {\n  const ___v = ")?;
                value.compile(ctx, f)?;
                f.write_str("\n  return ")?;
                for case in cases {
                    case.compile(ctx, f)?;
                }

                match default_case {
                    Some(default_case) => default_case.compile(ctx, f)?,
                    None => f.write_str(NIL)?,
                }
                f.write_str("})()")
            }
            Any::SwitchExpressionCase(SwitchExpressionCase {
                type_filter,
                outcome,
            }) => {
                f.write_str(INT)?;
                f.write_str("instanceOf(___v, ")?;
                compile_runtime_type(f, type_filter)?;
                f.write_str(") ? ")?;
                outcome.compile(ctx, f)?;
                f.write_str(" : ")
            }
            Any::ElementTag(ElementTag {
                tag_name,
                attributes,
                children,
            }) => {
                f.write_str("{ tag:'")?;
                tag_name.compile(ctx, f)?;
                f.write_str("', attributes:{ ")?;
                for (key, value) in attributes {
                    key.compile(ctx, f)?;
                    f.write_char(':')?;
                    value.compile(ctx, f)?;
                    f.write_str(", ")?;
                }
                f.write_str("}, children:[ ")?;
                for child in children {
                    child.compile(ctx, f)?;
                    f.write_str(", ")?;
                }
                f.write_str(" ] }")
            }
            Any::AsCast(AsCast { inner, as_type }) => {
                inner.compile(ctx, f)?;
                if ctx.include_types {
                    f.write_str(" as ")?;
                    as_type.compile(ctx, f)?;
                }
                Ok(())
            }
            Any::InstanceOf(InstanceOf {
                inner,
                possible_type,
            }) => {
                f.write_str(INT)?;
                f.write_str("instanceOf(")?;
                inner.compile(ctx, f)?;
                f.write_str(", ")?;
                compile_runtime_type(f, possible_type)?;
                f.write_str(")")
            }
            Any::ErrorExpression(ErrorExpression(inner)) => {
                f.write_str("{ kind: ___ERROR_SYM, value: ")?;
                inner.compile(ctx, f)?;
                f.write_str(" }")
            }
            Any::RegularExpression(RegularExpression { expr, flags }) => {
                f.write_char('/')?;
                f.write_str(expr.as_str())?;
                f.write_char('/')?;
                for flag in flags {
                    f.write_str(flag.into())?;
                }
                f.write_char(' ')
            }
            Any::AnyLiteral(_) => f.write_str("null"),
            Any::UnionType(UnionType(members)) => {
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }
                    member.compile(ctx, f)?;
                }
                Ok(())
            }
            Any::MaybeType(MaybeType(inner)) => {
                f.write_char('(')?;
                inner.compile(ctx, f)?;
                f.write_str(" | null | undefined)")
            }
            Any::NamedType(NamedType(name)) => name.compile(ctx, f),
            Any::GenericParamType(GenericParamType { name, extends }) => {
                name.compile(ctx, f)?;

                if let Some(extends) = extends {
                    f.write_str(" extends ")?;
                    extends.compile(ctx, f)?;
                }

                Ok(())
            }
            Any::ProcType(ProcType {
                type_params,
                args,
                args_spread,
                is_async,
                throws,
            }) => {
                if type_params.len() > 0 {
                    f.write_char('<')?;
                    for (index, param) in type_params.iter().enumerate() {
                        if index > 0 {
                            f.write_str(", ")?;
                        }

                        param.compile(ctx, f)?;
                    }
                    f.write_char('>')?;
                }

                f.write_char('(')?;
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    arg.compile(ctx, f)?;
                }
                f.write_str(") => void") // TODO: Error return type
            }
            Any::FuncType(FuncType {
                type_params,
                args,
                args_spread,
                returns,
            }) => {
                if type_params.len() > 0 {
                    f.write_char('<')?;
                    for (index, param) in type_params.iter().enumerate() {
                        if index > 0 {
                            f.write_str(", ")?;
                        }

                        param.compile(ctx, f)?;
                    }
                    f.write_char('>')?;
                }

                f.write_char('(')?;
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    arg.compile(ctx, f)?;
                }
                f.write_str(") => ")?;
                returns.compile(ctx, f)
            }
            Any::Arg(Arg {
                name,
                type_annotation,
                optional,
            }) => {
                name.compile(ctx, f)?;
                compile_type_annotation(ctx, f, type_annotation.as_ref())
            }
            Any::GenericType(GenericType { type_params, inner }) => todo!(),
            Any::TypeParam(TypeParam { name, extends }) => {
                name.compile(ctx, f)?;

                if let Some(extends) = extends {
                    f.write_str(" extends ")?;
                    extends.compile(ctx, f)?;
                }

                Ok(())
            }
            Any::BoundGenericType(BoundGenericType { type_args, generic }) => {
                generic.compile(ctx, f)?;
                f.write_char('<')?;
                for (index, arg) in type_args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    arg.compile(ctx, f)?;
                }
                f.write_char('>')
            }
            Any::ObjectType(ObjectType {
                entries,
                is_interface,
            }) => {
                f.write_str("{\n")?;

                for entry in entries {
                    match entry {
                        KeyValueOrSpread::KeyValue(key, value, optional) => {
                            if let Some(StringLiteralType(s)) =
                                key.try_downcast::<StringLiteralType>()
                            {
                                // TODO: Do this for any identifier-like
                                if s.as_str().chars().all(char::is_alphabetic) {
                                    f.write_str(s.as_str())?;
                                } else {
                                    key.compile(ctx, f)?;
                                }
                            } else {
                                key.compile(ctx, f)?;
                            }

                            if *optional {
                                f.write_char('?')?;
                            }
                            f.write_str(": ")?;
                            value.compile(ctx, f)?;
                        }
                        KeyValueOrSpread::Spread(spread) => {
                            f.write_str("...")?;
                            spread.compile(ctx, f)?;
                        }
                    };

                    f.write_char(',')?;
                }

                f.write_str("\n}")
            }
            Any::RecordType(RecordType {
                key_type,
                value_type,
            }) => {
                f.write_str("Record<")?;
                key_type.compile(ctx, f)?;
                f.write_str(", ")?;
                value_type.compile(ctx, f)?;
                f.write_str(">")
            }
            Any::ArrayType(ArrayType(element)) => {
                f.write_char('(')?;
                element.compile(ctx, f)?;
                f.write_str(")[]")
            }
            Any::TupleType(TupleType(members)) => {
                f.write_char('[')?;
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    match member {
                        ElementOrSpread::Element(element) => element.compile(ctx, f)?,
                        ElementOrSpread::Spread(spread) => {
                            f.write_str("...")?;
                            spread.compile(ctx, f)?;
                        }
                    }
                }
                f.write_char(']')
            }
            Any::StringLiteralType(StringLiteralType(value)) => {
                f.write_char('\'')?;
                f.write_str(value.as_str())?;
                f.write_char('\'')
            }
            Any::NumberLiteralType(NumberLiteralType(value)) => f.write_str(value.as_str()),
            Any::BooleanLiteralType(BooleanLiteralType(value)) => {
                f.write_str(if *value { "true" } else { "false" })
            }
            Any::StringType(_) => f.write_str("string"),
            Any::NumberType(_) => f.write_str("number"),
            Any::BooleanType(_) => f.write_str("boolean"),
            Any::NilType(_) => f.write_str("null | undefined"),
            Any::SpecialType(SpecialType { kind, inner }) => {
                f.write_str(INT)?;
                f.write_str(kind.into())?;
                f.write_char('<')?;
                inner.compile(ctx, f)?;
                f.write_char('>')
            }
            Any::ParenthesizedType(ParenthesizedType(inner)) => {
                f.write_char('(')?;
                inner.compile(ctx, f)?;
                f.write_char(')')
            }
            Any::TypeofType(_) => todo!(),
            Any::ModifierType(ModifierType { kind, inner }) => todo!(),
            Any::UnknownType(_) => f.write_str("unknown"),
            Any::RegularExpressionType(_) => f.write_str("RegExp"),
            Any::PropertyType(PropertyType {
                subject,
                property,
                optional,
            }) => {
                subject.compile(ctx, f)?;
                f.write_char('[')?;
                property.compile(ctx, f)?;
                f.write_char(']')
            }
            Any::IfElseStatement(IfElseStatement {
                cases,
                default_case,
            }) => {
                for (index, case) in cases.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" else ")?;
                    }

                    case.compile(ctx, f)?;
                }

                if let Some(default_case) = default_case {
                    f.write_str(" else ")?;
                    default_case.compile(ctx, f)?;
                }

                Ok(())
            }
            Any::IfElseStatementCase(IfElseStatementCase { condition, outcome }) => {
                f.write_str("if (")?;
                condition.compile(ctx, f)?;
                f.write_str(") ")?;
                outcome.compile(ctx, f)
            }
            Any::ForLoop(ForLoop {
                item_identifier,
                iterable,
                body,
            }) => {
                f.write_str("for (const ")?;
                item_identifier.compile(ctx, f)?;
                f.write_str(" of ")?;
                iterable.compile(ctx, f)?;
                f.write_str(".inner) ")?;
                body.compile(ctx, f)
            }
            Any::WhileLoop(WhileLoop { condition, body }) => {
                f.write_str("while (")?;
                condition.compile(ctx, f)?;
                f.write_char(')')?;
                body.compile(ctx, f)
            }
            Any::Assignment(Assignment {
                target,
                value,
                operator,
            }) => {
                f.write_str(INT)?;
                f.write_str("invalidate(")?;
                match target.details() {
                    Any::LocalIdentifier(LocalIdentifier(name)) => {
                        f.write_str(name.as_str())?;
                        if let Some(id) = ctx.current_module_qualifier() {
                            f.write_fmt(format_args!("_{}", id))?;
                        }
                        f.write_str(", 'value', ")?;

                        if let Some(operator) = operator {
                            f.write_str(name.as_str())?;
                            f.write_str(".value ")?;
                            f.write_str(operator.downcast().0.into())?;
                            f.write_char(' ')?;
                        }
                        value.compile(ctx, f)?;

                        f.write_str(")")?;
                    }
                    Any::PropertyAccessor(PropertyAccessor {
                        subject,
                        property,
                        optional: _,
                    }) => {
                        subject.compile(ctx, f)?;
                        f.write_str(", ")?;
                        match property {
                            Property::Expression(expr) => expr.compile(ctx, f)?,
                            Property::PlainIdentifier(ident) => {
                                f.write_char('\'')?;
                                f.write_str(ident.downcast().0.as_str())?;
                                f.write_char('\'')?;
                            }
                        };
                        f.write_str(", ")?;

                        if let Some(operator) = operator {
                            subject.compile(ctx, f)?;
                            f.write_char('[')?;
                            match property {
                                Property::Expression(expr) => expr.compile(ctx, f)?,
                                Property::PlainIdentifier(ident) => {
                                    f.write_char('\'')?;
                                    f.write_str(ident.downcast().0.as_str())?;
                                    f.write_char('\'')?;
                                }
                            };
                            f.write_char(']')?;
                            f.write_str(operator.downcast().0.into())?;
                            f.write_char(' ')?;
                        }
                        value.compile(ctx, f)?;

                        f.write_str(")")?;
                    }
                    _ => {}
                }

                Ok(())
            }
            Any::TryCatch(TryCatch {
                try_block,
                error_identifier,
                catch_block,
            }) => todo!(),
            Any::ThrowStatement(ThrowStatement { error_expression }) => todo!(),
            Any::Autorun(Autorun {
                effect_block,
                until,
            }) => {
                f.write_str(INT)?;
                f.write_str("autorun(() => {")?;
                effect_block.compile(ctx, f)?;
                f.write_str("});")
            }
            Any::PlainIdentifier(PlainIdentifier(name)) => f.write_str(name.as_str()),
            Any::DeclarationPlatforms(_) => Ok(()),
        }
    }
}

fn compile_string_contents<W: Write>(f: &mut W, contents: &Slice) -> Result {
    for (index, ch) in contents.as_str().char_indices() {
        if ch == '\\' && contents.as_str().chars().nth(index + 1) == Some('\'') {
            // do nothing
        } else {
            f.write_char(ch)?;
        }
    }

    Ok(())
}

fn compile_runtime_type<W: Write>(f: &mut W, typ: &AST<TypeExpression>) -> Result {
    match typ.downcast() {
        TypeExpression::UnionType(_) => todo!(),
        TypeExpression::MaybeType(_) => todo!(),
        TypeExpression::NamedType(_) => todo!(),
        TypeExpression::GenericParamType(_) => todo!(),
        TypeExpression::ProcType(_) => todo!(),
        TypeExpression::FuncType(_) => todo!(),
        TypeExpression::GenericType(_) => todo!(),
        TypeExpression::BoundGenericType(_) => todo!(),
        TypeExpression::ObjectType(_) => todo!(),
        TypeExpression::RecordType(_) => todo!(),
        TypeExpression::ArrayType(_) => todo!(),
        TypeExpression::TupleType(_) => todo!(),
        TypeExpression::SpecialType(_) => todo!(),
        TypeExpression::ModifierType(_) => todo!(),
        TypeExpression::TypeofType(_) => todo!(),
        TypeExpression::RegularExpressionType(_) => todo!(),
        TypeExpression::PropertyType(_) => todo!(),
        TypeExpression::StringLiteralType(StringLiteralType(value)) => {
            f.write_char('"')?;
            f.write_str(value.as_str())?;
            f.write_char('"')
        }
        TypeExpression::NumberLiteralType(NumberLiteralType(value)) => f.write_str(value.as_str()),
        TypeExpression::BooleanLiteralType(BooleanLiteralType(value)) => {
            f.write_str(if value { "true" } else { "false" })
        }
        TypeExpression::ParenthesizedType(ParenthesizedType(inner)) => {
            f.write_char('(')?;
            compile_runtime_type(f, &inner)?;
            f.write_char(')')
        }
        TypeExpression::StringType(_) => {
            f.write_str(INT)?;
            f.write_str("RT_STRING")
        }
        TypeExpression::NumberType(_) => {
            f.write_str(INT)?;
            f.write_str("RT_NUMBER")
        }
        TypeExpression::BooleanType(_) => {
            f.write_str(INT)?;
            f.write_str("RT_BOOLEAN")
        }
        TypeExpression::NilType(_) => {
            f.write_str(INT)?;
            f.write_str("RT_NIL")
        }
        TypeExpression::UnknownType(_) => {
            f.write_str(INT)?;
            f.write_str("RT_UNKNOWN")
        }
    }
}

// function compileRuntimeType(type: TypeExpression): string {
//     switch (type.kind) {
//         case 'unknown-type': return INT + 'RT_UNKNOWN';
//         case 'nil-type': return INT + 'RT_NIL';
//         case 'boolean-type': return INT + 'RT_BOOLEAN';
//         case 'number-type': return INT + 'RT_NUMBER';
//         case 'string-type': return INT + 'RT_STRING';
//         case 'literal-type': return `{ kind: ${INT}RT_LITERAL, value: ${JSON.stringify(type.value.value)} }`;
//         case 'array-type': return `{ kind: ${INT}RT_ARRAY, inner: ${compileRuntimeType(type.element)} }`;
//         case 'record-type': return `{ kind: ${INT}RT_RECORD, key: ${compileRuntimeType(type.keyType)}, value: ${compileRuntimeType(type.valueType)} }`;
//         case 'object-type': return `{ kind: ${INT}RT_OBJECT, entries: [${type.entries.map(({ name, type, optional }) =>
//             `{ key: '${getName(name)}', value: ${compileRuntimeType(type)}, optional: ${optional} }`
//         )}] }`;
//         case 'nominal-type': return `{ kind: ${INT}RT_NOMINAL, nominal: ${type.name}.sym }`
//         case 'error-type': return `{ kind: ${INT}RT_ERROR, inner: ${compileRuntimeType(type.inner)} }`
//         case 'iterator-type': return `{ kind: ${INT}RT_ITERATOR, inner: ${compileRuntimeType(type.inner)} }`
//         case 'plan-type': return `{ kind: ${INT}RT_PLAN, inner: ${compileRuntimeType(type.inner)} }`
//         case 'union-type': return `[ ${type.members.map(compileRuntimeType).join(', ')} ]`;
//     }

//     throw Error(`Couldn't runtime-compile type ${format(type)}`)
// }

impl Compilable for Arg {
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> Result {
        let Arg {
            name,
            type_annotation,
            optional,
        } = self;

        name.compile(ctx, f)?;
        if *optional {
            f.write_char('?')?;
        }
        compile_type_annotation(ctx, f, type_annotation.as_ref())?;

        Ok(())
    }
}

impl<T> Compilable for Option<T>
where
    T: Compilable,
{
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> Result {
        if let Some(sel) = self {
            sel.compile(ctx, f)?;
        }

        Ok(())
    }
}

impl<T> Compilable for Vec<T>
where
    T: Compilable,
{
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> Result {
        for el in self.iter() {
            el.compile(ctx, f)?;
        }

        Ok(())
    }
}

impl Compilable for DeclarationDestination {
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> Result {
        match self {
            DeclarationDestination::NameAndType(NameAndType {
                name,
                type_annotation,
            }) => {
                name.compile(ctx, f)?;
                if ctx.qualify_all_identifiers {
                    if let Some(id) = ctx.current_module_qualifier() {
                        f.write_fmt(format_args!("_{}", id))?;
                    }
                }
                if let Some(type_annotation) = type_annotation {
                    f.write_str(": ")?;
                    type_annotation.compile(ctx, f)?;
                }

                Ok(())
            }
            DeclarationDestination::Destructure(Destructure {
                properties,
                spread,
                destructure_kind,
            }) => {
                f.write_char(match destructure_kind {
                    DestructureKind::Array => '[',
                    DestructureKind::Object => '{',
                })?;

                for (index, property) in properties.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    property.compile(ctx, f)?;
                    if ctx.qualify_all_identifiers {
                        if let Some(id) = ctx.current_module_qualifier() {
                            f.write_fmt(format_args!("_{}", id))?;
                        }
                    }
                }

                if let Some(spread) = spread {
                    if properties.len() > 0 {
                        f.write_str(", ")?;
                    }

                    spread.compile(ctx, f)?;
                }

                f.write_char(match destructure_kind {
                    DestructureKind::Array => ']',
                    DestructureKind::Object => '}',
                })
            }
        }
    }
}

pub const INT: &str = "___";
pub const INT_FN: &str = "___fn_";
const NIL: &str = "undefined";

// --- Util functions ---

fn compile_function<'a, W: Write>(
    ctx: CompileContext<'a>,
    f: &mut W,
    name: Option<&str>,
    args: &Vec<AST<Arg>>,
    return_type_void: bool, // HACK
    return_type: Option<&AST<TypeExpression>>,
    body: &ASTAny,
) -> Result {
    f.write_str("function ")?;

    if let Some(name) = name {
        f.write_str(INT_FN)?;
        f.write_str(name)?;
    }

    f.write_char('(')?;
    for (index, arg) in args.iter().enumerate() {
        if index > 0 {
            f.write_str(", ")?;
        }

        arg.compile(ctx, f)?;
    }
    f.write_char(')')?;

    if ctx.include_types {
        if return_type_void {
            f.write_str(": void")?;
        } else {
            compile_type_annotation(ctx, f, return_type)?;
        }
    }

    f.write_char(' ')?;

    if let Any::Block(_) = body.details() {
        body.compile(ctx, f)?;
    } else {
        f.write_str("{ return ")?;
        body.compile(ctx, f)?;
        f.write_str(" }")?;
    }

    Ok(())
}

fn compile_type_annotation<'a, W: Write>(
    ctx: CompileContext<'a>,
    f: &mut W,
    type_annotation: Option<&AST<TypeExpression>>,
) -> Result {
    if ctx.include_types {
        if let Some(type_annotation) = type_annotation {
            f.write_str(": ")?;
            type_annotation.compile(ctx, f)?;
        }
    }

    Ok(())
}
