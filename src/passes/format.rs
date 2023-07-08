use crate::model::{ast::*, ParsedModule};
use std::fmt::{Display, Formatter, Result, Write};

#[derive(Debug, Clone, Copy)]
pub struct FormatContext {
    pub options: FormatOptions,
    pub current_indentation: usize,
}

impl FormatContext {
    pub fn indent(self) -> Self {
        FormatContext {
            options: self.options,
            current_indentation: self.current_indentation.saturating_add(1),
        }
    }

    pub fn unindent(self) -> Self {
        FormatContext {
            options: self.options,
            current_indentation: self.current_indentation.saturating_sub(1),
        }
    }

    pub fn write_indentation<W: Write>(&self, f: &mut W) -> Result {
        for _ in 0..self.current_indentation {
            f.write_str(self.options.indentation_chars)?;
        }

        Ok(())
    }
}

pub trait Formattable {
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result;
}

impl Formattable for ParsedModule {
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        match self {
            ParsedModule::Bagel { module_id: _, ast } => ast.format(f, ctx),
            ParsedModule::JavaScript { module_id: _ } => Ok(()),
            ParsedModule::Singleton {
                module_id: _,
                contents,
            } => f.write_str(contents.slice().as_str()),
        }
    }
}

impl<TKind> Formattable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        match self.details() {
            Any::Module(Module {
                module_id: _,
                declarations,
            }) => {
                for decl in declarations {
                    decl.format(f, ctx)?;
                    f.write_str("\n\n")?;
                }

                Ok(())
            }
            Any::ImportAllDeclaration(ImportAllDeclaration {
                platforms,
                name,
                path,
            }) => {
                f.write_str("import ")?;
                path.format(f, ctx)?;
                f.write_str(" as ")?;
                name.format(f, ctx)
            }
            Any::ImportDeclaration(ImportDeclaration {
                platforms,
                imports,
                path,
            }) => {
                f.write_str("from ")?;
                path.format(f, ctx)?;
                f.write_str(" import { ")?;
                format_multi(f, ctx, imports, ", ")?;
                f.write_str(" }")
            }
            Any::ImportItem(ImportItem { name, alias }) => {
                name.format(f, ctx)?;
                if let Some(alias) = alias {
                    f.write_str(" as ")?;
                    alias.format(f, ctx)?;
                }

                Ok(())
            }
            Any::TypeDeclaration(TypeDeclaration {
                name,
                declared_type,
                exported,
            }) => todo!(),
            Any::FuncDeclaration(FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            }) => {
                let type_annotation = func.downcast().type_annotation.downcast();

                write_if(f, "export ", *exported)?;
                f.write_str("func ")?;
                name.format(f, ctx)?;
                f.write_char('(')?;
                type_annotation.args.format(f, ctx)?;
                f.write_char(')')?;
                format_type_annotation(f, ctx, type_annotation.returns.as_ref())?;
                f.write_str(" => \n")?;

                let ctx = ctx.indent();
                ctx.write_indentation(f)?;
                func.downcast().body.format(f, ctx)
            }
            Any::ProcDeclaration(ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            }) => {
                let type_annotation = proc.downcast().type_annotation.downcast();

                write_if(f, "export ", *exported)?;
                f.write_str("proc ")?;
                name.format(f, ctx)?;

                f.write_char('(')?;
                type_annotation.args.format(f, ctx)?;
                f.write_char(')')?;
                if let Some(throws) = type_annotation.throws {
                    f.write_str(" throws ")?;
                    throws.format(f, ctx)?;
                }
                f.write_str(" |> ")?;

                proc.downcast().body.format(f, ctx)
            }
            Any::Decorator(Decorator { name, arguments }) => todo!(),
            Any::ValueDeclaration(ValueDeclaration {
                destination,
                value,
                is_const,
                exported,
                platforms,
            }) => {
                write_if(f, "export ", *exported)?;
                f.write_str(if *is_const { "const " } else { "let " })?;
                destination.format(f, ctx)?;
                f.write_str(" = ")?;
                value.format(f, ctx)
            }
            Any::SymbolDeclaration(SymbolDeclaration { name, exported }) => {
                write_if(f, "export ", *exported)?;
                f.write_str("symbol ")?;
                name.format(f, ctx)
            }
            Any::TestExprDeclaration(TestExprDeclaration {
                platforms,
                name,
                expr,
            }) => todo!(),
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
            Any::DeclarationPlatforms(DeclarationPlatforms { platforms }) => {
                f.write_char('[')?;
                format_multi(f, ctx, platforms, ", ")?;
                f.write_char(']')
            }
            Any::NilLiteral(_) => f.write_str("nil"),
            Any::BooleanLiteral(BooleanLiteral(value)) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            Any::NumberLiteral(NumberLiteral(value)) => f.write_str(value.as_str()),
            Any::StringLiteral(StringLiteral { tag, segments }) => {
                if let Some(tag) = tag {
                    tag.format(f, ctx)?;
                }
                f.write_str("'")?;
                format_multi(f, ctx, segments, "")?;
                f.write_str("'")
            }
            Any::ExactStringLiteral(ExactStringLiteral { tag, value }) => {
                if let Some(tag) = tag {
                    tag.format(f, ctx)?;
                }
                f.write_str("'")?;
                f.write_str(value.as_str())?;
                f.write_str("'")
            }
            Any::ArrayLiteral(ArrayLiteral(entries)) => {
                f.write_char('[')?;
                format_multi(f, ctx, entries, ", ")?;
                f.write_str(" ]")
            }
            Any::ObjectLiteral(ObjectLiteral(entries)) => {
                f.write_char('{')?;
                format_multi(f, ctx, entries, ", ")?;
                f.write_str(" }")
            }
            Any::SpreadExpression(SpreadExpression(inner)) => {
                f.write_str("...")?;
                inner.format(f, ctx)
            }
            Any::BinaryOperation(BinaryOperation { left, op, right }) => {
                left.format(f, ctx)?;
                f.write_char(' ')?;
                op.format(f, ctx)?;
                f.write_char(' ')?;
                right.format(f, ctx)
            }
            Any::BinaryOperator(BinaryOperator(op)) => f.write_str(op.into()),
            Any::NegationOperation(NegationOperation(inner)) => {
                f.write_char('!')?;
                inner.format(f, ctx)
            }
            Any::Parenthesis(Parenthesis(inner)) => {
                f.write_char('(')?;
                inner.format(f, ctx)?;
                f.write_char(')')
            }
            Any::LocalIdentifier(LocalIdentifier(name)) => f.write_str(name.as_str()),
            Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner,
            }) => {
                for dec in declarations {
                    dec.format(f, ctx)?;
                    f.write_str(",\n")?;
                    ctx.write_indentation(f)?;
                }

                inner.format(f, ctx)
            }
            Any::InlineDeclaration(InlineDeclaration { destination, value }) => {
                f.write_str("const ")?;
                destination.format(f, ctx)?;
                f.write_str(" = ")?;
                value.format(f, ctx)
            }
            Any::Func(Func {
                type_annotation,
                is_async,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                f.write_char('(')?;
                type_annotation.args.format(f, ctx)?;
                f.write_char(')')?;
                format_type_annotation(f, ctx, type_annotation.returns.as_ref())?;
                f.write_str(" => \n")?;

                let ctx = ctx.indent();
                body.format(f, ctx)
            }
            Any::Proc(Proc {
                type_annotation,
                is_async,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                f.write_char('(')?;
                type_annotation.args.format(f, ctx)?;
                f.write_char(')')?;
                if let Some(throws) = type_annotation.throws {
                    f.write_str(" throws ")?;
                    throws.format(f, ctx)?;
                }
                f.write_str(" |> ")?;

                body.format(f, ctx)
            }
            Any::Block(Block(statements)) => {
                f.write_str("{\n")?;
                let ctx = ctx.indent();

                for stmt in statements {
                    ctx.write_indentation(f)?;
                    stmt.format(f, ctx)?;
                    match stmt.downcast() {
                        Statement::Invocation(_) => f.write_char(';')?,
                        Statement::Assignment(_) => f.write_char(';')?,
                        Statement::ValueDeclaration(_) => f.write_char(';')?,
                        Statement::ThrowStatement(_) => f.write_char(';')?,
                        _ => {}
                    };
                    f.write_char('\n')?;
                }
                f.write_char('}')
            }
            Any::RangeExpression(RangeExpression { start, end }) => {
                start.format(f, ctx)?;
                f.write_str("..")?;
                end.format(f, ctx)
            }
            Any::AwaitExpression(AwaitExpression(inner)) => {
                f.write_str("await ")?;
                inner.format(f, ctx)
            }
            Any::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            }) => {
                if let Some(awaited_or_detached) = awaited_or_detached {
                    f.write_str(awaited_or_detached.into())?;
                }

                subject.format(f, ctx)?;

                if type_args.len() > 0 {
                    f.write_char('<')?;
                    format_multi(f, ctx, type_args, ", ")?;
                    f.write_char('>')?;
                }

                f.write_char('(')?;
                format_multi(f, ctx, args, ", ")?;

                if let Some(spread_args) = spread_args {
                    if args.len() > 0 {
                        f.write_str(", ")?;
                    }

                    spread_args.format(f, ctx)?;
                }

                f.write_char(')')?;

                if *bubbles {
                    f.write_char('?')?;
                }

                Ok(())
            }
            Any::PropertyAccessor(PropertyAccessor {
                subject,
                property,
                optional,
            }) => {
                subject.format(f, ctx)?;

                match property {
                    Property::Expression(expr) => {
                        if *optional {
                            f.write_str("?.")?;
                        }

                        f.write_char('[')?;
                        expr.format(f, ctx)?;
                        f.write_char(']')
                    }
                    Property::PlainIdentifier(ident) => {
                        if *optional {
                            f.write_str("?")?;
                        }

                        f.write_char('.')?;
                        ident.format(f, ctx)
                    }
                }
            }
            Any::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            }) => {
                format_multi(f, ctx, cases, " else ")?;

                if let Some(default_case) = default_case {
                    f.write_str(" else {\n")?;
                    let ctx = ctx.indent();
                    ctx.write_indentation(f)?;
                    default_case.format(f, ctx)?;
                    ctx.write_indentation(f)?;
                    f.write_str("\n}")?;
                }

                Ok(())
            }
            Any::IfElseExpressionCase(IfElseExpressionCase { condition, outcome }) => {
                f.write_str("if ")?;
                condition.format(f, ctx)?;
                f.write_str(" {\n")?;
                let ctx = ctx.indent();
                ctx.write_indentation(f)?;
                outcome.format(f, ctx)?;
                f.write_str("\n}")
            }
            Any::SwitchExpression(SwitchExpression {
                value,
                cases,
                default_case,
            }) => todo!(),
            Any::SwitchExpressionCase(SwitchExpressionCase {
                type_filter,
                outcome,
            }) => todo!(),
            Any::ElementTag(ElementTag {
                tag_name,
                attributes,
                children,
            }) => todo!(),
            Any::AsCast(AsCast { inner, as_type }) => todo!(),
            Any::InstanceOf(InstanceOf {
                inner,
                possible_type,
            }) => todo!(),
            Any::ErrorExpression(ErrorExpression(_)) => todo!(),
            Any::RegularExpression(RegularExpression { expr, flags }) => todo!(),
            Any::AnyLiteral(AnyLiteral) => f.write_str("nil"),
            Any::UnionType(UnionType(members)) => format_multi(f, ctx, members, " | "),
            Any::MaybeType(MaybeType(inner)) => {
                inner.format(f, ctx)?;
                f.write_char('?')
            }
            Any::NamedType(NamedType(name)) => name.format(f, ctx),
            Any::GenericParamType(GenericParamType { name, extends }) => todo!(),
            Any::ProcType(ProcType {
                type_params,
                args,
                args_spread,
                is_async,
                throws,
            }) => {
                if type_params.len() > 0 {
                    f.write_char('<')?;
                    format_multi(f, ctx, type_params, ", ")?;
                    f.write_char('>')?;
                }

                f.write_char('(')?;
                format_multi(f, ctx, args, ", ")?;
                f.write_char(')')?;

                if let Some(throws) = throws {
                    f.write_str(" throws ")?;
                    throws.format(f, ctx)?;
                }

                f.write_str(" |> {}")
            }
            Any::FuncType(FuncType {
                type_params,
                args,
                args_spread,
                returns,
            }) => {
                if type_params.len() > 0 {
                    f.write_char('<')?;
                    format_multi(f, ctx, type_params, ", ")?;
                    f.write_char('>')?;
                }

                f.write_char('(')?;
                format_multi(f, ctx, args, ", ")?;
                f.write_str(") => ")?;

                if let Some(returns) = returns {
                    returns.format(f, ctx)?;
                } else {
                    f.write_str("<none>")?;
                }

                Ok(())
            }
            Any::Arg(Arg {
                name,
                type_annotation,
                optional,
            }) => {
                name.format(f, ctx)?;
                if *optional {
                    f.write_char('?')?;
                }
                format_type_annotation(f, ctx, type_annotation.as_ref())
            }
            Any::GenericType(GenericType { type_params, inner }) => todo!(),
            Any::TypeParam(TypeParam { name, extends }) => {
                name.format(f, ctx)?;

                if let Some(extends) = extends {
                    f.write_str(" extends ")?;
                    extends.format(f, ctx)?;
                }

                Ok(())
            }
            Any::BoundGenericType(BoundGenericType { type_args, generic }) => todo!(),
            Any::ObjectType(ObjectType {
                entries,
                is_interface,
            }) => todo!(),
            Any::RecordType(RecordType {
                key_type,
                value_type,
            }) => todo!(),
            Any::ArrayType(ArrayType(_)) => todo!(),
            Any::TupleType(TupleType(_)) => todo!(),
            Any::SpecialType(SpecialType { kind, inner }) => {
                f.write_str(kind.into())?;
                f.write_char('<')?;
                inner.format(f, ctx)?;
                f.write_char('>')
            }
            Any::ModifierType(ModifierType { kind, inner }) => {
                f.write_str(kind.into())?;
                f.write_char(' ')?;
                inner.format(f, ctx)
            }
            Any::StringLiteralType(StringLiteralType(value)) => {
                f.write_char('\'')?;
                f.write_str(value.as_str())?;
                f.write_char('\'')
            }
            Any::NumberLiteralType(NumberLiteralType(value)) => f.write_str(value.as_str()),
            Any::BooleanLiteralType(BooleanLiteralType(value)) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            Any::StringType(_) => f.write_str("string"),
            Any::NumberType(_) => f.write_str("number"),
            Any::BooleanType(_) => f.write_str("boolean"),
            Any::NilType(_) => f.write_str("nil"),
            Any::UnknownType(_) => f.write_str("unknown"),
            Any::ParenthesizedType(ParenthesizedType(inner)) => {
                f.write_char('(')?;
                inner.format(f, ctx)?;
                f.write_char(')')
            }
            Any::TypeofType(TypeofType(inner)) => {
                f.write_str("typeof ")?;
                inner.format(f, ctx)
            }
            Any::RegularExpressionType(_) => todo!(),
            Any::PropertyType(PropertyType {
                subject,
                property,
                optional,
            }) => todo!(),
            Any::IfElseStatement(IfElseStatement {
                cases,
                default_case,
            }) => {
                format_multi(f, ctx, cases, " else ")?;

                if let Some(default_case) = default_case {
                    f.write_str(" else ")?;
                    default_case.format(f, ctx)?;
                }

                Ok(())
            }
            Any::IfElseStatementCase(IfElseStatementCase { condition, outcome }) => {
                f.write_str("if ")?;
                condition.format(f, ctx)?;
                f.write_str(" ")?;
                outcome.format(f, ctx)
            }
            Any::ForLoop(ForLoop {
                item_identifier,
                iterable,
                body,
            }) => todo!(),
            Any::WhileLoop(WhileLoop { condition, body }) => todo!(),
            Any::Assignment(Assignment {
                target,
                value,
                operator,
            }) => {
                target.format(f, ctx)?;
                f.write_char(' ')?;
                if let Some(operator) = operator {
                    operator.format(f, ctx)?;
                }
                f.write_str("= ")?;
                value.format(f, ctx)
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
            }) => todo!(),
            Any::PlainIdentifier(PlainIdentifier(name)) => f.write_str(name.as_str()),
        }
    }
}

impl Formattable for Arg {
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        let Arg {
            name,
            type_annotation,
            optional,
        } = self;

        name.format(f, ctx)?;
        if *optional {
            f.write_char('?')?;
        }
        format_type_annotation(f, ctx, type_annotation.as_ref())?;

        Ok(())
    }
}

impl<T: Formattable> Formattable for ElementOrSpread<T> {
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        match self {
            ElementOrSpread::Element(element) => element.format(f, ctx),
            ElementOrSpread::Spread(spread) => {
                f.write_str("...")?;
                spread.format(f, ctx)
            }
        }
    }
}

impl<T: Formattable> Formattable for KeyValueOrSpread<T> {
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        match self {
            KeyValueOrSpread::KeyValue(key, value, _) => {
                key.format(f, ctx)?;
                f.write_str(": ")?;
                value.format(f, ctx)
            }
            KeyValueOrSpread::Spread(expr) => {
                f.write_str("...")?;
                expr.format(f, ctx)
            }
        }
    }
}

impl Formattable for StringLiteralSegment {
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        match self {
            StringLiteralSegment::Slice(s) => f.write_str(s.as_str()),
            StringLiteralSegment::AST(expr) => {
                f.write_str("${")?;
                expr.format(f, ctx)?;
                f.write_str("}")
            }
        }
    }
}

impl Formattable for DeclarationDestination {
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        match self {
            DeclarationDestination::NameAndType(NameAndType {
                name,
                type_annotation,
            }) => {
                name.format(f, ctx)?;
                if let Some(type_annotation) = type_annotation {
                    f.write_str(": ")?;
                    type_annotation.format(f, ctx)?;
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

                    property.format(f, ctx)?;
                }

                if let Some(spread) = spread {
                    if properties.len() > 0 {
                        f.write_str(", ")?;
                    }

                    spread.format(f, ctx)?;
                }

                f.write_char(match destructure_kind {
                    DestructureKind::Array => ']',
                    DestructureKind::Object => '}',
                })
            }
        }
    }
}

impl<T> Formattable for Option<T>
where
    T: Formattable,
{
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        if let Some(sel) = self {
            sel.format(f, ctx)?;
        }

        Ok(())
    }
}

impl<T> Formattable for Vec<T>
where
    T: Formattable,
{
    fn format<W: Write>(&self, f: &mut W, ctx: FormatContext) -> Result {
        for el in self.iter() {
            el.format(f, ctx)?;
        }

        Ok(())
    }
}

fn format_type_annotation<W: Write>(
    f: &mut W,
    ctx: FormatContext,
    type_annotation: Option<&AST<TypeExpression>>,
) -> Result {
    if let Some(type_annotation) = type_annotation {
        f.write_str(": ")?;
        type_annotation.format(f, ctx)?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {
    indentation_chars: &'static str,
}

impl FormatOptions {
    pub const DEFAULT: FormatOptions = FormatOptions {
        indentation_chars: "  ",
    };
}

impl<TKind> Display for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(
            f,
            FormatContext {
                options: FormatOptions::DEFAULT,
                current_indentation: 0,
            },
        )
    }
}

fn format_multi<W: Write, T: Formattable>(
    f: &mut W,
    ctx: FormatContext,
    items: &Vec<T>,
    sep: &str,
) -> Result {
    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            f.write_str(sep)?;
        }

        item.format(f, ctx)?;
    }

    Ok(())
}

fn write_if<W: Write>(f: &mut W, s: &str, condition: bool) -> Result {
    if condition {
        f.write_str(s)?;
    }

    Ok(())
}
