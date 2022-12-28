use crate::model::{ast::*, module::Module};
use std::fmt::{Display, Formatter, Result, Write};

impl Module {
    pub fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        self.ast.format(f, opts)
    }
}

impl Display for ASTAny {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

pub trait Formattable {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result;
}

impl<TKind> Formattable for AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.details() {
            ASTDetails::Module { declarations } => {
                for decl in declarations {
                    decl.format(f, opts)?;
                    f.write_str("\n\n")?;
                }

                Ok(())
            }
            ASTDetails::ImportAllDeclaration { name, path } => todo!(),
            ASTDetails::ImportDeclaration { imports, path } => todo!(),
            ASTDetails::ImportItem { name, alias } => todo!(),
            ASTDetails::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => todo!(),
            ASTDetails::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => todo!(),
            ASTDetails::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => todo!(),
            ASTDetails::Decorator { name } => todo!(),
            ASTDetails::ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            } => {
                if *exported {
                    f.write_str("export ")?;
                }

                f.write_str(if *is_const { "const " } else { "let " })?;
                name.format(f, opts)?;
                if let Some(type_annotation) = type_annotation {
                    f.write_str(": ")?;
                    type_annotation.format(f, opts)?;
                }
                f.write_str(" = ")?;
                value.format(f, opts)
            }
            ASTDetails::TestExprDeclaration { name, expr } => todo!(),
            ASTDetails::TestBlockDeclaration { name, block } => todo!(),
            ASTDetails::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => todo!(),
            ASTDetails::NilLiteral => f.write_str("nil"),
            ASTDetails::BooleanLiteral(value) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            ASTDetails::NumberLiteral(value) => f.write_str(value.as_str()),
            ASTDetails::StringLiteral { tag, segments } => todo!(),
            ASTDetails::ExactStringLiteral { tag, value } => {
                f.write_str("\"")?;
                f.write_str(value.as_str())?;
                f.write_str("\"")
            }
            ASTDetails::ArrayLiteral(entries) => {
                f.write_char('[')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    entry.format(f, opts)?;
                }
                f.write_str(" ]")
            }
            ASTDetails::ObjectLiteral(entries) => {
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;

                    match entry {
                        ObjectLiteralEntry::KeyValue(KeyValue { key, value }) => {
                            key.format(f, opts)?;
                            f.write_str(": ")?;
                            value.format(f, opts)?;
                        }
                        ObjectLiteralEntry::Spread(Spread(expr)) => {
                            f.write_str("...")?;
                            expr.format(f, opts)?;
                        }
                    }
                }
                f.write_str(" }")
            }
            ASTDetails::BinaryOperation { left, op, right } => {
                left.format(f, opts)?;
                f.write_char(' ')?;
                op.format(f, opts)?;
                f.write_char(' ')?;
                right.format(f, opts)
            }
            ASTDetails::BinaryOperator(op) => f.write_str(op.into()),
            ASTDetails::NegationOperation(_) => todo!(),
            ASTDetails::Parenthesis(inner) => {
                f.write_char('(')?;
                inner.format(f, opts)?;
                f.write_char(')')
            }
            ASTDetails::LocalIdentifier(name) => f.write_str(name.as_str()),
            ASTDetails::InlineConstGroup {
                declarations,
                inner,
            } => todo!(),
            ASTDetails::InlineDeclaration {
                destination,
                awaited,
                value,
            } => todo!(),
            ASTDetails::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => {
                let type_annotation = type_annotation.downcast();

                f.write_char('(')?;
                type_annotation.args.format(f, opts)?;
                f.write_char(')')?;
                format_type_annotation(f, opts, type_annotation.returns.as_ref())?;
                f.write_str(" => ")?;

                body.format(f, opts)
            }
            ASTDetails::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            ASTDetails::Block(_) => todo!(),
            ASTDetails::JavascriptEscape(_) => todo!(),
            ASTDetails::RangeExpression { start, end } => todo!(),
            ASTDetails::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            ASTDetails::PropertyAccessor {
                subject,
                property,
                optional,
            } => todo!(),
            ASTDetails::IfElseExpression {
                cases,
                default_case,
            } => todo!(),
            ASTDetails::IfElseExpressionCase { condition, outcome } => todo!(),
            ASTDetails::SwitchExpression {
                value,
                cases,
                default_case,
            } => todo!(),
            ASTDetails::SwitchExpressionCase {
                type_filter,
                outcome,
            } => todo!(),
            ASTDetails::ElementTag {
                tag_name,
                attributes,
                children,
            } => todo!(),
            ASTDetails::AsCast { inner, as_type } => todo!(),
            ASTDetails::InstanceOf {
                inner,
                possible_type,
            } => todo!(),
            ASTDetails::ErrorExpression(_) => todo!(),
            ASTDetails::RegularExpression { expr, flags } => todo!(),
            ASTDetails::UnionType(members) => {
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }

                    member.format(f, opts)?;
                }

                Ok(())
            }
            ASTDetails::MaybeType(_) => todo!(),
            ASTDetails::NamedType(_) => todo!(),
            ASTDetails::GenericParamType { name, extends } => todo!(),
            ASTDetails::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            ASTDetails::FuncType {
                args,
                args_spread,
                is_pure,
                is_async,
                returns,
            } => {
                f.write_char('(')?;
                for arg in args {
                    arg.format(f, opts)?;
                }
                f.write_char(')')?;

                f.write_str(" => ")?;

                if let Some(returns) = returns {
                    returns.format(f, opts)?;
                } else {
                    f.write_str("<none>")?;
                }

                Ok(())
            }
            ASTDetails::Arg {
                name,
                type_annotation,
                optional,
            } => {
                name.format(f, opts)?;
                if *optional {
                    f.write_char('?')?;
                }
                format_type_annotation(f, opts, type_annotation.as_ref())
            }
            ASTDetails::GenericType { type_params, inner } => todo!(),
            ASTDetails::TypeParam { name, extends } => todo!(),
            ASTDetails::BoundGenericType { type_args, generic } => todo!(),
            ASTDetails::ObjectType {
                entries,
                is_interface,
            } => todo!(),
            ASTDetails::RecordType {
                key_type,
                value_type,
            } => todo!(),
            ASTDetails::ArrayType(_) => todo!(),
            ASTDetails::TupleType(_) => todo!(),
            ASTDetails::SpecialType { kind, inner } => todo!(),
            ASTDetails::ModifierType { kind, inner } => todo!(),
            ASTDetails::StringLiteralType(value) => {
                f.write_char('\'')?;
                f.write_str(value.as_str())?;
                f.write_char('\'')
            }
            ASTDetails::NumberLiteralType(value) => f.write_str(value.as_str()),
            ASTDetails::BooleanLiteralType(value) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            ASTDetails::StringType => f.write_str("string"),
            ASTDetails::NumberType => f.write_str("number"),
            ASTDetails::BooleanType => f.write_str("boolean"),
            ASTDetails::NilType => f.write_str("nil"),
            ASTDetails::ParenthesizedType(_) => todo!(),
            ASTDetails::TypeofType(_) => todo!(),
            ASTDetails::UnknownType => todo!(),
            ASTDetails::RegularExpressionType => todo!(),
            ASTDetails::PropertyType {
                subject,
                property,
                optional,
            } => todo!(),
            ASTDetails::DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            } => todo!(),
            ASTDetails::IfElseStatement {
                cases,
                default_case,
            } => todo!(),
            ASTDetails::IfElseStatementCase { condition, outcome } => todo!(),
            ASTDetails::ForLoop {
                item_identifier,
                iterator,
                body,
            } => todo!(),
            ASTDetails::WhileLoop { condition, body } => todo!(),
            ASTDetails::Assignment {
                target,
                value,
                operator,
            } => todo!(),
            ASTDetails::TryCatch {
                try_block,
                error_identifier,
                catch_block,
            } => todo!(),
            ASTDetails::ThrowStatement { error_expression } => todo!(),
            ASTDetails::Autorun {
                effect_block,
                until,
            } => todo!(),
            ASTDetails::PlainIdentifier(name) => f.write_str(name.as_str()),
        }
    }
}

impl Formattable for Arg {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        let Arg {
            name,
            type_annotation,
            optional,
        } = self;

        name.format(f, opts)?;
        if *optional {
            f.write_char('?')?;
        }
        format_type_annotation(f, opts, type_annotation.as_ref())?;

        Ok(())
    }
}

impl<T> Formattable for Option<T>
where
    T: Formattable,
{
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        if let Some(sel) = self {
            sel.format(f, opts);
        }

        Ok(())
    }
}

impl<T> Formattable for Vec<T>
where
    T: Formattable,
{
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        for el in self.iter() {
            el.format(f, opts);
        }

        Ok(())
    }
}

fn format_type_annotation<W: Write>(
    f: &mut W,
    opts: FormatOptions,
    type_annotation: Option<&ASTAny>,
) -> Result {
    if let Some(type_annotation) = type_annotation {
        f.write_str(": ")?;
        type_annotation.format(f, opts)?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {}

impl FormatOptions {
    pub const DEFAULT: FormatOptions = FormatOptions {};
}
