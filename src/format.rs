use crate::ast::*;

use std::fmt::{Display, Formatter, Result, Write};

pub trait Format {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result;

    fn to_string(&self) -> String {
        let mut buf = String::new();
        self.format(&mut buf, FormatOptions::DEFAULT).unwrap();
        buf
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {}

impl FormatOptions {
    pub const DEFAULT: FormatOptions = FormatOptions {};
}

impl<'a> Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for Module {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        for decl in &self.declarations {
            decl.format(f, opts)?;
        }

        Ok(())
    }
}

impl<'a> Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for Declaration {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self {
            Declaration::ImportAllDeclaration { src: _, name, path } => todo!(),
            Declaration::ImportDeclaration {
                src: _,
                imports,
                path,
            } => todo!(),
            Declaration::TypeDeclaration {
                src: _,
                name,
                declared_type,
            } => todo!(),
            Declaration::FuncDeclaration {
                src: _,
                name,
                func,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                src: _,
                name,
                proc,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ValueDeclaration {
                src: _,
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
                value.format(f, opts)?;
            }
            Declaration::TestExprDeclaration { src: _, name, expr } => todo!(),
            Declaration::TestBlockDeclaration {
                src: _,
                name,
                block,
            } => todo!(),
            Declaration::TestTypeDeclaration {
                src: _,
                name,
                destination_type,
                value_type,
            } => todo!(),
        };

        Ok(())
    }
}

impl<'a> Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for Expression {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self {
            Expression::NilLiteral { src: _ } => f.write_str("nil")?,
            Expression::BooleanLiteral { src: _, value } => f.write_str(match value {
                true => "true",
                false => "false",
            })?,
            Expression::NumberLiteral { src: _, value } => f.write_str(value.as_str())?,
            Expression::StringLiteral {
                src: _,
                tag,
                segments,
            } => todo!(),
            Expression::ExactStringLiteral { src: _, tag, value } => {
                f.write_str("\"")?;
                f.write_str(value.as_str())?;
                f.write_str("\"")?;
            }
            Expression::ArrayLiteral { src: _, entries } => {
                f.write_char('[')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    entry.format(f, opts)?;
                }
                f.write_str(" ]")?;
            }
            Expression::ObjectLiteral { src: _, entries } => {
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    entry.format(f, opts)?;
                }
                f.write_str(" }")?;
            }
            Expression::BinaryOperation {
                src: _,
                left,
                op,
                right,
            } => {
                left.format(f, opts)?;
                f.write_char(' ')?;
                op.format(f, opts)?;
                f.write_char(' ')?;
                right.format(f, opts)?;
            }
            Expression::NegationOperation { src: _, inner } => todo!(),
            Expression::Parenthesis { src: _, inner } => {
                f.write_char('(')?;
                inner.format(f, opts)?;
                f.write_char(')')?;
            }
            Expression::LocalIdentifier { src: _, name } => f.write_str(name.as_str())?,
            Expression::InlineConstGroup {
                src: _,
                declarations,
                inner,
            } => todo!(),
            Expression::Func {
                src: _,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                src: _,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                src: _,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                src: _,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { src: _, start, end } => todo!(),
            Expression::Invocation {
                src: _,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                src: _,
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                src: _,
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                src: _,
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                src: _,
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast {
                src: _,
                inner,
                as_type,
            } => todo!(),
            Expression::ErrorExpression { src: _, inner } => todo!(),
            Expression::RegularExpression {
                src: _,
                expr,
                flags,
            } => todo!(),
        };

        Ok(())
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for BinaryOperator {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_str(self.symbol())
    }
}

impl<'a> Display for ArrayLiteralEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for ArrayLiteralEntry {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self {
            ArrayLiteralEntry::Spread(spread) => {
                f.write_str("...")?;
                f.write_str(spread.name.as_str())
            }
            ArrayLiteralEntry::Element(element) => element.format(f, opts),
        }
    }
}

impl<'a> Display for ObjectLiteralEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for ObjectLiteralEntry {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self {
            ObjectLiteralEntry::Variable(x) => x.format(f, opts),
            ObjectLiteralEntry::Spread(spread) => {
                f.write_str("...")?;
                f.write_str(spread.name.as_str())
            }
            ObjectLiteralEntry::KeyValue(key, value) => {
                key.format(f, opts)?;
                f.write_str(": ")?;
                value.format(f, opts)
            }
        }
    }
}

impl<'a> Display for LocalIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for LocalIdentifier {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_str(self.name.as_str())
    }
}

impl<'a> Display for PlainIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for PlainIdentifier {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_str(self.name.as_str())
    }
}

impl<'a> Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for Statement {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self {
            Statement::DeclarationStatement {
                src: _,
                destination,
                value,
                awaited,
                is_const,
            } => todo!(),
            Statement::IfElseStatement {
                src: _,
                cases,
                default_case,
            } => todo!(),
            Statement::ForLoop {
                src: _,
                item_identifier,
                iterator,
                body,
            } => todo!(),
            Statement::WhileLoop {
                src: _,
                condition,
                body,
            } => todo!(),
            Statement::Assignment {
                src: _,
                target,
                value,
                operator,
            } => todo!(),
            Statement::TryCatch {
                src: _,
                try_block,
                error_identifier,
                catch_block,
            } => todo!(),
            Statement::ThrowStatement {
                src: _,
                error_expression,
            } => todo!(),
            Statement::Autorun {
                src: _,
                effect,
                until,
            } => todo!(),
            Statement::InvocationStatement(_) => todo!(),
        };

        Ok(())
    }
}

impl<'a> Display for TypeExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl<'a> Format for TypeExpression {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self {
            TypeExpression::UnionType { src: _, members } => {
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }

                    member.format(f, opts)?;
                }
            }
            TypeExpression::MaybeType { src: _, inner } => todo!(),
            TypeExpression::NamedType { src: _, name } => todo!(),
            TypeExpression::GenericParamType {
                src: _,
                name,
                extends,
            } => todo!(),
            TypeExpression::ProcType {
                src: _,
                args,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            TypeExpression::FuncType {
                src: _,
                args,
                is_pure,
                returns,
            } => todo!(),
            TypeExpression::GenericType {
                src: _,
                type_params,
                inner,
            } => todo!(),
            TypeExpression::BoundGenericType {
                src: _,
                type_args,
                generic,
            } => todo!(),
            TypeExpression::ObjectType {
                src: _,
                entries,
                mutability,
            } => todo!(),
            TypeExpression::InterfaceType {
                src: _,
                entries,
                mutability,
            } => todo!(),
            TypeExpression::RecordType {
                src: _,
                key_type,
                value_type,
                mutability,
            } => todo!(),
            TypeExpression::ArrayType {
                src: _,
                element,
                mutability,
            } => todo!(),
            TypeExpression::TupleType {
                src: _,
                members,
                mutability,
            } => todo!(),
            TypeExpression::ReadonlyType { src: _, inner } => todo!(),
            TypeExpression::StringType { src: _ } => f.write_str("string")?,
            TypeExpression::NumberType { src: _ } => f.write_str("number")?,
            TypeExpression::BooleanType { src: _ } => f.write_str("boolean")?,
            TypeExpression::NilType { src: _ } => f.write_str("nil")?,
            TypeExpression::LiteralType { src: _, value } => todo!(),
            TypeExpression::NominalType {
                src: _,
                name,
                inner,
            } => todo!(),
            TypeExpression::IteratorType { src: _, inner } => todo!(),
            TypeExpression::PlanType { src: _, inner } => todo!(),
            TypeExpression::ErrorType { src: _, inner } => todo!(),
            TypeExpression::ParenthesizedType { src: _, inner } => todo!(),
            TypeExpression::TypeofType { src: _, expression } => todo!(),
            TypeExpression::KeyofType { src: _, inner } => todo!(),
            TypeExpression::ValueofType { src: _, inner } => todo!(),
            TypeExpression::ElementofType { src: _, inner } => todo!(),
            TypeExpression::UnknownType { src: _, mutability } => todo!(),
            TypeExpression::PoisonedType { src: _ } => todo!(),
            TypeExpression::AnyType { src: _ } => todo!(),
            TypeExpression::RegularExpressionType { src: _ } => todo!(),
            TypeExpression::PropertyType {
                src: _,
                subject,
                property,
                optional,
            } => todo!(),
        };

        Ok(())
    }
}
