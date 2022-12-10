use crate::model::{ast::*, module::Module};

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

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for Module {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        for decl in &self.declarations {
            decl.format(f, opts)?;
        }

        Ok(())
    }
}

impl Display for Node<Declaration> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for Node<Declaration> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.this() {
            Declaration::ImportAllDeclaration { name, path } => todo!(),
            Declaration::ImportDeclaration { imports, path } => todo!(),
            Declaration::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => todo!(),
            Declaration::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ValueDeclaration {
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
            Declaration::TestExprDeclaration { name, expr } => todo!(),
            Declaration::TestBlockDeclaration { name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => todo!(),
        };

        Ok(())
    }
}

impl Display for Node<Expression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for Node<Expression> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.this() {
            Expression::NilLiteral => f.write_str("nil")?,
            Expression::BooleanLiteral(value) => f.write_str(match value {
                true => "true",
                false => "false",
            })?,
            Expression::NumberLiteral(value) => f.write_str(value.as_str())?,
            Expression::StringLiteral { tag, segments } => todo!(),
            Expression::ExactStringLiteral { tag, value } => {
                f.write_str("\"")?;
                f.write_str(value.as_str())?;
                f.write_str("\"")?;
            }
            Expression::ArrayLiteral(entries) => {
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
            Expression::ObjectLiteral(entries) => {
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
            Expression::BinaryOperation { left, op, right } => {
                left.format(f, opts)?;
                f.write_char(' ')?;
                op.format(f, opts)?;
                f.write_char(' ')?;
                right.format(f, opts)?;
            }
            Expression::NegationOperation(inner) => todo!(),
            Expression::Parenthesis(inner) => {
                f.write_char('(')?;
                inner.format(f, opts)?;
                f.write_char(')')?;
            }
            Expression::LocalIdentifier(name) => f.write_str(name.as_str())?,
            Expression::InlineConstGroup {
                declarations,
                inner,
            } => todo!(),
            Expression::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { start, end } => todo!(),
            Expression::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast { inner, as_type } => todo!(),
            Expression::InstanceOf {
                inner,
                possible_type,
            } => todo!(),
            Expression::ErrorExpression(inner) => todo!(),
            Expression::RegularExpression { expr, flags } => todo!(),
        };

        Ok(())
    }
}

impl Format for Node<Func> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_char('(')?;
        for arg in &self.this().type_annotation.this().args {
            arg.format(f, opts)?;
        }
        if let Some(spread) = &self.this().type_annotation.this().args_spread {
            f.write_str("...")?;
            spread.format(f, opts)?;
        }
        f.write_char(')')?;
        if let Some(return_type) = &self.this().type_annotation.this().returns {
            f.write_str(": ")?;
            return_type.format(f, opts)?;
        }
        f.write_str(" => ")?;

        match &self.this().body {
            FuncBody::Expression(expr) => expr.format(f, opts),
            FuncBody::Js(_) => todo!(),
        }
    }
}

impl Format for Node<Arg> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_str(self.this().name.0.as_str())?;

        if let Some(type_annotation) = &self.this().type_annotation {
            if self.this().optional {
                f.write_char('?')?;
            }

            f.write_str(": ")?;
            type_annotation.format(f, opts)?;
        }

        Ok(())
    }
}

impl Display for Node<BinaryOperator> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for Node<BinaryOperator> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_str(self.this().into())
    }
}

impl Display for Node<ArrayLiteralEntry> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for Node<ArrayLiteralEntry> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.this() {
            ArrayLiteralEntry::Spread(spread) => {
                f.write_str("...")?;
                f.write_str(spread.0.as_str())
            }
            ArrayLiteralEntry::Element(element) => element
                .clone()
                .with_slice(self.slice.clone())
                .format(f, opts),
        }
    }
}

impl Display for Node<ObjectLiteralEntry> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for Node<ObjectLiteralEntry> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.this() {
            ObjectLiteralEntry::Variable(x) => x.format(f, opts),
            ObjectLiteralEntry::Spread(spread) => {
                f.write_str("...")?;
                f.write_str(spread.0.as_str())
            }
            ObjectLiteralEntry::KeyAndValue(key, value) => {
                key.format(f, opts)?;
                f.write_str(": ")?;
                value.format(f, opts)
            }
        }
    }
}

impl Display for LocalIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for LocalIdentifier {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_str(self.0.as_str())
    }
}

impl Display for PlainIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for PlainIdentifier {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        f.write_str(self.0.as_str())
    }
}

impl Display for Node<Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for IdentifierOrExpression {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        todo!()
    }
}

impl Format for Node<Statement> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.this() {
            Statement::DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            } => todo!(),
            Statement::IfElseStatement {
                cases,
                default_case,
            } => todo!(),
            Statement::ForLoop {
                item_identifier,
                iterator,
                body,
            } => todo!(),
            Statement::WhileLoop { condition, body } => todo!(),
            Statement::Assignment {
                target,
                value,
                operator,
            } => todo!(),
            Statement::TryCatch {
                try_block,
                error_identifier,
                catch_block,
            } => todo!(),
            Statement::ThrowStatement { error_expression } => todo!(),
            Statement::Autorun { effect, until } => todo!(),
            Statement::InvocationStatement(_) => todo!(),
        };

        Ok(())
    }
}

impl Display for Node<TypeExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl Format for Node<TypeExpression> {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.this() {
            TypeExpression::UnionType(members) => {
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }

                    member.format(f, opts)?;
                }
            }
            TypeExpression::MaybeType(inner) => todo!(),
            TypeExpression::NamedType(name) => todo!(),
            TypeExpression::GenericParamType { name, extends } => todo!(),
            TypeExpression::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            TypeExpression::FuncType {
                args,
                args_spread,
                is_pure,
                returns,
            } => todo!(),
            TypeExpression::GenericType { type_params, inner } => todo!(),
            TypeExpression::BoundGenericType { type_args, generic } => todo!(),
            TypeExpression::ObjectType(entries) => todo!(),
            TypeExpression::InterfaceType(entries) => todo!(),
            TypeExpression::RecordType {
                key_type,
                value_type,
            } => todo!(),
            TypeExpression::ArrayType(element) => todo!(),
            TypeExpression::TupleType(members) => todo!(),
            TypeExpression::ReadonlyType(inner) => todo!(),
            TypeExpression::StringType => f.write_str("string")?,
            TypeExpression::NumberType => f.write_str("number")?,
            TypeExpression::BooleanType => f.write_str("boolean")?,
            TypeExpression::NilType => f.write_str("nil")?,
            TypeExpression::LiteralType(value) => todo!(),
            TypeExpression::IteratorType(inner) => todo!(),
            TypeExpression::PlanType(inner) => todo!(),
            TypeExpression::ErrorType(inner) => todo!(),
            TypeExpression::ParenthesizedType(inner) => todo!(),
            TypeExpression::TypeofType(expression) => todo!(),
            TypeExpression::KeyofType(inner) => todo!(),
            TypeExpression::ValueofType(inner) => todo!(),
            TypeExpression::ElementofType(inner) => todo!(),
            TypeExpression::UnknownType => todo!(),
            TypeExpression::PoisonedType => todo!(),
            TypeExpression::AnyType => todo!(),
            TypeExpression::RegularExpressionType {} => todo!(),
            TypeExpression::PropertyType {
                subject,
                property,
                optional,
            } => todo!(),
        };

        Ok(())
    }
}
