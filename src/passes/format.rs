use crate::model::{ast::*, module::Module};
use std::fmt::{Display, Formatter, Result, Write};

impl Module {
    pub fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        self.ast.format(f, opts)
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

impl AST {
    pub fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
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
            ASTDetails::StringLiteralRawSegment(_) => todo!(),
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
                    entry.format(f, opts)?;
                }
                f.write_str(" }")
            }
            ASTDetails::Spread(_) => todo!(),
            ASTDetails::KeyValue { key, value } => todo!(),
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
            ASTDetails::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => {
                let type_annotation = type_annotation.expect::<FuncType>();

                f.write_char('(')?;
                type_annotation.args.format(f, opts)?;
                f.write_char(')')?;
                if let Some(return_type) = type_annotation.returns {
                    f.write_str(": ")?;
                    return_type.format(f, opts)?;
                }
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
            ASTDetails::ArgsSeries(_) => todo!(),
            ASTDetails::Arg {
                name,
                type_annotation,
                optional,
            } => todo!(),
            ASTDetails::JavascriptEscapeExpression(_) => todo!(),
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
                returns,
            } => todo!(),
            ASTDetails::GenericType { type_params, inner } => todo!(),
            ASTDetails::TypeParam { name, extends } => todo!(),
            ASTDetails::BoundGenericType { type_args, generic } => todo!(),
            ASTDetails::ObjectType(_) => todo!(),
            ASTDetails::InterfaceType(_) => todo!(),
            ASTDetails::SpreadType(_) => todo!(),
            ASTDetails::KeyValueType { key, value } => todo!(),
            ASTDetails::RecordType {
                key_type,
                value_type,
            } => todo!(),
            ASTDetails::ArrayType(_) => todo!(),
            ASTDetails::TupleType(_) => todo!(),
            ASTDetails::ReadonlyType(_) => todo!(),
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
            ASTDetails::IteratorType(_) => todo!(),
            ASTDetails::PlanType(_) => todo!(),
            ASTDetails::ErrorType(_) => todo!(),
            ASTDetails::ParenthesizedType(_) => todo!(),
            ASTDetails::TypeofType(_) => todo!(),
            ASTDetails::KeyofType(_) => todo!(),
            ASTDetails::ValueofType(_) => todo!(),
            ASTDetails::ElementofType(_) => todo!(),
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
            ASTDetails::PlainIdentifier(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {}

impl FormatOptions {
    pub const DEFAULT: FormatOptions = FormatOptions {};
}
