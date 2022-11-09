use std::fmt::Result;

use crate::ast::{Declaration, Expression, Module, TypeExpression};

pub trait Compile {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result;
}

impl<'a> Compile for Module<'a> {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        for decl in &self.declarations {
            decl.compile(f)?;
        }

        Ok(())
    }
}

impl<'a> Compile for Declaration<'a> {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            Declaration::ValueDeclaration {
                span,
                name,
                type_annotation,
                value,
            } => {
                f.write_str("const ")?;
                f.write_str(&name.name)?;
                if let Some(type_annotation) = type_annotation {
                    type_annotation.compile(f)?;
                }
                f.write_str(" = ")?;
                value.compile(f)?;
                f.write_str(";")?;

                Ok(())
            }
            Declaration::ImportAllDeclaration { span, name, path } => todo!(),
            Declaration::ImportDeclaration {
                span,
                imports,
                path,
            } => todo!(),
            Declaration::TypeDeclaration {
                span,
                name,
                declared_type,
            } => todo!(),
            Declaration::FuncDeclaration {
                span,
                name,
                func,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                span,
                name,
                proc,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { span, name, expr } => todo!(),
            Declaration::TestBlockDeclaration { span, name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                span,
                name,
                destination_type,
                value_type,
            } => todo!(),
        }
    }
}

impl<'a> Compile for Expression<'a> {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            Expression::NilLiteral { span, p: _ } => f.write_str("undefined"),
            Expression::NumberLiteral { span, value } => f.write_str(value),
            Expression::BinaryOperation {
                span,
                left,
                op,
                right,
            } => todo!(),
            Expression::Parenthesis { span, inner } => todo!(),
            Expression::LocalIdentifier { span, name } => todo!(),
            Expression::InlineConstGroup {
                span,
                declarations,
                inner,
            } => todo!(),
            Expression::BooleanLiteral { span, p: _, value } => todo!(),
            Expression::StringLiteral { span, value } => todo!(),
            Expression::ExactStringLiteral {
                span,
                tag,
                segments,
            } => todo!(),
            Expression::ArrayLiteral { span, entries } => todo!(),
            Expression::ObjectLiteral { span, entries } => todo!(),
            Expression::NegationOperation { span, inner } => todo!(),
            Expression::Func {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { span, start, end } => todo!(),
            Expression::Invocation {
                span,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                span,
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                span,
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                span,
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                span,
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast {
                span,
                inner,
                as_type,
            } => todo!(),
            Expression::ErrorExpression { span, inner } => todo!(),
            Expression::RegularExpression { span, expr, flags } => todo!(),
        }
    }
}

impl<'a> Compile for TypeExpression<'a> {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            TypeExpression::UnknownType {
                span,
                p: _,
                mutability,
            } => f.write_str("unknown"),
            TypeExpression::NilType { span, p: _ } => f.write_str("null | undefined"),
            TypeExpression::BooleanType { span, p: _ } => f.write_str("boolean"),
            TypeExpression::NumberType { span, p: _ } => f.write_str("number"),
            TypeExpression::StringType { span, p: _ } => f.write_str("string"),
            TypeExpression::UnionType { span, members } => todo!(),
            TypeExpression::MaybeType { span, inner } => todo!(),
            TypeExpression::NamedType { span, name } => todo!(),
            TypeExpression::GenericParamType {
                span,
                name,
                extends,
            } => todo!(),
            TypeExpression::ProcType {
                span,
                args,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            TypeExpression::FuncType {
                span,
                args,
                is_pure,
                returns,
            } => todo!(),
            TypeExpression::GenericType {
                span,
                type_params,
                inner,
            } => todo!(),
            TypeExpression::BoundGenericType {
                span,
                type_args,
                generic,
            } => todo!(),
            TypeExpression::ObjectType {
                span,
                entries,
                mutability,
            } => todo!(),
            TypeExpression::InterfaceType {
                span,
                entries,
                mutability,
            } => todo!(),
            TypeExpression::RecordType {
                span,
                key_type,
                value_type,
                mutability,
            } => todo!(),
            TypeExpression::ArrayType {
                span,
                element,
                mutability,
            } => todo!(),
            TypeExpression::TupleType {
                span,
                members,
                mutability,
            } => todo!(),
            TypeExpression::ReadonlyType { span, inner } => todo!(),
            TypeExpression::LiteralType { span, value } => todo!(),
            TypeExpression::NominalType { span, name, inner } => todo!(),
            TypeExpression::IteratorType { span, inner } => todo!(),
            TypeExpression::PlanType { span, inner } => todo!(),
            TypeExpression::ErrorType { span, inner } => todo!(),
            TypeExpression::ParenthesizedType { span, inner } => todo!(),
            TypeExpression::TypeofType { span, expression } => todo!(),
            TypeExpression::KeyofType { span, inner } => todo!(),
            TypeExpression::ValueofType { span, inner } => todo!(),
            TypeExpression::ElementofType { span, inner } => todo!(),
            TypeExpression::PoisonedType { span, p: _ } => todo!(),
            TypeExpression::AnyType { span, p: _ } => todo!(),
            TypeExpression::RegularExpressionType { span, p: _ } => todo!(),
            TypeExpression::PropertyType {
                span,
                subject,
                property,
                optional,
            } => todo!(),
        }
    }
}
