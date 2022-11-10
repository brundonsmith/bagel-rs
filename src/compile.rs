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
                src,
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
            Declaration::ImportAllDeclaration { src, name, path } => todo!(),
            Declaration::ImportDeclaration { src, imports, path } => todo!(),
            Declaration::TypeDeclaration {
                src,
                name,
                declared_type,
            } => todo!(),
            Declaration::FuncDeclaration {
                src,
                name,
                func,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                src,
                name,
                proc,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { src, name, expr } => todo!(),
            Declaration::TestBlockDeclaration { src, name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                src,
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
            Expression::NilLiteral { src } => f.write_str("undefined"),
            Expression::NumberLiteral { src, value } => f.write_str(value),
            Expression::BinaryOperation {
                src,
                left,
                op,
                right,
            } => todo!(),
            Expression::Parenthesis { src, inner } => todo!(),
            Expression::LocalIdentifier { src, name } => todo!(),
            Expression::InlineConstGroup {
                src,
                declarations,
                inner,
            } => todo!(),
            Expression::BooleanLiteral { src, value } => todo!(),
            Expression::StringLiteral { src, value } => todo!(),
            Expression::ExactStringLiteral { src, tag, segments } => todo!(),
            Expression::ArrayLiteral { src, entries } => todo!(),
            Expression::ObjectLiteral { src, entries } => todo!(),
            Expression::NegationOperation { src, inner } => todo!(),
            Expression::Func {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { src, start, end } => todo!(),
            Expression::Invocation {
                src,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                src,
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                src,
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                src,
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                src,
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast {
                src,
                inner,
                as_type,
            } => todo!(),
            Expression::ErrorExpression { src, inner } => todo!(),
            Expression::RegularExpression { src, expr, flags } => todo!(),
        }
    }
}

impl<'a> Compile for TypeExpression<'a> {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            TypeExpression::UnknownType { src, mutability } => f.write_str("unknown"),
            TypeExpression::NilType { src } => f.write_str("null | undefined"),
            TypeExpression::BooleanType { src } => f.write_str("boolean"),
            TypeExpression::NumberType { src } => f.write_str("number"),
            TypeExpression::StringType { src } => f.write_str("string"),
            TypeExpression::UnionType { src, members } => todo!(),
            TypeExpression::MaybeType { src, inner } => todo!(),
            TypeExpression::NamedType { src, name } => todo!(),
            TypeExpression::GenericParamType { src, name, extends } => todo!(),
            TypeExpression::ProcType {
                src,
                args,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            TypeExpression::FuncType {
                src,
                args,
                is_pure,
                returns,
            } => todo!(),
            TypeExpression::GenericType {
                src,
                type_params,
                inner,
            } => todo!(),
            TypeExpression::BoundGenericType {
                src,
                type_args,
                generic,
            } => todo!(),
            TypeExpression::ObjectType {
                src,
                entries,
                mutability,
            } => todo!(),
            TypeExpression::InterfaceType {
                src,
                entries,
                mutability,
            } => todo!(),
            TypeExpression::RecordType {
                src,
                key_type,
                value_type,
                mutability,
            } => todo!(),
            TypeExpression::ArrayType {
                src,
                element,
                mutability,
            } => todo!(),
            TypeExpression::TupleType {
                src,
                members,
                mutability,
            } => todo!(),
            TypeExpression::ReadonlyType { src, inner } => todo!(),
            TypeExpression::LiteralType { src, value } => todo!(),
            TypeExpression::NominalType { src, name, inner } => todo!(),
            TypeExpression::IteratorType { src, inner } => todo!(),
            TypeExpression::PlanType { src, inner } => todo!(),
            TypeExpression::ErrorType { src, inner } => todo!(),
            TypeExpression::ParenthesizedType { src, inner } => todo!(),
            TypeExpression::TypeofType { src, expression } => todo!(),
            TypeExpression::KeyofType { src, inner } => todo!(),
            TypeExpression::ValueofType { src, inner } => todo!(),
            TypeExpression::ElementofType { src, inner } => todo!(),
            TypeExpression::PoisonedType { src } => todo!(),
            TypeExpression::AnyType { src } => todo!(),
            TypeExpression::RegularExpressionType { src } => todo!(),
            TypeExpression::PropertyType {
                src,
                subject,
                property,
                optional,
            } => todo!(),
        }
    }
}
