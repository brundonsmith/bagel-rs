use std::marker::PhantomData;

use crate::{
    ast::{BinaryOperator, Expression, Module, Mutability, TypeExpression, ValueDeclaration},
    resolve::{Binding, Resolve},
};

pub struct InferTypeContext<'a> {
    pub module: &'a Module<'a>,
}

impl<'a> Expression<'a> {
    pub fn infer_type(&self, ctx: &'a InferTypeContext) -> TypeExpression<'a> {
        match self {
            Expression::NilLiteral { span, p } => TypeExpression::NilType {
                span: span.clone(),
                p: PhantomData,
            },
            Expression::NumberLiteral { span, value: _ } => TypeExpression::NumberType {
                span: span.clone(),
                p: PhantomData,
            },
            Expression::BinaryOperation {
                span: _,
                left,
                op,
                right,
            } => {
                let left_type = left.infer_type(ctx);
                let right_type = right.infer_type(ctx);

                match op {
                    BinaryOperator::NullishCoalescing => todo!(),
                    BinaryOperator::Or => todo!(),
                    BinaryOperator::And => todo!(),
                    BinaryOperator::Equals => todo!(),
                    BinaryOperator::NotEquals => todo!(),
                    BinaryOperator::LessEqual => todo!(),
                    BinaryOperator::GreaterEqual => todo!(),
                    BinaryOperator::Less => todo!(),
                    BinaryOperator::Greater => todo!(),
                    BinaryOperator::Plus => todo!(),
                    BinaryOperator::Minus => todo!(),
                    BinaryOperator::Times => todo!(),
                    BinaryOperator::Divide => todo!(),
                    BinaryOperator::InstanceOf => todo!(),
                }
            }
            Expression::Parenthesis { span: _, inner } => inner.infer_type(ctx),
            Expression::LocalIdentifier { span, name } => {
                let binding = span
                    .as_ref()
                    .map(|range| ctx.module.resolve_symbol_within(name, &range.start))
                    .flatten();

                binding
                    .map(|binding| match binding {
                        Binding::ValueDeclaration(ValueDeclaration {
                            span: _,
                            name: _,
                            type_annotation,
                            value,
                        }) => type_annotation.unwrap_or_else(move || value.infer_type(ctx)),
                        Binding::InlineConstDeclaration(_) => todo!(),
                    })
                    .unwrap_or_else(|| TypeExpression::UnknownType {
                        span: span.clone(),
                        p: PhantomData,
                        mutability: Mutability::Readonly,
                    })
            }
            Expression::InlineConstGroup {
                span: _,
                declarations: _,
                inner,
            } => inner.infer_type(ctx),
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
