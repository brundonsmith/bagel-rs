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
            Expression::NilLiteral { src } => TypeExpression::NilType { src: *src },
            Expression::NumberLiteral { src, value: _ } => TypeExpression::NumberType { src: *src },
            Expression::BinaryOperation {
                src: _,
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
            Expression::Parenthesis { src: _, inner } => inner.infer_type(ctx),
            Expression::LocalIdentifier { src, name } => {
                let binding = src
                    .map(|s| ctx.module.resolve_symbol_within(name, s))
                    .flatten();

                binding
                    .map(|binding| match binding {
                        Binding::ValueDeclaration(ValueDeclaration {
                            src: _,
                            name: _,
                            type_annotation,
                            value,
                        }) => type_annotation.unwrap_or_else(move || value.infer_type(ctx)),
                        Binding::InlineConstDeclaration(_) => todo!(),
                    })
                    .unwrap_or_else(|| TypeExpression::UnknownType {
                        src: *src,
                        mutability: Mutability::Readonly,
                    })
            }
            Expression::InlineConstGroup {
                src: _,
                declarations: _,
                inner,
            } => inner.infer_type(ctx),
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
