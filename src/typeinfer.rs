use crate::{
    ast::*,
    check::CheckContext,
    resolve::{Binding, Resolve},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InferTypeContext<'a> {
    pub module: &'a Module,
}

impl<'a> From<CheckContext<'a>> for InferTypeContext<'a> {
    fn from(CheckContext { module }: CheckContext<'a>) -> Self {
        Self { module }
    }
}

impl<'a> Expression {
    pub fn infer_type(&self, ctx: InferTypeContext<'a>) -> TypeExpression {
        match self {
            Expression::NilLiteral { src } => TypeExpression::NilType { src: *src },
            Expression::NumberLiteral { src, value: _ } => TypeExpression::NumberType { src: *src },
            Expression::BinaryOperation {
                src,
                left,
                op,
                right,
            } => match op {
                BinaryOperator::NullishCoalescing => todo!(),
                BinaryOperator::Or => todo!(),
                BinaryOperator::And => todo!(),
                BinaryOperator::Equals => TypeExpression::BooleanType { src: *src },
                BinaryOperator::NotEquals => TypeExpression::BooleanType { src: *src },
                BinaryOperator::LessEqual => TypeExpression::BooleanType { src: *src },
                BinaryOperator::GreaterEqual => TypeExpression::BooleanType { src: *src },
                BinaryOperator::Less => TypeExpression::BooleanType { src: *src },
                BinaryOperator::Greater => TypeExpression::BooleanType { src: *src },
                BinaryOperator::Plus => {
                    let left_type = left.infer_type(ctx);
                    let right_type = right.infer_type(ctx);

                    let ctx: SubsumationContext = ctx.into();

                    if NUMBER_TYPE.subsumes(ctx, &left_type) {
                        if NUMBER_TYPE.subsumes(ctx, &right_type) {
                            return TypeExpression::NumberType { src: *src };
                        } else if STRING_TYPE.subsumes(ctx, &right_type) {
                            return TypeExpression::StringType { src: *src };
                        }
                    } else if STRING_TYPE.subsumes(ctx, &left_type) {
                        if NUMBER_TYPE.subsumes(ctx, &right_type)
                            || STRING_TYPE.subsumes(ctx, &right_type)
                        {
                            return TypeExpression::StringType { src: *src };
                        }
                    }

                    UNKNOWN_TYPE
                }
                BinaryOperator::Minus => TypeExpression::NumberType { src: *src },
                BinaryOperator::Times => TypeExpression::NumberType { src: *src },
                BinaryOperator::Divide => TypeExpression::NumberType { src: *src },
                BinaryOperator::InstanceOf => TypeExpression::BooleanType { src: *src },
            },
            Expression::Parenthesis { src: _, inner } => inner.infer_type(ctx),
            Expression::LocalIdentifier { src, name } => {
                let binding = src
                    .map(|s| ctx.module.resolve_symbol_within(name.as_str(), &s))
                    .flatten();

                binding
                    .map(|binding| match binding {
                        Binding::ValueDeclaration(ValueDeclaration {
                            src: _,
                            name: _,
                            type_annotation,
                            value,
                            is_const: _,
                            exported: _,
                            platforms: _,
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
            Expression::StringLiteral { src, tag, segments } => todo!(),
            Expression::ExactStringLiteral { src, tag, value } => todo!(),
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
