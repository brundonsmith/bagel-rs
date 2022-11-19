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

impl<'a> Src<Expression> {
    pub fn infer_type(&self, ctx: InferTypeContext<'a>) -> Src<TypeExpression> {
        Src {
            src: self.src,
            node: match &self.node {
                Expression::NilLiteral => TypeExpression::NilType,
                Expression::NumberLiteral { value: _ } => TypeExpression::NumberType,
                Expression::BinaryOperation { left, op, right } => match op.node {
                    BinaryOperator::NullishCoalescing => todo!(),
                    BinaryOperator::Or => todo!(),
                    BinaryOperator::And => todo!(),
                    BinaryOperator::Equals => TypeExpression::BooleanType,
                    BinaryOperator::NotEquals => TypeExpression::BooleanType,
                    BinaryOperator::LessEqual => TypeExpression::BooleanType,
                    BinaryOperator::GreaterEqual => TypeExpression::BooleanType,
                    BinaryOperator::Less => TypeExpression::BooleanType,
                    BinaryOperator::Greater => TypeExpression::BooleanType,
                    BinaryOperator::Plus => {
                        let left_type = left.infer_type(ctx);
                        let right_type = right.infer_type(ctx);

                        let ctx: SubsumationContext = ctx.into();

                        if NUMBER_TYPE.subsumes(ctx, &left_type) {
                            if NUMBER_TYPE.subsumes(ctx, &right_type) {
                                TypeExpression::NumberType
                            } else if STRING_TYPE.subsumes(ctx, &right_type) {
                                TypeExpression::StringType
                            } else {
                                TypeExpression::UnknownType {
                                    mutability: Mutability::Mutable,
                                }
                            }
                        } else if STRING_TYPE.subsumes(ctx, &left_type) {
                            if NUMBER_TYPE.subsumes(ctx, &right_type)
                                || STRING_TYPE.subsumes(ctx, &right_type)
                            {
                                TypeExpression::StringType
                            } else {
                                TypeExpression::UnknownType {
                                    mutability: Mutability::Mutable,
                                }
                            }
                        } else {
                            TypeExpression::UnknownType {
                                mutability: Mutability::Mutable,
                            }
                        }
                    }
                    BinaryOperator::Minus => TypeExpression::NumberType,
                    BinaryOperator::Times => TypeExpression::NumberType,
                    BinaryOperator::Divide => TypeExpression::NumberType,
                    BinaryOperator::InstanceOf => TypeExpression::BooleanType,
                },
                Expression::Parenthesis { inner } => inner.infer_type(ctx).node,
                Expression::LocalIdentifier { name } => {
                    let binding = self
                        .src
                        .map(|s| ctx.module.resolve_symbol_within(name.as_str(), &s))
                        .flatten();

                    binding
                        .map(|binding| match binding {
                            Binding::ValueDeclaration(ValueDeclaration {
                                name: _,
                                type_annotation,
                                value,
                                is_const: _,
                                exported: _,
                                platforms: _,
                            }) => {
                                type_annotation
                                    .unwrap_or_else(move || value.infer_type(ctx))
                                    .node
                            }
                            Binding::InlineConstDeclaration(_) => todo!(),
                        })
                        .unwrap_or(TypeExpression::UnknownType {
                            mutability: Mutability::Readonly,
                        })
                }
                Expression::InlineConstGroup {
                    declarations: _,
                    inner,
                } => inner.infer_type(ctx).node,
                Expression::BooleanLiteral { value } => todo!(),
                Expression::StringLiteral { tag, segments } => todo!(),
                Expression::ExactStringLiteral { tag, value } => todo!(),
                Expression::ArrayLiteral { entries } => todo!(),
                Expression::ObjectLiteral { entries } => todo!(),
                Expression::NegationOperation { inner } => todo!(),
                Expression::Func {
                    type_annotation,
                    is_async,
                    is_pure,
                    body,
                } => todo!(),
                Expression::JsFunc {
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
                Expression::JsProc {
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
                Expression::ErrorExpression { inner } => todo!(),
                Expression::RegularExpression { expr, flags } => todo!(),
            },
        }
    }
}
