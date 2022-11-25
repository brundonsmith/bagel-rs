use crate::{
    ast::*,
    bgl_type::Type,
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

impl<'a> From<ResolveContext<'a>> for InferTypeContext<'a> {
    fn from(ResolveContext { module }: ResolveContext<'a>) -> Self {
        Self { module }
    }
}

impl<'a> Src<Expression> {
    pub fn infer_type(&self, ctx: InferTypeContext<'a>) -> Type {
        match &self.node {
            Expression::NilLiteral => Type::NilType,
            Expression::NumberLiteral { value: _ } => Type::NumberType,
            Expression::BinaryOperation { left, op, right } => match op.node {
                BinaryOperator::NullishCoalescing => todo!(),
                BinaryOperator::Or => todo!(),
                BinaryOperator::And => todo!(),
                BinaryOperator::Equals => Type::BooleanType,
                BinaryOperator::NotEquals => Type::BooleanType,
                BinaryOperator::LessEqual => Type::BooleanType,
                BinaryOperator::GreaterEqual => Type::BooleanType,
                BinaryOperator::Less => Type::BooleanType,
                BinaryOperator::Greater => Type::BooleanType,
                BinaryOperator::Plus => {
                    let left_type = left.infer_type(ctx);
                    let right_type = right.infer_type(ctx);

                    if Type::NumberType.subsumes(&left_type) {
                        if Type::NumberType.subsumes(&right_type) {
                            Type::NumberType
                        } else if Type::StringType.subsumes(&right_type) {
                            Type::StringType
                        } else {
                            Type::UnknownType {
                                mutability: Mutability::Mutable,
                            }
                        }
                    } else if Type::StringType.subsumes(&left_type) {
                        if Type::NumberType.subsumes(&right_type)
                            || Type::StringType.subsumes(&right_type)
                        {
                            Type::StringType
                        } else {
                            Type::UnknownType {
                                mutability: Mutability::Mutable,
                            }
                        }
                    } else {
                        Type::UnknownType {
                            mutability: Mutability::Mutable,
                        }
                    }
                }
                BinaryOperator::Minus => Type::NumberType,
                BinaryOperator::Times => Type::NumberType,
                BinaryOperator::Divide => Type::NumberType,
                BinaryOperator::InstanceOf => Type::BooleanType,
            },
            Expression::Parenthesis { inner } => inner.infer_type(ctx),
            Expression::LocalIdentifier { name } => {
                let resolved = self
                    .src
                    .map(|s| ctx.module.resolve_symbol_within(name.as_str(), &s))
                    .flatten();

                match resolved {
                    Some(Binding::ValueDeclaration(binding)) => binding
                        .type_annotation
                        .map(|x| x.resolve(ctx.into()))
                        .unwrap_or_else(move || binding.value.infer_type(ctx)),
                    _ => Type::PoisonedType,
                }
            }
            Expression::InlineConstGroup {
                declarations: _,
                inner,
            } => inner.infer_type(ctx),
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
        }
    }
}
