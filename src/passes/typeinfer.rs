use crate::{
    model::ast::*,
    model::bgl_type::Type,
    passes::check::CheckContext,
    passes::resolve::{Binding, Resolve},
    ModulesStore,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InferTypeContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
}

impl<'a> From<CheckContext<'a>> for InferTypeContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
        }: CheckContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a> From<ResolveContext<'a>> for InferTypeContext<'a> {
    fn from(
        ResolveContext {
            modules,
            current_module,
            mutability: _,
        }: ResolveContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a> Src<Expression> {
    pub fn infer_type(&self, ctx: InferTypeContext<'a>) -> Type {
        match &self.node {
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
            },
            Expression::Parenthesis(inner) => inner.infer_type(ctx),
            Expression::LocalIdentifier(name) => {
                let resolved = ctx.current_module.resolve_symbol(name.as_str(), &self.src);

                if let Some(Binding::Declaration(binding)) = resolved {
                    if let Some((value, type_annotation)) = binding.node.get_type_and_value() {
                        return type_annotation
                            .map(|x| x.resolve(ctx.into()))
                            .unwrap_or(value.infer_type(ctx));
                    }
                }

                Type::PoisonedType
            }
            Expression::InlineConstGroup {
                declarations: _,
                inner,
            } => inner.infer_type(ctx),
            Expression::NilLiteral => Type::NilType,
            Expression::NumberLiteral { value } => Type::LiteralType {
                value: crate::model::bgl_type::LiteralTypeValue::NumberLiteral(value.clone()),
            },
            Expression::BooleanLiteral { value } => Type::LiteralType {
                value: crate::model::bgl_type::LiteralTypeValue::BooleanLiteral(*value),
            },
            Expression::ExactStringLiteral { value, tag: _ } => Type::LiteralType {
                value: crate::model::bgl_type::LiteralTypeValue::ExactString(value.clone()),
            },
            Expression::StringLiteral { tag, segments } => Type::StringType,
            Expression::ArrayLiteral { entries } => Type::TupleType {
                members: todo!(),
                mutability: Mutability::Literal,
            },
            Expression::ObjectLiteral { entries } => todo!(),
            Expression::NegationOperation(inner) => todo!(),
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
            } => Type::UnionType {
                members: cases
                    .iter()
                    .map(|case| &case.1)
                    .chain(default_case.iter().map(|c| c.as_ref()))
                    .map(|expr| expr.infer_type(ctx))
                    .collect(),
            },
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
            Expression::AsCast { inner: _, as_type } => as_type.resolve(ctx.into()),
            Expression::InstanceOf {
                inner: _,
                possible_type: _,
            } => Type::BooleanType,
            Expression::ErrorExpression { inner } => todo!(),
            Expression::RegularExpression { expr, flags } => todo!(),
        }
    }
}
