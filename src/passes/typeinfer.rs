use crate::{
    model::ast::*,
    model::{bgl_type::Type, module::Module},
    passes::check::CheckContext,
    passes::resolve::{Binding, Resolve},
    ModulesStore,
};

#[derive(Clone, Copy, Debug)]
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
                BinaryOperator::Equals => Type::ANY_BOOLEAN,
                BinaryOperator::NotEquals => Type::ANY_BOOLEAN,
                BinaryOperator::LessEqual => Type::ANY_BOOLEAN,
                BinaryOperator::GreaterEqual => Type::ANY_BOOLEAN,
                BinaryOperator::Less => Type::ANY_BOOLEAN,
                BinaryOperator::Greater => Type::ANY_BOOLEAN,
                BinaryOperator::Plus => {
                    let left_type = left.infer_type(ctx);
                    let right_type = right.infer_type(ctx);

                    if Type::ANY_NUMBER.subsumes(ctx.into(), &left_type) {
                        if Type::ANY_NUMBER.subsumes(ctx.into(), &right_type) {
                            Type::ANY_NUMBER
                        } else if Type::ANY_STRING.subsumes(ctx.into(), &right_type) {
                            Type::ANY_STRING
                        } else {
                            Type::UnknownType
                        }
                    } else if Type::ANY_STRING.subsumes(ctx.into(), &left_type) {
                        if Type::ANY_NUMBER.subsumes(ctx.into(), &right_type)
                            || Type::ANY_STRING.subsumes(ctx.into(), &right_type)
                        {
                            Type::ANY_STRING
                        } else {
                            Type::UnknownType
                        }
                    } else {
                        Type::UnknownType
                    }
                }
                BinaryOperator::Minus => Type::ANY_NUMBER,
                BinaryOperator::Times => Type::ANY_NUMBER,
                BinaryOperator::Divide => Type::ANY_NUMBER,
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
            Expression::NumberLiteral(value) => {
                let n = Some(value.as_str().parse().unwrap());

                Type::NumberType { min: n, max: n }
            }
            Expression::BooleanLiteral(value) => Type::BooleanType(Some(*value)),
            Expression::ExactStringLiteral { value, tag: _ } => {
                Type::StringType(Some(value.clone()))
            }
            Expression::StringLiteral {
                tag: _,
                segments: _,
            } => Type::ANY_STRING,
            Expression::ArrayLiteral(entries) => Type::TupleType(todo!()),
            Expression::ObjectLiteral(entries) => todo!(),
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
            } => Type::UnionType(
                cases
                    .iter()
                    .map(|case| &case.1)
                    .chain(default_case.iter().map(|c| c.as_ref()))
                    .map(|expr| expr.infer_type(ctx))
                    .collect(),
            ),
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
            } => Type::ANY_BOOLEAN,
            Expression::ErrorExpression(inner) => todo!(),
            Expression::RegularExpression { expr, flags } => todo!(),
        }
    }
}
