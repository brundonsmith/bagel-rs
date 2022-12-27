use crate::{
    model::ast::*,
    model::{
        ast::ASTDetails,
        bgl_type::{self, Type},
        module::Module,
    },
    passes::check::CheckContext,
    ModulesStore,
};

use super::resolve_type::ResolveContext;

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    pub fn infer_type<'a>(&self, ctx: InferTypeContext<'a>) -> Type {
        match self.details() {
            ASTDetails::BinaryOperation { left, op, right } => match op.details() {
                ASTDetails::BinaryOperator(op) => match op {
                    BinaryOperatorOp::NullishCoalescing => todo!(),
                    BinaryOperatorOp::Or => todo!(),
                    BinaryOperatorOp::And => todo!(),
                    BinaryOperatorOp::Equals => Type::ANY_BOOLEAN,
                    BinaryOperatorOp::NotEquals => Type::ANY_BOOLEAN,
                    BinaryOperatorOp::LessEqual => Type::ANY_BOOLEAN,
                    BinaryOperatorOp::GreaterEqual => Type::ANY_BOOLEAN,
                    BinaryOperatorOp::Less => Type::ANY_BOOLEAN,
                    BinaryOperatorOp::Greater => Type::ANY_BOOLEAN,
                    BinaryOperatorOp::Plus => {
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
                    BinaryOperatorOp::Minus => Type::ANY_NUMBER,
                    BinaryOperatorOp::Times => Type::ANY_NUMBER,
                    BinaryOperatorOp::Divide => Type::ANY_NUMBER,
                },
                _ => unreachable!(),
            },
            ASTDetails::Parenthesis(inner) => inner.infer_type(ctx),
            ASTDetails::LocalIdentifier(name) => {
                let resolved = self.resolve_symbol(name.as_str());

                if let Some(resolved) = resolved {
                    match resolved.details() {
                        ASTDetails::ProcDeclaration {
                            name: _,
                            proc,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        } => {
                            return proc.clone().upcast().infer_type(ctx);
                        }
                        ASTDetails::FuncDeclaration {
                            name: _,
                            func,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        } => {
                            return func.clone().upcast().infer_type(ctx);
                        }
                        ASTDetails::ValueDeclaration {
                            name: _,
                            type_annotation,
                            value,
                            is_const: _,
                            exported: _,
                            platforms: _,
                        } => {
                            return type_annotation
                                .as_ref()
                                .map(|t| t.resolve_type(ctx.into()))
                                .unwrap_or_else(|| value.infer_type(ctx));
                        }
                        ASTDetails::Arg {
                            name,
                            type_annotation,
                            optional,
                        } => {
                            return type_annotation
                                .as_ref()
                                .map(|t| t.resolve_type(ctx.into()))
                                .unwrap_or(Type::PoisonedType);
                        }
                        _ => todo!(),
                    }
                }

                Type::PoisonedType
            }
            ASTDetails::InlineConstGroup {
                declarations: _,
                inner,
            } => inner.infer_type(ctx),
            ASTDetails::NilLiteral => Type::NilType,
            ASTDetails::NumberLiteral(value) => {
                let n = Some(value.as_str().parse().unwrap());

                Type::NumberType { min: n, max: n }
            }
            ASTDetails::BooleanLiteral(value) => Type::BooleanType(Some(*value)),
            ASTDetails::ExactStringLiteral { value, tag: _ } => {
                Type::StringType(Some(value.clone()))
            }
            ASTDetails::StringLiteral {
                tag: _,
                segments: _,
            } => Type::ANY_STRING,
            ASTDetails::ArrayLiteral(entries) => Type::TupleType(todo!()),
            ASTDetails::ObjectLiteral(entries) => todo!(),
            ASTDetails::NegationOperation(inner) => todo!(),
            ASTDetails::Func {
                type_annotation,
                is_async: _,
                is_pure,
                body,
            } => {
                let type_annotation = type_annotation.downcast();

                Type::FuncType {
                    args: type_annotation
                        .args
                        .into_iter()
                        .map(|a| {
                            let a = a.downcast();

                            bgl_type::Arg {
                                name: a.name.downcast().0.as_str().to_owned(),
                                type_annotation: a
                                    .type_annotation
                                    .map(|a| a.resolve_type(ctx.into())),
                                optional: a.optional,
                            }
                        })
                        .collect(),
                    args_spread: type_annotation
                        .args_spread
                        .map(|a| a.resolve_type(ctx.into()))
                        .map(Box::new),
                    is_pure: *is_pure,
                    returns: Box::new(
                        type_annotation
                            .returns
                            .map(|r| r.resolve_type(ctx.into()))
                            .unwrap_or_else(|| body.infer_type(ctx)),
                    ),
                }
            }
            ASTDetails::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => {
                let type_annotation = type_annotation.downcast();

                Type::ProcType {
                    args: type_annotation
                        .args
                        .into_iter()
                        .map(|a| {
                            let a = a.downcast();

                            bgl_type::Arg {
                                name: a.name.downcast().0.as_str().to_owned(),
                                type_annotation: a
                                    .type_annotation
                                    .map(|a| a.resolve_type(ctx.into())),
                                optional: a.optional,
                            }
                        })
                        .collect(),
                    args_spread: type_annotation
                        .args_spread
                        .map(|a| a.resolve_type(ctx.into()))
                        .map(Box::new),
                    is_async: *is_async,
                    is_pure: *is_pure,
                    throws: Some(
                        type_annotation
                            .throws
                            .map(|r| r.resolve_type(ctx.into()))
                            .unwrap_or_else(|| body.infer_type(ctx)),
                    )
                    .map(Box::new),
                }
            }
            ASTDetails::JavascriptEscape(_) => Type::AnyType,
            ASTDetails::RangeExpression { start, end } => todo!(),
            ASTDetails::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => {
                let subject_type = subject.infer_type(ctx);

                if let Type::FuncType {
                    args: _,
                    args_spread: _,
                    is_pure: _,
                    returns,
                } = subject_type
                {
                    returns.as_ref().clone()
                } else {
                    Type::PoisonedType
                }
            }
            ASTDetails::PropertyAccessor {
                subject,
                property,
                optional,
            } => todo!(),
            ASTDetails::IfElseExpression {
                cases,
                default_case,
            } => Type::UnionType(
                cases
                    .iter()
                    .map(|case| case.downcast().outcome.infer_type(ctx))
                    .chain(
                        default_case
                            .as_ref()
                            .map(|case| case.infer_type(ctx))
                            .into_iter(),
                    )
                    .collect(),
            ),
            ASTDetails::SwitchExpression {
                value,
                cases,
                default_case,
            } => todo!(),
            ASTDetails::ElementTag {
                tag_name,
                attributes,
                children,
            } => todo!(),
            ASTDetails::AsCast { inner: _, as_type } => as_type.resolve_type(ctx.into()),
            ASTDetails::InstanceOf {
                inner: _,
                possible_type: _,
            } => Type::ANY_BOOLEAN,
            ASTDetails::ErrorExpression(inner) => todo!(),
            ASTDetails::RegularExpression { expr, flags } => todo!(),
            _ => unreachable!(),
        }
    }
}

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
