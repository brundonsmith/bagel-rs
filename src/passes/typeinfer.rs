use crate::{
    model::ast::*,
    model::{ast::Any, bgl_type::Type, errors::BagelError, module::Module},
    passes::check::CheckContext,
    ModulesStore,
};

use super::resolve_type::ResolveContext;

impl AST<Expression> {
    pub fn infer_type<'a>(&self, ctx: InferTypeContext<'a>) -> Type {
        match self.downcast() {
            Expression::BinaryOperation(BinaryOperation { left, op, right }) => {
                match op.downcast().0 {
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
                }
            }
            Expression::Parenthesis(Parenthesis(inner)) => inner.infer_type(ctx),
            Expression::LocalIdentifier(LocalIdentifier(name)) => {
                let resolved = self.resolve_symbol(name.as_str());

                if let Some(resolved) = resolved {
                    match resolved.details() {
                        Any::ImportAllDeclaration(ImportAllDeclaration { name, path }) => todo!(),
                        Any::ImportDeclaration(ImportDeclaration { imports: _, path }) => {
                            return ctx
                                .modules
                                .import(
                                    &ctx.current_module.module_id,
                                    path.downcast().value.as_str(),
                                )
                                .map(|other_module| {
                                    other_module.get_declaration(name.as_str(), true)
                                })
                                .flatten()
                                .map(|decl| decl.declaration_type(ctx))
                                .flatten()
                                .unwrap_or(Type::PoisonedType);
                        }
                        Any::ProcDeclaration(ProcDeclaration {
                            name: _,
                            proc,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        }) => {
                            return proc.clone().recast::<Expression>().infer_type(ctx);
                        }
                        Any::FuncDeclaration(FuncDeclaration {
                            name: _,
                            func,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        }) => {
                            return func.clone().recast::<Expression>().infer_type(ctx);
                        }
                        Any::ValueDeclaration(ValueDeclaration {
                            name: _,
                            type_annotation,
                            value,
                            is_const: _,
                            exported: _,
                            platforms: _,
                        }) => {
                            return type_annotation
                                .as_ref()
                                .map(|t| t.resolve_type(ctx.into()))
                                .unwrap_or_else(|| value.infer_type(ctx));
                        }
                        Any::Arg(Arg {
                            name,
                            type_annotation,
                            optional,
                        }) => {
                            return type_annotation
                                .as_ref()
                                .map(|t| t.resolve_type(ctx.into()))
                                .unwrap_or(Type::PoisonedType);
                        }
                        Any::InlineDeclaration(InlineDeclaration {
                            destination,
                            awaited,
                            value,
                        }) => {
                            return match destination {
                                DeclarationDestination::NameAndType(NameAndType {
                                    name: _,
                                    type_annotation,
                                }) => type_annotation
                                    .as_ref()
                                    .map(|x| x.resolve_type(ctx.into()))
                                    .unwrap_or_else(|| value.infer_type(ctx)),
                                DeclarationDestination::Destructure(_) => todo!(),
                            }
                        }
                        Any::DeclarationStatement(DeclarationStatement {
                            destination,
                            value,
                            awaited,
                            is_const,
                        }) => {
                            return match destination {
                                DeclarationDestination::NameAndType(NameAndType {
                                    name: _,
                                    type_annotation,
                                }) => type_annotation
                                    .as_ref()
                                    .map(|x| x.resolve_type(ctx.into()))
                                    .unwrap_or_else(|| {
                                        let base_type = value.infer_type(ctx);

                                        if *is_const {
                                            base_type
                                        } else {
                                            base_type.broaden_for_mutation()
                                        }
                                    }),
                                DeclarationDestination::Destructure(_) => todo!(),
                            };
                        }
                        _ => todo!(),
                    }
                }

                Type::PoisonedType
            }
            Expression::InlineConstGroup(InlineConstGroup {
                declarations: _,
                inner,
            }) => inner.infer_type(ctx),
            Expression::NilLiteral(_) => Type::NilType,
            Expression::NumberLiteral(NumberLiteral(value)) => {
                let n = Some(value.as_str().parse().unwrap());

                Type::NumberType { min: n, max: n }
            }
            Expression::BooleanLiteral(BooleanLiteral(value)) => Type::BooleanType(Some(value)),
            Expression::ExactStringLiteral(ExactStringLiteral { value, tag: _ }) => {
                Type::StringType(Some(value.clone()))
            }
            Expression::StringLiteral(StringLiteral {
                tag: _,
                segments: _,
            }) => Type::ANY_STRING,
            Expression::ArrayLiteral(ArrayLiteral(members)) => {
                if members
                    .iter()
                    .any(|member| matches!(member, ArrayLiteralEntry::Spread(_)))
                {
                    let mut member_types: Vec<Type> = vec![];
                    let mut bail_out_to_union = false;

                    for member in members {
                        match member {
                            ArrayLiteralEntry::Spread(spread_expr) => {
                                let spread_inner = spread_expr.downcast().0;
                                let spread_type = spread_inner.infer_type(ctx);

                                match spread_type {
                                    Type::TupleType(members) => {
                                        for member in members {
                                            member_types.push(member);
                                        }
                                    }
                                    Type::ArrayType(element) => {
                                        bail_out_to_union = true;
                                        member_types.push(element.as_ref().clone());
                                    }
                                    _ => todo!(),
                                }
                            }
                            ArrayLiteralEntry::Expression(expr) => {
                                member_types.push(expr.infer_type(ctx))
                            }
                        }
                    }

                    if bail_out_to_union {
                        Type::ArrayType(Box::new(Type::UnionType(member_types)))
                    } else {
                        Type::TupleType(member_types)
                    }
                } else {
                    Type::TupleType(
                        members
                            .iter()
                            .map(|member| match member {
                                ArrayLiteralEntry::Expression(expr) => expr.infer_type(ctx),
                                ArrayLiteralEntry::Spread(_) => {
                                    unreachable!("Handled in if-clause above")
                                }
                            })
                            .collect(),
                    )
                }
            }
            Expression::ObjectLiteral(ObjectLiteral(entries)) => todo!(),
            Expression::NegationOperation(NegationOperation(inner)) => todo!(),
            Expression::Func(Func {
                type_annotation,
                is_async: _,
                is_pure,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                Type::FuncType {
                    args: type_annotation
                        .args
                        .into_iter()
                        .map(|a| {
                            a.downcast()
                                .type_annotation
                                .map(|a| a.resolve_type(ctx.into()))
                        })
                        .collect(),
                    args_spread: type_annotation
                        .args_spread
                        .map(|a| a.resolve_type(ctx.into()))
                        .map(Box::new),
                    is_pure,
                    returns: Box::new(
                        type_annotation
                            .returns
                            .map(|r| r.resolve_type(ctx.into()))
                            .unwrap_or_else(|| body.infer_type(ctx)),
                    ),
                }
            }
            Expression::Proc(Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                Type::ProcType {
                    args: type_annotation
                        .args
                        .into_iter()
                        .map(|a| {
                            a.downcast()
                                .type_annotation
                                .map(|a| a.resolve_type(ctx.into()))
                        })
                        .collect(),
                    args_spread: type_annotation
                        .args_spread
                        .map(|a| a.resolve_type(ctx.into()))
                        .map(Box::new),
                    is_async,
                    is_pure,
                    throws: type_annotation
                        .throws
                        .map(|throws| throws.resolve_type(ctx.into()))
                        .map(Box::new),
                }
            }
            Expression::JavascriptEscape(_) => Type::AnyType,
            Expression::RangeExpression(RangeExpression { start, end }) => todo!(),
            Expression::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            }) => {
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
            Expression::PropertyAccessor(PropertyAccessor {
                subject,
                property,
                optional,
            }) => {
                let subject_type = subject.try_recast::<Expression>().unwrap().infer_type(ctx);
                let property_type = property.try_recast::<Expression>().unwrap().infer_type(ctx);

                // TODO: optional

                subject_type
                    .indexed(&property_type)
                    .unwrap_or(Type::PoisonedType)
            }
            Expression::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            }) => Type::UnionType(
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
            Expression::SwitchExpression(SwitchExpression {
                value,
                cases,
                default_case,
            }) => todo!(),
            Expression::ElementTag(ElementTag {
                tag_name,
                attributes,
                children,
            }) => todo!(),
            Expression::AsCast(AsCast { inner: _, as_type }) => as_type.resolve_type(ctx.into()),
            Expression::InstanceOf(InstanceOf {
                inner: _,
                possible_type: _,
            }) => Type::ANY_BOOLEAN,
            Expression::ErrorExpression(ErrorExpression(inner)) => todo!(),
            Expression::RegularExpression(RegularExpression { expr, flags }) => todo!(),
        }
    }
}

impl AST<Declaration> {
    pub fn declaration_type<'a>(&self, ctx: InferTypeContext<'a>) -> Option<Type> {
        match self.downcast() {
            Declaration::FuncDeclaration(FuncDeclaration {
                name: _,
                func,
                exported: _,
                platforms: _,
                decorators: _,
            }) => Some(func.recast::<Expression>().infer_type(ctx)),
            Declaration::ProcDeclaration(ProcDeclaration {
                name: _,
                proc,
                exported: _,
                platforms: _,
                decorators: _,
            }) => Some(proc.recast::<Expression>().infer_type(ctx)),
            Declaration::ValueDeclaration(ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            }) => Some(
                type_annotation
                    .map(|t| t.resolve_type(ctx.into()))
                    .unwrap_or_else(|| value.infer_type(ctx)),
            ),

            Declaration::ImportAllDeclaration(_) => None,
            Declaration::ImportDeclaration(_) => None,
            Declaration::TypeDeclaration(_) => None,
            Declaration::TestExprDeclaration(_) => None,
            Declaration::TestBlockDeclaration(_) => None,
            Declaration::TestTypeDeclaration(_) => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InferTypeContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
}

impl<'a, F: FnMut(BagelError)> From<&mut CheckContext<'a, F>> for InferTypeContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            report_error: _,
        }: &mut CheckContext<'a, F>,
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
