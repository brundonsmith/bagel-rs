use crate::model::{
    ast::*,
    bgl_type::{SubsumationContext, Type},
    errors::BagelError,
    module::{Module, ModulesStore},
};

use super::{check::CheckContext, typeinfer::InferTypeContext};

impl AST<TypeExpression> {
    pub fn resolve_type<'a>(&self, ctx: ResolveContext<'a>) -> Type {
        match self.downcast() {
            TypeExpression::GenericParamType(GenericParamType { name, extends }) => todo!(),
            TypeExpression::ProcType(ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            }) => Type::ProcType {
                args: args
                    .into_iter()
                    .map(|a| {
                        a.downcast()
                            .type_annotation
                            .as_ref()
                            .map(|s| s.resolve_type(ctx))
                    })
                    .collect(),
                args_spread: args_spread
                    .as_ref()
                    .map(|s| s.resolve_type(ctx))
                    .map(Box::new),
                is_pure,
                is_async,
                throws: throws.as_ref().map(|s| s.resolve_type(ctx)).map(Box::new),
            },
            TypeExpression::FuncType(FuncType {
                args,
                args_spread,
                is_pure,
                is_async: _,
                returns,
            }) => Type::FuncType {
                args: args
                    .into_iter()
                    .map(|a| {
                        a.downcast()
                            .type_annotation
                            .as_ref()
                            .map(|s| s.resolve_type(ctx))
                    })
                    .collect(),
                args_spread: args_spread
                    .as_ref()
                    .map(|s| s.resolve_type(ctx))
                    .map(Box::new),
                is_pure,
                returns: returns
                    .as_ref()
                    .map(|s| s.resolve_type(ctx))
                    .map(Box::new)
                    .unwrap(),
            },
            TypeExpression::GenericType(GenericType { type_params, inner }) => todo!(),
            TypeExpression::BoundGenericType(BoundGenericType { type_args, generic }) => todo!(),
            TypeExpression::ObjectType(ObjectType {
                entries,
                is_interface,
            }) => todo!(),
            //  match inner.resolve_type(ctx) {
            //     Type::RecordType {
            //         key_type,
            //         value_type,
            //         mutability,
            //     } => key_type.as_ref().clone(),
            //     Type::ObjectType {
            //         entries,
            //         mutability,
            //         is_interface,
            //     } => Type::UnionType(
            //         entries
            //             .into_iter()
            //             .map(|(key, _)| Type::StringType(Some(key)))
            //             .collect(),
            //     ),
            //     _ => Type::PoisonedType,
            // },
            TypeExpression::ModifierType(ModifierType { kind, inner }) => {
                let inner = Box::new(inner.resolve_type(ctx));

                match kind {
                    ModifierTypeKind::Readonly => Type::ReadonlyType(inner),
                    ModifierTypeKind::Keyof => Type::KeyofType(inner),
                    ModifierTypeKind::Valueof => Type::ValueofType(inner),
                    ModifierTypeKind::Elementof => Type::ElementofType(inner),
                }
            }
            TypeExpression::SpecialType(SpecialType { kind, inner }) => {
                let inner = Box::new(inner.resolve_type(ctx));

                match kind {
                    SpecialTypeKind::Iterator => Type::IteratorType(inner),
                    SpecialTypeKind::Plan => Type::PlanType(inner),
                    SpecialTypeKind::Error => Type::ErrorType(inner),
                }
            }
            // match inner.resolve_type(ctx) {
            //     Type::ArrayType(element) => element.as_ref().clone(),
            //     Type::TupleType(members) => Type::UnionType(members),
            //     _ => Type::PoisonedType,
            // },
            TypeExpression::UnionType(UnionType(members)) => {
                Type::UnionType(members.iter().map(|m| m.resolve_type(ctx)).collect())
            }
            TypeExpression::MaybeType(MaybeType(inner)) => {
                inner.resolve_type(ctx).union(Type::NilType)
            }
            TypeExpression::NamedType(NamedType(name)) => Type::NamedType(name.clone()),
            TypeExpression::RegularExpressionType(_) => Type::RegularExpressionType {},
            TypeExpression::RecordType(RecordType {
                key_type,
                value_type,
            }) => Type::RecordType {
                key_type: Box::new(key_type.resolve_type(ctx)),
                value_type: Box::new(value_type.resolve_type(ctx)),
            },
            TypeExpression::ArrayType(ArrayType(element)) => {
                Type::ArrayType(Box::new(element.resolve_type(ctx)))
            }
            TypeExpression::TupleType(TupleType(members)) => {
                Type::TupleType(members.iter().map(|x| x.resolve_type(ctx)).collect())
            }
            TypeExpression::StringType(_) => Type::ANY_STRING,
            TypeExpression::NumberType(_) => Type::ANY_NUMBER,
            TypeExpression::BooleanType(_) => Type::ANY_BOOLEAN,
            TypeExpression::StringLiteralType(StringLiteralType(value)) => {
                Type::StringType(Some(value.clone()))
            }
            TypeExpression::NumberLiteralType(NumberLiteralType(value)) => {
                let n = Some(value.as_str().parse().unwrap());
                Type::NumberType { min: n, max: n }
            }
            TypeExpression::BooleanLiteralType(BooleanLiteralType(value)) => {
                Type::BooleanType(Some(value))
            }
            TypeExpression::NilType(_) => Type::NilType,
            TypeExpression::ParenthesizedType(ParenthesizedType(inner)) => inner.resolve_type(ctx),
            TypeExpression::TypeofType(TypeofType(expression)) => expression.infer_type(ctx.into()),
            TypeExpression::UnknownType(_) => Type::UnknownType,
            TypeExpression::PropertyType(PropertyType {
                subject,
                property,
                optional: _,
            }) => {
                let subject_type = subject.resolve_type(ctx);

                match subject_type {
                    Type::ObjectType {
                        entries,
                        is_interface: _,
                    } => entries
                        .into_iter()
                        .find(|(key, value)| key.as_str() == property.slice().as_str())
                        .map(|(key, value)| value.as_ref().clone())
                        .unwrap_or(Type::PoisonedType),
                    _ => Type::PoisonedType,
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ResolveContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
}

impl<'a> From<InferTypeContext<'a>> for ResolveContext<'a> {
    fn from(
        InferTypeContext {
            modules,
            current_module,
        }: InferTypeContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a, F: FnMut(BagelError)> From<&mut CheckContext<'a, F>> for ResolveContext<'a> {
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

impl<'a> From<SubsumationContext<'a>> for ResolveContext<'a> {
    fn from(
        SubsumationContext {
            modules,
            current_module,
            dest_mutability: _,
            val_mutability: _,
        }: SubsumationContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}
