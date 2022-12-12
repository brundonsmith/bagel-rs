use crate::model::{
    ast::{ASTDetails, AST},
    bgl_type::Type,
    module::{Module, ModulesStore},
};

use super::{check::CheckContext, typeinfer::InferTypeContext};

impl AST {
    pub fn resolve_type<'a>(&self, ctx: ResolveContext<'a>) -> Type {
        match self.details() {
            ASTDetails::GenericParamType { name, extends } => todo!(),
            ASTDetails::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            ASTDetails::FuncType {
                args,
                args_spread,
                is_pure,
                returns,
            } => todo!(),
            ASTDetails::GenericType { type_params, inner } => todo!(),
            ASTDetails::BoundGenericType { type_args, generic } => todo!(),
            ASTDetails::ObjectType(entries) => todo!(),
            ASTDetails::InterfaceType(entries) => todo!(),
            ASTDetails::KeyofType(inner) => Type::KeyofType(Box::new(inner.resolve_type(ctx))),
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
            ASTDetails::ValueofType(inner) => Type::ValueofType(Box::new(inner.resolve_type(ctx))),
            ASTDetails::ElementofType(inner) => {
                Type::ElementofType(Box::new(inner.resolve_type(ctx)))
            }
            // match inner.resolve_type(ctx) {
            //     Type::ArrayType(element) => element.as_ref().clone(),
            //     Type::TupleType(members) => Type::UnionType(members),
            //     _ => Type::PoisonedType,
            // },
            ASTDetails::UnionType(members) => {
                Type::UnionType(members.iter().map(|m| m.resolve_type(ctx)).collect())
            }
            ASTDetails::MaybeType(inner) => inner.resolve_type(ctx).union(Type::NilType),
            ASTDetails::NamedType(name) => Type::NamedType {
                module_id: ctx.current_module.module_id.clone(),
                name: name.slice().clone(),
            },
            ASTDetails::RegularExpressionType {} => Type::RegularExpressionType {},
            ASTDetails::RecordType {
                key_type,
                value_type,
            } => Type::RecordType {
                key_type: Box::new(key_type.resolve_type(ctx)),
                value_type: Box::new(value_type.resolve_type(ctx)),
            },
            ASTDetails::ArrayType(element) => Type::ArrayType(Box::new(element.resolve_type(ctx))),
            ASTDetails::TupleType(members) => {
                Type::TupleType(members.iter().map(|x| x.resolve_type(ctx)).collect())
            }
            ASTDetails::ReadonlyType(inner) => {
                Type::ReadonlyType(Box::new(inner.resolve_type(ctx)))
            }
            ASTDetails::StringType => Type::ANY_STRING,
            ASTDetails::NumberType => Type::ANY_NUMBER,
            ASTDetails::BooleanType => Type::ANY_BOOLEAN,
            ASTDetails::NilType => Type::NilType,
            ASTDetails::ExactStringLiteral { tag: _, value } => {
                Type::StringType(Some(value.clone()))
            }
            ASTDetails::NumberLiteral(value) => {
                let n = Some(value.as_str().parse().unwrap());
                Type::NumberType { min: n, max: n }
            }
            ASTDetails::BooleanLiteral(value) => todo!(),
            ASTDetails::IteratorType(inner) => {
                Type::IteratorType(Box::new(inner.resolve_type(ctx)))
            }
            ASTDetails::PlanType(inner) => Type::PlanType(Box::new(inner.resolve_type(ctx))),
            ASTDetails::ErrorType(inner) => Type::ErrorType(Box::new(inner.resolve_type(ctx))),
            ASTDetails::ParenthesizedType(inner) => inner.resolve_type(ctx),
            ASTDetails::TypeofType(expression) => expression.infer_type(ctx.into()),
            ASTDetails::UnknownType => Type::UnknownType,
            ASTDetails::PropertyType {
                subject,
                property,
                optional: _,
            } => {
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
            _ => todo!(),
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

impl<'a> From<CheckContext<'a>> for ResolveContext<'a> {
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
