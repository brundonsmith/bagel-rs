use std::rc::Rc;

use crate::{
    cli::ModulesStore,
    model::{
        ast::{
            ArrayType, BooleanLiteralType, BoundGenericType, ElementOrSpread, FuncType,
            GenericParamType, GenericType, KeyValueOrSpread, MaybeType, ModifierType, NamedType,
            NumberLiteralType, ObjectType, ParenthesizedType, ProcType, PropertyType, RecordType,
            SpecialType, StringLiteralType, TupleType, TypeExpression, TypeofType, UnionType, AST,
        },
        Mutability, ParsedModule, Type, TypeParam,
    },
    utils::Rcable,
};

#[derive(Clone, Copy, Debug)]
pub struct ResolveTypeContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a ParsedModule,
}

impl AST<TypeExpression> {
    pub fn resolve_type<'a>(&self, ctx: ResolveTypeContext<'a>) -> Type {
        match self.downcast() {
            TypeExpression::ModifierType(ModifierType { kind, inner }) => Type::MetaType {
                kind: kind.into(),
                inner: inner.resolve_type(ctx).rc(),
            },
            TypeExpression::GenericParamType(GenericParamType { name, extends }) => extends
                .map(|t| t.resolve_type(ctx))
                .unwrap_or(Type::UnknownType(Mutability::Mutable)),
            TypeExpression::GenericType(GenericType { type_params, inner }) => todo!(),
            TypeExpression::BoundGenericType(BoundGenericType { type_args, generic }) => {
                Type::BoundGenericType {
                    type_args: type_args
                        .into_iter()
                        .map(|arg| arg.resolve_type(ctx))
                        .collect(),
                    generic: generic.resolve_type(ctx).rc(),
                }
            }
            TypeExpression::ProcType(ProcType {
                type_params,
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            }) => {
                let proc_type = Type::ProcType {
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
                        .map(|s| s.downcast().type_annotation.clone())
                        .flatten()
                        .map(|s| s.resolve_type(ctx))
                        .map(Rc::new),
                    is_pure,
                    is_async,
                    throws: throws.as_ref().map(|s| s.resolve_type(ctx)).map(Rc::new),
                };

                if type_params.len() > 0 {
                    Type::GenericType {
                        type_params: type_params
                            .into_iter()
                            .map(|param| TypeParam {
                                name: param.downcast().name.downcast().0.clone(),
                                extends: param
                                    .downcast()
                                    .extends
                                    .map(|extends| extends.resolve_type(ctx)),
                            })
                            .collect(),
                        inner: proc_type.rc(),
                    }
                } else {
                    proc_type
                }
            }
            TypeExpression::FuncType(FuncType {
                type_params,
                args,
                args_spread,
                is_pure,
                is_async: _,
                returns,
            }) => {
                let func_type = Type::FuncType {
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
                        .map(|s| s.downcast().type_annotation.clone())
                        .flatten()
                        .map(|s| s.resolve_type(ctx))
                        .map(Rc::new),
                    is_pure,
                    returns: returns
                        .as_ref()
                        .map(|s| s.resolve_type(ctx))
                        .map(Rc::new)
                        .unwrap(),
                };

                if type_params.len() > 0 {
                    Type::GenericType {
                        type_params: type_params
                            .into_iter()
                            .map(|param| TypeParam {
                                name: param.downcast().name.downcast().0.clone(),
                                extends: param
                                    .downcast()
                                    .extends
                                    .map(|extends| extends.resolve_type(ctx)),
                            })
                            .collect(),
                        inner: func_type.rc(),
                    }
                } else {
                    func_type
                }
            }
            TypeExpression::SpecialType(SpecialType { kind, inner }) => Type::MetaType {
                kind: kind.into(),
                inner: inner.resolve_type(ctx).rc(),
            },
            TypeExpression::ObjectType(ObjectType {
                entries,
                is_interface,
            }) => Type::ObjectType {
                mutability: Mutability::Mutable,
                entries: entries
                    .into_iter()
                    .map(|entry| match entry {
                        KeyValueOrSpread::KeyValue(key, value, optional) => {
                            KeyValueOrSpread::KeyValue(
                                key.resolve_type(ctx),
                                value.resolve_type(ctx),
                                optional,
                            )
                        }
                        KeyValueOrSpread::Spread(spread) => {
                            KeyValueOrSpread::Spread(spread.resolve_type(ctx))
                        }
                    })
                    .collect(),
                is_interface,
            },
            TypeExpression::UnionType(UnionType(members)) => {
                Type::UnionType(members.iter().map(|m| m.resolve_type(ctx)).collect())
            }
            TypeExpression::MaybeType(MaybeType(inner)) => {
                inner.resolve_type(ctx).union(Type::NilType)
            }
            TypeExpression::NamedType(NamedType(name)) => Type::NamedType {
                mutability: Mutability::Mutable,
                name: name.clone(),
            },
            TypeExpression::RecordType(RecordType {
                key_type,
                value_type,
            }) => Type::RecordType {
                mutability: Mutability::Mutable,
                key_type: key_type.resolve_type(ctx).rc(),
                value_type: value_type.resolve_type(ctx).rc(),
            },
            TypeExpression::ArrayType(ArrayType(element)) => Type::ArrayType {
                mutability: Mutability::Mutable,
                element_type: element.resolve_type(ctx).rc(),
            },
            TypeExpression::TupleType(TupleType(members)) => Type::TupleType {
                mutability: Mutability::Mutable,
                members: members
                    .iter()
                    .map(|x| match x {
                        ElementOrSpread::Element(element) => {
                            ElementOrSpread::Element(element.resolve_type(ctx))
                        }
                        ElementOrSpread::Spread(spread) => {
                            ElementOrSpread::Spread(spread.resolve_type(ctx))
                        }
                    })
                    .collect(),
            },
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
            TypeExpression::ParenthesizedType(ParenthesizedType(inner)) => inner.resolve_type(ctx),
            TypeExpression::TypeofType(TypeofType(expression)) => expression.infer_type(ctx.into()),
            TypeExpression::PropertyType(PropertyType {
                subject,
                property,
                optional: _,
            }) => Type::PropertyType {
                subject: subject.resolve_type(ctx).rc(),
                property: property.resolve_type(ctx).rc(),
            },
            TypeExpression::RegularExpressionType(_) => Type::RegularExpressionType {},
            TypeExpression::StringType(_) => Type::ANY_STRING,
            TypeExpression::NumberType(_) => Type::ANY_NUMBER,
            TypeExpression::BooleanType(_) => Type::ANY_BOOLEAN,
            TypeExpression::NilType(_) => Type::NilType,
            TypeExpression::UnknownType(_) => Type::UnknownType(Mutability::Mutable),
        }
    }
}
