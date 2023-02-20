use std::rc::Rc;

use crate::model::{
    ast::*,
    bgl_type::{Mutability, SubsumationContext, Type},
    module::{Module, ModulesStore},
};

use super::{check::CheckContext, typeinfer::InferTypeContext};

impl AST<TypeExpression> {
    pub fn resolve_type<'a>(&self, ctx: ResolveContext<'a>) -> Type {
        match self.downcast() {
            TypeExpression::ModifierType(ModifierType { kind, inner }) => Type::ModifierType {
                kind,
                inner: Rc::new(inner.resolve_type(ctx)),
            },
            TypeExpression::GenericParamType(GenericParamType { name, extends }) => todo!(),
            TypeExpression::GenericType(GenericType { type_params, inner }) => todo!(),
            TypeExpression::BoundGenericType(BoundGenericType { type_args, generic }) => todo!(),
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
                    .map(|s| s.downcast().type_annotation.clone())
                    .flatten()
                    .map(|s| s.resolve_type(ctx))
                    .map(Rc::new),
                is_pure,
                is_async,
                throws: throws.as_ref().map(|s| s.resolve_type(ctx)).map(Rc::new),
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
            },
            TypeExpression::SpecialType(SpecialType { kind, inner }) => Type::SpecialType {
                kind,
                inner: Rc::new(inner.resolve_type(ctx)),
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
                key_type: Rc::new(key_type.resolve_type(ctx)),
                value_type: Rc::new(value_type.resolve_type(ctx)),
            },
            TypeExpression::ArrayType(ArrayType(element)) => Type::ArrayType {
                mutability: Mutability::Mutable,
                element_type: Rc::new(element.resolve_type(ctx)),
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
                subject: Rc::new(subject.resolve_type(ctx)),
                property: Rc::new(property.resolve_type(ctx)),
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

impl<'a> From<&CheckContext<'a>> for ResolveContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            nearest_func_or_proc: _,
        }: &CheckContext<'a>,
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
            symbols_encountered: _,
        }: SubsumationContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}
