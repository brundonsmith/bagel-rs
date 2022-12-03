use enum_variant_type::EnumVariantType;

use crate::{
    model::ast::{BooleanLiteral, ExactStringLiteral, Expression, NumberLiteral, PlainIdentifier},
    model::{bgl_type::Type, slice::Slice},
    passes::check::CheckContext,
    passes::typeinfer::InferTypeContext,
    ModulesStore,
};

use super::{Module, ModuleID, Src};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum TypeExpression {
    #[evt(derive(Debug, Clone, PartialEq))]
    UnionType(Vec<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    MaybeType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    NamedType(PlainIdentifier),

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericParamType {
        name: PlainIdentifier,
        extends: Option<Box<Src<TypeExpression>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcType {
        args: Vec<Src<Arg>>,
        args_spread: Option<Box<Src<TypeExpression>>>,
        is_pure: bool,
        is_async: bool,
        throws: Option<Box<Src<TypeExpression>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncType {
        args: Vec<Src<Arg>>,
        args_spread: Option<Box<Src<TypeExpression>>>,
        is_pure: bool,
        returns: Option<Box<Src<TypeExpression>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericType {
        type_params: Vec<(PlainIdentifier, Option<Src<TypeExpression>>)>,
        inner: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BoundGenericType {
        type_args: Vec<Src<TypeExpression>>,
        generic: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectType(Vec<ObjectTypeEntry>),

    #[evt(derive(Debug, Clone, PartialEq))]
    InterfaceType(Vec<(PlainIdentifier, Src<TypeExpression>)>),

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        key_type: Box<Src<TypeExpression>>,
        value_type: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType(Vec<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ReadonlyType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    StringType,

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberType,

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanType,

    #[evt(derive(Debug, Clone, PartialEq))]
    NilType,

    #[evt(derive(Debug, Clone, PartialEq))]
    LiteralType(LiteralTypeValue),

    #[evt(derive(Debug, Clone, PartialEq))]
    IteratorType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    PlanType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ParenthesizedType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeofType(Src<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyofType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueofType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementofType(Box<Src<TypeExpression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    UnknownType,

    #[evt(derive(Debug, Clone, PartialEq))]
    PoisonedType,

    #[evt(derive(Debug, Clone, PartialEq))]
    AnyType,

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpressionType, // TODO: Number of match groups?

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyType {
        subject: Box<Src<TypeExpression>>,
        property: PlainIdentifier, // TODO:  | TypeExpression?
        optional: bool,
    },
}

impl Src<TypeExpression> {
    pub fn resolve<'a>(&self, ctx: ResolveContext<'a>) -> Type {
        match &self.node {
            TypeExpression::GenericParamType { name, extends } => todo!(),
            TypeExpression::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            TypeExpression::FuncType {
                args,
                args_spread,
                is_pure,
                returns,
            } => todo!(),
            TypeExpression::GenericType { type_params, inner } => todo!(),
            TypeExpression::BoundGenericType { type_args, generic } => todo!(),
            TypeExpression::ObjectType(entries) => todo!(),
            TypeExpression::InterfaceType(entries) => todo!(),
            TypeExpression::KeyofType(inner) => Type::KeyofType(Box::new(inner.resolve(ctx))),
            //  match inner.resolve(ctx) {
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
            TypeExpression::ValueofType(inner) => Type::ValueofType(Box::new(inner.resolve(ctx))),
            TypeExpression::ElementofType(inner) => {
                Type::ElementofType(Box::new(inner.resolve(ctx)))
            }
            // match inner.resolve(ctx) {
            //     Type::ArrayType(element) => element.as_ref().clone(),
            //     Type::TupleType(members) => Type::UnionType(members),
            //     _ => Type::PoisonedType,
            // },
            TypeExpression::UnionType(members) => {
                Type::UnionType(members.iter().map(|m| m.resolve(ctx)).collect())
            }
            TypeExpression::MaybeType(inner) => inner.resolve(ctx).union(Type::NilType),
            TypeExpression::NamedType(name) => Type::NamedType {
                module_id: ctx.current_module.module_id.clone(),
                name: name.0.clone(),
            },
            TypeExpression::RegularExpressionType {} => Type::RegularExpressionType {},
            TypeExpression::RecordType {
                key_type,
                value_type,
            } => Type::RecordType {
                key_type: Box::new(key_type.resolve(ctx)),
                value_type: Box::new(value_type.resolve(ctx)),
            },
            TypeExpression::ArrayType(element) => Type::ArrayType(Box::new(element.resolve(ctx))),
            TypeExpression::TupleType(members) => {
                Type::TupleType(members.iter().map(|x| x.resolve(ctx)).collect())
            }
            TypeExpression::ReadonlyType(inner) => Type::ReadonlyType(Box::new(inner.resolve(ctx))),
            TypeExpression::StringType => Type::ANY_STRING,
            TypeExpression::NumberType => Type::ANY_NUMBER,
            TypeExpression::BooleanType => Type::ANY_BOOLEAN,
            TypeExpression::NilType => Type::NilType,
            TypeExpression::LiteralType(value) => match value {
                LiteralTypeValue::ExactString(s) => Type::StringType(Some(s.value.clone())),
                LiteralTypeValue::NumberLiteral(n) => {
                    let n = Some(n.0.as_str().parse().unwrap());

                    Type::NumberType { min: n, max: n }
                }
                LiteralTypeValue::BooleanLiteral(_) => todo!(),
            },
            TypeExpression::IteratorType(inner) => Type::IteratorType(Box::new(inner.resolve(ctx))),
            TypeExpression::PlanType(inner) => Type::PlanType(Box::new(inner.resolve(ctx))),
            TypeExpression::ErrorType(inner) => Type::ErrorType(Box::new(inner.resolve(ctx))),
            TypeExpression::ParenthesizedType(inner) => inner.resolve(ctx),
            TypeExpression::TypeofType(expression) => expression.infer_type(ctx.into()),
            TypeExpression::UnknownType => Type::UnknownType,
            TypeExpression::PoisonedType => Type::PoisonedType,
            TypeExpression::AnyType => Type::AnyType,
            TypeExpression::PropertyType {
                subject,
                property,
                optional: _,
            } => {
                let subject_type = subject.resolve(ctx);

                match subject_type {
                    Type::ObjectType {
                        entries,
                        is_interface: _,
                    } => entries
                        .into_iter()
                        .find(|(key, value)| key.as_str() == property.0.as_str())
                        .map(|(key, value)| value.as_ref().clone())
                        .unwrap_or(Type::PoisonedType),
                    _ => Type::PoisonedType,
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Constant,
    Readonly,
    Mutable,
    Literal,
}

impl Mutability {
    pub fn is_mutable(&self) -> bool {
        match self {
            Mutability::Constant => false,
            Mutability::Readonly => false,
            Mutability::Mutable => true,
            Mutability::Literal => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arg {
    pub name: PlainIdentifier,
    pub type_annotation: Option<Src<TypeExpression>>,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectTypeEntry {
    Spread(NamedType),
    KeyValue(PlainIdentifier, Box<Src<TypeExpression>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralTypeValue {
    ExactString(ExactStringLiteral),
    NumberLiteral(NumberLiteral),
    BooleanLiteral(BooleanLiteral),
}
