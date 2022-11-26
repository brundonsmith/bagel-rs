use enum_variant_type::EnumVariantType;

use crate::{
    model::ast::{BooleanLiteral, ExactStringLiteral, Expression, NumberLiteral, PlainIdentifier},
    model::bgl_type::Type,
    passes::check::CheckContext,
    passes::typeinfer::InferTypeContext,
    ModulesStore,
};

use super::{Module, ModuleID, Src};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum TypeExpression {
    #[evt(derive(Debug, Clone, PartialEq))]
    UnionType { members: Vec<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    MaybeType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    NamedType { name: Src<PlainIdentifier> },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericParamType {
        name: Src<PlainIdentifier>,
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
        type_params: Vec<(Src<PlainIdentifier>, Option<Src<TypeExpression>>)>,
        inner: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BoundGenericType {
        type_args: Vec<Src<TypeExpression>>,
        generic: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectType { entries: Vec<ObjectTypeEntry> },

    #[evt(derive(Debug, Clone, PartialEq))]
    InterfaceType {
        entries: Vec<(Src<PlainIdentifier>, Src<TypeExpression>)>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        key_type: Box<Src<TypeExpression>>,
        value_type: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType { element: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType { members: Vec<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    ReadonlyType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringType,

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberType,

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanType,

    #[evt(derive(Debug, Clone, PartialEq))]
    NilType,

    #[evt(derive(Debug, Clone, PartialEq))]
    LiteralType { value: LiteralTypeValue },

    #[evt(derive(Debug, Clone, PartialEq))]
    NominalType {
        module_id: ModuleID,
        name: String,
        inner: Option<Box<Src<TypeExpression>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IteratorType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    PlanType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    ParenthesizedType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeofType { expression: Src<Expression> },

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyofType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueofType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementofType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    UnknownType,

    #[evt(derive(Debug, Clone, PartialEq))]
    PoisonedType,

    #[evt(derive(Debug, Clone, PartialEq))]
    AnyType,

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpressionType {
        // TODO: Number of match groups?
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyType {
        subject: Box<Src<TypeExpression>>,
        property: Src<PlainIdentifier>, // TODO:  | TypeExpression
        optional: bool,
    },
}

impl Src<TypeExpression> {
    pub fn resolve<'a>(&self, ctx: ResolveContext<'a>) -> Type {
        match &self.node {
            TypeExpression::UnionType { members } => Type::UnionType {
                members: members.iter().map(|m| m.resolve(ctx)).collect(),
            },
            TypeExpression::MaybeType { inner } => inner.resolve(ctx).union(Type::NilType),
            TypeExpression::NamedType { name } => Type::NamedType {
                module_id: ctx.current_module.module_id.clone(),
                name: name.node.0.clone(),
                index: name.src.unwrap().start,
            },
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
            TypeExpression::ObjectType { entries } => todo!(),
            TypeExpression::InterfaceType { entries } => todo!(),
            TypeExpression::RecordType {
                key_type,
                value_type,
            } => Type::RecordType {
                key_type: Box::new(key_type.resolve(ctx)),
                value_type: Box::new(value_type.resolve(ctx)),
                mutability: ctx.mutability,
            },
            TypeExpression::ArrayType { element } => Type::ArrayType {
                element: Box::new(element.resolve(ctx)),
                mutability: ctx.mutability,
            },
            TypeExpression::TupleType { members } => Type::TupleType {
                members: members.iter().map(|x| x.resolve(ctx)).collect(),
                mutability: ctx.mutability,
            },
            TypeExpression::ReadonlyType { inner } => {
                inner.resolve(ctx.with_mutability(Mutability::Readonly))
            }
            TypeExpression::StringType => Type::StringType,
            TypeExpression::NumberType => Type::NumberType,
            TypeExpression::BooleanType => Type::BooleanType,
            TypeExpression::NilType => Type::NilType,
            TypeExpression::LiteralType { value } => Type::LiteralType {
                value: value.clone().into(),
            },
            TypeExpression::NominalType {
                module_id,
                name,
                inner,
            } => Type::NominalType {
                module_id: module_id.clone(),
                name: name.clone(),
                inner: inner.as_ref().map(|inner| Box::new(inner.resolve(ctx))),
            },
            TypeExpression::IteratorType { inner } => Type::IteratorType {
                inner: Box::new(inner.resolve(ctx)),
            },
            TypeExpression::PlanType { inner } => Type::PlanType {
                inner: Box::new(inner.resolve(ctx)),
            },
            TypeExpression::ErrorType { inner } => Type::ErrorType {
                inner: Box::new(inner.resolve(ctx)),
            },
            TypeExpression::ParenthesizedType { inner } => inner.resolve(ctx),
            TypeExpression::TypeofType { expression } => expression.infer_type(ctx.into()),
            TypeExpression::KeyofType { inner } => todo!(),
            TypeExpression::ValueofType { inner } => todo!(),
            TypeExpression::ElementofType { inner } => todo!(),
            TypeExpression::UnknownType => Type::UnknownType {
                mutability: ctx.mutability,
            },
            TypeExpression::PoisonedType => Type::PoisonedType,
            TypeExpression::AnyType => Type::AnyType,
            TypeExpression::RegularExpressionType {} => todo!(),
            TypeExpression::PropertyType {
                subject,
                property,
                optional,
            } => {
                let subject_type = subject.resolve(ctx);

                match subject_type {
                    Type::ObjectType {
                        entries,
                        mutability,
                        is_interface,
                    } => entries
                        .into_iter()
                        .find(|(key, value)| *key == property.node.0)
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
    pub mutability: Mutability,
}

impl<'a> ResolveContext<'a> {
    pub fn with_mutability(self, mutability: Mutability) -> Self {
        Self {
            modules: self.modules,
            current_module: self.current_module,
            mutability,
        }
    }
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
            mutability: Mutability::Mutable,
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
            mutability: Mutability::Mutable,
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
    pub name: Src<PlainIdentifier>,
    pub type_annotation: Option<Src<TypeExpression>>,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectTypeEntry {
    Spread(NamedType),
    KeyValue(Src<PlainIdentifier>, Box<Src<TypeExpression>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralTypeValue {
    ExactString(ExactStringLiteral),
    NumberLiteral(NumberLiteral),
    BooleanLiteral(BooleanLiteral),
}
