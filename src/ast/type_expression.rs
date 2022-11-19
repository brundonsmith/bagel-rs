use colored::Colorize;
use enum_variant_type::EnumVariantType;

use crate::{
    ast::{BooleanLiteral, ExactStringLiteral, Expression, Module, NumberLiteral, PlainIdentifier},
    check::CheckContext,
    errors::SubsumationIssue,
    typeinfer::InferTypeContext,
};

use super::Src;

#[derive(Clone, Debug, EnumVariantType)]
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
        args: Src<Args>,
        is_pure: bool,
        is_async: bool,
        throws: Option<Box<Src<TypeExpression>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncType {
        args: Src<Args>,
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
    ObjectType {
        entries: Vec<ObjectTypeEntry>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InterfaceType {
        entries: Vec<(Src<PlainIdentifier>, Src<TypeExpression>)>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        key_type: Box<Src<TypeExpression>>,
        value_type: Box<Src<TypeExpression>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType {
        element: Box<Src<TypeExpression>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType {
        members: Vec<Src<TypeExpression>>,
        mutability: Mutability,
    },

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
    TypeofType { expression: Expression },

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyofType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueofType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementofType { inner: Box<Src<TypeExpression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    UnknownType { mutability: Mutability },

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

pub const NIL_TYPE: Src<TypeExpression> = Src {
    src: None,
    node: TypeExpression::NilType,
};
pub const BOOLEAN_TYPE: Src<TypeExpression> = Src {
    src: None,
    node: TypeExpression::BooleanType,
};
pub const NUMBER_TYPE: Src<TypeExpression> = Src {
    src: None,
    node: TypeExpression::NumberType,
};
pub const STRING_TYPE: Src<TypeExpression> = Src {
    src: None,
    node: TypeExpression::StringType,
};
pub const UNKNOWN_TYPE: Src<TypeExpression> = Src {
    src: None,
    node: TypeExpression::UnknownType {
        mutability: Mutability::Mutable,
    },
};

impl Src<TypeExpression> {
    pub fn subsumation_issues(
        &self,
        ctx: SubsumationContext,
        other: &Self,
    ) -> Option<Vec<SubsumationIssue>> {
        let destination = self.clone().resolve(ctx.into());
        let value = other.clone().resolve(ctx.into());

        if destination == value {
            return None;
        }

        match &destination.node {
            TypeExpression::UnionType { members } => {
                if members.iter().any(|member| member.subsumes(ctx, &value)) {
                    return None;
                }
            }
            _ => {}
        };

        Some(vec![format!(
            "Type {} is not assignable to type {}",
            value.to_string().blue(),
            destination.to_string().blue()
        )
        .into()])
    }

    pub fn subsumes(&self, ctx: SubsumationContext, other: &Self) -> bool {
        self.subsumation_issues(ctx, other).is_none()
    }

    pub fn resolve(self, ctx: ResolveContext) -> Self {
        match self {
            _ => self,
        }
    }

    pub fn union(self, other: Self) -> Self {
        Src {
            src: None,
            node: TypeExpression::UnionType {
                members: vec![self, other],
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SubsumationContext<'a> {
    pub module: &'a Module,
}

impl<'a> From<CheckContext<'a>> for SubsumationContext<'a> {
    fn from(CheckContext { module }: CheckContext<'a>) -> Self {
        Self { module }
    }
}

impl<'a> From<InferTypeContext<'a>> for SubsumationContext<'a> {
    fn from(InferTypeContext { module }: InferTypeContext<'a>) -> Self {
        Self { module }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ResolveContext<'a> {
    pub module: &'a Module,
}

impl<'a> From<SubsumationContext<'a>> for ResolveContext<'a> {
    fn from(SubsumationContext { module }: SubsumationContext<'a>) -> Self {
        Self { module }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Constant,
    Readonly,
    Mutable,
    Literal,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Args {
    Individual(Vec<Arg>),
    Spread {
        name: Option<Src<PlainIdentifier>>,
        type_annotation: Box<Src<TypeExpression>>,
    },
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

impl PartialEq for TypeExpression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::UnionType { members: l_members }, Self::UnionType { members: r_members }) => {
                l_members == r_members
            }
            (Self::MaybeType { inner: l_inner }, Self::MaybeType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (Self::NamedType { name: l_name }, Self::NamedType { name: r_name }) => {
                l_name == r_name
            }
            (
                Self::GenericParamType {
                    name: l_name,
                    extends: l_extends,
                },
                Self::GenericParamType {
                    name: r_name,
                    extends: r_extends,
                },
            ) => l_name == r_name && l_extends == r_extends,
            (
                Self::ProcType {
                    args: l_args,
                    is_pure: l_is_pure,
                    is_async: l_is_async,
                    throws: l_throws,
                },
                Self::ProcType {
                    args: r_args,
                    is_pure: r_is_pure,
                    is_async: r_is_async,
                    throws: r_throws,
                },
            ) => {
                l_args == r_args
                    && l_is_pure == r_is_pure
                    && l_is_async == r_is_async
                    && l_throws == r_throws
            }
            (
                Self::FuncType {
                    args: l_args,
                    is_pure: l_is_pure,
                    returns: l_returns,
                },
                Self::FuncType {
                    args: r_args,
                    is_pure: r_is_pure,
                    returns: r_returns,
                },
            ) => l_args == r_args && l_is_pure == r_is_pure && l_returns == r_returns,
            (
                Self::GenericType {
                    type_params: l_type_params,
                    inner: l_inner,
                },
                Self::GenericType {
                    type_params: r_type_params,
                    inner: r_inner,
                },
            ) => l_type_params == r_type_params && l_inner == r_inner,
            (
                Self::BoundGenericType {
                    type_args: l_type_args,
                    generic: l_generic,
                },
                Self::BoundGenericType {
                    type_args: r_type_args,
                    generic: r_generic,
                },
            ) => l_type_args == r_type_args && l_generic == r_generic,
            (
                Self::ObjectType {
                    entries: l_entries,
                    mutability: l_mutability,
                },
                Self::ObjectType {
                    entries: r_entries,
                    mutability: r_mutability,
                },
            ) => l_entries == r_entries && l_mutability == r_mutability,
            (
                Self::InterfaceType {
                    entries: l_entries,
                    mutability: l_mutability,
                },
                Self::InterfaceType {
                    entries: r_entries,
                    mutability: r_mutability,
                },
            ) => l_entries == r_entries && l_mutability == r_mutability,
            (
                Self::RecordType {
                    key_type: l_key_type,
                    value_type: l_value_type,
                    mutability: l_mutability,
                },
                Self::RecordType {
                    key_type: r_key_type,
                    value_type: r_value_type,
                    mutability: r_mutability,
                },
            ) => {
                l_key_type == r_key_type
                    && l_value_type == r_value_type
                    && l_mutability == r_mutability
            }
            (
                Self::ArrayType {
                    element: l_element,
                    mutability: l_mutability,
                },
                Self::ArrayType {
                    element: r_element,
                    mutability: r_mutability,
                },
            ) => l_element == r_element && l_mutability == r_mutability,
            (
                Self::TupleType {
                    members: l_members,
                    mutability: l_mutability,
                },
                Self::TupleType {
                    members: r_members,
                    mutability: r_mutability,
                },
            ) => l_members == r_members && l_mutability == r_mutability,
            (Self::ReadonlyType { inner: l_inner }, Self::ReadonlyType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (Self::StringType, Self::StringType) => true,
            (Self::NumberType, Self::NumberType) => true,
            (Self::BooleanType, Self::BooleanType) => true,
            (Self::NilType, Self::NilType) => true,
            (Self::LiteralType { value: l_value }, Self::LiteralType { value: r_value }) => {
                l_value == r_value
            }
            (
                Self::NominalType {
                    name: l_name,
                    inner: l_inner,
                },
                Self::NominalType {
                    name: r_name,
                    inner: r_inner,
                },
            ) => l_name == r_name && l_inner == r_inner,
            (Self::IteratorType { inner: l_inner }, Self::IteratorType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (Self::PlanType { inner: l_inner }, Self::PlanType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (Self::ErrorType { inner: l_inner }, Self::ErrorType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (
                Self::ParenthesizedType { inner: l_inner },
                Self::ParenthesizedType { inner: r_inner },
            ) => l_inner == r_inner,
            (
                Self::TypeofType {
                    expression: l_expression,
                },
                Self::TypeofType {
                    expression: r_expression,
                },
            ) => l_expression == r_expression,
            (Self::KeyofType { inner: l_inner }, Self::KeyofType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (Self::ValueofType { inner: l_inner }, Self::ValueofType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (Self::ElementofType { inner: l_inner }, Self::ElementofType { inner: r_inner }) => {
                l_inner == r_inner
            }
            (
                Self::UnknownType {
                    mutability: l_mutability,
                },
                Self::UnknownType {
                    mutability: r_mutability,
                },
            ) => l_mutability == r_mutability,
            (Self::PoisonedType, Self::PoisonedType) => true,
            (Self::AnyType, Self::AnyType) => true,
            (Self::RegularExpressionType {}, Self::RegularExpressionType {}) => true,
            (
                Self::PropertyType {
                    subject: l_subject,
                    property: l_property,
                    optional: l_optional,
                },
                Self::PropertyType {
                    subject: r_subject,
                    property: r_property,
                    optional: r_optional,
                },
            ) => l_subject == r_subject && l_property == r_property && l_optional == r_optional,
            _ => false,
        }
    }
}
