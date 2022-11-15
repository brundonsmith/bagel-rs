use colored::Colorize;
use enum_variant_type::EnumVariantType;

use crate::{
    ast::{BooleanLiteral, ExactStringLiteral, Expression, Module, NumberLiteral, PlainIdentifier},
    check::CheckContext,
    errors::SubsumationIssue,
    slice::Slice,
    typeinfer::InferTypeContext,
};

#[derive(Clone, Debug, EnumVariantType)]
pub enum TypeExpression {
    #[evt(derive(Debug, Clone, PartialEq))]
    UnionType {
        src: Option<Slice>,
        members: Vec<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    MaybeType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NamedType {
        src: Option<Slice>,
        name: PlainIdentifier,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericParamType {
        src: Option<Slice>,
        name: PlainIdentifier,
        extends: Option<Box<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcType {
        src: Option<Slice>,
        args: Args,
        is_pure: bool,
        is_async: bool,
        throws: Option<Box<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncType {
        src: Option<Slice>,
        args: Args,
        is_pure: bool,
        returns: Option<Box<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericType {
        src: Option<Slice>,
        type_params: Vec<(PlainIdentifier, Option<TypeExpression>)>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BoundGenericType {
        src: Option<Slice>,
        type_args: Vec<TypeExpression>,
        generic: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectType {
        src: Option<Slice>,
        entries: Vec<ObjectTypeEntry>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InterfaceType {
        src: Option<Slice>,
        entries: Vec<(PlainIdentifier, TypeExpression)>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        src: Option<Slice>,
        key_type: Box<TypeExpression>,
        value_type: Box<TypeExpression>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType {
        src: Option<Slice>,
        element: Box<TypeExpression>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType {
        src: Option<Slice>,
        members: Vec<TypeExpression>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ReadonlyType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringType { src: Option<Slice> },

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberType { src: Option<Slice> },

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanType { src: Option<Slice> },

    #[evt(derive(Debug, Clone, PartialEq))]
    NilType { src: Option<Slice> },

    #[evt(derive(Debug, Clone, PartialEq))]
    LiteralType {
        src: Option<Slice>,
        value: LiteralTypeValue,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NominalType {
        src: Option<Slice>,
        name: String,
        inner: Option<Box<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IteratorType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PlanType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ParenthesizedType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeofType {
        src: Option<Slice>,
        expression: Expression,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyofType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueofType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementofType {
        src: Option<Slice>,
        inner: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    UnknownType {
        src: Option<Slice>,

        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PoisonedType { src: Option<Slice> },

    #[evt(derive(Debug, Clone, PartialEq))]
    AnyType { src: Option<Slice> },

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpressionType {
        src: Option<Slice>,
        // TODO: Number of match groups?
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyType {
        src: Option<Slice>,
        subject: Box<TypeExpression>,
        property: PlainIdentifier, // TODO:  | TypeExpression
        optional: bool,
    },
}

pub const NIL_TYPE: TypeExpression = TypeExpression::NilType { src: None };
pub const BOOLEAN_TYPE: TypeExpression = TypeExpression::BooleanType { src: None };
pub const NUMBER_TYPE: TypeExpression = TypeExpression::NumberType { src: None };
pub const STRING_TYPE: TypeExpression = TypeExpression::StringType { src: None };
pub const UNKNOWN_TYPE: TypeExpression = TypeExpression::UnknownType {
    src: None,
    mutability: Mutability::Mutable,
};

impl TypeExpression {
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

        match &destination {
            TypeExpression::UnionType { src, members } => {
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
        TypeExpression::UnionType {
            src: None,
            members: vec![self, other],
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
        name: Option<PlainIdentifier>,
        type_annotation: Box<TypeExpression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arg {
    pub name: PlainIdentifier,
    pub type_annotation: Option<TypeExpression>,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectTypeEntry {
    Spread(NamedType),
    KeyValue(PlainIdentifier, Box<TypeExpression>),
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
            (
                Self::UnionType {
                    src: _,
                    members: l_members,
                },
                Self::UnionType {
                    src: _,
                    members: r_members,
                },
            ) => l_members == r_members,
            (
                Self::MaybeType {
                    src: _,
                    inner: l_inner,
                },
                Self::MaybeType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::NamedType {
                    src: _,
                    name: l_name,
                },
                Self::NamedType {
                    src: _,
                    name: r_name,
                },
            ) => l_name == r_name,
            (
                Self::GenericParamType {
                    src: _,
                    name: l_name,
                    extends: l_extends,
                },
                Self::GenericParamType {
                    src: _,
                    name: r_name,
                    extends: r_extends,
                },
            ) => l_name == r_name && l_extends == r_extends,
            (
                Self::ProcType {
                    src: _,
                    args: l_args,
                    is_pure: l_is_pure,
                    is_async: l_is_async,
                    throws: l_throws,
                },
                Self::ProcType {
                    src: _,
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
                    src: _,
                    args: l_args,
                    is_pure: l_is_pure,
                    returns: l_returns,
                },
                Self::FuncType {
                    src: _,
                    args: r_args,
                    is_pure: r_is_pure,
                    returns: r_returns,
                },
            ) => l_args == r_args && l_is_pure == r_is_pure && l_returns == r_returns,
            (
                Self::GenericType {
                    src: _,
                    type_params: l_type_params,
                    inner: l_inner,
                },
                Self::GenericType {
                    src: _,
                    type_params: r_type_params,
                    inner: r_inner,
                },
            ) => l_type_params == r_type_params && l_inner == r_inner,
            (
                Self::BoundGenericType {
                    src: _,
                    type_args: l_type_args,
                    generic: l_generic,
                },
                Self::BoundGenericType {
                    src: _,
                    type_args: r_type_args,
                    generic: r_generic,
                },
            ) => l_type_args == r_type_args && l_generic == r_generic,
            (
                Self::ObjectType {
                    src: _,
                    entries: l_entries,
                    mutability: l_mutability,
                },
                Self::ObjectType {
                    src: _,
                    entries: r_entries,
                    mutability: r_mutability,
                },
            ) => l_entries == r_entries && l_mutability == r_mutability,
            (
                Self::InterfaceType {
                    src: _,
                    entries: l_entries,
                    mutability: l_mutability,
                },
                Self::InterfaceType {
                    src: _,
                    entries: r_entries,
                    mutability: r_mutability,
                },
            ) => l_entries == r_entries && l_mutability == r_mutability,
            (
                Self::RecordType {
                    src: _,
                    key_type: l_key_type,
                    value_type: l_value_type,
                    mutability: l_mutability,
                },
                Self::RecordType {
                    src: _,
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
                    src: _,
                    element: l_element,
                    mutability: l_mutability,
                },
                Self::ArrayType {
                    src: _,
                    element: r_element,
                    mutability: r_mutability,
                },
            ) => l_element == r_element && l_mutability == r_mutability,
            (
                Self::TupleType {
                    src: _,
                    members: l_members,
                    mutability: l_mutability,
                },
                Self::TupleType {
                    src: _,
                    members: r_members,
                    mutability: r_mutability,
                },
            ) => l_members == r_members && l_mutability == r_mutability,
            (
                Self::ReadonlyType {
                    src: _,
                    inner: l_inner,
                },
                Self::ReadonlyType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (Self::StringType { src: _ }, Self::StringType { src: _ }) => true,
            (Self::NumberType { src: _ }, Self::NumberType { src: _ }) => true,
            (Self::BooleanType { src: _ }, Self::BooleanType { src: _ }) => true,
            (Self::NilType { src: _ }, Self::NilType { src: _ }) => true,
            (
                Self::LiteralType {
                    src: _,
                    value: l_value,
                },
                Self::LiteralType {
                    src: _,
                    value: r_value,
                },
            ) => l_value == r_value,
            (
                Self::NominalType {
                    src: _,
                    name: l_name,
                    inner: l_inner,
                },
                Self::NominalType {
                    src: _,
                    name: r_name,
                    inner: r_inner,
                },
            ) => l_name == r_name && l_inner == r_inner,
            (
                Self::IteratorType {
                    src: _,
                    inner: l_inner,
                },
                Self::IteratorType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::PlanType {
                    src: _,
                    inner: l_inner,
                },
                Self::PlanType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::ErrorType {
                    src: _,
                    inner: l_inner,
                },
                Self::ErrorType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::ParenthesizedType {
                    src: _,
                    inner: l_inner,
                },
                Self::ParenthesizedType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::TypeofType {
                    src: _,
                    expression: l_expression,
                },
                Self::TypeofType {
                    src: _,
                    expression: r_expression,
                },
            ) => l_expression == r_expression,
            (
                Self::KeyofType {
                    src: _,
                    inner: l_inner,
                },
                Self::KeyofType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::ValueofType {
                    src: _,
                    inner: l_inner,
                },
                Self::ValueofType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::ElementofType {
                    src: _,
                    inner: l_inner,
                },
                Self::ElementofType {
                    src: _,
                    inner: r_inner,
                },
            ) => l_inner == r_inner,
            (
                Self::UnknownType {
                    src: _,
                    mutability: l_mutability,
                },
                Self::UnknownType {
                    src: _,
                    mutability: r_mutability,
                },
            ) => l_mutability == r_mutability,
            (Self::PoisonedType { src: _ }, Self::PoisonedType { src: _ }) => true,
            (Self::AnyType { src: _ }, Self::AnyType { src: _ }) => true,
            (Self::RegularExpressionType { src: _ }, Self::RegularExpressionType { src: _ }) => {
                true
            }
            (
                Self::PropertyType {
                    src: _,
                    subject: l_subject,
                    property: l_property,
                    optional: l_optional,
                },
                Self::PropertyType {
                    src: _,
                    subject: r_subject,
                    property: r_property,
                    optional: r_optional,
                },
            ) => l_subject == r_subject && l_property == r_property && l_optional == r_optional,
            _ => false,
        }
    }
}
