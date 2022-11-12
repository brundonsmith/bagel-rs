use std::{borrow::Cow, marker::PhantomData, ops::Range};

use enum_variant_type::EnumVariantType;

use crate::slice::Slice;

use super::{BooleanLiteral, ExactStringLiteral, Expression, NumberLiteral, PlainIdentifier};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum TypeExpression<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    UnionType {
        src: Option<Slice<'a>>,
        members: Vec<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    MaybeType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NamedType {
        src: Option<Slice<'a>>,
        name: PlainIdentifier<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericParamType {
        src: Option<Slice<'a>>,
        name: PlainIdentifier<'a>,
        extends: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcType {
        src: Option<Slice<'a>>,
        args: Args<'a>,
        is_pure: bool,
        is_async: bool,
        throws: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncType {
        src: Option<Slice<'a>>,
        args: Args<'a>,
        is_pure: bool,
        returns: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericType {
        src: Option<Slice<'a>>,
        type_params: Vec<(PlainIdentifier<'a>, Option<TypeExpression<'a>>)>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BoundGenericType {
        src: Option<Slice<'a>>,
        type_args: Vec<TypeExpression<'a>>,
        generic: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectType {
        src: Option<Slice<'a>>,
        entries: Vec<ObjectTypeEntry<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InterfaceType {
        src: Option<Slice<'a>>,
        entries: Vec<(PlainIdentifier<'a>, TypeExpression<'a>)>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        src: Option<Slice<'a>>,
        key_type: Box<TypeExpression<'a>>,
        value_type: Box<TypeExpression<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType {
        src: Option<Slice<'a>>,
        element: Box<TypeExpression<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType {
        src: Option<Slice<'a>>,
        members: Vec<TypeExpression<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ReadonlyType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringType { src: Option<Slice<'a>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberType { src: Option<Slice<'a>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanType { src: Option<Slice<'a>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    NilType { src: Option<Slice<'a>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    LiteralType {
        src: Option<Slice<'a>>,
        value: LiteralTypeValue<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NominalType {
        src: Option<Slice<'a>>,
        name: Slice<'a>,
        inner: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IteratorType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PlanType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ParenthesizedType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeofType {
        src: Option<Slice<'a>>,
        expression: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyofType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueofType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementofType {
        src: Option<Slice<'a>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    UnknownType {
        src: Option<Slice<'a>>,

        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PoisonedType { src: Option<Slice<'a>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    AnyType { src: Option<Slice<'a>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpressionType {
        src: Option<Slice<'a>>,
        // TODO: Number of match groups?
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyType {
        src: Option<Slice<'a>>,
        subject: Box<TypeExpression<'a>>,
        property: PlainIdentifier<'a>, // TODO:  | TypeExpression
        optional: bool,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Constant,
    Readonly,
    Mutable,
    Literal,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Args<'a> {
    Individual(Vec<Arg<'a>>),
    Spread {
        name: Option<PlainIdentifier<'a>>,
        type_annotation: Box<TypeExpression<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arg<'a> {
    pub name: PlainIdentifier<'a>,
    pub type_annotation: Option<TypeExpression<'a>>,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectTypeEntry<'a> {
    Spread(NamedType<'a>),
    KeyValue(PlainIdentifier<'a>, Box<TypeExpression<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralTypeValue<'a> {
    ExactString(ExactStringLiteral<'a>),
    NumberLiteral(NumberLiteral<'a>),
    BooleanLiteral(BooleanLiteral<'a>),
}
