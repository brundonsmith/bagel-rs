use std::{borrow::Cow, marker::PhantomData, ops::Range};

use enum_variant_type::EnumVariantType;

use super::{BooleanLiteral, ExactStringLiteral, Expression, NumberLiteral, PlainIdentifier};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum TypeExpression<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    UnionType {
        span: Option<Range<usize>>,
        members: Vec<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    MaybeType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NamedType {
        span: Option<Range<usize>>,
        name: PlainIdentifier<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericParamType {
        span: Option<Range<usize>>,
        name: PlainIdentifier<'a>,
        extends: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcType {
        span: Option<Range<usize>>,
        args: Args<'a>,
        is_pure: bool,
        is_async: bool,
        throws: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncType {
        span: Option<Range<usize>>,
        args: Args<'a>,
        is_pure: bool,
        returns: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericType {
        span: Option<Range<usize>>,
        type_params: Vec<(PlainIdentifier<'a>, Option<TypeExpression<'a>>)>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BoundGenericType {
        span: Option<Range<usize>>,
        type_args: Vec<TypeExpression<'a>>,
        generic: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectType {
        span: Option<Range<usize>>,
        entries: Vec<ObjectTypeEntry<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InterfaceType {
        span: Option<Range<usize>>,
        entries: Vec<(PlainIdentifier<'a>, TypeExpression<'a>)>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        span: Option<Range<usize>>,
        key_type: Box<TypeExpression<'a>>,
        value_type: Box<TypeExpression<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType {
        span: Option<Range<usize>>,
        element: Box<TypeExpression<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType {
        span: Option<Range<usize>>,
        members: Vec<TypeExpression<'a>>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ReadonlyType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NilType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    LiteralType {
        span: Option<Range<usize>>,
        value: LiteralTypeValue<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NominalType {
        span: Option<Range<usize>>,
        name: Cow<'a, str>,
        inner: Option<Box<TypeExpression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IteratorType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PlanType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ParenthesizedType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeofType {
        span: Option<Range<usize>>,
        expression: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyofType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueofType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementofType {
        span: Option<Range<usize>>,
        inner: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    UnknownType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
        mutability: Mutability,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PoisonedType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AnyType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpressionType {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
        // TODO: Number of match groups?
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyType {
        span: Option<Range<usize>>,
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
