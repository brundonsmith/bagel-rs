use std::{borrow::Cow, collections::HashMap, marker::PhantomData, ops::Range};

use enum_variant_type::EnumVariantType;
use lazy_static::lazy_static;

use super::{FuncType, PlainIdentifier, ProcType, Sourced, Statement, TypeExpression};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Expression<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    NilLiteral { src: Option<&'a str> },

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteral { src: Option<&'a str>, value: bool },

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteral {
        src: Option<&'a str>,
        value: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteral {
        src: Option<&'a str>,
        value: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ExactStringLiteral {
        src: Option<&'a str>,
        tag: Option<PlainIdentifier<'a>>,
        segments: Vec<StringLiteralSegment<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayLiteral {
        src: Option<&'a str>,
        entries: Vec<ArrayLiteralEntry<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectLiteral {
        src: Option<&'a str>,
        entries: Vec<ObjectLiteralEntry<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperation {
        src: Option<&'a str>,
        left: Box<Expression<'a>>,
        op: BinaryOperator,
        right: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NegationOperation {
        src: Option<&'a str>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Parenthesis {
        src: Option<&'a str>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    LocalIdentifier {
        src: Option<&'a str>,
        name: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineConstGroup {
        src: Option<&'a str>,
        declarations: Vec<InlineConstDeclaration<'a>>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Func {
        src: Option<&'a str>,
        type_annotation: Box<FuncType<'a>>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JsFunc {
        src: Option<&'a str>,
        type_annotation: Box<FuncType<'a>>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Proc {
        src: Option<&'a str>,
        type_annotation: Box<ProcType<'a>>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JsProc {
        src: Option<&'a str>,
        type_annotation: Box<ProcType<'a>>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JavascriptEscapeExpression(JavascriptEscape<'a>),

    #[evt(derive(Debug, Clone, PartialEq))]
    RangeExpression {
        src: Option<&'a str>,
        start: Box<Expression<'a>>,
        end: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Invocation {
        src: Option<&'a str>,
        subject: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
        spread_args: Option<Box<Expression<'a>>>,
        type_args: Vec<TypeExpression<'a>>,
        bubbles: bool,
        awaited_or_detached: Option<AwaitOrDetach>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyAccessor {
        src: Option<&'a str>,
        subject: Box<Expression<'a>>,
        property: Option<PlainIdentifier<'a>>, // TODO:  | Expression
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpression {
        src: Option<&'a str>,
        cases: Vec<(Expression<'a>, Expression<'a>)>,
        default_case: Option<Box<Expression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpression {
        src: Option<&'a str>,
        value: Box<Expression<'a>>,
        cases: Vec<(TypeExpression<'a>, Expression<'a>)>,
        default_case: Option<Box<Expression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementTag {
        src: Option<&'a str>,
        tag_name: PlainIdentifier<'a>,
        attributes: Vec<ObjectLiteralEntry<'a>>,
        children: Vec<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AsCast {
        src: Option<&'a str>,
        inner: Box<Expression<'a>>,
        as_type: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorExpression {
        src: Option<&'a str>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpression {
        src: Option<&'a str>,
        expr: Cow<'a, str>,
        flags: Vec<RegularExpressionFlag>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegularExpressionFlag {
    D,
    G,
    I,
    M,
    S,
    U,
    Y,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AwaitOrDetach {
    Await,
    Detach,
}

#[derive(Clone, Debug, PartialEq)]
pub struct JavascriptEscape<'a> {
    pub src: Option<&'a str>,
    pub js: Cow<'a, str>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StringLiteralSegment<'a> {
    String(Cow<'a, str>),
    Expression(Expression<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayLiteralEntry<'a> {
    Spread(LocalIdentifier<'a>),
    Element(Expression<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectLiteralEntry<'a> {
    Variable(LocalIdentifier<'a>),
    Spread(LocalIdentifier<'a>),
    KeyValue(PlainIdentifier<'a>, Expression<'a>),
}

impl<'a> Sourced for Expression<'a> {
    fn src(&self) -> Option<&str> {
        match self {
            Expression::NilLiteral { src } => *src,
            Expression::NumberLiteral { src, value } => *src,
            Expression::BinaryOperation {
                src,
                left,
                op,
                right,
            } => *src,
            Expression::Parenthesis { src, inner } => *src,
            Expression::LocalIdentifier { src, name } => *src,
            Expression::InlineConstGroup {
                src,
                declarations,
                inner,
            } => *src,
            Expression::BooleanLiteral { src, value } => *src,
            Expression::StringLiteral { src, value } => *src,
            Expression::ExactStringLiteral { src, tag, segments } => *src,
            Expression::ArrayLiteral { src, entries } => *src,
            Expression::ObjectLiteral { src, entries } => *src,
            Expression::NegationOperation { src, inner } => *src,
            Expression::Func {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => *src,
            Expression::JsFunc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => *src,
            Expression::Proc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => *src,
            Expression::JsProc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => *src,
            Expression::JavascriptEscapeExpression(expr) => expr.src,
            Expression::RangeExpression { src, start, end } => *src,
            Expression::Invocation {
                src,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => *src,
            Expression::PropertyAccessor {
                src,
                subject,
                property,
                optional,
            } => *src,
            Expression::IfElseExpression {
                src,
                cases,
                default_case,
            } => *src,
            Expression::SwitchExpression {
                src,
                value,
                cases,
                default_case,
            } => *src,
            Expression::ElementTag {
                src,
                tag_name,
                attributes,
                children,
            } => *src,
            Expression::AsCast {
                src,
                inner,
                as_type,
            } => *src,
            Expression::ErrorExpression { src, inner } => *src,
            Expression::RegularExpression { src, expr, flags } => *src,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InlineConstDeclaration<'a> {
    pub src: Option<&'a str>,
    pub name: PlainIdentifier<'a>,
    pub type_annotation: Option<TypeExpression<'a>>,
    pub value: Box<Expression<'a>>,
}

impl<'a> Sourced for InlineConstDeclaration<'a> {
    fn src(&self) -> Option<&str> {
        self.src
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    NullishCoalescing,
    Or,
    And,
    Equals,
    NotEquals,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Plus,
    Minus,
    Times,
    Divide,
    InstanceOf,
}

#[derive(Clone, Debug)]
pub struct BinaryOperatorType<'a> {
    pub left: TypeExpression<'a>,
    pub right: TypeExpression<'a>,
    pub output: TypeExpression<'a>,
}

lazy_static! {
    pub static ref BINARY_OPERATOR_TYPES: HashMap<BinaryOperator, Vec<BinaryOperatorType<'static>>> = {
        let mut hm = HashMap::new();

        hm.insert(
            BinaryOperator::Plus,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType { src: None },
                right: TypeExpression::NumberType { src: None },
                output: TypeExpression::NumberType { src: None },
            }],
        );

        hm.insert(
            BinaryOperator::Minus,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType { src: None },
                right: TypeExpression::NumberType { src: None },
                output: TypeExpression::NumberType { src: None },
            }],
        );

        hm.insert(
            BinaryOperator::Times,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType { src: None },
                right: TypeExpression::NumberType { src: None },
                output: TypeExpression::NumberType { src: None },
            }],
        );

        hm.insert(
            BinaryOperator::Divide,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType { src: None },
                right: TypeExpression::NumberType { src: None },
                output: TypeExpression::NumberType { src: None },
            }],
        );

        hm
    };
}
