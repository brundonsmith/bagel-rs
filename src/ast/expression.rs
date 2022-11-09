use std::{borrow::Cow, collections::HashMap, marker::PhantomData, ops::Range};

use enum_variant_type::EnumVariantType;
use lazy_static::lazy_static;

use super::{FuncType, PlainIdentifier, ProcType, Span, Statement, TypeExpression};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Expression<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    NilLiteral {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteral {
        span: Option<Range<usize>>,
        p: PhantomData<&'a str>,
        value: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteral {
        span: Option<Range<usize>>,
        value: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteral {
        span: Option<Range<usize>>,
        value: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ExactStringLiteral {
        span: Option<Range<usize>>,
        tag: Option<PlainIdentifier<'a>>,
        segments: Vec<StringLiteralSegment<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayLiteral {
        span: Option<Range<usize>>,
        entries: Vec<ArrayLiteralEntry<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectLiteral {
        span: Option<Range<usize>>,
        entries: Vec<ObjectLiteralEntry<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperation {
        span: Option<Range<usize>>,
        left: Box<Expression<'a>>,
        op: BinaryOperator,
        right: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NegationOperation {
        span: Option<Range<usize>>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Parenthesis {
        span: Option<Range<usize>>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    LocalIdentifier {
        span: Option<Range<usize>>,
        name: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineConstGroup {
        span: Option<Range<usize>>,
        declarations: Vec<InlineConstDeclaration<'a>>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Func {
        span: Option<Range<usize>>,
        type_annotation: Box<FuncType<'a>>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JsFunc {
        span: Option<Range<usize>>,
        type_annotation: Box<FuncType<'a>>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Proc {
        span: Option<Range<usize>>,
        type_annotation: Box<ProcType<'a>>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JsProc {
        span: Option<Range<usize>>,
        type_annotation: Box<ProcType<'a>>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: Cow<'a, str>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JavascriptEscapeExpression(JavascriptEscape<'a>),

    #[evt(derive(Debug, Clone, PartialEq))]
    RangeExpression {
        span: Option<Range<usize>>,
        start: Box<Expression<'a>>,
        end: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Invocation {
        span: Option<Range<usize>>,
        subject: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
        spread_args: Option<Box<Expression<'a>>>,
        type_args: Vec<TypeExpression<'a>>,
        bubbles: bool,
        awaited_or_detached: Option<AwaitOrDetach>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyAccessor {
        span: Option<Range<usize>>,
        subject: Box<Expression<'a>>,
        property: Option<PlainIdentifier<'a>>, // TODO:  | Expression
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpression {
        span: Option<Range<usize>>,
        cases: Vec<(Expression<'a>, Expression<'a>)>,
        default_case: Option<Box<Expression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpression {
        span: Option<Range<usize>>,
        value: Box<Expression<'a>>,
        cases: Vec<(TypeExpression<'a>, Expression<'a>)>,
        default_case: Option<Box<Expression<'a>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementTag {
        span: Option<Range<usize>>,
        tag_name: PlainIdentifier<'a>,
        attributes: Vec<ObjectLiteralEntry<'a>>,
        children: Vec<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AsCast {
        span: Option<Range<usize>>,
        inner: Box<Expression<'a>>,
        as_type: Box<TypeExpression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorExpression {
        span: Option<Range<usize>>,
        inner: Box<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpression {
        span: Option<Range<usize>>,
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
    pub span: Option<Range<usize>>,
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

impl<'a> Span for Expression<'a> {
    fn span(&self) -> Option<&Range<usize>> {
        match self {
            Expression::NilLiteral { span, p: _ } => span.as_ref(),
            Expression::NumberLiteral { span, value } => span.as_ref(),
            Expression::BinaryOperation {
                span,
                left,
                op,
                right,
            } => span.as_ref(),
            Expression::Parenthesis { span, inner } => span.as_ref(),
            Expression::LocalIdentifier { span, name } => span.as_ref(),
            Expression::InlineConstGroup {
                span,
                declarations,
                inner,
            } => span.as_ref(),
            Expression::BooleanLiteral { span, value, p: _ } => span.as_ref(),
            Expression::StringLiteral { span, value } => span.as_ref(),
            Expression::ExactStringLiteral {
                span,
                tag,
                segments,
            } => span.as_ref(),
            Expression::ArrayLiteral { span, entries } => span.as_ref(),
            Expression::ObjectLiteral { span, entries } => span.as_ref(),
            Expression::NegationOperation { span, inner } => span.as_ref(),
            Expression::Func {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => span.as_ref(),
            Expression::JsFunc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => span.as_ref(),
            Expression::Proc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => span.as_ref(),
            Expression::JsProc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => span.as_ref(),
            Expression::JavascriptEscapeExpression(expr) => expr.span.as_ref(),
            Expression::RangeExpression { span, start, end } => span.as_ref(),
            Expression::Invocation {
                span,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => span.as_ref(),
            Expression::PropertyAccessor {
                span,
                subject,
                property,
                optional,
            } => span.as_ref(),
            Expression::IfElseExpression {
                span,
                cases,
                default_case,
            } => span.as_ref(),
            Expression::SwitchExpression {
                span,
                value,
                cases,
                default_case,
            } => span.as_ref(),
            Expression::ElementTag {
                span,
                tag_name,
                attributes,
                children,
            } => span.as_ref(),
            Expression::AsCast {
                span,
                inner,
                as_type,
            } => span.as_ref(),
            Expression::ErrorExpression { span, inner } => span.as_ref(),
            Expression::RegularExpression { span, expr, flags } => span.as_ref(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InlineConstDeclaration<'a> {
    pub span: Option<Range<usize>>,
    pub name: PlainIdentifier<'a>,
    pub type_annotation: Option<TypeExpression<'a>>,
    pub value: Box<Expression<'a>>,
}

impl<'a> Span for InlineConstDeclaration<'a> {
    fn span(&self) -> Option<&Range<usize>> {
        self.span.as_ref()
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
                left: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                right: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                output: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
            }],
        );

        hm.insert(
            BinaryOperator::Minus,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                right: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                output: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
            }],
        );

        hm.insert(
            BinaryOperator::Times,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                right: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                output: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
            }],
        );

        hm.insert(
            BinaryOperator::Divide,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                right: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
                output: TypeExpression::NumberType {
                    span: None,
                    p: PhantomData,
                },
            }],
        );

        hm
    };
}
