use enum_variant_type::EnumVariantType;

use crate::slice::Slice;

use super::{FuncType, PlainIdentifier, ProcType, Sourced, Statement, TypeExpression};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Expression {
    #[evt(derive(Debug, Clone, PartialEq))]
    NilLiteral { src: Option<Slice> },

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteral { src: Option<Slice>, value: bool },

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteral { src: Option<Slice>, value: String },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteral {
        src: Option<Slice>,
        tag: Option<PlainIdentifier>,
        segments: Vec<StringLiteralSegment>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ExactStringLiteral {
        src: Option<Slice>,
        tag: Option<PlainIdentifier>,
        value: String,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayLiteral {
        src: Option<Slice>,
        entries: Vec<ArrayLiteralEntry>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectLiteral {
        src: Option<Slice>,
        entries: Vec<ObjectLiteralEntry>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperation {
        src: Option<Slice>,
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NegationOperation {
        src: Option<Slice>,
        inner: Box<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Parenthesis {
        src: Option<Slice>,
        inner: Box<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    LocalIdentifier { src: Option<Slice>, name: String },

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineConstGroup {
        src: Option<Slice>,
        declarations: Vec<InlineConstDeclaration>,
        inner: Box<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Func {
        src: Option<Slice>,
        type_annotation: Box<FuncType>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: Box<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JsFunc {
        src: Option<Slice>,
        type_annotation: Box<FuncType>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: String,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Proc {
        src: Option<Slice>,
        type_annotation: Box<ProcType>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: Vec<Statement>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JsProc {
        src: Option<Slice>,
        type_annotation: Box<ProcType>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: String,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JavascriptEscapeExpression(JavascriptEscape),

    #[evt(derive(Debug, Clone, PartialEq))]
    RangeExpression {
        src: Option<Slice>,
        start: Box<Expression>,
        end: Box<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Invocation {
        src: Option<Slice>,
        subject: Box<Expression>,
        args: Vec<Expression>,
        spread_args: Option<Box<Expression>>,
        type_args: Vec<TypeExpression>,
        bubbles: bool,
        awaited_or_detached: Option<AwaitOrDetach>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyAccessor {
        src: Option<Slice>,
        subject: Box<Expression>,
        property: Option<PlainIdentifier>, // TODO:  | Expression
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpression {
        src: Option<Slice>,
        cases: Vec<(Expression, Expression)>,
        default_case: Option<Box<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpression {
        src: Option<Slice>,
        value: Box<Expression>,
        cases: Vec<(TypeExpression, Expression)>,
        default_case: Option<Box<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementTag {
        src: Option<Slice>,
        tag_name: PlainIdentifier,
        attributes: Vec<ObjectLiteralEntry>,
        children: Vec<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AsCast {
        src: Option<Slice>,
        inner: Box<Expression>,
        as_type: Box<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorExpression {
        src: Option<Slice>,
        inner: Box<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpression {
        src: Option<Slice>,
        expr: String,
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
pub struct JavascriptEscape {
    pub src: Option<Slice>,
    pub js: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StringLiteralSegment {
    String(Slice),
    Expression(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayLiteralEntry {
    Spread(LocalIdentifier),
    Element(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectLiteralEntry {
    Variable(LocalIdentifier),
    Spread(LocalIdentifier),
    KeyValue(PlainIdentifier, Expression),
}

impl Sourced for Expression {
    fn src(&self) -> Option<Slice> {
        match self {
            Expression::NilLiteral { src } => src.clone(),
            Expression::NumberLiteral { src, value } => src.clone(),
            Expression::BinaryOperation {
                src,
                left,
                op,
                right,
            } => src.clone(),
            Expression::Parenthesis { src, inner } => src.clone(),
            Expression::LocalIdentifier { src, name } => src.clone(),
            Expression::InlineConstGroup {
                src,
                declarations,
                inner,
            } => src.clone(),
            Expression::BooleanLiteral { src, value } => src.clone(),
            Expression::StringLiteral { src, tag, segments } => src.clone(),
            Expression::ExactStringLiteral { src, tag, value } => src.clone(),
            Expression::ArrayLiteral { src, entries } => src.clone(),
            Expression::ObjectLiteral { src, entries } => src.clone(),
            Expression::NegationOperation { src, inner } => src.clone(),
            Expression::Func {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => src.clone(),
            Expression::JsFunc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => src.clone(),
            Expression::Proc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => src.clone(),
            Expression::JsProc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => src.clone(),
            Expression::JavascriptEscapeExpression(expr) => expr.src.clone(),
            Expression::RangeExpression { src, start, end } => src.clone(),
            Expression::Invocation {
                src,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => src.clone(),
            Expression::PropertyAccessor {
                src,
                subject,
                property,
                optional,
            } => src.clone(),
            Expression::IfElseExpression {
                src,
                cases,
                default_case,
            } => src.clone(),
            Expression::SwitchExpression {
                src,
                value,
                cases,
                default_case,
            } => src.clone(),
            Expression::ElementTag {
                src,
                tag_name,
                attributes,
                children,
            } => src.clone(),
            Expression::AsCast {
                src,
                inner,
                as_type,
            } => src.clone(),
            Expression::ErrorExpression { src, inner } => src.clone(),
            Expression::RegularExpression { src, expr, flags } => src.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InlineConstDeclaration {
    pub src: Option<Slice>,
    pub name: PlainIdentifier,
    pub type_annotation: Option<TypeExpression>,
    pub value: Box<Expression>,
}

impl Sourced for InlineConstDeclaration {
    fn src(&self) -> Option<Slice> {
        self.src.clone()
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

impl BinaryOperator {
    pub const fn symbol(self) -> &'static str {
        match self {
            BinaryOperator::NullishCoalescing => "??",
            BinaryOperator::Or => "||",
            BinaryOperator::And => "&&",
            BinaryOperator::Equals => "==",
            BinaryOperator::NotEquals => "!=",
            BinaryOperator::LessEqual => "<=",
            BinaryOperator::GreaterEqual => ">=",
            BinaryOperator::Less => "<",
            BinaryOperator::Greater => ">",
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Times => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::InstanceOf => "instanceof",
        }
    }

    pub fn from_symbol(symbol: &str) -> Self {
        match symbol {
            "??" => BinaryOperator::NullishCoalescing,
            "||" => BinaryOperator::Or,
            "&&" => BinaryOperator::And,
            "==" => BinaryOperator::Equals,
            "!=" => BinaryOperator::NotEquals,
            "<=" => BinaryOperator::LessEqual,
            ">=" => BinaryOperator::GreaterEqual,
            "<" => BinaryOperator::Less,
            ">" => BinaryOperator::Greater,
            "+" => BinaryOperator::Plus,
            "-" => BinaryOperator::Minus,
            "*" => BinaryOperator::Times,
            "/" => BinaryOperator::Divide,
            "instanceof" => BinaryOperator::InstanceOf,
            _ => panic!(
                "from_symbol() called with invalid operator symbol \"{}\"",
                symbol
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BinaryOperatorType {
    pub left: TypeExpression,
    pub right: TypeExpression,
    pub output: TypeExpression,
}
