use enum_variant_type::EnumVariantType;
use strum_macros::{EnumString, IntoStaticStr};

use crate::model::slice::Slice;

use super::{FuncType, PlainIdentifier, ProcType, Src, Statement, TypeExpression};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Expression {
    #[evt(derive(Debug, Clone, PartialEq))]
    NilLiteral,

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteral { value: bool },

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteral { value: String },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteral {
        tag: Option<Src<PlainIdentifier>>,
        segments: Vec<Src<StringLiteralSegment>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ExactStringLiteral {
        tag: Option<Src<PlainIdentifier>>,
        value: String,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayLiteral {
        entries: Vec<Src<ArrayLiteralEntry>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectLiteral {
        entries: Vec<Src<ObjectLiteralEntry>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperation {
        left: Box<Src<Expression>>,
        op: Src<BinaryOperator>,
        right: Box<Src<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NegationOperation(Box<Src<Expression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    Parenthesis(Box<Src<Expression>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    LocalIdentifier(String),

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineConstGroup {
        declarations: Vec<Src<InlineConstDeclaration>>,
        inner: Box<Src<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Func {
        type_annotation: Src<FuncType>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: FuncBody,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Proc {
        type_annotation: Src<ProcType>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: ProcBody,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JavascriptEscapeExpression(String),

    #[evt(derive(Debug, Clone, PartialEq))]
    RangeExpression {
        start: Box<Src<Expression>>,
        end: Box<Src<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Invocation {
        subject: Box<Src<Expression>>,
        args: Vec<Expression>,
        spread_args: Option<Box<Src<Expression>>>,
        type_args: Vec<Src<TypeExpression>>,
        bubbles: bool,
        awaited_or_detached: Option<Src<AwaitOrDetach>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyAccessor {
        subject: Box<Src<Expression>>,
        property: Option<Src<PlainIdentifier>>, // TODO:  | Expression
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpression {
        cases: Vec<(Src<Expression>, Src<Expression>)>,
        default_case: Option<Box<Src<Expression>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpression {
        value: Box<Src<Expression>>,
        cases: Vec<(Src<TypeExpression>, Src<Expression>)>,
        default_case: Option<Box<Src<Expression>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementTag {
        tag_name: Src<PlainIdentifier>,
        attributes: Vec<Src<ObjectLiteralEntry>>,
        children: Vec<Src<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AsCast {
        inner: Box<Src<Expression>>,
        as_type: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InstanceOf {
        inner: Box<Src<Expression>>,
        possible_type: Box<Src<TypeExpression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorExpression { inner: Box<Src<Expression>> },

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpression {
        expr: String,
        flags: Vec<Src<RegularExpressionFlag>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncBody {
    Expression(Box<Src<Expression>>),
    Js(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProcBody {
    Statements(Vec<Src<Statement>>),
    Js(String),
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

#[derive(Clone, Debug, PartialEq)]
pub struct InlineConstDeclaration {
    pub name: PlainIdentifier,
    pub type_annotation: Option<Src<TypeExpression>>,
    pub value: Box<Src<Expression>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString, IntoStaticStr)]
pub enum BinaryOperator {
    #[strum(serialize = "??")]
    NullishCoalescing,
    #[strum(serialize = "||")]
    Or,
    #[strum(serialize = "&&")]
    And,
    #[strum(serialize = "==")]
    Equals,
    #[strum(serialize = "!=")]
    NotEquals,
    #[strum(serialize = "<=")]
    LessEqual,
    #[strum(serialize = ">=")]
    GreaterEqual,
    #[strum(serialize = "<")]
    Less,
    #[strum(serialize = ">")]
    Greater,
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "*")]
    Times,
    #[strum(serialize = "/")]
    Divide,
}
