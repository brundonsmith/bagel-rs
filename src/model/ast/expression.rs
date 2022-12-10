use enum_variant_type::EnumVariantType;
use strum_macros::{EnumString, IntoStaticStr};

use crate::model::slice::Slice;

use super::{FuncType, Node, PlainIdentifier, ProcType, Statement, TypeExpression};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Expression {
    #[evt(derive(Debug, Clone, PartialEq))]
    NilLiteral,

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteral(bool),

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteral(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteral {
        tag: Option<PlainIdentifier>,
        segments: Vec<Node<StringLiteralSegment>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ExactStringLiteral {
        tag: Option<PlainIdentifier>,
        value: Slice,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayLiteral(Vec<Node<ArrayLiteralEntry>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectLiteral(Vec<Node<ObjectLiteralEntry>>),

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperation {
        left: Node<Expression>,
        op: Node<BinaryOperator>,
        right: Node<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    NegationOperation(Node<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    Parenthesis(Node<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    LocalIdentifier(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineConstGroup {
        declarations: Vec<Node<InlineConstDeclaration>>,
        inner: Node<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Func {
        type_annotation: Node<FuncType>, // TODO:  | GenericFuncType
        is_async: bool,
        is_pure: bool,
        body: FuncBody,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Proc {
        type_annotation: Node<ProcType>, // TODO:  | GenericProcType
        is_async: bool,
        is_pure: bool,
        body: ProcBody,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JavascriptEscapeExpression(String),

    #[evt(derive(Debug, Clone, PartialEq))]
    RangeExpression {
        start: Node<Expression>,
        end: Node<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Invocation {
        subject: Node<Expression>,
        args: Vec<Expression>,
        spread_args: Option<Node<Expression>>,
        type_args: Vec<Node<TypeExpression>>,
        bubbles: bool,
        awaited_or_detached: Option<Node<AwaitOrDetach>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyAccessor {
        subject: Node<Expression>,
        property: Node<IdentifierOrExpression>,
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpression {
        cases: Vec<Node<(Node<Expression>, Node<Expression>)>>,
        default_case: Option<Node<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpression {
        value: Node<Expression>,
        cases: Vec<(Node<TypeExpression>, Node<Expression>)>,
        default_case: Option<Node<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementTag {
        tag_name: PlainIdentifier,
        attributes: Vec<Node<ObjectLiteralEntry>>,
        children: Vec<Node<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AsCast {
        inner: Node<Expression>,
        as_type: Node<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InstanceOf {
        inner: Node<Expression>,
        possible_type: Node<TypeExpression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorExpression(Node<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpression {
        expr: String,
        flags: Vec<Node<RegularExpressionFlag>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncBody {
    Expression(Node<Expression>),
    Js(Node<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProcBody {
    Statements(Vec<Node<Statement>>),
    Js(Node<String>),
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
    Expression(Node<Expression>),
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
    KeyAndValue(IdentifierOrExpression, Node<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierOrExpression {
    PlainIdentifier(PlainIdentifier),
    Expression(Node<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct InlineConstDeclaration {
    pub name: PlainIdentifier,
    pub type_annotation: Option<Node<TypeExpression>>,
    pub value: Node<Expression>,
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
