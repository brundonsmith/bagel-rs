use enum_variant_type::EnumVariantType;
use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Display, ops::Range};

pub trait Span {
    fn span(&self) -> &Range<usize>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleName(String);

impl Into<ModuleName> for String {
    fn into(self) -> ModuleName {
        ModuleName(self)
    }
}

impl Into<String> for ModuleName {
    fn into(self) -> String {
        self.0
    }
}

impl<'a> Into<&'a str> for &'a ModuleName {
    fn into(self) -> &'a str {
        self.0.as_str()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub module_name: ModuleName,
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlainIdentifier {
    pub span: Range<usize>,
    pub name: String,
}

impl Span for PlainIdentifier {
    fn span(&self) -> &Range<usize> {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Declaration {
    ValueDeclaration {
        span: Range<usize>,
        name: PlainIdentifier,
        type_annotation: Option<TypeExpression>,
        value: Expression,
    },
}

impl Span for Declaration {
    fn span(&self) -> &Range<usize> {
        match self {
            Declaration::ValueDeclaration {
                span,
                name: _,
                type_annotation: _,
                value: _,
            } => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Expression {
    NilLiteral {
        span: Range<usize>,
    },
    NumberLiteral {
        span: Range<usize>,
        value: String,
    },
    BinaryOperation {
        span: Range<usize>,
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Parenthesis {
        span: Range<usize>,
        inner: Box<Expression>,
    },
    LocalIdentifier {
        span: Range<usize>,
        name: String,
    },
    InlineConstGroup {
        span: Range<usize>,
        declarations: Vec<InlineConstDeclaration>,
        inner: Box<Expression>,
    },
}

impl Span for Expression {
    fn span(&self) -> &Range<usize> {
        match self {
            Expression::NilLiteral { span } => span,
            Expression::NumberLiteral { span, value } => span,
            Expression::BinaryOperation {
                span,
                left,
                op,
                right,
            } => span,
            Expression::Parenthesis { span, inner } => span,
            Expression::LocalIdentifier { span, name } => span,
            Expression::InlineConstGroup {
                span,
                declarations,
                inner,
            } => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InlineConstDeclaration {
    pub span: Range<usize>,
    pub name: PlainIdentifier,
    pub type_annotation: Option<TypeExpression>,
    pub value: Box<Expression>,
}

impl Span for InlineConstDeclaration {
    fn span(&self) -> &Range<usize> {
        &self.span
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
}

impl BinaryOperator {
    pub fn s(&self) -> &'static str {
        match self {
            BinaryOperator::NullishCoalescing => todo!(),
            BinaryOperator::Or => todo!(),
            BinaryOperator::And => todo!(),
            BinaryOperator::Equals => todo!(),
            BinaryOperator::NotEquals => todo!(),
            BinaryOperator::LessEqual => todo!(),
            BinaryOperator::GreaterEqual => todo!(),
            BinaryOperator::Less => todo!(),
            BinaryOperator::Greater => todo!(),
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Times => "*",
            BinaryOperator::Divide => "/",
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.s())
    }
}

#[derive(Clone, Debug)]
pub struct BinaryOperatorType {
    pub left: TypeExpression,
    pub right: TypeExpression,
    pub output: TypeExpression,
}

lazy_static! {
    pub static ref BINARY_OPERATOR_TYPES: HashMap<BinaryOperator, Vec<BinaryOperatorType>> = {
        let mut hm = HashMap::new();

        hm.insert(
            BinaryOperator::Plus,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType,
                right: TypeExpression::NumberType,
                output: TypeExpression::NumberType,
            }],
        );

        hm.insert(
            BinaryOperator::Minus,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType,
                right: TypeExpression::NumberType,
                output: TypeExpression::NumberType,
            }],
        );

        hm.insert(
            BinaryOperator::Times,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType,
                right: TypeExpression::NumberType,
                output: TypeExpression::NumberType,
            }],
        );

        hm.insert(
            BinaryOperator::Divide,
            vec![BinaryOperatorType {
                left: TypeExpression::NumberType,
                right: TypeExpression::NumberType,
                output: TypeExpression::NumberType,
            }],
        );

        hm
    };
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExpression {
    UnknownType,
    NilType,
    BooleanType,
    NumberType,
    StringType,
}

impl Display for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeExpression::UnknownType => f.write_str("unknown"),
            TypeExpression::NilType => f.write_str("nil"),
            TypeExpression::BooleanType => f.write_str("boolean"),
            TypeExpression::NumberType => f.write_str("number"),
            TypeExpression::StringType => f.write_str("string"),
        }
    }
}
