use std::fmt::{Debug, Display};

use lazy_static::lazy_static;

use std::collections::HashMap;

use super::{ast::AST, type_expressions::TypeExpression};

#[derive(Clone, Debug)]
pub enum Expression {
    NilLiteral,
    NumberLiteral {
        value: String,
    },
    BinaryOperator {
        left: AST<Expression>,
        op: BinaryOperator,
        right: AST<Expression>,
    },
    Parenthesis {
        inner: AST<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::NilLiteral => f.write_str("nil"),
            Expression::NumberLiteral { value } => f.write_str(value),
            Expression::BinaryOperator { left, op, right } => {
                Display::fmt(&left, f)?;
                f.write_str(" ")?;
                Display::fmt(op, f)?;
                f.write_str(" ")?;
                Display::fmt(&right, f)
            }
            Expression::Parenthesis { inner } => {
                f.write_str("(")?;
                Display::fmt(&inner, f)?;
                f.write_str(")")
            }
        }
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

// impl BinaryOp {

//     pub fn as_str(&self) -> &str {
//         match self {
//             BinaryOp::Plus => "+",
//             BinaryOp::Minus => "-",
//             BinaryOp::Mul => "*",
//             BinaryOp::Div => "/",
//         }
//     }

//     pub fn from_str(s: &str) -> Result<Self, ()> {
//         match s {
//             "+" => Ok(BinaryOp::Plus),
//             "-" => Ok(BinaryOp::Minus),
//             "*" => Ok(BinaryOp::Mul),
//             "/" => Ok(BinaryOp::Div),
//             _ => Err(())
//         }
//     }
// }
