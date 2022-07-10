use std::{fmt::Display, rc::Weak};

use super::{expressions::Expression, type_expressions::TypeExpression};

#[derive(Clone, Debug)]
pub struct SourceInfo {
    pub parent: Option<Weak<ASTEnum>>,
    pub module: Option<ModuleName>,
    pub start_index: Option<usize>,
    pub end_index: Option<usize>,
}

impl SourceInfo {
    pub fn empty() -> Self {
        Self {
            parent: None,
            module: None,
            start_index: None,
            end_index: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModuleName(pub String);

#[derive(Clone, Debug)]
pub struct AST {
    pub source_info: SourceInfo,
    pub node: ASTEnum,
}

impl Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

impl AST {
    pub fn from_node(node: ASTEnum) -> Self {
        Self {
            source_info: SourceInfo::empty(),
            node,
        }
    }

    pub fn visit<F: FnMut(&AST)>(&self, mut cb: &mut F) {
        cb(self);

        match &self.node {
            ASTEnum::Expression(x) => match x {
                Expression::NilLiteral => {}
                Expression::NumberLiteral { value } => {}
                Expression::BinaryOperator { left, op, right } => {
                    left.visit(cb);
                    right.visit(cb);
                }
                Expression::Parenthesis { inner } => {
                    inner.visit(cb);
                }
            },
            ASTEnum::TypeExpression(x) => match x {
                TypeExpression::UnknownType => {}
                TypeExpression::NilType => {}
                TypeExpression::BooleanType => {}
                TypeExpression::NumberType => {}
                TypeExpression::StringType => {}
            },
        };
    }
}

#[derive(Clone, Debug)]
pub enum ASTEnum {
    Expression(Expression),
    TypeExpression(TypeExpression),
}

impl Display for ASTEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTEnum::Expression(x) => Display::fmt(x, f),
            ASTEnum::TypeExpression(x) => Display::fmt(x, f),
        }
    }
}
