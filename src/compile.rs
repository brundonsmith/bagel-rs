use crate::model::{
    ast::{ASTEnum, AST},
    expressions::Expression,
    misc::Context,
    type_expressions::TypeExpression,
};

pub fn compile(_ctx: &Context, ast: &AST) -> String {
    match &ast.node {
        ASTEnum::Expression(expression) => match &expression {
            Expression::NilLiteral => "undefined".to_owned(),
            Expression::NumberLiteral { value } => value.clone(),
            Expression::BinaryOperator { left, op, right } => {
                format!("{} {} {}", left, op, right)
            }
            Expression::Parenthesis { inner } => format!("({})", inner.node),
        },
        ASTEnum::TypeExpression(x) => match x {
            TypeExpression::UnknownType => todo!(),
            TypeExpression::NilType => todo!(),
            TypeExpression::BooleanType => todo!(),
            TypeExpression::NumberType => todo!(),
            TypeExpression::StringType => todo!(),
        },
    }
}
