use crate::model::{
    ast::ASTEnum, expressions::Expression, misc::Context, type_expressions::TypeExpression,
};

pub fn compile(_ctx: &Context, ast: &ASTEnum) -> String {
    match &ast {
        ASTEnum::Expression(expression) => match &expression.node {
            Expression::NilLiteral => "undefined".to_owned(),
            Expression::NumberLiteral { value } => value.clone(),
            Expression::BinaryOperator { left, op, right } => {
                format!("{} {} {}", left, op, right)
            }
            Expression::Parenthesis { inner } => format!("({})", inner.node),
            Expression::ParseError => "<parse error>".to_owned(),
        },
        ASTEnum::TypeExpression(x) => match &x.node {
            TypeExpression::UnknownType => todo!(),
            TypeExpression::NilType => todo!(),
            TypeExpression::BooleanType => todo!(),
            TypeExpression::NumberType => todo!(),
            TypeExpression::StringType => todo!(),
        },
        ASTEnum::PlainIdentifier(x) => x.node.0.clone(),
        ASTEnum::NameAndType { name, typ } => match typ {
            Some(typ) => format!("{}: {}", name, typ.node),
            None => name.node.0.clone(),
        },
    }
}
