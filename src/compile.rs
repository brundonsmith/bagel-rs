use crate::model::{
    ast::ASTEnum, expressions::Expression, misc::Context, type_expressions::TypeExpression,
};

pub fn compile(_ctx: &Context, ast: &ASTEnum) -> Result<String, ()> {
    match &ast {
        ASTEnum::Expression(Ok(expression)) => match &expression.node {
            Expression::NilLiteral => Ok("undefined".to_owned()),
            Expression::NumberLiteral { value } => Ok(value.clone()),
            Expression::BinaryOperator {
                left: Ok(left),
                op,
                right: Ok(right),
            } => Ok(format!("{} {} {}", left, op, right)),
            Expression::Parenthesis { inner: Ok(inner) } => Ok(format!("({})", inner.node)),
            _ => Err(()),
        },
        ASTEnum::TypeExpression(Ok(x)) => match &x.node {
            TypeExpression::UnknownType => todo!(),
            TypeExpression::NilType => todo!(),
            TypeExpression::BooleanType => todo!(),
            TypeExpression::NumberType => todo!(),
            TypeExpression::StringType => todo!(),
        },
        ASTEnum::PlainIdentifier(Ok(x)) => Ok(x.node.0.clone()),
        ASTEnum::NameAndType {
            name: Ok(name),
            typ: Some(Ok(typ)),
        } => Ok(format!("{}: {}", name, typ.node)),
        ASTEnum::NameAndType {
            name: Ok(name),
            typ: None,
        } => Ok(name.node.0.clone()),
        _ => Err(()),
    }
}
