use crate::model::{
    ast::AST,
    misc::{Context, Error},
    type_expressions::TypeExpression,
};

pub fn typecheck(ctx: &mut Context, ast: &AST) {
    ast.visit(&mut |ast| match &ast.node {
        crate::model::ast::ASTEnum::Expression(x) => match x {
            crate::model::expressions::Expression::NilLiteral => todo!(),
            crate::model::expressions::Expression::NumberLiteral { value } => todo!(),
            crate::model::expressions::Expression::BinaryOperator { left, op, right } => {
                (ctx.report_error)(Error::TypeError);
            }
            crate::model::expressions::Expression::Parenthesis { inner } => todo!(),
        },
        crate::model::ast::ASTEnum::TypeExpression(x) => match x {
            TypeExpression::UnknownType => todo!(),
            TypeExpression::NilType => todo!(),
            TypeExpression::BooleanType => todo!(),
            TypeExpression::NumberType => todo!(),
            TypeExpression::StringType => todo!(),
        },
    })
}

pub fn subsumation_issues(
    ctx: &Context,
    destination: &TypeExpression,
    value: &TypeExpression,
) -> Option<Vec<String>> {
    if destination == value {
        return None;
    }

    Some(vec![format!(
        "Type '{:?}' is not assignable to type '{:?}'",
        destination, value
    )])
}
