use crate::{
    model::{
        expressions::{Expression, BINARY_OPERATOR_TYPES},
        misc::Context,
        type_expressions::TypeExpression,
    },
    typecheck::subsumation_issues,
};

pub fn infer_type(ctx: &Context, ast: &Expression) -> TypeExpression {
    let base_type = infer_type_inner(ctx, ast);

    base_type
}

fn infer_type_inner(ctx: &Context, ast: &Expression) -> TypeExpression {
    match ast {
        Expression::NilLiteral => TypeExpression::NilType,
        Expression::NumberLiteral { value: _ } => TypeExpression::NumberType,
        Expression::BinaryOperator { left, op, right } => {
            let left_type = infer_type(ctx, &left.node);
            let right_type = infer_type(ctx, &right.node);

            let matched_op_type = BINARY_OPERATOR_TYPES.get(op).unwrap().iter().find(|t| {
                subsumation_issues(ctx, &t.left, &left_type).is_none()
                    && subsumation_issues(ctx, &t.right, &right_type).is_none()
            });

            matched_op_type
                .map(|t| t.output.clone())
                .unwrap_or(TypeExpression::UnknownType)
        }
        Expression::Parenthesis { inner } => infer_type(ctx, &inner.node),
        Expression::ParseError => TypeExpression::UnknownType,
    }
}
