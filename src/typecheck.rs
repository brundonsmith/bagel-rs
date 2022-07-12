use crate::{
    model::{
        ast::{visit_ast, ASTEnum, AST},
        expressions::{Expression, BINARY_OPERATOR_TYPES},
        misc::{Context, Error},
        type_expressions::TypeExpression,
    },
    typeinfer::infer_type,
};

pub fn typecheck(ctx: &mut Context, ast: &ASTEnum) {
    visit_ast(ast, &mut |ast| match ast {
        ASTEnum::Expression(AST::Ok(x)) => match &x.node {
            Expression::NilLiteral => {}
            Expression::NumberLiteral { value: _ } => {}
            Expression::BinaryOperator {
                left: AST::Ok(left),
                op,
                right: AST::Ok(right),
            } => {
                let left_type = infer_type(ctx, &left.node);
                let right_type = infer_type(ctx, &right.node);

                let matched_op_type = BINARY_OPERATOR_TYPES.get(op).unwrap().iter().find(|t| {
                    subsumation_issues(ctx, &t.left, &left_type).is_none()
                        && subsumation_issues(ctx, &t.right, &right_type).is_none()
                });

                if matched_op_type.is_none() {
                    (ctx.report_error)(Error::TypeError {
                        ast: ast.clone(),
                        msg: format!(
                            "Operator {} can't be applied to types {} and {}",
                            op.s(),
                            left_type,
                            right_type
                        ),
                    })
                }
            }
            Expression::Parenthesis { inner: _ } => {}
            _ => {}
        },
        ASTEnum::TypeExpression(AST::Ok(x)) => match &x.node {
            TypeExpression::UnknownType => {}
            TypeExpression::NilType => {}
            TypeExpression::BooleanType => {}
            TypeExpression::NumberType => {}
            TypeExpression::StringType => {}
        },
        ASTEnum::PlainIdentifier(_) => {}
        ASTEnum::NameAndType { name: _, typ: _ } => {}
        _ => {}
    })
}

pub fn subsumation_issues(
    _ctx: &Context,
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
