use crate::{
    model::{
        ast::{visit_ast, ASTEnum, AST},
        expressions::BINARY_OPERATOR_TYPES,
        misc::{Context, Error},
        type_expressions::TypeExpression,
    },
    typeinfer::infer_type,
};

pub fn typecheck(ctx: &mut Context, ast: &AST) {
    visit_ast(ast, &mut |ast| match &ast.node {
        crate::model::ast::ASTEnum::Expression(x) => match x {
            crate::model::expressions::Expression::NilLiteral => {}
            crate::model::expressions::Expression::NumberLiteral { value: _ } => {}
            crate::model::expressions::Expression::BinaryOperator { left, op, right } => {
                if let (ASTEnum::Expression(left), ASTEnum::Expression(right)) =
                    (&left.node, &right.node)
                {
                    let left_type = infer_type(ctx, &left);
                    let right_type = infer_type(ctx, &right);

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
            }
            crate::model::expressions::Expression::Parenthesis { inner: _ } => {}
        },
        crate::model::ast::ASTEnum::TypeExpression(x) => match x {
            TypeExpression::UnknownType => {}
            TypeExpression::NilType => {}
            TypeExpression::BooleanType => {}
            TypeExpression::NumberType => {}
            TypeExpression::StringType => {}
        },
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
