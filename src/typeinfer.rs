use crate::{
    ast::{Expression, Module, TypeExpression},
    resolve::{Binding, Resolve},
};

pub struct InferTypeContext<'a> {
    pub module: &'a Module,
}

impl Expression {
    pub fn infer_type(&self, ctx: &InferTypeContext) -> TypeExpression {
        match self {
            Expression::NilLiteral { span: _ } => TypeExpression::NilType,
            Expression::NumberLiteral { span: _, value: _ } => TypeExpression::NumberType,
            Expression::BinaryOperation {
                span: _,
                left,
                op,
                right,
            } => {
                let left_type = left.infer_type(ctx);
                let right_type = right.infer_type(ctx);

                match op {
                    crate::ast::BinaryOperator::NullishCoalescing => todo!(),
                    crate::ast::BinaryOperator::Or => todo!(),
                    crate::ast::BinaryOperator::And => todo!(),
                    crate::ast::BinaryOperator::Equals => todo!(),
                    crate::ast::BinaryOperator::NotEquals => todo!(),
                    crate::ast::BinaryOperator::LessEqual => todo!(),
                    crate::ast::BinaryOperator::GreaterEqual => todo!(),
                    crate::ast::BinaryOperator::Less => todo!(),
                    crate::ast::BinaryOperator::Greater => todo!(),
                    crate::ast::BinaryOperator::Plus => todo!(),
                    crate::ast::BinaryOperator::Minus => todo!(),
                    crate::ast::BinaryOperator::Times => todo!(),
                    crate::ast::BinaryOperator::Divide => todo!(),
                }
            }
            Expression::Parenthesis { span: _, inner } => inner.infer_type(ctx),
            Expression::LocalIdentifier { span, name } => {
                match ctx.module.resolve_symbol_within(name, &span.start) {
                    Some(binding) => match binding {
                        Binding::ValueDeclaration(decl) => decl
                            .type_annotation
                            .unwrap_or_else(|| decl.value.infer_type(ctx)),
                        Binding::InlineConstDeclaration(_) => todo!(),
                    },
                    None => TypeExpression::UnknownType,
                }
            }
            Expression::InlineConstGroup {
                span: _,
                declarations: _,
                inner,
            } => inner.infer_type(ctx),
        }
    }
}
