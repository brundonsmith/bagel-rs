use crate::{
    ast::{Declaration, Expression, InlineConstDeclaration, Module, TypeExpression},
    errors::BagelError,
    resolve::Resolve,
};

pub struct CheckContext<'a> {
    pub module: &'a Module,
}

pub trait Check {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F);
}

impl Check for Module {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        for decl in &self.declarations {
            decl.check(ctx, report_error);
        }
    }
}

impl Check for Declaration {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        match self {
            Declaration::ValueDeclaration {
                span,
                name,
                type_annotation,
                value,
            } => {
                if let Some(type_annotation) = type_annotation {
                    type_annotation.check(ctx, report_error);
                }
                value.check(ctx, report_error);
            }
        }
    }
}

impl Check for Expression {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        match self {
            Expression::BinaryOperation {
                span,
                left,
                op,
                right,
            } => todo!(),
            Expression::Parenthesis { span, inner } => inner.check(ctx, report_error),
            Expression::LocalIdentifier { span, name } => {
                if ctx
                    .module
                    .resolve_symbol_within(name, &span.start)
                    .is_none()
                {
                    report_error(BagelError::TypeError {
                        module_name: ctx.module.module_name.clone(),
                        span: span.clone(),
                    })
                }
            }
            Expression::InlineConstGroup {
                span,
                declarations,
                inner,
            } => {
                for decl in declarations {
                    decl.check(ctx, report_error);
                }

                inner.check(ctx, report_error);
            }

            Expression::NilLiteral { span } => {}
            Expression::NumberLiteral { span, value } => {}
        };
    }
}

impl Check for InlineConstDeclaration {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        if let Some(type_annotation) = &self.type_annotation {
            type_annotation.check(ctx, report_error);
        }

        self.value.check(ctx, report_error);
    }
}

impl Check for TypeExpression {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        todo!()
    }
}
