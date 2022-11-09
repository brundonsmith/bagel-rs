use crate::{
    ast::{Declaration, Expression, InlineConstDeclaration, Module, TypeExpression},
    errors::BagelError,
    resolve::Resolve,
};

pub struct CheckContext<'a> {
    pub module: &'a Module<'a>,
}

pub trait Check {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F);
}

impl<'a> Check for Module<'a> {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        for decl in &self.declarations {
            decl.check(ctx, report_error);
        }
    }
}

impl<'a> Check for Declaration<'a> {
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
            Declaration::ImportAllDeclaration { span, name, path } => todo!(),
            Declaration::ImportDeclaration {
                span,
                imports,
                path,
            } => todo!(),
            Declaration::TypeDeclaration {
                span,
                name,
                declared_type,
            } => todo!(),
            Declaration::FuncDeclaration {
                span,
                name,
                func,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                span,
                name,
                proc,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { span, name, expr } => todo!(),
            Declaration::TestBlockDeclaration { span, name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                span,
                name,
                destination_type,
                value_type,
            } => todo!(),
        }
    }
}

impl<'a> Check for Expression<'a> {
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
                if let Some(span) = span {
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

            Expression::NilLiteral { span, p: _ } => {}
            Expression::NumberLiteral { span, value } => {}
            Expression::BooleanLiteral { span, p, value } => todo!(),
            Expression::StringLiteral { span, value } => todo!(),
            Expression::ExactStringLiteral {
                span,
                tag,
                segments,
            } => todo!(),
            Expression::ArrayLiteral { span, entries } => todo!(),
            Expression::ObjectLiteral { span, entries } => todo!(),
            Expression::NegationOperation { span, inner } => todo!(),
            Expression::Func {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { span, start, end } => todo!(),
            Expression::Invocation {
                span,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                span,
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                span,
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                span,
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                span,
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast {
                span,
                inner,
                as_type,
            } => todo!(),
            Expression::ErrorExpression { span, inner } => todo!(),
            Expression::RegularExpression { span, expr, flags } => todo!(),
        };
    }
}

impl<'a> Check for InlineConstDeclaration<'a> {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        if let Some(type_annotation) = &self.type_annotation {
            type_annotation.check(ctx, report_error);
        }

        self.value.check(ctx, report_error);
    }
}

impl<'a> Check for TypeExpression<'a> {
    fn check<F: FnMut(BagelError)>(&self, ctx: &CheckContext, report_error: &mut F) {
        todo!()
    }
}
