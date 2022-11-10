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
                src,
                name,
                type_annotation,
                value,
            } => {
                if let Some(type_annotation) = type_annotation {
                    type_annotation.check(ctx, report_error);
                }
                value.check(ctx, report_error);
            }
            Declaration::ImportAllDeclaration { src, name, path } => todo!(),
            Declaration::ImportDeclaration { src, imports, path } => todo!(),
            Declaration::TypeDeclaration {
                src,
                name,
                declared_type,
            } => todo!(),
            Declaration::FuncDeclaration {
                src,
                name,
                func,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                src,
                name,
                proc,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { src, name, expr } => todo!(),
            Declaration::TestBlockDeclaration { src, name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                src,
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
                src,
                left,
                op,
                right,
            } => todo!(),
            Expression::Parenthesis { src, inner } => inner.check(ctx, report_error),
            Expression::LocalIdentifier { src, name } => {
                if let Some(src) = src {
                    if ctx.module.resolve_symbol_within(name, src).is_none() {
                        report_error(BagelError::TypeError {
                            module_name: ctx.module.module_name.clone(),
                            src,
                        })
                    }
                }
            }
            Expression::InlineConstGroup {
                src,
                declarations,
                inner,
            } => {
                for decl in declarations {
                    decl.check(ctx, report_error);
                }

                inner.check(ctx, report_error);
            }

            Expression::NilLiteral { src } => {}
            Expression::NumberLiteral { src, value } => {}
            Expression::BooleanLiteral { src, value } => todo!(),
            Expression::StringLiteral { src, value } => todo!(),
            Expression::ExactStringLiteral { src, tag, segments } => todo!(),
            Expression::ArrayLiteral { src, entries } => todo!(),
            Expression::ObjectLiteral { src, entries } => todo!(),
            Expression::NegationOperation { src, inner } => todo!(),
            Expression::Func {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { src, start, end } => todo!(),
            Expression::Invocation {
                src,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                src,
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                src,
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                src,
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                src,
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast {
                src,
                inner,
                as_type,
            } => todo!(),
            Expression::ErrorExpression { src, inner } => todo!(),
            Expression::RegularExpression { src, expr, flags } => todo!(),
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
