use crate::{ast::*, errors::BagelError, resolve::Resolve, typeinfer::InferTypeContext};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CheckContext<'a> {
    pub module: &'a Module,
}

pub trait Check<'a> {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F);
}

impl<'a> Check<'a> for Module {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        for decl in &self.declarations {
            decl.check(ctx, report_error);
        }
    }
}

impl<'a> Check<'a> for Declaration {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        match self {
            Declaration::ValueDeclaration {
                src,
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            } => {
                value.check(ctx, report_error);

                if let Some(type_annotation) = type_annotation {
                    type_annotation.check(ctx, report_error);

                    let value_type = value.infer_type(ctx.into());
                    let issues = type_annotation.subsumation_issues(ctx.into(), &value_type);

                    if let Some(issues) = issues {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: value.src(),
                            issues,
                        });
                    }
                }
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

impl<'a> Check<'a> for Expression {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        match self {
            Expression::BinaryOperation {
                src: _,
                left,
                op,
                right,
            } => {
                let ctx: InferTypeContext = ctx.into();
                let left_type = left.infer_type(ctx);
                let right_type = right.infer_type(ctx);

                let number_or_string = NUMBER_TYPE.union(STRING_TYPE);

                if *op == BinaryOperator::Plus {
                    if let Some(issues) =
                        number_or_string.subsumation_issues(ctx.into(), &left_type)
                    {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: left.src(),
                            issues,
                        })
                    }

                    if let Some(issues) =
                        number_or_string.subsumation_issues(ctx.into(), &right_type)
                    {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: right.src(),
                            issues,
                        })
                    }
                } else if *op == BinaryOperator::Minus
                    || *op == BinaryOperator::Times
                    || *op == BinaryOperator::Divide
                {
                    if let Some(issues) = NUMBER_TYPE.subsumation_issues(ctx.into(), &left_type) {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: left.src(),
                            issues,
                        })
                    }

                    if let Some(issues) = NUMBER_TYPE.subsumation_issues(ctx.into(), &right_type) {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: right.src(),
                            issues,
                        })
                    }
                }

                // match op {
                //     BinaryOperator::NullishCoalescing => todo!(),
                //     BinaryOperator::Or => todo!(),
                //     BinaryOperator::And => todo!(),
                //     BinaryOperator::Equals => todo!(),
                //     BinaryOperator::NotEquals => todo!(),
                //     BinaryOperator::LessEqual => todo!(),
                //     BinaryOperator::GreaterEqual => todo!(),
                //     BinaryOperator::Less => todo!(),
                //     BinaryOperator::Greater => todo!(),
                //     BinaryOperator::InstanceOf => todo!(),
                // }
            }
            Expression::Parenthesis { src, inner } => inner.check(ctx, report_error),
            Expression::LocalIdentifier { src, name } => {
                if let Some(src) = src {
                    if ctx
                        .module
                        .resolve_symbol_within(name.as_str(), &src)
                        .is_none()
                    {
                        report_error(BagelError::NotFoundError {
                            src: Some(src.clone()),
                            module_id: ctx.module.module_id.clone(),
                            identifier: LocalIdentifier {
                                src: Some(src.clone()),
                                name: name.clone(),
                            },
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
            Expression::StringLiteral { src, tag, segments } => todo!(),
            Expression::ExactStringLiteral { src, tag, value } => todo!(),
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

impl<'a> Check<'a> for InlineConstDeclaration {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        if let Some(type_annotation) = &self.type_annotation {
            type_annotation.check(ctx, report_error);
        }

        self.value.check(ctx, report_error);
    }
}

impl<'a> Check<'a> for TypeExpression {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        todo!()
    }
}
