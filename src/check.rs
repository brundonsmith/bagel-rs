use crate::{
    ast::*, bgl_type::Type, errors::BagelError, resolve::Resolve, typeinfer::InferTypeContext,
};

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

impl<'a> Check<'a> for Src<Declaration> {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        match &self.node {
            Declaration::ValueDeclaration {
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
                    let issues = type_annotation
                        .resolve(ctx.into())
                        .subsumation_issues(&value_type);

                    if let Some(issues) = issues {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: value.src,
                            issues,
                        });
                    }
                }
            }
            Declaration::ImportAllDeclaration { name, path } => todo!(),
            Declaration::ImportDeclaration { imports, path } => todo!(),
            Declaration::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => todo!(),
            Declaration::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { name, expr } => todo!(),
            Declaration::TestBlockDeclaration { name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => todo!(),
        }
    }
}

impl<'a> Check<'a> for Src<Expression> {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        match &self.node {
            Expression::BinaryOperation { left, op, right } => {
                let ctx: InferTypeContext = ctx.into();
                let left_type = left.infer_type(ctx);
                let right_type = right.infer_type(ctx);

                let number_or_string = Type::NumberType.union(Type::StringType);

                if op.node == BinaryOperator::Plus {
                    if let Some(issues) = number_or_string.subsumation_issues(&left_type) {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: left.src,
                            issues,
                        })
                    }

                    if let Some(issues) = number_or_string.subsumation_issues(&right_type) {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: right.src,
                            issues,
                        })
                    }
                } else if op.node == BinaryOperator::Minus
                    || op.node == BinaryOperator::Times
                    || op.node == BinaryOperator::Divide
                {
                    if let Some(issues) = Type::NumberType.subsumation_issues(&left_type) {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: left.src,
                            issues,
                        })
                    }

                    if let Some(issues) = Type::NumberType.subsumation_issues(&right_type) {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.module.module_id.clone(),
                            src: right.src,
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
            Expression::Parenthesis { inner } => inner.check(ctx, report_error),
            Expression::LocalIdentifier { name } => {
                if let Some(src) = self.src {
                    if ctx
                        .module
                        .resolve_symbol_within(name.as_str(), &src)
                        .is_none()
                    {
                        report_error(BagelError::NotFoundError {
                            module_id: ctx.module.module_id.clone(),
                            identifier: Src {
                                src: Some(src),
                                node: LocalIdentifier { name: name.clone() },
                            },
                        })
                    }
                }
            }
            Expression::InlineConstGroup {
                declarations,
                inner,
            } => {
                for decl in declarations {
                    decl.check(ctx, report_error);
                }

                inner.check(ctx, report_error);
            }

            Expression::NilLiteral => {}
            Expression::NumberLiteral { value } => {}
            Expression::BooleanLiteral { value } => todo!(),
            Expression::StringLiteral { tag, segments } => todo!(),
            Expression::ExactStringLiteral { tag, value } => todo!(),
            Expression::ArrayLiteral { entries } => todo!(),
            Expression::ObjectLiteral { entries } => todo!(),
            Expression::NegationOperation { inner } => todo!(),
            Expression::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { start, end } => todo!(),
            Expression::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast { inner, as_type } => todo!(),
            Expression::ErrorExpression { inner } => todo!(),
            Expression::RegularExpression { expr, flags } => todo!(),
        };
    }
}

impl<'a> Check<'a> for Src<InlineConstDeclaration> {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        if let Some(type_annotation) = &self.node.type_annotation {
            type_annotation.check(ctx, report_error);
        }

        self.node.value.check(ctx, report_error);
    }
}

impl<'a> Check<'a> for Src<TypeExpression> {
    fn check<F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {}
}
