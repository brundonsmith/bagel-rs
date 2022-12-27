use crate::{
    model::{
        ast::BinaryOperatorOp,
        errors::BagelError,
        module::{Module, ModulesStore},
    },
    model::{ast::*, bgl_type::Type, errors::blue_string, slice::Slice},
    passes::typeinfer::InferTypeContext,
    DEBUG_MODE,
};
use std::fmt::Debug;
use std::time::SystemTime;

impl Module {
    pub fn check<'a, F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        let start = SystemTime::now();

        self.ast.clone().upcast().check(ctx, report_error);

        if DEBUG_MODE {
            println!(
                "* Checking {} took {}ms",
                self.module_id,
                start.elapsed().unwrap().as_millis()
            );
        }
    }
}

pub trait Checkable {
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F);
}

impl<TKind> Checkable for AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        let mut check_subsumation =
            |destination: &Type, value: Type, slice: &Slice, report_error: &mut F| {
                let issues = destination.subsumation_issues(ctx.into(), &value);

                if let Some(issues) = issues {
                    report_error(BagelError::AssignmentError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: slice.clone(),
                        issues,
                    });
                }
            };

        match self.details() {
            ASTDetails::Module { declarations } => {
                declarations.check(ctx, report_error);

                // todo!("Check for name duplicates");
            }
            ASTDetails::ImportAllDeclaration { name, path } => todo!(),
            ASTDetails::ImportDeclaration { imports, path } => todo!(),
            ASTDetails::ImportItem { name, alias } => todo!(),
            ASTDetails::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => {
                name.check(ctx, report_error);
                declared_type.check(ctx, report_error);
            }
            ASTDetails::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => {
                name.check(ctx, report_error);
                func.check(ctx, report_error);
                decorators.check(ctx, report_error);
            }
            ASTDetails::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => {
                name.check(ctx, report_error);
                proc.check(ctx, report_error);
                decorators.check(ctx, report_error);
            }
            ASTDetails::Decorator { name } => todo!(),
            ASTDetails::ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            } => {
                name.check(ctx, report_error);
                type_annotation.check(ctx, report_error);
                value.check(ctx, report_error);

                if let Some(type_annotation) = type_annotation {
                    check_subsumation(
                        &type_annotation.resolve_type(ctx.into()),
                        value.infer_type(ctx.into()),
                        value.slice(),
                        report_error,
                    );
                }
            }
            ASTDetails::TestExprDeclaration { name, expr } => todo!(),
            ASTDetails::TestBlockDeclaration { name, block } => todo!(),
            ASTDetails::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => todo!(),
            ASTDetails::StringLiteral { tag, segments } => todo!(),
            ASTDetails::ArrayLiteral(_) => todo!(),
            ASTDetails::ObjectLiteral(_) => todo!(),
            ASTDetails::BinaryOperation { left, op, right } => {
                let ctx: InferTypeContext = ctx.into();
                let left_type = left.infer_type(ctx);
                let right_type = right.infer_type(ctx);

                let number_or_string = Type::ANY_NUMBER.union(Type::ANY_STRING);

                if let ASTDetails::BinaryOperator(operator) = op.details() {
                    if operator == &BinaryOperatorOp::Plus {
                        check_subsumation(&number_or_string, left_type, left.slice(), report_error);
                        check_subsumation(
                            &number_or_string,
                            right_type,
                            right.slice(),
                            report_error,
                        );
                    } else if operator == &BinaryOperatorOp::Minus
                        || operator == &BinaryOperatorOp::Times
                        || operator == &BinaryOperatorOp::Divide
                    {
                        check_subsumation(&Type::ANY_NUMBER, left_type, left.slice(), report_error);
                        check_subsumation(
                            &Type::ANY_NUMBER,
                            right_type,
                            right.slice(),
                            report_error,
                        );
                    } else if operator == &BinaryOperatorOp::Equals
                        || operator == &BinaryOperatorOp::NotEquals
                    {
                        if !left_type.subsumes(ctx.into(), &right_type)
                            && !right_type.subsumes(ctx.into(), &left_type)
                        {
                            report_error(BagelError::MiscError {
                                module_id: ctx.current_module.module_id.clone(),
                                src: op.slice().clone(),
                                message: format!(
                                    "Can't compare types {} and {} because they have no overlap",
                                    blue_string(&format!("{}", left_type)),
                                    blue_string(&format!("{}", right_type)),
                                ),
                            });
                        }
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
            ASTDetails::NegationOperation(inner) => {
                inner.check(ctx, report_error);

                check_subsumation(
                    &Type::ANY_NUMBER,
                    inner.infer_type(ctx.into()),
                    inner.slice(),
                    report_error,
                );
            }
            ASTDetails::Parenthesis(inner) => inner.check(ctx, report_error),
            ASTDetails::LocalIdentifier(name) => {
                if self.resolve_symbol(name.as_str()).is_none() {
                    report_error(BagelError::NotFoundError {
                        module_id: ctx.current_module.module_id.clone(),
                        identifier: self.clone().upcast(),
                    });
                }
            }
            ASTDetails::InlineConstGroup {
                declarations,
                inner,
            } => {
                for InlineDeclaration {
                    destination,
                    awaited,
                    value,
                } in declarations
                {
                    match destination {
                        DeclarationDestination::NameAndType(NameAndType {
                            name,
                            type_annotation,
                        }) => {
                            name.check(ctx, report_error);
                            type_annotation.check(ctx, report_error);
                        }
                        DeclarationDestination::Destructure(Destructure {
                            properties,
                            spread,
                            destructure_kind,
                        }) => {
                            properties.check(ctx, report_error);
                            spread.check(ctx, report_error);
                        }
                    }
                    value.check(ctx, report_error);
                }

                inner.check(ctx, report_error);
            }
            ASTDetails::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => {
                type_annotation.clone().upcast().check(ctx, report_error);
                body.check(ctx, report_error);

                let type_annotation = type_annotation.downcast();
                if let Some(return_type) = type_annotation.returns {
                    check_subsumation(
                        &return_type.resolve_type(ctx.into()),
                        body.infer_type(ctx.into()),
                        body.slice(),
                        report_error,
                    );
                }
            }
            ASTDetails::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => {
                type_annotation.check(ctx, report_error);
                body.check(ctx, report_error);
            }
            ASTDetails::Block(statements) => {}
            ASTDetails::JavascriptEscape(_) => todo!(),
            ASTDetails::RangeExpression { start, end } => todo!(),
            ASTDetails::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => {
                subject.check(ctx, report_error);
                type_args.check(ctx, report_error);
                args.check(ctx, report_error);
                spread_args.check(ctx, report_error);

                let subject_type = subject.infer_type(ctx.into());
                let types = match &subject_type {
                    Type::FuncType {
                        args,
                        args_spread,
                        is_pure: _,
                        returns: _,
                    } => Some((args, args_spread)),
                    Type::ProcType {
                        args,
                        args_spread,
                        is_pure: _,
                        is_async: _,
                        throws: _,
                    } => Some((args, args_spread)),
                    _ => None,
                };

                if let Some((arg_types, args_spread_type)) = types {
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(arg_type) = arg_types.get(i) {
                            if let Some(type_annotation) = &arg_type {
                                check_subsumation(
                                    type_annotation,
                                    arg.infer_type(ctx.into()),
                                    arg.slice(),
                                    report_error,
                                );
                            }
                        }
                    }
                } else {
                    report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: subject.slice().clone(),
                        message: format!(
                            "{} is of type {} and cannot be called",
                            blue_string(&format!("{}", subject)),
                            blue_string(&format!("{}", subject_type)),
                        ),
                    });
                }

                // TODO: Check that type_args fit
                // TODO: Check appropriateness of bubbles and awaited_or_detached
            }
            ASTDetails::PropertyAccessor {
                subject,
                property,
                optional,
            } => todo!(),
            ASTDetails::IfElseExpression {
                cases,
                default_case,
            } => {
                cases.check(ctx, report_error);
                default_case.check(ctx, report_error);

                let truthiness_safe = Type::get_truthiness_safe_types();
                for case in cases {
                    let IfElseExpressionCase { condition, outcome } = case.downcast();

                    check_subsumation(
                        &truthiness_safe,
                        condition.infer_type(ctx.into()),
                        condition.slice(),
                        report_error,
                    );
                }
            }
            ASTDetails::IfElseExpressionCase { condition, outcome } => {
                condition.check(ctx, report_error);
                outcome.check(ctx, report_error);
            }
            ASTDetails::SwitchExpression {
                value,
                cases,
                default_case,
            } => todo!(),
            ASTDetails::SwitchExpressionCase {
                type_filter,
                outcome,
            } => todo!(),
            ASTDetails::ElementTag {
                tag_name,
                attributes,
                children,
            } => todo!(),
            ASTDetails::AsCast { inner, as_type } => todo!(),
            ASTDetails::InstanceOf {
                inner,
                possible_type,
            } => todo!(),
            ASTDetails::ErrorExpression(_) => todo!(),
            ASTDetails::RegularExpression { expr, flags } => todo!(),
            ASTDetails::UnionType(_) => todo!(),
            ASTDetails::MaybeType(_) => todo!(),
            ASTDetails::NamedType(_) => todo!(),
            ASTDetails::GenericParamType { name, extends } => todo!(),
            ASTDetails::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            ASTDetails::FuncType {
                args,
                args_spread,
                is_pure,
                is_async,
                returns,
            } => {
                args.check(ctx, report_error);
                args_spread.check(ctx, report_error);
                returns.check(ctx, report_error);
            }
            ASTDetails::Arg {
                name,
                type_annotation,
                optional,
            } => {
                name.check(ctx, report_error);
                // TODO: Check that no optionsl args come before non-optional args
                type_annotation.check(ctx, report_error);
            }
            ASTDetails::GenericType { type_params, inner } => todo!(),
            ASTDetails::TypeParam { name, extends } => todo!(),
            ASTDetails::BoundGenericType { type_args, generic } => todo!(),
            ASTDetails::ObjectType {
                entries,
                is_interface,
            } => todo!(),
            ASTDetails::RecordType {
                key_type,
                value_type,
            } => {
                key_type.check(ctx, report_error);
                value_type.check(ctx, report_error);

                check_subsumation(
                    &Type::ANY_NUMBER.union(Type::ANY_STRING),
                    key_type.resolve_type(ctx.into()),
                    key_type.slice(),
                    report_error,
                );
            }
            ASTDetails::ArrayType(element) => element.check(ctx, report_error),
            ASTDetails::TupleType(members) => members.check(ctx, report_error),
            ASTDetails::SpecialType { kind, inner } => todo!(),
            ASTDetails::ModifierType { kind, inner } => todo!(),
            ASTDetails::ParenthesizedType(_) => todo!(),
            ASTDetails::TypeofType(_) => todo!(),
            ASTDetails::PropertyType {
                subject,
                property,
                optional,
            } => todo!(),
            ASTDetails::MaybeType(inner) => inner.check(ctx, report_error),
            ASTDetails::UnionType(members) => members.check(ctx, report_error),
            ASTDetails::DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            } => todo!(),
            ASTDetails::IfElseStatement {
                cases,
                default_case,
            } => todo!(),
            ASTDetails::IfElseStatementCase { condition, outcome } => todo!(),
            ASTDetails::ForLoop {
                item_identifier,
                iterator,
                body,
            } => todo!(),
            ASTDetails::WhileLoop { condition, body } => {
                condition.check(ctx, report_error);
                body.check(ctx, report_error);

                check_subsumation(
                    &Type::get_truthiness_safe_types(),
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    report_error,
                );
            }
            ASTDetails::Assignment {
                target,
                value,
                operator,
            } => todo!(),
            ASTDetails::TryCatch {
                try_block,
                error_identifier,
                catch_block,
            } => todo!(),
            ASTDetails::ThrowStatement { error_expression } => todo!(),
            ASTDetails::Autorun {
                effect_block,
                until,
            } => {
                effect_block.check(ctx, report_error);
                until.check(ctx, report_error);

                if let Some(until) = until {
                    check_subsumation(
                        &Type::ANY_BOOLEAN,
                        until.infer_type(ctx.into()),
                        until.slice(),
                        report_error,
                    );
                }
            }

            // intentionally have nothing to check
            ASTDetails::NilLiteral => {}
            ASTDetails::NumberLiteral(_) => {}
            ASTDetails::BooleanLiteral(_) => {}
            ASTDetails::ExactStringLiteral { tag: _, value: _ } => {}
            ASTDetails::BinaryOperator(_) => {}
            ASTDetails::PlainIdentifier(_) => {}

            ASTDetails::RegularExpressionType => {}
            ASTDetails::StringLiteralType(_) => {}
            ASTDetails::NumberLiteralType(_) => {}
            ASTDetails::BooleanLiteralType(_) => {}
            ASTDetails::StringType => {}
            ASTDetails::NumberType => {}
            ASTDetails::BooleanType => {}
            ASTDetails::NilType => {}
            ASTDetails::UnknownType => {}
        }
    }
}

impl<T> Checkable for Option<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        if let Some(sel) = self {
            sel.check(ctx, report_error);
        }
    }
}

impl<T> Checkable for Vec<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        for el in self.iter() {
            el.check(ctx, report_error);
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CheckContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
}
