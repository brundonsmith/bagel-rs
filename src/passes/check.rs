use crate::{
    model::{
        ast::BinaryOperatorOp,
        errors::BagelError,
        module::{Module, ModulesStore},
    },
    model::{ast::*, bgl_type::Type},
    passes::typeinfer::InferTypeContext,
    DEBUG_MODE,
};
use std::fmt::Debug;
use std::time::SystemTime;

impl Module {
    pub fn check<'a, F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        let start = SystemTime::now();

        self.ast.check(ctx, report_error);

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

impl Checkable for AST {
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: CheckContext<'a>, report_error: &mut F) {
        match self.details() {
            ASTDetails::Module { declarations } => {
                for decl in declarations {
                    decl.check(ctx, report_error);
                }
            }
            ASTDetails::ImportAllDeclaration { name, path } => todo!(),
            ASTDetails::ImportDeclaration { imports, path } => todo!(),
            ASTDetails::ImportItem { name, alias } => todo!(),
            ASTDetails::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => todo!(),
            ASTDetails::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => todo!(),
            ASTDetails::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => todo!(),
            ASTDetails::Decorator { name } => todo!(),
            ASTDetails::ValueDeclaration {
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
                        .resolve_type(ctx.into())
                        .subsumation_issues(ctx.into(), &value_type);

                    if let Some(issues) = issues {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: value.slice().clone(),
                            issues,
                        });
                    }
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

                if op.details() == &ASTDetails::BinaryOperator(BinaryOperatorOp::Plus) {
                    if let Some(issues) =
                        number_or_string.subsumation_issues(ctx.into(), &left_type)
                    {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: left.slice().clone(),
                            issues,
                        })
                    }

                    if let Some(issues) =
                        number_or_string.subsumation_issues(ctx.into(), &right_type)
                    {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: right.slice().clone(),
                            issues,
                        })
                    }
                } else if op.details() == &ASTDetails::BinaryOperator(BinaryOperatorOp::Minus)
                    || op.details() == &ASTDetails::BinaryOperator(BinaryOperatorOp::Times)
                    || op.details() == &ASTDetails::BinaryOperator(BinaryOperatorOp::Divide)
                {
                    if let Some(issues) =
                        Type::ANY_NUMBER.subsumation_issues(ctx.into(), &left_type)
                    {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: left.slice().clone(),
                            issues,
                        })
                    }

                    if let Some(issues) =
                        Type::ANY_NUMBER.subsumation_issues(ctx.into(), &right_type)
                    {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: right.slice().clone(),
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
            ASTDetails::BinaryOperator(_) => todo!(),
            ASTDetails::NegationOperation(_) => todo!(),
            ASTDetails::Parenthesis(inner) => inner.check(ctx, report_error),
            ASTDetails::LocalIdentifier(name) => {
                if self.resolve_symbol(name.as_str()).is_none() {
                    report_error(BagelError::NotFoundError {
                        module_id: ctx.current_module.module_id.clone(),
                        identifier: self.clone(),
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
            } => todo!(),
            ASTDetails::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            ASTDetails::Block(_) => todo!(),
            ASTDetails::JavascriptEscape(_) => todo!(),
            ASTDetails::RangeExpression { start, end } => todo!(),
            ASTDetails::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            ASTDetails::PropertyAccessor {
                subject,
                property,
                optional,
            } => todo!(),
            ASTDetails::IfElseExpression {
                cases,
                default_case,
            } => {
                let truthiness_safe = Type::get_truthiness_safe_types();

                for case in cases {
                    let IfElseExpressionCase { condition, outcome } =
                        case.expect::<IfElseExpressionCase>();

                    let condition_type = condition.infer_type(ctx.into());

                    if let Some(issues) =
                        truthiness_safe.subsumation_issues(ctx.into(), &condition_type)
                    {
                        report_error(BagelError::AssignmentError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: condition.slice().clone(),
                            issues,
                        });
                    }

                    condition.check(ctx, report_error);
                    outcome.check(ctx, report_error);
                }

                if let Some(default_case) = default_case {
                    default_case.check(ctx, report_error);
                }
            }
            ASTDetails::IfElseExpressionCase { condition, outcome } => todo!(),
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
            } => todo!(),
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
            } => todo!(),
            ASTDetails::ArrayType(_) => todo!(),
            ASTDetails::TupleType(_) => todo!(),
            ASTDetails::SpecialType { kind, inner } => todo!(),
            ASTDetails::ModifierType { kind, inner } => todo!(),
            ASTDetails::ParenthesizedType(_) => todo!(),
            ASTDetails::TypeofType(_) => todo!(),
            ASTDetails::PropertyType {
                subject,
                property,
                optional,
            } => todo!(),
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
            ASTDetails::WhileLoop { condition, body } => todo!(),
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
            } => todo!(),
            ASTDetails::PlainIdentifier(_) => todo!(),

            // intentionally have nothing to check
            ASTDetails::NilLiteral => {}
            ASTDetails::NumberLiteral(_) => {}
            ASTDetails::BooleanLiteral(_) => {}
            ASTDetails::ExactStringLiteral { tag: _, value: _ } => {}

            ASTDetails::RegularExpressionType => {}
            ASTDetails::StringLiteralType(_) => {}
            ASTDetails::NumberLiteralType(_) => {}
            ASTDetails::BooleanLiteralType(_) => {}
            ASTDetails::StringType => {}
            ASTDetails::NumberType => {}
            ASTDetails::BooleanType => {}
            ASTDetails::NilType => {}
            ASTDetails::UnionType(members) => {}
            ASTDetails::MaybeType(inner) => {}
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
