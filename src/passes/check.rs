use crate::{
    model::{
        ast::*,
        bgl_type::{SubsumationContext, Type},
        errors::blue_string,
        slice::Slice,
    },
    model::{
        ast::{self, BinaryOperatorOp},
        errors::BagelError,
        module::{Module, ModulesStore},
    },
    DEBUG_MODE,
};
use std::fmt::Debug;
use std::time::SystemTime;

impl Module {
    pub fn check<'a, F: FnMut(BagelError)>(&self, ctx: &mut CheckContext<'a, F>) {
        let start = SystemTime::now();

        self.ast.check(ctx);

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
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &mut CheckContext<'a, F>);
}

impl<TKind> Checkable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &mut CheckContext<'a, F>) {
        let module_id = &ctx.current_module.module_id.clone();
        let subsumation_context = SubsumationContext::from(&*ctx);
        let check_subsumation =
            |destination: &Type, value: Type, slice: &Slice, report_error: &mut F| {
                let issues = destination.subsumation_issues(subsumation_context, &value);

                if let Some(issues) = issues {
                    report_error(BagelError::AssignmentError {
                        module_id: module_id.clone(),
                        src: slice.clone(),
                        issues,
                    });
                }
            };

        match self.details() {
            Any::Module(ast::Module { declarations }) => {
                declarations.check(ctx);

                // todo!("Check for name duplicates");
            }
            Any::ImportAllDeclaration(ImportAllDeclaration { name, path }) => {
                name.check(ctx);
                path.check(ctx);

                let path_name = path.downcast();
                let path_name = path_name.value.as_str();

                if ctx.modules.import(module_id, path_name).is_none() {
                    ctx.report_error(BagelError::MiscError {
                        module_id: module_id.clone(),
                        src: path.slice().clone(),
                        message: format!(
                            "Couldn't find module {} from module {}",
                            blue_string(path_name),
                            blue_string(module_id)
                        ),
                    });
                }
            }
            Any::ImportDeclaration(ImportDeclaration { imports, path }) => {
                imports.check(ctx);
                path.check(ctx);

                let path_name = path.downcast();
                let path_name = path_name.value.as_str();

                let imported_module = ctx.modules.import(module_id, path_name);

                match imported_module {
                    None => ctx.report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: path.slice().clone(),
                        message: format!(
                            "Couldn't find module {} from module {}",
                            blue_string(path_name),
                            blue_string(module_id)
                        ),
                    }),
                    Some(imported_module) => {
                        for item in imports {
                            let item_downcast = item.downcast();
                            let item_name = item_downcast.name.downcast();
                            let item_name = item_name.0.as_str();

                            let decl = imported_module.get_declaration(item_name, true);

                            if decl.is_none() {
                                ctx.report_error(BagelError::MiscError {
                                    module_id: ctx.current_module.module_id.clone(),
                                    src: item.slice().clone(),
                                    message: format!(
                                        "No exported member named {} found in module {}",
                                        blue_string(item_name),
                                        blue_string(&imported_module.module_id)
                                    ),
                                })
                            }
                        }
                    }
                }
            }
            Any::ImportItem(ImportItem { name, alias }) => {
                name.check(ctx);
                alias.check(ctx);
            }
            Any::TypeDeclaration(TypeDeclaration {
                name,
                declared_type,
                exported,
            }) => {
                name.check(ctx);
                declared_type.check(ctx);
            }
            Any::FuncDeclaration(FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            }) => {
                name.check(ctx);
                func.check(ctx);
                decorators.check(ctx);
            }
            Any::ProcDeclaration(ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            }) => {
                name.check(ctx);
                proc.check(ctx);
                decorators.check(ctx);
            }
            Any::Decorator(Decorator { name }) => todo!(),
            Any::ValueDeclaration(ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            }) => {
                name.check(ctx);
                type_annotation.check(ctx);
                value.check(ctx);

                if let Some(type_annotation) = type_annotation {
                    check_subsumation(
                        &type_annotation.resolve_type(ctx.into()),
                        value.infer_type(ctx.into()),
                        value.slice(),
                        ctx.report_error,
                    );
                }
            }
            Any::TestExprDeclaration(TestExprDeclaration { name, expr }) => todo!(),
            Any::TestBlockDeclaration(TestBlockDeclaration { name, block }) => todo!(),
            Any::TestTypeDeclaration(TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            }) => todo!(),
            Any::StringLiteral(StringLiteral { tag, segments }) => todo!(),
            Any::ArrayLiteral(ArrayLiteral(members)) => {
                for member in members {
                    match member {
                        ArrayLiteralEntry::Expression(expr) => expr.check(ctx),
                        ArrayLiteralEntry::Spread(spread_expr) => {
                            spread_expr.check(ctx);

                            let spread_inner = spread_expr.downcast().0;

                            check_subsumation(
                                &Type::any_array(),
                                spread_inner.infer_type(ctx.into()),
                                spread_inner.slice(),
                                ctx.report_error,
                            );
                        }
                    }
                }
            }
            Any::ObjectLiteral(ObjectLiteral(_)) => todo!(),
            Any::SpreadExpression(SpreadExpression(inner)) => {
                inner.check(ctx);
            }
            Any::BinaryOperation(BinaryOperation { left, op, right }) => {
                let left_type = left.infer_type(ctx.into());
                let right_type = right.infer_type(ctx.into());

                let number_or_string = Type::ANY_NUMBER.union(Type::ANY_STRING);

                let operator = op.downcast().0;

                if operator == BinaryOperatorOp::Plus {
                    check_subsumation(&number_or_string, left_type, left.slice(), ctx.report_error);
                    check_subsumation(
                        &number_or_string,
                        right_type,
                        right.slice(),
                        ctx.report_error,
                    );
                } else if operator == BinaryOperatorOp::Minus
                    || operator == BinaryOperatorOp::Times
                    || operator == BinaryOperatorOp::Divide
                {
                    check_subsumation(&Type::ANY_NUMBER, left_type, left.slice(), ctx.report_error);
                    check_subsumation(
                        &Type::ANY_NUMBER,
                        right_type,
                        right.slice(),
                        ctx.report_error,
                    );
                } else if operator == BinaryOperatorOp::Equals
                    || operator == BinaryOperatorOp::NotEquals
                {
                    if !left_type.subsumes((&*ctx).into(), &right_type)
                        && !right_type.subsumes((&*ctx).into(), &left_type)
                    {
                        ctx.report_error(BagelError::MiscError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: op.slice().clone(),
                            message: format!(
                                "Can't compare types {} and {} because they have no overlap",
                                blue_string(&left_type),
                                blue_string(&right_type),
                            ),
                        });
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
            Any::NegationOperation(NegationOperation(inner)) => {
                inner.check(ctx);

                check_subsumation(
                    &Type::ANY_NUMBER,
                    inner.infer_type(ctx.into()),
                    inner.slice(),
                    ctx.report_error,
                );
            }
            Any::Parenthesis(Parenthesis(inner)) => inner.check(ctx),
            Any::LocalIdentifier(LocalIdentifier(name)) => {
                // TODO: Make sure it isn't a type
                if self.resolve_symbol(name.as_str()).is_none() {
                    ctx.report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: self.slice().clone(),
                        message: format!(
                            "Couldn't resolve identifier {} in this scope",
                            blue_string(name.as_str())
                        ),
                    });
                }
            }
            Any::NamedType(NamedType(name)) => {
                // TODO: Make sure it isn't an expression
                let name = name.downcast();
                let name_str = name.0.as_str();
                if self.resolve_symbol(name_str).is_none() {
                    ctx.report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: self.slice().clone(),
                        message: format!(
                            "Couldn't resolve identifier {} in this scope",
                            blue_string(name_str)
                        ),
                    });
                }
            }
            Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner,
            }) => {
                declarations.check(ctx);
                inner.check(ctx);
            }
            Any::InlineDeclaration(InlineDeclaration {
                destination,
                awaited,
                value,
            }) => {
                match destination {
                    DeclarationDestination::NameAndType(NameAndType {
                        name,
                        type_annotation,
                    }) => {
                        name.check(ctx);
                        type_annotation.check(ctx);

                        if let Some(type_annotation) = type_annotation {
                            check_subsumation(
                                &type_annotation.resolve_type(ctx.into()),
                                value.infer_type(ctx.into()),
                                value.slice(),
                                ctx.report_error,
                            );
                        }
                    }
                    DeclarationDestination::Destructure(Destructure {
                        properties,
                        spread,
                        destructure_kind,
                    }) => {
                        properties.check(ctx);
                        spread.check(ctx);
                    }
                }
                value.check(ctx);
            }
            Any::Func(Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                type_annotation.check(ctx);
                body.check(ctx);

                let type_annotation = type_annotation.downcast();
                if let Some(return_type) = type_annotation.returns {
                    check_subsumation(
                        &return_type.resolve_type(ctx.into()),
                        body.infer_type(ctx.into()),
                        body.slice(),
                        ctx.report_error,
                    );
                }
            }
            Any::Proc(Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                type_annotation.check(ctx);
                body.check(ctx);
            }
            Any::Block(Block(statements)) => {
                statements.check(ctx);
            }
            Any::RangeExpression(RangeExpression { start, end }) => {
                start.check(ctx);
                end.check(ctx);

                check_subsumation(
                    &Type::ANY_NUMBER,
                    start.infer_type(ctx.into()),
                    start.slice(),
                    ctx.report_error,
                );

                check_subsumation(
                    &Type::ANY_NUMBER,
                    end.infer_type(ctx.into()),
                    end.slice(),
                    ctx.report_error,
                );
            }
            Any::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            }) => {
                subject.check(ctx);
                type_args.check(ctx);
                args.check(ctx);
                spread_args.check(ctx);

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
                                    ctx.report_error,
                                );
                            }
                        }
                    }
                } else {
                    ctx.report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: subject.slice().clone(),
                        message: format!(
                            "{} is of type {} and cannot be called",
                            blue_string(&subject.clone().upcast()),
                            blue_string(&subject_type),
                        ),
                    });
                }

                // TODO: Check that type_args fit
                // TODO: Check appropriateness of bubbles and awaited_or_detached
            }
            Any::PropertyAccessor(PropertyAccessor {
                subject,
                property,
                optional,
            }) => {
                subject.check(ctx);
                property.check(ctx);

                let subject_type = subject.infer_type(ctx.into());
                let property_type = property
                    .clone()
                    .try_recast::<Expression>()
                    .unwrap()
                    .infer_type(ctx.into());

                // TODO: detect unnecessary optional
                // TODO: detect valid optional

                if subject_type.indexed(&property_type).is_none() {
                    ctx.report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: property.slice().clone(),
                        message: format!(
                            "{} cannot be used to index type {}",
                            blue_string(&property_type),
                            blue_string(&subject_type)
                        ),
                    })
                }
            }
            Any::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            }) => {
                cases.check(ctx);
                default_case.check(ctx);
            }
            Any::IfElseExpressionCase(IfElseExpressionCase { condition, outcome }) => {
                condition.check(ctx);
                outcome.check(ctx);

                let truthiness_safe = Type::truthiness_safe_types();
                check_subsumation(
                    &truthiness_safe,
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    ctx.report_error,
                );
            }
            Any::SwitchExpression(SwitchExpression {
                value,
                cases,
                default_case,
            }) => todo!(),
            Any::SwitchExpressionCase(SwitchExpressionCase {
                type_filter,
                outcome,
            }) => todo!(),
            Any::ElementTag(ElementTag {
                tag_name,
                attributes,
                children,
            }) => todo!(),
            Any::AsCast(AsCast { inner, as_type }) => {
                inner.check(ctx);
                as_type.check(ctx);

                check_subsumation(
                    &as_type.resolve_type(ctx.into()),
                    inner.infer_type(ctx.into()),
                    inner.slice(),
                    ctx.report_error,
                );
            }
            Any::InstanceOf(InstanceOf {
                inner,
                possible_type,
            }) => {
                inner.check(ctx);
                possible_type.check(ctx);

                check_subsumation(
                    &inner.infer_type(ctx.into()),
                    possible_type.resolve_type(ctx.into()),
                    inner.slice(),
                    ctx.report_error,
                );
            }
            Any::ErrorExpression(ErrorExpression(_)) => todo!(),
            Any::UnionType(UnionType(members)) => {
                members.check(ctx);
            }
            Any::MaybeType(MaybeType(inner)) => inner.check(ctx),
            Any::GenericParamType(GenericParamType { name, extends }) => todo!(),
            Any::ProcType(ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            }) => {
                args.check(ctx);
                args_spread.check(ctx);
                throws.check(ctx);
            }
            Any::FuncType(FuncType {
                args,
                args_spread,
                is_pure,
                is_async,
                returns,
            }) => {
                args.check(ctx);
                args_spread.check(ctx);
                returns.check(ctx);
            }
            Any::Arg(Arg {
                name,
                type_annotation,
                optional,
            }) => {
                name.check(ctx);
                // TODO: Check that no optionsl args come before non-optional args
                type_annotation.check(ctx);
            }
            Any::GenericType(GenericType { type_params, inner }) => todo!(),
            Any::TypeParam(TypeParam { name, extends }) => todo!(),
            Any::BoundGenericType(BoundGenericType { type_args, generic }) => todo!(),
            Any::ObjectType(ObjectType {
                entries,
                is_interface,
            }) => todo!(),
            Any::RecordType(RecordType {
                key_type,
                value_type,
            }) => {
                key_type.check(ctx);
                value_type.check(ctx);

                check_subsumation(
                    &Type::ANY_NUMBER.union(Type::ANY_STRING),
                    key_type.resolve_type(ctx.into()),
                    key_type.slice(),
                    ctx.report_error,
                );
            }
            Any::ArrayType(ArrayType(element)) => element.check(ctx),
            Any::TupleType(TupleType(members)) => members.check(ctx),
            Any::SpecialType(SpecialType { kind: _, inner }) => {
                inner.check(ctx);
            }
            Any::ModifierType(ModifierType { kind, inner }) => match kind {
                ModifierTypeKind::Readonly => {}
                ModifierTypeKind::Keyof => {
                    let inner_type = inner.resolve_type(ctx.into());

                    match inner_type {
                        Type::RecordType {
                            key_type: _,
                            value_type: _,
                        } => {}
                        Type::ObjectType {
                            entries: _,
                            is_interface: _,
                        } => {}
                        _ => ctx.report_error(BagelError::MiscError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: self.slice().clone(),
                            message: format!(
                                "Cannot apply {} to {}",
                                blue_string("keyof"),
                                blue_string(&inner_type)
                            ),
                        }),
                    }
                }
                ModifierTypeKind::Valueof => {
                    let inner_type = inner.resolve_type(ctx.into());

                    match inner_type {
                        Type::RecordType {
                            key_type: _,
                            value_type: _,
                        } => {}
                        Type::ObjectType {
                            entries: _,
                            is_interface: _,
                        } => {}
                        _ => ctx.report_error(BagelError::MiscError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: self.slice().clone(),
                            message: format!(
                                "Cannot apply {} to {}",
                                blue_string("valueof"),
                                blue_string(&inner_type)
                            ),
                        }),
                    }
                }
                ModifierTypeKind::Elementof => {
                    let inner_type = inner.resolve_type(ctx.into());

                    match inner_type {
                        Type::ArrayType(_) => {}
                        Type::TupleType(_) => {}
                        _ => ctx.report_error(BagelError::MiscError {
                            module_id: ctx.current_module.module_id.clone(),
                            src: self.slice().clone(),
                            message: format!(
                                "Cannot apply {} to {}",
                                blue_string("keyof"),
                                blue_string(&inner_type)
                            ),
                        }),
                    }
                }
            },
            Any::TypeofType(TypeofType(_)) => todo!(),
            Any::PropertyType(PropertyType {
                subject,
                property,
                optional,
            }) => todo!(),
            Any::MaybeType(MaybeType(inner)) => inner.check(ctx),
            Any::UnionType(UnionType(members)) => members.check(ctx),
            Any::DeclarationStatement(DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            }) => {
                match destination {
                    DeclarationDestination::NameAndType(NameAndType {
                        name,
                        type_annotation,
                    }) => {
                        name.check(ctx);
                        type_annotation.check(ctx);

                        if let Some(type_annotation) = type_annotation {
                            check_subsumation(
                                &type_annotation.resolve_type(ctx.into()),
                                value.infer_type(ctx.into()),
                                value.slice(),
                                ctx.report_error,
                            );
                        }
                    }
                    DeclarationDestination::Destructure(Destructure {
                        properties,
                        spread,
                        destructure_kind,
                    }) => {
                        properties.check(ctx);
                        spread.check(ctx);
                    }
                }
                value.check(ctx);
            }
            Any::IfElseStatement(IfElseStatement {
                cases,
                default_case,
            }) => {
                cases.check(ctx);
                default_case.check(ctx);
            }
            Any::IfElseStatementCase(IfElseStatementCase { condition, outcome }) => {
                condition.check(ctx);
                outcome.check(ctx);

                let truthiness_safe = Type::truthiness_safe_types();
                check_subsumation(
                    &truthiness_safe,
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    ctx.report_error,
                );
            }
            Any::ForLoop(ForLoop {
                item_identifier,
                iterator,
                body,
            }) => todo!(),
            Any::WhileLoop(WhileLoop { condition, body }) => {
                condition.check(ctx);
                body.check(ctx);

                check_subsumation(
                    &Type::truthiness_safe_types(),
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    ctx.report_error,
                );
            }
            Any::Assignment(Assignment {
                target,
                value,
                operator,
            }) => {
                target.check(ctx);
                value.check(ctx);
                operator.check(ctx);

                // TODO: Check operator compatibility for value and target

                check_subsumation(
                    &target.infer_type(ctx.into()),
                    value.infer_type(ctx.into()),
                    value.slice(),
                    ctx.report_error,
                );
            }
            Any::TryCatch(TryCatch {
                try_block,
                error_identifier,
                catch_block,
            }) => todo!(),
            Any::ThrowStatement(ThrowStatement { error_expression }) => todo!(),
            Any::Autorun(Autorun {
                effect_block,
                until,
            }) => {
                effect_block.check(ctx);
                until.check(ctx);

                if let Some(until) = until {
                    check_subsumation(
                        &Type::ANY_BOOLEAN,
                        until.infer_type(ctx.into()),
                        until.slice(),
                        ctx.report_error,
                    );
                }
            }

            // intentionally have nothing to check
            Any::JavascriptEscape(_) => {}
            Any::NilLiteral(_) => {}
            Any::NumberLiteral(_) => {}
            Any::BooleanLiteral(_) => {}
            Any::ExactStringLiteral(_) => {}
            Any::BinaryOperator(_) => {}
            Any::PlainIdentifier(_) => {}
            Any::RegularExpression(_) => {}

            Any::ParenthesizedType(_) => {}
            Any::RegularExpressionType(_) => {}
            Any::StringLiteralType(_) => {}
            Any::NumberLiteralType(_) => {}
            Any::BooleanLiteralType(_) => {}
            Any::StringType(_) => {}
            Any::NumberType(_) => {}
            Any::BooleanType(_) => {}
            Any::NilType(_) => {}
            Any::UnknownType(_) => {}
        }
    }
}

impl<T> Checkable for Option<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &mut CheckContext<'a, F>) {
        if let Some(sel) = self {
            sel.check(ctx);
        }
    }
}

impl<T> Checkable for Vec<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &mut CheckContext<'a, F>) {
        for el in self.iter() {
            el.check(ctx);
        }
    }
}

#[derive(Debug)]
pub struct CheckContext<'a, F: FnMut(BagelError)> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
    pub report_error: &'a mut F,
}

impl<'a, F: FnMut(BagelError)> CheckContext<'a, F> {
    pub fn report_error(&mut self, error: BagelError) {
        (self.report_error)(error)
    }
}
