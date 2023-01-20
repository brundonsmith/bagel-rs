use crate::{
    model::{
        ast::*,
        bgl_type::{
            any_array, any_error, any_iterator, any_object, any_plan, string_template_safe_types,
            truthiness_safe_types, SubsumationContext, Type,
        },
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

use super::typeinfer::binary_operation_type;

impl Module {
    pub fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
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
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F);
}

impl<TKind> Checkable for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + TryFrom<TKind>,
    Any: From<TKind>,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        let module_id = &ctx.current_module.module_id.clone();
        let subsumation_context = SubsumationContext::from(ctx);
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

        let mut check_binary_operation =
            |BinaryOperation { left, op, right }: &BinaryOperation, report_error: &mut F| {
                let module_id = &ctx.current_module.module_id.clone();
                let subsumation_context = SubsumationContext::from(ctx);
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

                let left_type = left.infer_type(ctx.into());
                let right_type = right.infer_type(ctx.into());

                let number_or_string = Type::ANY_NUMBER.union(Type::ANY_STRING);

                let operator = op.downcast().0;

                if operator == BinaryOperatorOp::Plus {
                    check_subsumation(&number_or_string, left_type, left.slice(), report_error);
                    check_subsumation(&number_or_string, right_type, right.slice(), report_error);
                } else if operator == BinaryOperatorOp::Minus
                    || operator == BinaryOperatorOp::Times
                    || operator == BinaryOperatorOp::Divide
                {
                    check_subsumation(&Type::ANY_NUMBER, left_type, left.slice(), report_error);
                    check_subsumation(&Type::ANY_NUMBER, right_type, right.slice(), report_error);
                } else if operator == BinaryOperatorOp::Equals
                    || operator == BinaryOperatorOp::NotEquals
                {
                    if !left_type.subsumes(ctx.into(), &right_type)
                        && !right_type.subsumes(ctx.into(), &left_type)
                    {
                        report_error(BagelError::MiscError {
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
            };

        match self.details() {
            Any::Module(ast::Module { declarations }) => {
                declarations.check(ctx, report_error);

                // todo!("Check for name duplicates");
            }
            Any::ImportAllDeclaration(ImportAllDeclaration { name, path }) => {
                name.check(ctx, report_error);
                path.check(ctx, report_error);

                let path_name = path.downcast();
                let path_name = path_name.value.as_str();

                if ctx.modules.import_raw(module_id, path_name).is_none() {
                    report_error(BagelError::MiscError {
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
                imports.check(ctx, report_error);
                path.check(ctx, report_error);

                let path_name = path.downcast();
                let path_name = path_name.value.as_str();

                let imported_module = ctx.modules.import_raw(module_id, path_name);

                match imported_module {
                    None => report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: path.slice().clone(),
                        message: format!(
                            "Couldn't find module {} from module {}",
                            blue_string(path_name),
                            blue_string(module_id)
                        ),
                    }),
                    Some(Err(_)) => {}
                    Some(Ok(imported_module)) => {
                        for item in imports {
                            let item_downcast = item.downcast();
                            let item_name = item_downcast.name.downcast();
                            let item_name = item_name.0.as_str();

                            let decl = imported_module.get_declaration(item_name, true);

                            if decl.is_none() {
                                report_error(BagelError::MiscError {
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
                name.check(ctx, report_error);
                alias.check(ctx, report_error);
            }
            Any::TypeDeclaration(TypeDeclaration {
                name,
                declared_type,
                exported,
            }) => {
                name.check(ctx, report_error);
                declared_type.check(ctx, report_error);
            }
            Any::FuncDeclaration(FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            }) => {
                name.check(ctx, report_error);
                func.check(ctx, report_error);
                decorators.check(ctx, report_error);
            }
            Any::ProcDeclaration(ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            }) => {
                name.check(ctx, report_error);
                proc.check(ctx, report_error);
                decorators.check(ctx, report_error);
            }
            Any::Decorator(Decorator { name, arguments }) => {
                name.check(ctx, report_error);
                arguments.check(ctx, report_error);

                // TODO: convert to invocation and check argument correctness
            }
            Any::ValueDeclaration(ValueDeclaration {
                destination,
                value,
                is_const,
                exported,
                platforms,
            }) => {
                destination.check(ctx, report_error);
                value.check(ctx, report_error);

                if let DeclarationDestination::NameAndType(NameAndType {
                    name: _,
                    type_annotation: Some(type_annotation),
                }) = destination
                {
                    check_subsumation(
                        &type_annotation.resolve_type(ctx.into()),
                        value.infer_type(ctx.into()),
                        value.slice(),
                        report_error,
                    );
                }

                if !*is_const && matches!(destination, DeclarationDestination::Destructure(_)) {
                    report_error(BagelError::MiscError {
                        module_id: module_id.clone(),
                        src: self.slice().clone(),
                        message: format!("Can only destructure when declaring a const"),
                    });
                }
            }
            Any::TestExprDeclaration(TestExprDeclaration { name, expr }) => todo!(),
            Any::TestBlockDeclaration(TestBlockDeclaration { name, block }) => todo!(),
            Any::TestTypeDeclaration(TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            }) => todo!(),
            Any::StringLiteral(StringLiteral { tag: _, segments }) => {
                for segment in segments {
                    match segment {
                        StringLiteralSegment::Slice(_) => {}
                        StringLiteralSegment::AST(insert) => {
                            insert.check(ctx, report_error);

                            check_subsumation(
                                &string_template_safe_types(),
                                insert.infer_type(ctx.into()),
                                insert.slice(),
                                report_error,
                            );
                        }
                    }
                }
            }
            Any::ArrayLiteral(ArrayLiteral(members)) => {
                members.check(ctx, report_error);

                for member in members {
                    match member {
                        ElementOrSpread::Element(_) => {}
                        ElementOrSpread::Spread(spread) => {
                            check_subsumation(
                                &any_array(),
                                spread.infer_type(ctx.into()),
                                spread.slice(),
                                report_error,
                            );
                        }
                    }
                }
            }
            Any::ObjectLiteral(ObjectLiteral(entries)) => {
                entries.check(ctx, report_error);

                for entry in entries {
                    match entry {
                        KeyValueOrSpread::KeyValue(_, _) => {}
                        KeyValueOrSpread::Spread(spread) => {
                            check_subsumation(
                                &any_object(),
                                spread.infer_type(ctx.into()),
                                spread.slice(),
                                report_error,
                            );
                        }
                    }
                }
            }
            Any::SpreadExpression(SpreadExpression(inner)) => {
                inner.check(ctx, report_error);
            }
            Any::BinaryOperation(op) => {
                check_binary_operation(op, report_error);
            }
            Any::NegationOperation(NegationOperation(inner)) => {
                inner.check(ctx, report_error);

                check_subsumation(
                    &truthiness_safe_types(),
                    inner.infer_type(ctx.into()),
                    inner.slice(),
                    report_error,
                );
            }
            Any::Parenthesis(Parenthesis(inner)) => inner.check(ctx, report_error),
            Any::LocalIdentifier(LocalIdentifier(name)) => {
                // TODO: Make sure it isn't a type
                if self.resolve_symbol(name.as_str()).is_none() {
                    report_error(BagelError::MiscError {
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
                    report_error(BagelError::MiscError {
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
                declarations.check(ctx, report_error);
                inner.check(ctx, report_error);
            }
            Any::InlineDeclaration(InlineDeclaration { destination, value }) => {
                match destination {
                    DeclarationDestination::NameAndType(NameAndType {
                        name,
                        type_annotation,
                    }) => {
                        name.check(ctx, report_error);
                        type_annotation.check(ctx, report_error);

                        if let Some(type_annotation) = type_annotation {
                            check_subsumation(
                                &type_annotation.resolve_type(ctx.into()),
                                value.infer_type(ctx.into()),
                                value.slice(),
                                report_error,
                            );
                        }
                    }
                    DeclarationDestination::Destructure(Destructure {
                        properties,
                        spread,
                        destructure_kind,
                    }) => {
                        properties.check(ctx, report_error);
                        spread.check(ctx, report_error);

                        // TODO: Check that value is the right type and all destructured properties exist
                    }
                }
                value.check(ctx, report_error);
            }
            Any::Func(Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                let ctx = &ctx.in_func(self.clone().upcast().try_recast::<Func>().unwrap());

                type_annotation.check(ctx, report_error);
                body.check(ctx, report_error);

                let type_annotation = type_annotation.downcast();

                // check for duplicate arg names
                let mut duplicated_args = Vec::new();
                for (index, arg) in type_annotation.args.iter().enumerate() {
                    for other_arg in type_annotation.args[index + 1..].iter() {
                        if other_arg.downcast().name.downcast().0.as_str()
                            == arg.downcast().name.downcast().0.as_str()
                            && !duplicated_args.contains(&other_arg.downcast().name)
                        {
                            duplicated_args.push(other_arg.downcast().name);
                        }
                    }
                }

                for arg in duplicated_args {
                    report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: arg.slice().clone(),
                        message: format!(
                            "Duplicate arg name {}",
                            blue_string(arg.downcast().0.as_str())
                        ),
                    });
                }

                // check that returned value matches return type
                if let Some(return_type) = type_annotation.returns {
                    check_subsumation(
                        &return_type.resolve_type(ctx.into()),
                        body.infer_type(ctx.into()),
                        body.slice(),
                        report_error,
                    );
                }
            }
            Any::Proc(Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                let ctx = &ctx.in_proc(self.clone().upcast().try_recast::<Proc>().unwrap());

                type_annotation.check(ctx, report_error);
                body.check(ctx, report_error);
            }
            Any::Block(Block(statements)) => {
                statements.check(ctx, report_error);
            }
            Any::RangeExpression(RangeExpression { start, end }) => {
                start.check(ctx, report_error);
                end.check(ctx, report_error);

                check_subsumation(
                    &Type::ANY_NUMBER,
                    start.infer_type(ctx.into()),
                    start.slice(),
                    report_error,
                );

                check_subsumation(
                    &Type::ANY_NUMBER,
                    end.infer_type(ctx.into()),
                    end.slice(),
                    report_error,
                );
            }
            Any::AwaitExpression(AwaitExpression(inner)) => {
                check_subsumation(
                    &any_plan(),
                    inner.infer_type(ctx.into()),
                    inner.slice(),
                    report_error,
                );

                if !ctx
                    .nearest_func_or_proc
                    .as_ref()
                    .map(FuncOrProc::is_async)
                    .unwrap_or(false)
                {
                    report_error(BagelError::MiscError {
                        module_id: ctx.current_module.module_id.clone(),
                        src: inner.slice().clone(),
                        message: format!("Can only await expressions inside an async func or proc"),
                    });
                }
            }
            Any::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            }) => {
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
                subject.check(ctx, report_error);
                match property {
                    Property::Expression(expr) => expr.check(ctx, report_error),
                    Property::PlainIdentifier(ident) => ident.check(ctx, report_error),
                };

                let subject_type = subject.infer_type(ctx.into());
                let (property_type, property_slice) = match property {
                    Property::Expression(expr) => {
                        (expr.infer_type(ctx.into()), expr.slice().clone())
                    }
                    Property::PlainIdentifier(ident) => (
                        Type::StringType(Some(ident.downcast().0.clone())),
                        ident.slice().clone(),
                    ),
                };

                // TODO: detect unnecessary optional
                // TODO: detect valid optional

                // if subject_type.indexed(&property_type).is_none() {
                //     report_error(BagelError::MiscError {
                //         module_id: ctx.current_module.module_id.clone(),
                //         src: property_slice,
                //         message: format!(
                //             "{} cannot be used to index type {}",
                //             blue_string(&property_type),
                //             blue_string(&subject_type)
                //         ),
                //     })
                // }
            }
            Any::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            }) => {
                cases.check(ctx, report_error);
                default_case.check(ctx, report_error);
            }
            Any::IfElseExpressionCase(IfElseExpressionCase { condition, outcome }) => {
                condition.check(ctx, report_error);
                outcome.check(ctx, report_error);

                check_subsumation(
                    &truthiness_safe_types(),
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    report_error,
                );
            }
            Any::SwitchExpression(SwitchExpression {
                value,
                cases,
                default_case,
            }) => {
                value.check(ctx, report_error);
                cases.check(ctx, report_error);
                default_case.check(ctx, report_error);

                let value_type = value.infer_type(ctx.into());
                for case in cases {
                    let case = case.downcast();

                    check_subsumation(
                        &value_type,
                        case.type_filter.resolve_type(ctx.into()),
                        case.type_filter.slice(),
                        report_error,
                    );
                }
            }
            Any::SwitchExpressionCase(SwitchExpressionCase {
                type_filter,
                outcome,
            }) => {
                type_filter.check(ctx, report_error);
                outcome.check(ctx, report_error);
            }
            Any::ElementTag(ElementTag {
                tag_name,
                attributes,
                children,
            }) => todo!(),
            Any::AsCast(AsCast { inner, as_type }) => {
                inner.check(ctx, report_error);
                as_type.check(ctx, report_error);

                check_subsumation(
                    &as_type.resolve_type(ctx.into()),
                    inner.infer_type(ctx.into()),
                    inner.slice(),
                    report_error,
                );
            }
            Any::InstanceOf(InstanceOf {
                inner,
                possible_type,
            }) => {
                inner.check(ctx, report_error);
                possible_type.check(ctx, report_error);

                check_subsumation(
                    &inner.infer_type(ctx.into()),
                    possible_type.resolve_type(ctx.into()),
                    inner.slice(),
                    report_error,
                );
            }
            Any::UnionType(UnionType(members)) => {
                members.check(ctx, report_error);
            }
            Any::MaybeType(MaybeType(inner)) => inner.check(ctx, report_error),
            Any::GenericParamType(GenericParamType { name, extends }) => todo!(),
            Any::ProcType(ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            }) => {
                args.check(ctx, report_error);
                args_spread.check(ctx, report_error);
                throws.check(ctx, report_error);
            }
            Any::FuncType(FuncType {
                args,
                args_spread,
                is_pure,
                is_async,
                returns,
            }) => {
                args.check(ctx, report_error);
                args_spread.check(ctx, report_error);
                returns.check(ctx, report_error);
            }
            Any::Arg(Arg {
                name,
                type_annotation,
                optional,
            }) => {
                name.check(ctx, report_error);
                // TODO: Check that no optionsl args come before non-optional args
                type_annotation.check(ctx, report_error);
            }
            Any::GenericType(GenericType { type_params, inner }) => todo!(),
            Any::TypeParam(TypeParam { name, extends }) => todo!(),
            Any::BoundGenericType(BoundGenericType { type_args, generic }) => todo!(),
            Any::ObjectType(ObjectType {
                entries,
                is_interface: _,
            }) => {
                entries.check(ctx, report_error);

                // TODO: Check that each spread can be spread into this type
            }
            Any::RecordType(RecordType {
                key_type,
                value_type,
            }) => {
                key_type.check(ctx, report_error);
                value_type.check(ctx, report_error);

                check_subsumation(
                    &Type::ANY_NUMBER.union(Type::ANY_STRING),
                    key_type.resolve_type(ctx.into()),
                    key_type.slice(),
                    report_error,
                );
            }
            Any::ArrayType(ArrayType(element)) => element.check(ctx, report_error),
            Any::TupleType(TupleType(members)) => members.check(ctx, report_error),
            Any::SpecialType(SpecialType { kind: _, inner }) => {
                inner.check(ctx, report_error);
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
                        _ => report_error(BagelError::MiscError {
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
                        _ => report_error(BagelError::MiscError {
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
                        _ => report_error(BagelError::MiscError {
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
            Any::TypeofType(TypeofType(expr)) => expr.check(ctx, report_error),
            Any::PropertyType(PropertyType {
                subject,
                property,
                optional,
            }) => todo!(),
            Any::MaybeType(MaybeType(inner)) => inner.check(ctx, report_error),
            Any::UnionType(UnionType(members)) => members.check(ctx, report_error),
            Any::IfElseStatement(IfElseStatement {
                cases,
                default_case,
            }) => {
                cases.check(ctx, report_error);
                default_case.check(ctx, report_error);
            }
            Any::IfElseStatementCase(IfElseStatementCase { condition, outcome }) => {
                condition.check(ctx, report_error);
                outcome.check(ctx, report_error);

                check_subsumation(
                    &truthiness_safe_types(),
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    report_error,
                );
            }
            Any::ForLoop(ForLoop {
                item_identifier,
                iterator,
                body,
            }) => {
                item_identifier.check(ctx, report_error);
                iterator.check(ctx, report_error);
                body.check(ctx, report_error);

                check_subsumation(
                    &any_iterator(),
                    iterator.infer_type(ctx.into()),
                    iterator.slice(),
                    report_error,
                );
            }
            Any::WhileLoop(WhileLoop { condition, body }) => {
                condition.check(ctx, report_error);
                body.check(ctx, report_error);

                check_subsumation(
                    &truthiness_safe_types(),
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    report_error,
                );
            }
            Any::Assignment(Assignment {
                target,
                value,
                operator,
            }) => {
                target.check(ctx, report_error);
                value.check(ctx, report_error);
                operator.check(ctx, report_error);

                let (invalid_target, reason) = match target.details() {
                    Any::LocalIdentifier(ident) => {
                        let resolved = target.resolve_symbol(ident.0.as_str());

                        if let Some(resolved) = resolved {
                            match resolved.details() {
                                Any::ValueDeclaration(ValueDeclaration {
                                    destination: _,
                                    value: _,
                                    is_const,
                                    exported: _,
                                    platforms: _,
                                }) => {
                                    if *is_const {
                                        (true, Some("it's a constant"))
                                    } else {
                                        (false, None)
                                    }
                                }
                                Any::Arg(_) => (true, Some("it's an argument")),
                                _ => (true, None),
                            }
                        } else {
                            (false, None) // invalid, but skip reporting error
                        }
                    }
                    Any::PropertyAccessor(_) => {
                        // TODO: Check that subject is mutable
                        // TODO: Can't be an optional property access
                        todo!()
                    }
                    _ => (true, None),
                };

                if invalid_target {
                    report_error(BagelError::MiscError {
                        module_id: module_id.clone(),
                        src: target.slice().clone(),
                        message: match reason {
                            Some(reason) => format!(
                                "Can't assign to {} because {}",
                                blue_string(target),
                                reason
                            ),
                            None => format!("Can't assign to {}", blue_string(target)),
                        },
                    });
                } else {
                    match operator.as_ref() {
                        Some(op) => {
                            let operation = BinaryOperation {
                                left: target.clone(),
                                op: op.clone(),
                                right: value.clone(),
                            };

                            check_binary_operation(&operation, report_error);

                            check_subsumation(
                                &target.infer_type(ctx.into()),
                                binary_operation_type(ctx.into(), &operation),
                                value.slice(),
                                report_error,
                            );
                        }
                        None => {
                            check_subsumation(
                                &target.infer_type(ctx.into()),
                                value.infer_type(ctx.into()),
                                value.slice(),
                                report_error,
                            );
                        }
                    }
                }
            }
            Any::TryCatch(TryCatch {
                try_block,
                error_identifier,
                catch_block,
            }) => todo!(),
            Any::ThrowStatement(ThrowStatement { error_expression }) => {
                error_expression.check(ctx, report_error);

                check_subsumation(
                    &any_error(),
                    error_expression.infer_type(ctx.into()),
                    error_expression.slice(),
                    report_error,
                );
            }
            Any::Autorun(Autorun {
                effect_block,
                until,
            }) => {
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
            Any::ErrorExpression(ErrorExpression(inner)) => inner.check(ctx, report_error),

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
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        if let Some(sel) = self {
            sel.check(ctx, report_error);
        }
    }
}

impl<T> Checkable for Vec<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        for el in self.iter() {
            el.check(ctx, report_error);
        }
    }
}

impl<T> Checkable for KeyValueOrSpread<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        match self {
            KeyValueOrSpread::KeyValue(key, value) => {
                key.check(ctx, report_error);
                value.check(ctx, report_error);
            }
            KeyValueOrSpread::Spread(spread) => {
                spread.check(ctx, report_error);
            }
        }
    }
}

impl<T> Checkable for ElementOrSpread<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        match self {
            ElementOrSpread::Element(element) => {
                element.check(ctx, report_error);
            }
            ElementOrSpread::Spread(spread) => {
                spread.check(ctx, report_error);
            }
        }
    }
}

impl Checkable for DeclarationDestination {
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        match self {
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
                destructure_kind: _,
            }) => {
                properties.check(ctx, report_error);
                spread.check(ctx, report_error);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
    pub nearest_func_or_proc: Option<FuncOrProc>,
}

impl<'a> CheckContext<'a> {
    pub fn in_func(&self, func: AST<Func>) -> CheckContext<'a> {
        CheckContext {
            modules: self.modules,
            current_module: self.current_module,
            nearest_func_or_proc: Some(FuncOrProc::Func(func)),
        }
    }

    pub fn in_proc(&self, proc: AST<Proc>) -> CheckContext<'a> {
        CheckContext {
            modules: self.modules,
            current_module: self.current_module,
            nearest_func_or_proc: Some(FuncOrProc::Proc(proc)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FuncOrProc {
    Func(AST<Func>),
    Proc(AST<Proc>),
}

impl FuncOrProc {
    pub fn is_async(&self) -> bool {
        match self {
            FuncOrProc::Func(func) => func.downcast().is_async,
            FuncOrProc::Proc(proc) => proc.downcast().is_async,
        }
    }

    pub fn is_pure(&self) -> bool {
        match self {
            FuncOrProc::Func(func) => func.downcast().is_pure,
            FuncOrProc::Proc(proc) => proc.downcast().is_pure,
        }
    }
}
