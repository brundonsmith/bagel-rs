use std::fmt::Debug;
use std::time::SystemTime;

use colored::Color;

#[allow(unused_imports)]
use crate::utils::Loggable;
use crate::{
    cli::ModulesStore,
    model::{
        any_array, any_callable, any_error, any_function, any_iterable, any_object, any_plan,
        any_procedure, ast::*, blue_string, number_or_string, string_template_safe_types,
        BagelError, ModuleType, Mutability, ParsedModule, Slice, SubsumationContext, Type,
    },
    passes::{binary_operation_type, INT},
    utils::cli_label,
    DEBUG_MODE,
};

#[derive(Debug, Clone)]
pub struct CheckContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a ParsedModule,
    pub nearest_func_or_proc: Option<FuncOrProc>,
}

impl ParsedModule {
    pub fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        let start = SystemTime::now();

        match self {
            ParsedModule::Bagel { module_id: _, ast } => ast.check(ctx, report_error),
            ParsedModule::JavaScript { module_id } => {}
            ParsedModule::Singleton {
                module_id: _,
                contents,
            } => contents.check(ctx, report_error),
        }

        if DEBUG_MODE {
            println!(
                "{} Checking {} took {}ms",
                cli_label("Debug", Color::Blue),
                self.module_id(),
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
        let module_id = &ctx.current_module.module_id().clone();
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

        let check_binary_operation = |BinaryOperation { left, op, right }: &BinaryOperation,
                                      report_error: &mut F| {
            left.check(ctx, report_error);
            op.check(ctx, report_error);
            right.check(ctx, report_error);

            let module_id = &ctx.current_module.module_id().clone();
            let subsumation_context = SubsumationContext::from(ctx);

            let left_type = left.infer_type(ctx.into());
            let right_type = right.infer_type(ctx.into());

            let number_or_string = number_or_string();

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
                        module_id: ctx.current_module.module_id().clone(),
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
            Any::Module(Module {
                module_id: _,
                declarations,
            }) => {
                declarations.check(ctx, report_error);

                // todo!("Check for name duplicates");
            }
            Any::ImportAllDeclaration(ImportAllDeclaration {
                platforms,
                name,
                path,
            }) => {
                name.check(ctx, report_error);
                path.check(ctx, report_error);

                let path_name = path.downcast();
                let path_name = path_name.value.as_str();

                let other_module_type =
                    module_id.imported(path_name).as_ref().map(ModuleType::from);

                match other_module_type {
                    Some(ModuleType::Bagel) => {
                        report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: path.slice().clone(),
                            message: format!(
                                "Bagel modules don't have default-exports and can't be imported this way"
                            ),
                        });
                    }
                    None => {
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
                    _ => {}
                }
            }
            Any::ImportDeclaration(ImportDeclaration {
                platforms,
                imports,
                path,
            }) => {
                imports.check(ctx, report_error);
                path.check(ctx, report_error);

                let path_name = path.downcast();
                let path_name = path_name.value.as_str();
                // let path_extension = path_name.split('.').last();
                // let is_js_file = path_extension
                //     .map(|ext| JS_FILE_EXTENSIONS.contains(&ext))
                //     .unwrap_or(false);

                // match platforms {
                //     Some(_) => {
                //         if !is_js_file {
                //             report_error(BagelError::MiscError {
                //                 module_id: module_id.clone(),
                //                 src: self.slice().clone(),
                //                 message: format!(
                //                     "Can only specify valid platforms for an imported module if it's a JavaScript file"
                //                 ),
                //             });
                //         }
                //     }
                //     None => {
                //         if is_js_file {
                //             report_error(BagelError::MiscError {
                //                 module_id: module_id.clone(),
                //                 src: self.slice().clone(),
                //                 message: format!(
                //                     "Imports of JavaScript files must specify which platforms they can be used on, eg. [node, browser]"
                //                 ),
                //             });
                //         } else {

                let other_module_id = module_id.imported(path_name);

                match other_module_id {
                    Some(other_module_id) => match ModuleType::from(&other_module_id) {
                        ModuleType::Bagel => {
                            let other_module =
                                ctx.modules.get(&other_module_id).unwrap().as_ref().unwrap();

                            for item in imports {
                                let item_downcast = item.downcast();
                                let item_name = item_downcast.name.downcast();
                                let item_name = item_name.0.as_str();

                                let decl = other_module.get_declaration(item_name, true);

                                if decl.is_none() {
                                    report_error(BagelError::MiscError {
                                        module_id: module_id.clone(),
                                        src: item.slice().clone(),
                                        message: format!(
                                            "No exported member named {} found in module {}",
                                            blue_string(item_name),
                                            blue_string(module_id)
                                        ),
                                    })
                                }
                            }
                        }
                        ModuleType::JavaScript => {}
                        _ => {
                            report_error(BagelError::MiscError {
                                module_id: module_id.clone(),
                                src: path.slice().clone(),
                                message: format!(
                                    "This kind of module doesn't expose named imports; import it as a single value instead: {}",
                                    blue_string(format!("import '{}' as myModule", path.downcast().value.as_str()))
                                ),
                            });
                        }
                    },
                    None => report_error(BagelError::MiscError {
                        module_id: module_id.clone(),
                        src: path.slice().clone(),
                        message: format!(
                            "Couldn't find module {} from module {}",
                            blue_string(path_name),
                            blue_string(module_id)
                        ),
                    }),
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
                value.check(ctx, report_error);

                check_declaration_destination(
                    ctx,
                    report_error,
                    self.slice().clone(),
                    destination,
                    value.clone(),
                    *is_const,
                );
            }
            Any::TestExprDeclaration(TestExprDeclaration {
                platforms,
                name,
                expr,
            }) => {
                platforms.check(ctx, report_error);
                name.check(ctx, report_error);
                expr.check(ctx, report_error);

                check_subsumation(
                    &expr.infer_type(ctx.into()),
                    any_error(),
                    expr.slice(),
                    report_error,
                );
            }
            Any::TestBlockDeclaration(TestBlockDeclaration {
                platforms,
                name,
                block,
            }) => todo!(),
            Any::TestTypeDeclaration(TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            }) => todo!(),
            Any::DeclarationPlatforms(DeclarationPlatforms { platforms }) => {
                for platform in platforms {
                    if !VALID_PLATFORMS.contains(&platform.downcast().0.as_str()) {
                        report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: platform.slice().clone(),
                            message: format!(
                                "{} is not one of the valid platforms ({})",
                                blue_string(platform.downcast().0.as_str()),
                                VALID_PLATFORMS.join(", ")
                            ),
                        });
                    }
                }
            }
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
                        KeyValueOrSpread::KeyValue(_, _, _) => {}
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
                    &Type::ANY_BOOLEAN,
                    inner.infer_type(ctx.into()),
                    inner.slice(),
                    report_error,
                );
            }
            Any::Parenthesis(Parenthesis(inner)) => inner.check(ctx, report_error),
            Any::LocalIdentifier(LocalIdentifier(name)) => {
                // TODO: Make sure it isn't a type

                if name.as_str().starts_with(INT) {
                    if !ctx.current_module.module_id().is_std_lib() {
                        // Not allowed to start with ___
                        report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: self.slice().clone(),
                            message: format!(
                                "{} isn't a valid identifier; identifiers can't start with {}",
                                blue_string(name.as_str()),
                                blue_string(INT),
                            ),
                        });
                    } else {
                        // do nothing; don't check this identifier
                    }
                } else if name != JS_GLOBAL_IDENTIFIER {
                    match self.resolve_symbol(ctx.into(), name.as_str()) {
                        None => {
                            // Identifier can't be resolved
                            report_error(BagelError::MiscError {
                                module_id: module_id.clone(),
                                src: self.slice().clone(),
                                message: format!(
                                    "Couldn't resolve identifier {} in this scope",
                                    blue_string(name.as_str())
                                ),
                            });
                        }
                        Some(resolved) => {
                            if let Some(nearest_func_or_proc) = &ctx.nearest_func_or_proc {
                                // Violation of pure func/proc boundary
                                let purity_failure = match nearest_func_or_proc {
                                    FuncOrProc::Func(func) => {
                                        if func.downcast().is_pure && !func.contains(&resolved) {
                                            Some("function")
                                        } else {
                                            None
                                        }
                                    }
                                    FuncOrProc::Proc(proc) => {
                                        if proc.downcast().is_pure && !proc.contains(&resolved) {
                                            Some("procedure")
                                        } else {
                                            None
                                        }
                                    }
                                };

                                if let Some(failure) = purity_failure {
                                    report_error(BagelError::MiscError {
                                    module_id: module_id.clone(),
                                    src: self.slice().clone(),
                                    message: format!(
                                        "Pure {}s can only reference identifiers declared within their own scope!",
                                        failure,
                                    ),
                                });
                                }

                                // Violation of platform-specific constraints
                                // if let Some(decl) = resolved.find_parent_of_type::<ImportDeclaration>()
                                // {
                                //     if let Some(import_platforms) = decl.downcast().platforms {
                                //         let allowed_platforms = match self
                                //             .clone()
                                //             .upcast()
                                //             .find_parent(|p| {
                                //                 p.try_downcast::<FuncDeclaration>().is_some()
                                //                     || p.try_downcast::<ProcDeclaration>().is_some()
                                //             })
                                //             .map(|p| p.downcast())
                                //         {
                                //             Some(Any::FuncDeclaration(FuncDeclaration {
                                //                 platforms,
                                //                 name: _,
                                //                 func: _,
                                //                 exported: _,
                                //                 decorators: _,
                                //             })) => platforms,
                                //             Some(Any::ProcDeclaration(ProcDeclaration {
                                //                 platforms,
                                //                 name: _,
                                //                 proc: _,
                                //                 exported: _,
                                //                 decorators: _,
                                //             })) => platforms,
                                //             Some(Any::ValueDeclaration(ValueDeclaration {
                                //                 platforms,
                                //                 value: _,
                                //                 exported: _,
                                //                 destination: _,
                                //                 is_const: _,
                                //             })) => platforms,
                                //             _ => None,
                                //         };

                                //         if let Some(allowed_platforms) = allowed_platforms {
                                //             let import_platforms: PlatformSet =
                                //                 (&import_platforms).try_into().unwrap();
                                //             let allowed_platforms: PlatformSet =
                                //                 (&allowed_platforms).try_into().unwrap();

                                //             // platform mismatch
                                //             if !import_platforms.subset_of(allowed_platforms) {
                                //                 report_error(
                                //                     BagelError::MiscError {
                                //                         module_id: module_id.clone(),
                                //                         src: self.slice().clone(),
                                //                         message: format!(
                                //                             "Identifier {} is tagged as being specific to platforms {}, so it can't be referenced from within this declaration, which is tagged for platforms {}",
                                //                             blue_string(name.as_str()),
                                //                             blue_string(import_platforms),
                                //                             blue_string(allowed_platforms)
                                //                         ),
                                //                     }
                                //                 );
                                //             }
                                //         } else {
                                //             // can only reference platform-specific imports from platform-specific declarations
                                //             report_error(BagelError::MiscError {
                                //                 module_id: module_id.clone(),
                                //                 src: self.slice().clone(),
                                //                 message: format!(
                                //                     "Identifier {} was imported from a JavaScript file, so it can only be referenced from within platform-specific Bagel declarations",
                                //                     blue_string(name.as_str())
                                //                 ),
                                //             });
                                //         }
                                //     }
                                // }

                                // if we access a platform-specific import or a JS global from outside of a platform-specific context
                            }
                        }
                    }
                }
            }
            Any::NamedType(NamedType(name)) => {
                // TODO: Make sure it isn't an expression
                let name = name.downcast();
                let name_str = name.0.as_str();
                if name.0.as_str() != JS_GLOBAL_IDENTIFIER
                    && self.resolve_symbol(ctx.into(), name_str).is_none()
                {
                    report_error(BagelError::MiscError {
                        module_id: module_id.clone(),
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
                value.check(ctx, report_error);

                check_declaration_destination(
                    ctx,
                    report_error,
                    self.slice().clone(),
                    destination,
                    value.clone(),
                    true,
                );
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
                        module_id: module_id.clone(),
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
                        module_id: module_id.clone(),
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

                if let Some(inv) = method_call_as_invocation(
                    ctx.into(),
                    self.clone().upcast().try_recast::<Invocation>().unwrap(),
                ) {
                    // if this is a method-style call that can be transformed to a plain invocation, check that instead
                    inv.check(ctx, report_error);
                } else {
                    let subject_type = subject.infer_type(ctx.into());

                    let in_statement_context =
                        self.parent().unwrap().try_recast::<Statement>().is_some();

                    if in_statement_context && any_function().subsumes(ctx.into(), &subject_type) {
                        report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: subject.slice().clone(),
                            message: "Functions can't be called as statements".to_owned(),
                        });
                    } else if !in_statement_context
                        && any_procedure().subsumes(ctx.into(), &subject_type)
                    {
                        report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: subject.slice().clone(),
                            message: "Procedures can't be called in expressions".to_owned(),
                        });
                    } else if !any_callable().subsumes(ctx.into(), &subject_type) {
                        report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: subject.slice().clone(),
                            message: format!(
                                "{} is of type {} and cannot be called",
                                blue_string(&subject.clone().upcast()),
                                blue_string(&subject_type),
                            ),
                        });
                    } else {
                        let args_type = Type::TupleType {
                            mutability: Mutability::Literal,
                            members: args
                                .iter()
                                .map(|arg| arg.infer_type(ctx.into()))
                                .map(ElementOrSpread::Element)
                                .collect(),
                        };

                        check_subsumation(
                            &subject_type.parameters(),
                            args_type,
                            self.slice(),
                            report_error,
                        );
                    }
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

                if subject_type == Type::PoisonedType {
                    return;
                }

                let (property_type, property_slice) = match property {
                    Property::Expression(expr) => {
                        (expr.infer_type(ctx.into()), expr.slice().clone())
                    }
                    Property::PlainIdentifier(ident) => (
                        Type::StringType(Some(ident.downcast().0.clone())),
                        ident.slice().clone(),
                    ),
                };

                if property_type == Type::PoisonedType {
                    return;
                }

                // TODO: detect unnecessary optional
                // TODO: detect valid optional

                let is_method_call = self
                    .parent()
                    .map(|parent| parent.upcast().try_recast::<Expression>())
                    .flatten()
                    .is_some();

                if !is_method_call && !subject_type.property_exists(ctx.into(), &property_type) {
                    // doesn't have the property

                    match property {
                        Property::Expression(_) => report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: property_slice,
                            message: format!(
                                "{} cannot be used to index type {}",
                                blue_string(&property_type),
                                blue_string(&subject_type)
                            ),
                        }),
                        Property::PlainIdentifier(ident) => {
                            let ident_as_local: AST<LocalIdentifier> = ident.clone().into();
                            let ident_type =
                                ident_as_local.recast::<Expression>().infer_type(ctx.into());

                            // if it's not a method call that's re-arrangeable
                            if !matches!(
                                ident_type,
                                Type::FuncType {
                                    args: _,
                                    args_spread: _,
                                    is_pure: _,
                                    returns: _
                                }
                            ) && !matches!(
                                ident_type,
                                Type::ProcType {
                                    args: _,
                                    args_spread: _,
                                    is_pure: _,
                                    is_async: _,
                                    throws: _
                                }
                            ) {
                                report_error(BagelError::MiscError {
                                    module_id: module_id.clone(),
                                    src: property_slice,
                                    message: format!(
                                        "Property {} does not exist on type {}",
                                        blue_string(&ident),
                                        blue_string(&subject_type)
                                    ),
                                })
                            }
                        }
                    }
                }
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
                    &Type::ANY_BOOLEAN,
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
            }) => {
                tag_name.check(ctx, report_error);
                attributes.check(ctx, report_error);
                children.check(ctx, report_error);
            }
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
                    possible_type.slice(),
                    report_error,
                );
            }
            Any::GenericParamType(GenericParamType { name, extends }) => todo!(),
            Any::ProcType(ProcType {
                type_params,
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            }) => {
                type_params.check(ctx, report_error);
                args.check(ctx, report_error);
                args_spread.check(ctx, report_error);
                throws.check(ctx, report_error);
            }
            Any::FuncType(FuncType {
                type_params,
                args,
                args_spread,
                is_pure,
                is_async,
                returns,
            }) => {
                type_params.check(ctx, report_error);
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
            Any::GenericType(GenericType { type_params, inner }) => {
                type_params.check(ctx, report_error);
                inner.check(ctx, report_error);
            }
            Any::TypeParam(TypeParam { name, extends }) => {
                name.check(ctx, report_error);
                extends.check(ctx, report_error);
            }
            Any::BoundGenericType(BoundGenericType { type_args, generic }) => {
                type_args.check(ctx, report_error);
                generic.check(ctx, report_error);

                // TODO: Check that generic type is actually generic, and that type args fit
            }
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
            Any::ModifierType(ModifierType { kind, inner }) => {
                let expected_type = match kind {
                    ModifierTypeKind::Readonly => None,
                    ModifierTypeKind::Keyof => Some(any_object()),
                    ModifierTypeKind::Valueof => Some(any_object()),
                    ModifierTypeKind::Elementof => Some(any_array()),
                };

                if let Some(expected_type) = expected_type {
                    let inner_type = inner.resolve_type(ctx.into());

                    if !expected_type.subsumes(ctx.into(), &inner_type) {
                        let kind_str: &str = kind.into();

                        report_error(BagelError::MiscError {
                            module_id: module_id.clone(),
                            src: self.slice().clone(),
                            message: format!(
                                "Cannot apply {} to {}",
                                blue_string(kind_str),
                                blue_string(&inner_type)
                            ),
                        });
                    }
                }
            }
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
                    &Type::ANY_BOOLEAN,
                    condition.infer_type(ctx.into()),
                    condition.slice(),
                    report_error,
                );
            }
            Any::ForLoop(ForLoop {
                item_identifier,
                iterable,
                body,
            }) => {
                item_identifier.check(ctx, report_error);
                iterable.check(ctx, report_error);
                body.check(ctx, report_error);

                check_subsumation(
                    &any_iterable(),
                    iterable.infer_type(ctx.into()),
                    iterable.slice(),
                    report_error,
                );
            }
            Any::WhileLoop(WhileLoop { condition, body }) => {
                condition.check(ctx, report_error);
                body.check(ctx, report_error);

                check_subsumation(
                    &Type::ANY_BOOLEAN,
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
                        let resolved = target.resolve_symbol(ctx.into(), ident.0.as_str());

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
                    Any::PropertyAccessor(PropertyAccessor {
                        subject,
                        property: _,
                        optional,
                    }) => {
                        if *optional {
                            (
                                true,
                                Some("?. can't be used on the left side of an assignment"),
                            )
                        } else {
                            let subject_type = subject.infer_type(ctx.into());

                            if let Some(mutability) = subject_type.mutability(ctx.into()) {
                                match mutability {
                                    Mutability::Constant => (true, Some("")),
                                    Mutability::Readonly => (true, Some("")),
                                    _ => (false, None),
                                }
                            } else {
                                (false, None)
                            }
                        }
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

                    // TODO: Check that proeprty exists?
                }
            }
            Any::TryCatch(TryCatch {
                try_block,
                error_identifier,
                catch_block,
            }) => {
                try_block.check(ctx, report_error);
                error_identifier.check(ctx, report_error);
                catch_block.check(ctx, report_error);

                if try_block
                    .clone()
                    .recast::<Statement>()
                    .throws(ctx.into())
                    .is_none()
                {
                    report_error(BagelError::MiscError {
                        module_id: module_id.clone(),
                        src: try_block.slice().clone(),
                        message: format!(
                            "Try/catch is redundant; try block doesn't throw anything"
                        ),
                    });
                }
            }
            Any::ThrowStatement(ThrowStatement { error_expression }) => {
                error_expression.check(ctx, report_error);

                let error_type = error_expression.infer_type(ctx.into());

                // check that we're throwing an Error
                check_subsumation(
                    &any_error(),
                    error_type.clone(),
                    error_expression.slice(),
                    report_error,
                );

                if let FuncOrProc::Proc(proc) = ctx.nearest_func_or_proc.as_ref().unwrap() {
                    if let Some(throws) = proc.downcast().type_annotation.downcast().throws {
                        // check that error type matches declared throws-type
                        check_subsumation(
                            &throws.resolve_type(ctx.into()),
                            error_type,
                            error_expression.slice(),
                            report_error,
                        );
                    }
                } else {
                    unreachable!()
                }
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
            Any::SymbolDeclaration(_) => {}

            Any::NilLiteral(_) => {}
            Any::NumberLiteral(_) => {}
            Any::BooleanLiteral(_) => {}
            Any::ExactStringLiteral(_) => {}
            Any::BinaryOperator(_) => {}
            Any::PlainIdentifier(_) => {}
            Any::RegularExpression(_) => {}
            Any::AnyLiteral(_) => {}

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

fn check_declaration_destination<'a, F: FnMut(BagelError)>(
    ctx: &CheckContext<'a>,
    report_error: &mut F,
    self_slice: Slice,
    destination: &DeclarationDestination,
    value: AST<Expression>,
    is_const: bool,
) {
    let module_id = &ctx.current_module.module_id().clone();
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

    match destination {
        DeclarationDestination::NameAndType(NameAndType {
            name,
            type_annotation,
        }) => {
            name.check(ctx, report_error);
            type_annotation.check(ctx, report_error);

            if let Some(type_annotation) = type_annotation {
                let type_annotation = type_annotation.resolve_type(ctx.into());
                let type_annotation = if is_const {
                    type_annotation.with_mutability(Mutability::Constant)
                } else {
                    type_annotation
                };

                check_subsumation(
                    &type_annotation,
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

            if !is_const {
                report_error(BagelError::MiscError {
                    module_id: module_id.clone(),
                    src: self_slice,
                    message: format!("Can only destructure when declaring a const"),
                });
            }

            let value_type = value.infer_type(ctx.into());
            match destructure_kind {
                DestructureKind::Array => {
                    check_subsumation(
                        &any_array(),
                        value_type.clone(),
                        value.slice(),
                        report_error,
                    );

                    for (index, property) in properties.iter().enumerate() {
                        if !value_type.property_exists(ctx.into(), &index.into()) {
                            report_error(BagelError::MiscError {
                                module_id: module_id.clone(),
                                src: property.slice().clone(),
                                message: format!(
                                    "There is no element {} on type {}",
                                    blue_string(index),
                                    blue_string(&value_type)
                                ),
                            });
                        }
                    }
                }
                DestructureKind::Object => {
                    check_subsumation(
                        &any_object(),
                        value_type.clone(),
                        value.slice(),
                        report_error,
                    );

                    for property in properties {
                        let name = property.downcast().0.clone();

                        if !value_type.property_exists(ctx.into(), &name.clone().into()) {
                            report_error(BagelError::MiscError {
                                module_id: module_id.clone(),
                                src: property.slice().clone(),
                                message: format!(
                                    "Property {} does not exist on type {}",
                                    blue_string(name.as_str()),
                                    blue_string(&value_type)
                                ),
                            });
                        }
                    }
                }
            }
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
            KeyValueOrSpread::KeyValue(key, value, _) => {
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

impl<T, U> Checkable for (T, U)
where
    T: Checkable,
    U: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        self.0.check(ctx, report_error);
        self.1.check(ctx, report_error);
    }
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
