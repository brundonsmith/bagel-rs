use crate::{
    cli::ModulesStore,
    model::{ast::*, ParsedModule},
};

#[derive(Clone, Copy, Debug)]
pub struct ResolveSymbolContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a ParsedModule,
    pub follow_imports: bool,
}

impl<'a> ResolveSymbolContext<'a> {
    pub fn follow_imports(self, follow_imports: bool) -> Self {
        Self {
            modules: self.modules,
            current_module: self.current_module,
            follow_imports,
        }
    }
}

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    pub fn resolve_symbol<'a>(
        &self,
        ctx: ResolveSymbolContext<'a>,
        symbol: &str,
    ) -> Option<ASTAny> {
        match self.details() {
            Any::Module(Module {
                module_id: _,
                declarations,
            }) => {
                let self_index = if self.try_downcast::<ValueDeclaration>().is_some() {
                    declarations.iter().enumerate().find_map(|(index, decl)| {
                        if self.ptr_eq::<Declaration>(decl) {
                            Some(index)
                        } else {
                            None
                        }
                    })
                } else {
                    None
                };

                if let Some(found) = declarations.iter().enumerate().find_map(|(index, decl)| {
                    if let Some(self_index) = self_index {
                        if index >= self_index {
                            return None;
                        }
                    }

                    match decl.downcast() {
                        Declaration::ImportAllDeclaration(ImportAllDeclaration {
                            platforms,
                            name,
                            path,
                        }) => {
                            if name.downcast().0 == symbol {
                                if !ctx.follow_imports {
                                    return Some(decl.clone().upcast());
                                } else {
                                    return ctx
                                        .current_module
                                        .module_id()
                                        .imported(&path.downcast().value.as_str())
                                        .map(|other_module_id| {
                                            ctx.modules
                                                .get(&other_module_id)
                                                .map(|res| res.as_ref().ok())
                                        })
                                        .flatten()
                                        .flatten()
                                        .map(|other_module| match other_module {
                                            ParsedModule::Bagel {
                                                module_id: _,
                                                ast: _,
                                            } => None,
                                            ParsedModule::JavaScript { module_id: _ } => None,
                                            ParsedModule::Singleton {
                                                module_id: _,
                                                contents,
                                            } => Some(contents.clone().upcast()),
                                        })
                                        .flatten();
                                }
                            }
                        }
                        Declaration::ImportDeclaration(ImportDeclaration {
                            platforms,
                            imports,
                            path,
                        }) => {
                            if imports.iter().any(|item| {
                                let item = item.downcast();

                                match item.alias {
                                    Some(alias) => alias.downcast().0 == symbol,
                                    None => item.name.downcast().0 == symbol,
                                }
                            }) {
                                if !ctx.follow_imports {
                                    return Some(decl.clone().upcast());
                                } else {
                                    return ctx
                                        .current_module
                                        .module_id()
                                        .imported(&path.downcast().value.as_str())
                                        .map(|other_module_id| {
                                            ctx.modules
                                                .get(&other_module_id)
                                                .map(|res| res.as_ref().ok())
                                        })
                                        .flatten()
                                        .flatten()
                                        .map(|other_module| match other_module {
                                            ParsedModule::Bagel {
                                                module_id: _,
                                                ast: _,
                                            } => other_module
                                                .get_declaration(symbol, true)
                                                .map(|decl| decl.upcast()),
                                            ParsedModule::JavaScript { module_id: _ } => None,
                                            ParsedModule::Singleton {
                                                module_id: _,
                                                contents: _,
                                            } => None,
                                        })
                                        .flatten();
                                }
                            }
                        }
                        Declaration::TypeDeclaration(TypeDeclaration {
                            name,
                            declared_type: _,
                            exported: _,
                        }) => {
                            if name.downcast().0 == symbol {
                                return Some(decl.clone().upcast());
                            }
                        }
                        Declaration::FuncDeclaration(FuncDeclaration {
                            name,
                            func: _,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        }) => {
                            if name.downcast().0 == symbol {
                                return Some(decl.clone().upcast());
                            }
                        }
                        Declaration::ProcDeclaration(ProcDeclaration {
                            name,
                            proc: _,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        }) => {
                            if name.downcast().0 == symbol {
                                return Some(decl.clone().upcast());
                            }
                        }
                        Declaration::ValueDeclaration(ValueDeclaration {
                            destination,
                            value: _,
                            is_const: _,
                            exported: _,
                            platforms: _,
                        }) => {
                            if match destination {
                                DeclarationDestination::NameAndType(NameAndType {
                                    name,
                                    type_annotation: _,
                                }) => name.downcast().0 == symbol,
                                DeclarationDestination::Destructure(Destructure {
                                    properties,
                                    spread,
                                    destructure_kind,
                                }) => todo!(),
                            } {
                                return Some(decl.clone().upcast());
                            }
                        }
                        Declaration::SymbolDeclaration(SymbolDeclaration { name, exported: _ }) => {
                            if name.downcast().0 == symbol {
                                return Some(decl.clone().upcast());
                            }
                        }
                        Declaration::TestExprDeclaration(_) => {}
                        Declaration::TestBlockDeclaration(_) => {}
                        Declaration::TestTypeDeclaration(_) => {}
                    }

                    None
                }) {
                    return Some(found);
                }
            }
            Any::Func(Func {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            }) => {
                let func_type = type_annotation.downcast();

                if let Some(found) = func_type
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0 == symbol)
                {
                    return Some(found.clone().upcast());
                }

                if let Some(spread) = func_type.args_spread {
                    if spread.downcast().name.downcast().0 == symbol {
                        return Some(spread.clone().upcast());
                    }
                }

                if let Some(found) = func_type
                    .type_params
                    .iter()
                    .find(|param| param.downcast().name.downcast().0 == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Any::Proc(Proc {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            }) => {
                let proc_type = type_annotation.downcast();

                if let Some(found) = proc_type
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0 == symbol)
                {
                    return Some(found.clone().upcast());
                }

                if let Some(spread) = proc_type.args_spread {
                    if spread.downcast().name.downcast().0 == symbol {
                        return Some(spread.clone().upcast());
                    }
                }

                if let Some(found) = proc_type
                    .type_params
                    .iter()
                    .find(|param| param.downcast().name.downcast().0 == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner: _,
            }) => {
                if let Some(found) =
                    declarations
                        .iter()
                        .find(|arg| match arg.downcast().destination {
                            DeclarationDestination::NameAndType(NameAndType {
                                name,
                                type_annotation: _,
                            }) => name.downcast().0 == symbol,
                            DeclarationDestination::Destructure(Destructure {
                                properties,
                                spread,
                                destructure_kind: _,
                            }) => properties.iter().any(|property| {
                                property.downcast().0 == symbol
                                    || spread
                                        .as_ref()
                                        .map(|spread| spread.downcast().0 == symbol)
                                        .unwrap_or(false)
                            }),
                        })
                {
                    return Some(found.clone().upcast());
                }
            }
            Any::Block(Block(statements)) => {
                if let Some(found) = statements
                    .iter()
                    .skip_while(|stmt| !self.ptr_eq::<Statement>(*stmt))
                    .find(|stmt| match stmt.details() {
                        Any::ValueDeclaration(ValueDeclaration {
                            destination,
                            value: _,
                            is_const: _,
                            exported: _,
                            platforms: _,
                        }) => match destination {
                            DeclarationDestination::NameAndType(NameAndType {
                                name,
                                type_annotation,
                            }) => name.downcast().0 == symbol,
                            DeclarationDestination::Destructure(_) => todo!(),
                        },
                        _ => false,
                    })
                {
                    return Some(found.clone().upcast());
                }
            }
            Any::ForLoop(ForLoop {
                item_identifier,
                iterable: _,
                body: _,
            }) => {
                if item_identifier.downcast().0 == symbol {
                    return self.parent();
                }
            }
            Any::TryCatch(TryCatch {
                try_block: _,
                error_identifier,
                catch_block,
            }) => {
                if error_identifier.downcast().0 == symbol
                    && self
                        .clone()
                        .upcast()
                        .ptr_eq::<Any>(&catch_block.clone().upcast())
                {
                    return self.parent();
                }
            }
            Any::FuncType(FuncType {
                type_params,
                args: _,
                args_spread: _,
                is_pure: _,
                is_async: _,
                returns: _,
            }) => {
                if let Some(found) = type_params
                    .iter()
                    .find(|param| param.downcast().name.downcast().0 == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Any::ProcType(ProcType {
                type_params,
                args: _,
                args_spread: _,
                is_pure: _,
                is_async: _,
                throws: _,
            }) => {
                if let Some(found) = type_params
                    .iter()
                    .find(|param| param.downcast().name.downcast().0 == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Any::GenericType(GenericType { type_params, inner }) => {
                if let Some(found) = type_params
                    .iter()
                    .find(|param| param.downcast().name.downcast().0 == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            _ => {}
        };

        self.parent()
            .map(|parent| parent.resolve_symbol(ctx, symbol))
            .flatten()
    }
}
