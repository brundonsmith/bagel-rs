use crate::model::ast::{self, *};

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    pub fn resolve_symbol(&self, symbol: &str) -> Option<ASTAny> {
        match self.parent().as_ref().map(|p| p.details()) {
            Some(Any::Module(ast::Module {
                module_id: _,
                declarations,
            })) => {
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
                            path: _,
                        }) => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        Declaration::ImportDeclaration(ImportDeclaration {
                            platforms,
                            imports,
                            path: _,
                        }) => {
                            if imports.iter().any(|item| {
                                let item = item.downcast();

                                match item.alias {
                                    Some(alias) => alias.downcast().0.as_str() == symbol,
                                    None => item.name.downcast().0.as_str() == symbol,
                                }
                            }) {
                                return Some(decl.clone());
                            }
                        }
                        Declaration::TypeDeclaration(TypeDeclaration {
                            name,
                            declared_type: _,
                            exported: _,
                        }) => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        Declaration::FuncDeclaration(FuncDeclaration {
                            name,
                            func: _,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        }) => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        Declaration::ProcDeclaration(ProcDeclaration {
                            name,
                            proc: _,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        }) => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
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
                                    type_annotation,
                                }) => name.downcast().0.as_str() == symbol,
                                DeclarationDestination::Destructure(_) => todo!(),
                            } {
                                return Some(decl.clone());
                            }
                        }
                        Declaration::SymbolDeclaration(SymbolDeclaration { name, exported: _ }) => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        Declaration::TestExprDeclaration(_) => {}
                        Declaration::TestBlockDeclaration(_) => {}
                        Declaration::TestTypeDeclaration(_) => {}
                    }

                    None
                }) {
                    return Some(found.upcast());
                }
            }
            Some(Any::Func(Func {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            })) => {
                let func_type = type_annotation.downcast();

                if let Some(found) = func_type
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }

                if let Some(spread) = func_type.args_spread {
                    if spread.downcast().name.downcast().0.as_str() == symbol {
                        return Some(spread.clone().upcast());
                    }
                }
            }
            Some(Any::Proc(Proc {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            })) => {
                let proc_type = type_annotation.downcast();

                if let Some(found) = proc_type
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }

                if let Some(spread) = proc_type.args_spread {
                    if spread.downcast().name.downcast().0.as_str() == symbol {
                        return Some(spread.clone().upcast());
                    }
                }
            }
            Some(Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner: _,
            })) => {
                if let Some(found) =
                    declarations
                        .iter()
                        .find(|arg| match arg.downcast().destination {
                            DeclarationDestination::NameAndType(NameAndType {
                                name,
                                type_annotation: _,
                            }) => name.downcast().0.as_str() == symbol,
                            DeclarationDestination::Destructure(Destructure {
                                properties,
                                spread,
                                destructure_kind: _,
                            }) => properties.iter().any(|property| {
                                property.downcast().0.as_str() == symbol
                                    || spread
                                        .as_ref()
                                        .map(|spread| spread.downcast().0.as_str() == symbol)
                                        .unwrap_or(false)
                            }),
                        })
                {
                    return Some(found.clone().upcast());
                }
            }
            Some(Any::Block(Block(statements))) => {
                let self_index = statements
                    .iter()
                    .enumerate()
                    .find(|(_, stmt)| self.ptr_eq::<Statement>(*stmt))
                    .map(|(index, _)| index)
                    .unwrap();

                if let Some(found) =
                    statements
                        .iter()
                        .take(self_index)
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
                                }) => name.downcast().0.as_str() == symbol,
                                DeclarationDestination::Destructure(_) => todo!(),
                            },
                            _ => false,
                        })
                {
                    return Some(found.clone().upcast());
                }
            }
            Some(Any::ForLoop(ForLoop {
                item_identifier,
                iterator: _,
                body: _,
            })) => {
                if item_identifier.downcast().0.as_str() == symbol {
                    return self.parent();
                }
            }
            Some(Any::TryCatch(TryCatch {
                try_block: _,
                error_identifier,
                catch_block,
            })) => {
                if error_identifier.downcast().0.as_str() == symbol
                    && self
                        .clone()
                        .upcast()
                        .ptr_eq::<Any>(&catch_block.clone().upcast())
                {
                    return self.parent();
                }
            }
            _ => {}
        };

        self.parent()
            .map(|parent| parent.resolve_symbol(symbol))
            .flatten()
    }
}
