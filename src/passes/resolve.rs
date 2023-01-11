use crate::model::ast::{self, *};

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    pub fn resolve_symbol(&self, symbol: &str) -> Option<ASTAny> {
        match self.parent().as_ref().map(|p| p.details()) {
            Some(Any::Module(ast::Module { declarations })) => {
                if let Some(found) = declarations.iter().find_map(|decl| {
                    match decl.downcast() {
                        Declaration::ImportAllDeclaration(ImportAllDeclaration {
                            name,
                            path: _,
                        }) => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        Declaration::ImportDeclaration(ImportDeclaration { imports, path: _ }) => {
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
                            name,
                            type_annotation: _,
                            value: _,
                            is_const: _,
                            exported: _,
                            platforms: _,
                        }) => {
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
                if let Some(found) = type_annotation
                    .downcast()
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Some(Any::Proc(Proc {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            })) => {
                if let Some(found) = type_annotation
                    .downcast()
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Some(Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner,
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
                let self_upcast = self.clone().upcast();
                let self_index = statements
                    .iter()
                    .enumerate()
                    .find(|(_, stmt)| (*stmt).clone().upcast() == self_upcast)
                    .map(|(index, _)| index)
                    .unwrap();

                if let Some(found) =
                    statements
                        .iter()
                        .take(self_index)
                        .find(|stmt| match stmt.details() {
                            Any::DeclarationStatement(DeclarationStatement {
                                destination,
                                value: _,
                                is_const: _,
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
            _ => {}
        };

        self.parent()
            .map(|parent| parent.resolve_symbol(symbol))
            .flatten()
    }
}
