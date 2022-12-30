use crate::model::ast::*;

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    pub fn resolve_symbol(&self, symbol: &str) -> Option<ASTAny> {
        match self.parent().as_ref().map(|p| p.details()) {
            Some(ASTDetails::Module { declarations }) => {
                if let Some(found) = declarations.iter().find_map(|decl| {
                    match decl.details() {
                        ASTDetails::ImportAllDeclaration { name, path: _ } => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        ASTDetails::ImportDeclaration { imports, path: _ } => {
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
                        ASTDetails::TypeDeclaration {
                            name,
                            declared_type: _,
                            exported: _,
                        } => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        ASTDetails::FuncDeclaration {
                            name,
                            func: _,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        } => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        ASTDetails::ProcDeclaration {
                            name,
                            proc: _,
                            exported: _,
                            platforms: _,
                            decorators: _,
                        } => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        ASTDetails::ValueDeclaration {
                            name,
                            type_annotation: _,
                            value: _,
                            is_const: _,
                            exported: _,
                            platforms: _,
                        } => {
                            if name.downcast().0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        _ => {}
                    }

                    None
                }) {
                    return Some(found.upcast());
                }
            }
            Some(ASTDetails::Func {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            }) => {
                if let Some(found) = type_annotation
                    .downcast()
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Some(ASTDetails::Proc {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            }) => {
                if let Some(found) = type_annotation
                    .downcast()
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            Some(ASTDetails::InlineConstGroup {
                declarations,
                inner,
            }) => {
                if let Some(found) =
                    declarations
                        .iter()
                        .find(|arg| match arg.downcast().destination {
                            DeclarationDestination::NameAndType(NameAndType {
                                name,
                                type_annotation: _,
                            }) => name.downcast().0.as_str() == symbol,
                            DeclarationDestination::Destructure(_) => todo!(),
                        })
                {
                    return Some(found.clone().upcast());
                }
            }
            Some(ASTDetails::Block(statements)) => {
                let self_upcast = self.clone().upcast();
                let self_index = statements
                    .iter()
                    .enumerate()
                    .find(|(_, stmt)| **stmt == self_upcast)
                    .map(|(index, _)| index)
                    .unwrap();

                if let Some(found) =
                    statements
                        .iter()
                        .take(self_index)
                        .find(|stmt| match stmt.details() {
                            ASTDetails::DeclarationStatement {
                                destination,
                                value: _,
                                awaited: _,
                                is_const: _,
                            } => match destination {
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
