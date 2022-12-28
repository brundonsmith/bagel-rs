use crate::model::ast::*;

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    pub fn resolve_symbol(&self, symbol: &str) -> Option<ASTAny> {
        match self.details() {
            ASTDetails::Module { declarations } => {
                if let Some(found) = declarations.iter().find_map(|decl| {
                    match decl.details() {
                        ASTDetails::ImportAllDeclaration { name, path } => todo!(),
                        ASTDetails::ImportDeclaration { imports, path } => todo!(),
                        ASTDetails::TypeDeclaration {
                            name,
                            declared_type: _,
                            exported: _,
                        } => {
                            let name = name.downcast();

                            if name.0.as_str() == symbol {
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
                            let name = name.downcast();

                            if name.0.as_str() == symbol {
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
                            let name = name.downcast();

                            if name.0.as_str() == symbol {
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
                            let name = name.downcast();

                            if name.0.as_str() == symbol {
                                return Some(decl.clone());
                            }
                        }
                        _ => {}
                    }

                    None
                }) {
                    return Some(found);
                }
            }
            ASTDetails::Func {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            } => {
                if let Some(found) = type_annotation
                    .downcast()
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            ASTDetails::Proc {
                type_annotation,
                is_async: _,
                is_pure: _,
                body: _,
            } => {
                if let Some(found) = type_annotation
                    .downcast()
                    .args
                    .iter()
                    .find(|arg| arg.downcast().name.downcast().0.as_str() == symbol)
                {
                    return Some(found.clone().upcast());
                }
            }
            ASTDetails::InlineConstGroup {
                declarations,
                inner,
            } => {
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
            _ => {}
        };

        self.parent()
            .map(|parent| parent.resolve_symbol(symbol))
            .flatten()
    }
}
