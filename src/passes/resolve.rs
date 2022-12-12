use crate::model::ast::{ASTDetails, AST};

impl AST {
    pub fn resolve_symbol(&self, symbol: &str) -> Option<AST> {
        match self.details() {
            ASTDetails::Module { declarations } => {
                return declarations.iter().find_map(|decl| {
                    match decl.details() {
                        ASTDetails::ImportAllDeclaration { name, path } => todo!(),
                        ASTDetails::ImportDeclaration { imports, path } => todo!(),
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
                        ASTDetails::ValueDeclaration {
                            name,
                            type_annotation,
                            value,
                            is_const,
                            exported,
                            platforms,
                        } => {
                            if let ASTDetails::PlainIdentifier(name) = name.details() {
                                if name.as_str() == symbol {
                                    return Some(decl.clone());
                                }
                            }
                        }
                        _ => {}
                    };

                    None
                });
            }
            _ => {}
        };

        self.parent()
            .map(|parent| parent.resolve_symbol(symbol))
            .flatten()
    }
}
