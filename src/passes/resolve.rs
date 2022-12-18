use crate::model::ast::*;

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
                            declared_type: _,
                            exported: _,
                        } => {
                            let name = name.expect::<PlainIdentifier>();

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
                            let name = name.expect::<PlainIdentifier>();

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
                            let name = name.expect::<PlainIdentifier>();

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
                            let name = name.expect::<PlainIdentifier>();

                            if name.0.as_str() == symbol {
                                return Some(decl.clone());
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
