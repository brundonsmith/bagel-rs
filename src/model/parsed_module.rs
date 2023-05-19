use crate::model::{
    ast::{
        Declaration, DeclarationDestination, Destructure, Expression, FuncDeclaration, Module,
        NameAndType, ProcDeclaration, SymbolDeclaration, TypeDeclaration, ValueDeclaration, AST,
    },
    ModuleID,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ParsedModule {
    Bagel {
        module_id: ModuleID,
        ast: AST<Module>,
    },
    JavaScript {
        module_id: ModuleID,
    },
    Singleton {
        module_id: ModuleID,
        contents: AST<Expression>,
    },
}

impl ParsedModule {
    pub fn module_id(&self) -> &ModuleID {
        match self {
            ParsedModule::Bagel { module_id, ast: _ } => module_id,
            ParsedModule::JavaScript { module_id } => module_id,
            ParsedModule::Singleton {
                module_id,
                contents: _,
            } => module_id,
        }
    }

    pub fn get_declaration(
        &self,
        item_name: &str,
        must_be_exported: bool,
    ) -> Option<AST<Declaration>> {
        match self {
            ParsedModule::Bagel { module_id: _, ast } => ast
                .downcast()
                .declarations
                .iter()
                .find(|decl| -> bool {
                    match decl.downcast() {
                        Declaration::ValueDeclaration(ValueDeclaration {
                            destination,
                            exported,
                            value: _,
                            is_const: _,
                            platforms: _,
                        }) => {
                            (!must_be_exported || exported)
                                && match destination {
                                    DeclarationDestination::NameAndType(NameAndType {
                                        name,
                                        type_annotation: _,
                                    }) => name.downcast().0 == item_name,
                                    DeclarationDestination::Destructure(Destructure {
                                        properties,
                                        spread,
                                        destructure_kind: _,
                                    }) => {
                                        properties.iter().any(|p| p.downcast().0 == item_name)
                                            || spread
                                                .map(|s| s.downcast().0 == item_name)
                                                .unwrap_or(false)
                                    }
                                }
                        }
                        Declaration::FuncDeclaration(FuncDeclaration {
                            name,
                            exported,
                            func: _,
                            platforms: _,
                            decorators: _,
                        }) => (!must_be_exported || exported) && name.downcast().0 == item_name,
                        Declaration::ProcDeclaration(ProcDeclaration {
                            name,
                            exported,
                            proc: _,
                            platforms: _,
                            decorators: _,
                        }) => (!must_be_exported || exported) && name.downcast().0 == item_name,
                        Declaration::SymbolDeclaration(SymbolDeclaration { name, exported }) => {
                            (!must_be_exported || exported) && name.downcast().0 == item_name
                        }
                        Declaration::TypeDeclaration(TypeDeclaration {
                            name,
                            declared_type: _,
                            exported,
                        }) => (!must_be_exported || exported) && name.downcast().0 == item_name,

                        Declaration::ImportAllDeclaration(_) => false,
                        Declaration::ImportDeclaration(_) => false,
                        Declaration::TestExprDeclaration(_) => false,
                        Declaration::TestBlockDeclaration(_) => false,
                        Declaration::TestTypeDeclaration(_) => false,
                    }
                })
                .cloned(),
            ParsedModule::JavaScript { module_id: _ } => None,
            ParsedModule::Singleton {
                module_id,
                contents,
            } => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ModuleType {
    Bagel,
    JavaScript,
    JSON,
    Raw,
}

impl From<&ModuleID> for ModuleType {
    fn from(module_id: &ModuleID) -> Self {
        let ext = match module_id {
            ModuleID::Local(path) => path
                .extension()
                .map(|ext| ext.to_str())
                .flatten()
                .unwrap_or(""),
            ModuleID::Remote(url) => {
                let url_str = url.as_str();
                let last_slash = url_str.char_indices().filter(|(_, ch)| *ch == '/').last();

                if let Some((last_slash_index, _)) = last_slash {
                    let dot_index = url_str[last_slash_index..]
                        .char_indices()
                        .filter(|(_, ch)| *ch == '.')
                        .next()
                        .map(|(index, ch)| last_slash_index + index);

                    if let Some(dot_index) = dot_index {
                        &url_str[dot_index + 1..]
                    } else {
                        ""
                    }
                } else {
                    ""
                }
            }
            ModuleID::Artificial(name) => {
                let dot = name[..name.len() - 1]
                    .char_indices()
                    .filter(|(_, ch)| *ch == '.')
                    .next();

                if let Some((dot_index, _)) = dot {
                    &name[dot_index + 1..]
                } else {
                    ""
                }
            }
        };

        match ext {
            "bgl" => ModuleType::Bagel,
            "js" => ModuleType::JavaScript,
            "jsx" => ModuleType::JavaScript,
            "ts" => ModuleType::JavaScript,
            "tsx" => ModuleType::JavaScript,
            "json" => ModuleType::JSON,
            _ => ModuleType::Raw,
        }
    }
}
