use super::ast::{
    self, AnyLiteral, ArrayLiteral, BooleanLiteral, Declaration, Destructure, ElementOrSpread,
    ExactStringLiteral, Expression, FuncDeclaration, ImportAllDeclaration, ImportDeclaration,
    KeyValueOrSpread, NameAndType, NilLiteral, NumberLiteral, ObjectLiteral, ProcDeclaration,
    SymbolDeclaration, ValueDeclaration, WithSlice, AST,
};
use super::errors::ParseError;
use super::slice::Slice;
use crate::passes::compile::CompileContext;
use crate::passes::parse::parse;
use crate::utils::cli_label;
use colored::Color;
use memoize::memoize;
use reqwest::Url;
use serde_json::Value;
use std::fmt::{Display, Write};
use std::path::Path;
use std::{collections::HashMap, path::PathBuf, rc::Rc};

#[derive(Debug)]
pub struct ModulesStore {
    modules: HashMap<ModuleID, Result<Module, ParseError>>,
}

impl ModulesStore {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    pub fn load_module_and_dependencies(&mut self, module_id: ModuleID, clean: bool) {
        if let Some(mut module_src) = module_id.load(clean) {
            module_src.push('\n'); // https://github.com/Geal/nom/issues/1573
            let module_src = Rc::new(module_src);

            let module_type = ModuleType::from(&module_id);

            match module_type {
                ModuleType::JavaScript => {
                    let module_src = Slice::new(module_src);

                    self.modules.insert(
                        module_id.clone(),
                        Ok(Module::Singleton {
                            module_id: module_id.clone(),
                            contents: AnyLiteral.as_ast(module_src).recast::<Expression>(),
                        }),
                    );
                }
                ModuleType::JSON => {
                    let module_src = Slice::new(module_src);

                    let parsed: Value = serde_json::from_str(module_src.as_str()).unwrap();
                    let contents = json_value_to_ast(module_src, parsed);

                    self.modules.insert(
                        module_id.clone(),
                        Ok(Module::Singleton {
                            module_id: module_id.clone(),
                            contents,
                        }),
                    );
                }
                ModuleType::Raw => {
                    let module_src = Slice::new(module_src);

                    self.modules.insert(
                        module_id.clone(),
                        Ok(Module::Singleton {
                            module_id: module_id.clone(),
                            contents: ExactStringLiteral {
                                tag: None,
                                value: module_src.clone(),
                            }
                            .as_ast(module_src)
                            .recast::<Expression>(),
                        }),
                    );
                }
                ModuleType::Bagel => {
                    let parsed = parse(module_id.clone(), module_src.clone());
                    self.modules.insert(
                        module_id.clone(),
                        parsed.map(|ast| Module::Bagel {
                            module_id: module_id.clone(),
                            ast,
                        }),
                    );

                    if let Some(ast::Module {
                        module_id: _,
                        declarations,
                    }) = self
                        .modules
                        .get(&module_id)
                        .map(|res| {
                            res.as_ref().ok().map(|module| match module {
                                Module::Bagel { module_id: _, ast } => Some(ast.downcast()),
                                Module::Singleton {
                                    module_id: _,
                                    contents: _,
                                } => None,
                            })
                        })
                        .flatten()
                        .flatten()
                    {
                        let imported =
                            declarations
                                .iter()
                                .filter_map(|decl| match decl.downcast() {
                                    Declaration::ImportAllDeclaration(ImportAllDeclaration {
                                        platforms: _,
                                        name: _,
                                        path,
                                    }) => Some(path.downcast().value.as_str().to_owned()),
                                    Declaration::ImportDeclaration(ImportDeclaration {
                                        platforms: _,
                                        imports: _,
                                        path,
                                    }) => Some(path.downcast().value.as_str().to_owned()),
                                    _ => None,
                                });

                        for path in imported {
                            let other_module_id = module_id.imported(&path);

                            if let Some(other_module_id) = other_module_id {
                                if !self.modules.contains_key(&other_module_id) {
                                    self.load_module_and_dependencies(other_module_id, clean);
                                }
                            } else {
                                // TODO: Improve this, make it a proper error
                                println!("ERROR: Malformed import path {:?}", path);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ModuleID, &Result<Module, ParseError>)> {
        self.modules.iter()
    }

    pub fn import_raw(
        &self,
        current_module_id: &ModuleID,
        path: &str,
    ) -> Option<Result<&Module, &ParseError>> {
        current_module_id
            .imported(&path)
            .map(|other_module_id| self.modules.get(&other_module_id).map(|res| res.as_ref()))
            .flatten()
    }

    pub fn import(&self, current_module_id: &ModuleID, path: &str) -> Option<&Module> {
        self.import_raw(current_module_id, path)
            .map(|res| res.ok())
            .flatten()
    }

    pub fn get(&self, module_id: &ModuleID) -> Option<&Module> {
        self.modules
            .get(module_id)
            .map(|res| res.as_ref().ok())
            .flatten()
    }

    pub fn bundle(&self) -> String {
        let mut buf = String::new();

        // VERY NAIVE FOR NOW
        for module in self
            .modules
            .iter()
            .filter_map(|(_, module)| module.as_ref().ok())
        {
            module.compile(
                CompileContext {
                    modules: self,
                    current_module: module,
                    include_types: false,
                },
                &mut buf,
            );
        }

        buf.write_str("\n\nmain();");

        buf
    }
}

impl From<HashMap<ModuleID, Result<Module, ParseError>>> for ModulesStore {
    fn from(modules: HashMap<ModuleID, Result<Module, ParseError>>) -> Self {
        Self { modules }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ModuleID {
    Local(Rc<PathBuf>),
    Remote(Rc<Url>),
    Artificial(Rc<String>),
}

impl ModuleID {
    pub fn load(&self, clean: bool) -> Option<String> {
        match self {
            ModuleID::Local(path) => std::fs::read_to_string(path.as_ref()).ok(),
            ModuleID::Remote(url) => {
                let cache_dir = cache_dir();
                let cache_path = cache_dir.as_ref().map(|base_dir| {
                    base_dir.join(url_escape::encode_component(url.as_str()).to_string() + ".bgl")
                });

                if let Some(cache_dir) = cache_dir {
                    if !cache_dir.exists() {
                        std::fs::create_dir_all(cache_dir);
                    }
                }

                if !clean {
                    if let Some(cache_path) = &cache_path {
                        if let Some(cached) = std::fs::read_to_string(cache_path).ok() {
                            return Some(cached);
                        }
                    }
                }

                let loaded = reqwest::blocking::get(url.as_ref().clone())
                    .ok()
                    .filter(|res| res.status().is_success())
                    .map(|res| res.text().ok())
                    .flatten();

                if let Some(loaded) = loaded {
                    println!("{} {}", cli_label("Downloaded", Color::Green), url.as_str());

                    if let Some(cache_path) = &cache_path {
                        let write_res = std::fs::write(cache_path, &loaded);

                        if write_res.is_err() {
                            println!(
                                "{} failed writing cache of module {}",
                                cli_label("Warning", Color::Yellow),
                                url.as_str()
                            );
                        }
                    }

                    return Some(loaded);
                } else {
                    println!(
                        "{} couldn't load remote module {}",
                        cli_label("Error", Color::Red),
                        url.as_str()
                    );
                }

                None
            }
            ModuleID::Artificial(_) => unreachable!(),
        }
        .map(|raw_string| {
            if raw_string.contains('\r') {
                raw_string.chars().filter(|c| *c != '\r').collect()
            } else {
                raw_string
            }
        })
    }

    pub fn imported(&self, imported: &str) -> Option<ModuleID> {
        if imported.starts_with("https://") || imported.starts_with("http://") {
            Url::parse(imported).ok().map(ModuleID::from)
        } else if imported.starts_with("/") {
            ModuleID::try_from(Path::new(imported)).ok()
        } else {
            match self {
                ModuleID::Local(this) => this
                    .parent()
                    .map(|dir| dir.to_path_buf().join(imported).canonicalize().ok())
                    .flatten()
                    .map(|path| ModuleID::try_from(path.as_path()).ok())
                    .flatten(),
                ModuleID::Remote(this) => this.join(imported).ok().map(ModuleID::from),
                ModuleID::Artificial(_) => Some(ModuleID::Artificial(Rc::new(imported.to_owned()))),
            }
        }
    }
}

#[memoize]
fn cache_dir() -> Option<PathBuf> {
    match std::env::consts::OS {
        "macos" => std::env::var("HOME")
            .map(|home_dir| PathBuf::from(home_dir).join("Library/Caches"))
            .ok(),
        "windows" => std::env::var("LOCALAPPDATA")
            .map(PathBuf::from)
            .or(std::env::var("USERPROFILE")
                .map(PathBuf::from)
                .map(|dir| dir.join("AppData/Local")))
            .map(|base_dir| base_dir.join("Cache"))
            .ok(),
        "linux" => std::env::var("XDG_CACHE_HOME").map(PathBuf::from).ok(),
        _ => None,
    }
    .map(|base_dir| base_dir.join("bagel"))
}

impl Display for ModuleID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleID::Local(p) => f.write_str(&p.to_string_lossy()),
            ModuleID::Remote(s) => f.write_str(s.as_str()),
            ModuleID::Artificial(s) => f.write_str(s.as_str()),
        }
    }
}

impl TryFrom<&Path> for ModuleID {
    type Error = std::io::Error;

    fn try_from(p: &Path) -> Result<ModuleID, std::io::Error> {
        Ok(ModuleID::Local(Rc::new(p.canonicalize()?)))
    }
}

impl From<Url> for ModuleID {
    fn from(s: Url) -> Self {
        Self::Remote(Rc::new(s))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ModuleType {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Module {
    Bagel {
        module_id: ModuleID,
        ast: AST<ast::Module>,
    },
    Singleton {
        module_id: ModuleID,
        contents: AST<Expression>,
    },
}

impl Module {
    pub fn module_id(&self) -> &ModuleID {
        match self {
            Module::Bagel { module_id, ast: _ } => module_id,
            Module::Singleton {
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
            Module::Bagel { module_id, ast } => ast
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
                                    ast::DeclarationDestination::NameAndType(NameAndType {
                                        name,
                                        type_annotation: _,
                                    }) => name.downcast().0 == item_name,
                                    ast::DeclarationDestination::Destructure(Destructure {
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
                        Declaration::TypeDeclaration(_) => todo!(),

                        Declaration::ImportAllDeclaration(_) => false,
                        Declaration::ImportDeclaration(_) => false,
                        Declaration::TestExprDeclaration(_) => false,
                        Declaration::TestBlockDeclaration(_) => false,
                        Declaration::TestTypeDeclaration(_) => false,
                    }
                })
                .cloned(),
            Module::Singleton {
                module_id,
                contents,
            } => None,
        }
    }
}

fn json_value_to_ast(module_src: Slice, value: Value) -> AST<Expression> {
    match value {
        Value::Null => NilLiteral.as_ast(module_src).recast::<Expression>(),
        Value::Bool(value) => BooleanLiteral(value)
            .as_ast(module_src)
            .recast::<Expression>(),
        Value::Number(value) => {
            NumberLiteral(Slice::new(Rc::new(value.to_string()))) // HACK: Not shared with module_src
                .as_ast(module_src)
                .recast::<Expression>()
        }
        Value::String(value) => ExactStringLiteral {
            tag: None,
            value: Slice::new(Rc::new(value)), // HACK: Not shared with module_src
        }
        .as_ast(module_src)
        .recast::<Expression>(),
        Value::Array(members) => ArrayLiteral(
            members
                .into_iter()
                .map(|member| {
                    ElementOrSpread::Element(json_value_to_ast(module_src.clone(), member))
                })
                .collect(),
        )
        .as_ast(module_src)
        .recast::<Expression>(),
        Value::Object(entries) => ObjectLiteral(
            entries
                .into_iter()
                .map(|(key, value)| {
                    KeyValueOrSpread::KeyValue(
                        ExactStringLiteral {
                            tag: None,
                            value: Slice::new(Rc::new(key)),
                        }
                        .as_ast(module_src.clone())
                        .recast::<Expression>(),
                        json_value_to_ast(module_src.clone(), value),
                        false,
                    )
                })
                .collect(),
        )
        .as_ast(module_src)
        .recast::<Expression>(),
    }
}
