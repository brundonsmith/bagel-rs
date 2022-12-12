use super::ast::{ASTDetails, AST};
use super::errors::ParseError;
use super::slice::Slice;
use crate::passes::parse::parse;
use crate::utils::cli_label;
use colored::Color;
use memoize::memoize;
use reqwest::Url;
use std::fmt::Display;
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
        if let Some(mut bgl) = module_id.load(clean) {
            bgl.push('\n'); // https://github.com/Geal/nom/issues/1573
            let bgl_rc = Rc::new(bgl);

            let parsed = parse(module_id.clone(), bgl_rc.clone());
            self.modules.insert(
                module_id.clone(),
                parsed.map(|ast| Module {
                    module_id: module_id.clone(),
                    src: Slice::new(bgl_rc.clone()),
                    ast,
                }),
            );

            if let Some(Ok(ASTDetails::Module { declarations })) = self
                .modules
                .get(&module_id)
                .map(|res| res.as_ref().map(|module| module.ast.details()))
            {
                let imported: Vec<String> = declarations
                    .iter()
                    .filter_map(|decl| match decl.details() {
                        ASTDetails::ImportAllDeclaration { name: _, path } => {
                            match path.details() {
                                ASTDetails::ExactStringLiteral { tag: _, value } => {
                                    Some(value.as_str().to_owned())
                                }
                                _ => None,
                            }
                        }
                        ASTDetails::ImportDeclaration { imports: _, path } => {
                            match path.details() {
                                ASTDetails::ExactStringLiteral { tag: _, value } => {
                                    Some(value.as_str().to_owned())
                                }
                                _ => None,
                            }
                        }
                        _ => None,
                    })
                    .collect();

                for path in imported {
                    let other_module_id =
                        if path.starts_with("https://") || path.starts_with("http://") {
                            ModuleID::from(Url::parse(path.as_str()).unwrap())
                        } else {
                            let path = Path::new(&path);
                            let full_path = path.to_owned(); //module_id.as_path().join(path);

                            ModuleID::try_from(full_path.as_path()).unwrap()
                        };

                    if !self.modules.contains_key(&other_module_id) {
                        self.load_module_and_dependencies(other_module_id, clean);
                    }
                }
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ModuleID, &Result<Module, ParseError>)> {
        self.modules.iter()
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

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub module_id: ModuleID,
    pub src: Slice,
    pub ast: AST,
}
