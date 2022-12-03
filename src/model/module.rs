use std::fmt::Display;
use std::path::Path;
use std::{collections::HashMap, path::PathBuf, rc::Rc};

use crate::passes::parse::parse;

use super::ast::{Declaration, Src};
use super::errors::ParseError;

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

    pub fn load_module_and_dependencies(&mut self, module_id: ModuleID) {
        if let Some(mut bgl) = module_id.load() {
            bgl.push('\n'); // https://github.com/Geal/nom/issues/1573

            let parsed = parse(module_id.clone(), Rc::new(bgl));
            self.modules.insert(module_id.clone(), parsed);

            if let Some(Ok(parsed)) = self.modules.get(&module_id) {
                let imported: Vec<String> = parsed
                    .declarations
                    .iter()
                    .filter_map(|decl| {
                        if let Declaration::ImportAllDeclaration { name: _, path } = &decl.node {
                            Some(path.node.value.as_str().to_owned())
                        } else if let Declaration::ImportDeclaration { imports: _, path } =
                            &decl.node
                        {
                            Some(path.node.value.as_str().to_owned())
                        } else {
                            None
                        }
                    })
                    .collect();

                for path in imported {
                    let path = Path::new(&path);
                    let full_path = path.to_owned(); //module_id.as_path().join(path);
                    let other_module_id = ModuleID::try_from(full_path.as_path()).unwrap();
                    // TODO: Handle https modules

                    if !self.modules.contains_key(&other_module_id) {
                        self.load_module_and_dependencies(other_module_id);
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
    Remote(Rc<String>),
}

impl ModuleID {
    pub fn load(&self) -> Option<String> {
        match self {
            ModuleID::Local(path) => std::fs::read_to_string(path.as_ref()).ok(),
            ModuleID::Remote(url) => reqwest::blocking::get(url.as_ref())
                .ok()
                .map(|res| res.text().ok())
                .flatten(),
        }
    }
}

impl Display for ModuleID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleID::Local(p) => f.write_str(&p.to_string_lossy()),
            ModuleID::Remote(s) => f.write_str(s),
        }
    }
}

impl TryFrom<&Path> for ModuleID {
    type Error = std::io::Error;

    fn try_from(p: &Path) -> Result<ModuleID, std::io::Error> {
        Ok(ModuleID::Local(Rc::new(p.canonicalize()?)))
    }
}

impl From<String> for ModuleID {
    fn from(s: String) -> Self {
        Self::Remote(Rc::new(s))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub module_id: ModuleID,
    pub src: Rc<String>,
    pub declarations: Vec<Src<Declaration>>,
}
