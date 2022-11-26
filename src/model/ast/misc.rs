use std::{
    fmt::Display,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::model::slice::Slice;

use super::Declaration;

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

#[derive(Clone, Debug, PartialEq)]
pub struct PlainIdentifier(pub Slice);

#[derive(Clone, Debug, PartialEq)]
pub struct Src<T> {
    pub src: Slice,
    pub node: T,
}

impl<T: Clone + std::fmt::Debug + PartialEq> Src<T> {
    pub fn contains(&self, other: &Slice) -> bool {
        self.src.contains(other)
    }

    pub fn spanning<O>(&self, other: &Src<O>) -> Slice {
        self.src.clone().spanning(&other.src)
    }

    pub fn map<O, F: Fn(T) -> O>(self, f: F) -> Src<O> {
        Src {
            src: self.src,
            node: f(self.node),
        }
    }
}

pub trait Srcable: Clone + std::fmt::Debug + PartialEq {
    fn with_src(self, src: Slice) -> Src<Self> {
        Src { src, node: self }
    }
}

impl<T: Clone + std::fmt::Debug + PartialEq> Srcable for T {}
