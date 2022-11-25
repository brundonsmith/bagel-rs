use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use crate::slice::Slice;

use super::Declaration;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ModuleID {
    Local(PathBuf),
    Remote(String),
}

impl ModuleID {
    pub fn load(&self) -> Option<String> {
        match self {
            ModuleID::Local(path) => std::fs::read_to_string(path).ok(),
            ModuleID::Remote(url) => reqwest::blocking::get(url)
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
        Ok(ModuleID::Local(p.canonicalize()?))
    }
}

impl From<String> for ModuleID {
    fn from(s: String) -> Self {
        Self::Remote(s)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub module_id: ModuleID,
    pub src: String,
    pub declarations: Vec<Src<Declaration>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlainIdentifier {
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Src<T> {
    pub src: Option<Slice>,
    pub node: T,
}

impl<T: Clone + std::fmt::Debug + PartialEq> Src<T> {
    pub fn contains(&self, other: &Slice) -> bool {
        self.src.map(|s| s.contains(other)).unwrap_or(false)
    }

    pub fn spanning<O>(&self, other: &Src<O>) -> Option<Slice> {
        self.src
            .map(|left_src| {
                other
                    .src
                    .map(move |right_src| left_src.spanning(&right_src))
            })
            .flatten()
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
        Src {
            src: Some(src),
            node: self,
        }
    }

    fn with_opt_src(self, src: Option<Slice>) -> Src<Self> {
        Src { src, node: self }
    }

    fn no_src(self) -> Src<Self> {
        Src {
            src: None,
            node: self,
        }
    }
}

impl<T: Clone + std::fmt::Debug + PartialEq> Srcable for T {}
