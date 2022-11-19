use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use crate::slice::Slice;

use super::Declaration;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleID(PathBuf);

impl ModuleID {
    pub fn as_str(&self) -> Cow<'_, str> {
        self.0.to_string_lossy()
    }

    pub fn as_path(&self) -> &Path {
        self.0.as_path()
    }
}

impl TryFrom<&Path> for ModuleID {
    type Error = std::io::Error;

    fn try_from(p: &Path) -> Result<ModuleID, std::io::Error> {
        Ok(ModuleID(p.canonicalize()?))
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
