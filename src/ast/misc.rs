use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use crate::slice::Slice;

use super::Declaration;

pub trait Sourced {
    fn src(&self) -> Option<Slice>;

    fn contains(&self, other: &Slice) -> bool {
        self.src().map(|s| s.contains(other)).unwrap_or(false)
    }

    fn spanning<O: Sourced>(&self, other: &O) -> Option<Slice> {
        self.src()
            .map(|left_src| {
                other
                    .src()
                    .map(move |right_src| left_src.spanning(&right_src))
            })
            .flatten()
    }
}

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
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlainIdentifier {
    pub src: Option<Slice>,
    pub name: String,
}

impl Sourced for PlainIdentifier {
    fn src(&self) -> Option<Slice> {
        self.src.clone()
    }
}
