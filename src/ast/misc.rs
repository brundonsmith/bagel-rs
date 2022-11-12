use crate::slice::Slice;

use super::Declaration;

pub trait Sourced<'a> {
    fn src(&self) -> Option<Slice<'a>>;

    fn contains(&self, other: Slice<'a>) -> bool {
        self.src().map(|s| s.contains(other)).unwrap_or(false)
    }

    fn spanning<O: Sourced<'a>>(&self, other: &O) -> Option<Slice<'a>> {
        self.src()
            .map(|left_src| {
                other
                    .src()
                    .map(move |right_src| left_src.spanning(right_src))
            })
            .flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleName(String);

impl Into<ModuleName> for String {
    fn into(self) -> ModuleName {
        ModuleName(self)
    }
}

impl Into<String> for ModuleName {
    fn into(self) -> String {
        self.0
    }
}

impl<'a> Into<&'a str> for &'a ModuleName {
    fn into(self) -> &'a str {
        self.0.as_str()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module<'a> {
    pub module_name: ModuleName,
    pub declarations: Vec<Declaration<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlainIdentifier<'a> {
    pub src: Option<Slice<'a>>,
    pub name: Slice<'a>,
}

impl<'a> Sourced<'a> for PlainIdentifier<'a> {
    fn src(&self) -> Option<Slice<'a>> {
        self.src
    }
}
