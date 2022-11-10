use std::ops::Range;

use super::Declaration;

pub trait Sourced {
    fn src(&self) -> Option<&str>;

    fn contains(&self, other: &str) -> bool {
        self.src()
            .map(|s| slice_contains(s, other))
            .unwrap_or(false)
    }
}

fn slice_contains(slice: &str, other: &str) -> bool {
    let slice_start = slice.as_ptr() as usize;
    let slice_end = slice_start + slice.len();

    let other_start = other.as_ptr() as usize;
    let other_end = other_start + other.len();

    other_start >= slice_start && other_end <= slice_end
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
    pub src: Option<&'a str>,
    pub name: &'a str,
}

impl<'a> Sourced for PlainIdentifier<'a> {
    fn src(&self) -> Option<&str> {
        self.src
    }
}
