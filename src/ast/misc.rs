use std::ops::Range;

use super::Declaration;

pub trait Span {
    fn span(&self) -> Option<&Range<usize>>;

    fn contains(&self, index: &usize) -> bool {
        self.span()
            .map(|range| range.contains(index))
            .unwrap_or(false)
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
    pub span: Option<Range<usize>>,
    pub name: &'a str,
}

impl<'a> Span for PlainIdentifier<'a> {
    fn span(&self) -> Option<&Range<usize>> {
        self.span.as_ref()
    }
}
