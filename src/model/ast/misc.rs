use crate::model::slice::Slice;

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
