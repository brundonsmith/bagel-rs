use std::rc::Rc;

use colored::{Color, Colorize};
use nom::IResult;

use crate::{
    model::{
        ast::{ASTAny, ASTInner, Any, AST},
        Slice, Type,
    },
    passes::RawParseError,
};

pub trait Rcable {
    fn rc(self) -> Rc<Self>;
}

impl<TKind> Rcable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn rc(self) -> Rc<Self> {
        Rc::new(self)
    }
}

impl Rcable for ASTInner {
    fn rc(self) -> Rc<Self> {
        Rc::new(self)
    }
}

impl Rcable for Type {
    fn rc(self) -> Rc<Self> {
        Rc::new(self)
    }
}

impl Rcable for String {
    fn rc(self) -> Rc<Self> {
        Rc::new(self)
    }
}

pub trait Loggable: std::fmt::Debug {
    fn log(self) -> Self;
}

impl Loggable for Slice {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl<TKind> Loggable for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + std::fmt::Debug,
    Any: From<TKind>,
{
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl Loggable for Type {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl Loggable for &Type {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl Loggable for (Slice, ASTAny) {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl Loggable for IResult<Slice, ASTAny, RawParseError> {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl Loggable for (Slice, Slice) {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl<T: std::fmt::Debug> Loggable for Option<T> {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

pub fn cli_label(s: &str, color: Color) -> String {
    format!("{: <13}", s).color(color).to_string()
}
