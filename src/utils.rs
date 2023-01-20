use colored::{Color, Colorize};
use nom::IResult;

use crate::{
    model::{ast::ASTAny, bgl_type::Type, slice::Slice},
    passes::parse::RawParseError,
};

pub trait Loggable: std::fmt::Debug {
    fn log(self) -> Self;
}

impl Loggable for Slice {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl Loggable for ASTAny {
    fn log(self) -> Self {
        println!("{:?}", &self);
        self
    }
}

impl Loggable for Type {
    fn log(self) -> Self {
        println!("{}", &self);
        self
    }
}

impl Loggable for &Type {
    fn log(self) -> Self {
        println!("{}", &self);
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

pub fn cli_label(s: &str, color: Color) -> String {
    format!("{: <11}", s).color(color).to_string()
}
