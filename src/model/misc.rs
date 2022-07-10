use std::collections::HashMap;

use super::ast::{ASTEnum, ModuleName};

pub struct Context<'a> {
    pub all_modules: &'a HashMap<ModuleName, ASTEnum>,
    pub report_error: &'a mut dyn FnMut(Error),
}

#[derive(Debug, Clone)]
pub enum Error {
    TypeError,
}
