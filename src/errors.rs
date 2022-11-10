use std::ops::Range;

use enum_variant_type::EnumVariantType;

use crate::ast::ModuleName;

#[derive(Debug, Clone, PartialEq, EnumVariantType)]
pub enum BagelError<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    ParseError {
        src: &'a str,
        module_name: ModuleName,
        message: String,
    },
    #[evt(derive(Debug, Clone, PartialEq))]
    TypeError {
        src: &'a str,
        module_name: ModuleName,
    },
}

impl<'a> BagelError<'a> {
    pub fn pretty_print(&self, code: &'a str, color: bool) -> String {
        match self {
            BagelError::ParseError {
                src,
                module_name,
                message,
            } => todo!(),
            BagelError::TypeError { src, module_name } => todo!(),
        }
    }
}
