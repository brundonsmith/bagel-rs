use std::ops::Range;

use enum_variant_type::EnumVariantType;

use crate::ast::ModuleName;

#[derive(Debug, Clone, PartialEq, EnumVariantType)]
pub enum BagelError {
    #[evt(derive(Debug, Clone, PartialEq))]
    ParseError {
        index: usize,
        module_name: ModuleName,
        message: String,
    },
    #[evt(derive(Debug, Clone, PartialEq))]
    TypeError {
        module_name: ModuleName,
        span: Range<usize>,
    },
}

impl BagelError {
    pub fn pretty_print(&self, code: &str, color: bool) -> String {
        match self {
            BagelError::ParseError {
                index,
                module_name,
                message,
            } => todo!(),
            BagelError::TypeError { module_name, span } => todo!(),
        }
    }
}
