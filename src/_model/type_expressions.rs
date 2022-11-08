use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExpression {
    UnknownType,
    NilType,
    BooleanType,
    NumberType,
    StringType,
}

impl Display for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeExpression::UnknownType => f.write_str("unknown"),
            TypeExpression::NilType => f.write_str("nil"),
            TypeExpression::BooleanType => f.write_str("boolean"),
            TypeExpression::NumberType => f.write_str("number"),
            TypeExpression::StringType => f.write_str("string"),
        }
    }
}
