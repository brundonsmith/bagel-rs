use std::ops::Range;

use enum_variant_type::EnumVariantType;

use super::{BinaryOperator, Expression, Invocation, LocalIdentifier, PlainIdentifier};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Statement<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        src: Option<&'a str>,
        destination: (), // TODO  NameAndType | Destructure
        value: Expression<'a>,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        src: Option<&'a str>,
        cases: Vec<(Expression<'a>, Vec<Statement<'a>>)>,
        default_case: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        src: Option<&'a str>,
        item_identifier: PlainIdentifier<'a>,
        iterator: Expression<'a>,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        src: Option<&'a str>,
        condition: Expression<'a>,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        src: Option<&'a str>,
        target: LocalIdentifier<'a>, // TODO  | PropertyAccessor
        value: Expression<'a>,
        operator: Option<BinaryOperator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        src: Option<&'a str>,
        try_block: Vec<Statement<'a>>,
        error_identifier: PlainIdentifier<'a>,
        catch_block: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement {
        src: Option<&'a str>,
        error_expression: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        src: Option<&'a str>,
        effect: Vec<Statement<'a>>,
        until: Option<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InvocationStatement(Invocation<'a>),
}
