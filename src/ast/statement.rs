use std::ops::Range;

use enum_variant_type::EnumVariantType;

use crate::slice::Slice;

use super::{BinaryOperator, Expression, Invocation, LocalIdentifier, PlainIdentifier};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Statement<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        src: Option<Slice<'a>>,
        destination: (), // TODO  NameAndType | Destructure
        value: Expression<'a>,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        src: Option<Slice<'a>>,
        cases: Vec<(Expression<'a>, Vec<Statement<'a>>)>,
        default_case: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        src: Option<Slice<'a>>,
        item_identifier: PlainIdentifier<'a>,
        iterator: Expression<'a>,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        src: Option<Slice<'a>>,
        condition: Expression<'a>,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        src: Option<Slice<'a>>,
        target: LocalIdentifier<'a>, // TODO  | PropertyAccessor
        value: Expression<'a>,
        operator: Option<BinaryOperator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        src: Option<Slice<'a>>,
        try_block: Vec<Statement<'a>>,
        error_identifier: PlainIdentifier<'a>,
        catch_block: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement {
        src: Option<Slice<'a>>,
        error_expression: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        src: Option<Slice<'a>>,
        effect: Vec<Statement<'a>>,
        until: Option<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InvocationStatement(Invocation<'a>),
}
