use std::ops::Range;

use enum_variant_type::EnumVariantType;

use super::{BinaryOperator, Expression, Invocation, LocalIdentifier, PlainIdentifier};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Statement<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        span: Option<Range<usize>>,
        destination: (), // TODO  NameAndType | Destructure
        value: Expression<'a>,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        span: Option<Range<usize>>,
        cases: Vec<(Expression<'a>, Vec<Statement<'a>>)>,
        default_case: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        span: Option<Range<usize>>,
        item_identifier: PlainIdentifier<'a>,
        iterator: Expression<'a>,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        span: Option<Range<usize>>,
        condition: Expression<'a>,
        body: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        span: Option<Range<usize>>,
        target: LocalIdentifier<'a>, // TODO  | PropertyAccessor
        value: Expression<'a>,
        operator: Option<BinaryOperator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        span: Option<Range<usize>>,
        try_block: Vec<Statement<'a>>,
        error_identifier: PlainIdentifier<'a>,
        catch_block: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement {
        span: Option<Range<usize>>,
        error_expression: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        span: Option<Range<usize>>,
        effect: Vec<Statement<'a>>,
        until: Option<Expression<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InvocationStatement(Invocation<'a>),
}
