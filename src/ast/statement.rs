use enum_variant_type::EnumVariantType;

use crate::slice::Slice;

use super::{BinaryOperator, Expression, Invocation, LocalIdentifier, PlainIdentifier};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Statement {
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        src: Option<Slice>,
        destination: (), // TODO  NameAndType | Destructure
        value: Expression,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        src: Option<Slice>,
        cases: Vec<(Expression, Vec<Statement>)>,
        default_case: Vec<Statement>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        src: Option<Slice>,
        item_identifier: PlainIdentifier,
        iterator: Expression,
        body: Vec<Statement>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        src: Option<Slice>,
        condition: Expression,
        body: Vec<Statement>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        src: Option<Slice>,
        target: LocalIdentifier, // TODO  | PropertyAccessor
        value: Expression,
        operator: Option<BinaryOperator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        src: Option<Slice>,
        try_block: Vec<Statement>,
        error_identifier: PlainIdentifier,
        catch_block: Vec<Statement>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement {
        src: Option<Slice>,
        error_expression: Expression,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        src: Option<Slice>,
        effect: Vec<Statement>,
        until: Option<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InvocationStatement(Invocation),
}
