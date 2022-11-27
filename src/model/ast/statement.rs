use enum_variant_type::EnumVariantType;

use super::{
    BinaryOperator, Expression, Invocation, LocalIdentifier, PlainIdentifier, PropertyAccessor,
    Src, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Statement {
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        destination: Src<Destination>,
        value: Src<Expression>,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        cases: Vec<(Src<Expression>, Vec<Src<Statement>>)>,
        default_case: Option<Vec<Src<Statement>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        item_identifier: PlainIdentifier,
        iterator: Src<Expression>,
        body: Vec<Src<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        condition: Src<Expression>,
        body: Vec<Src<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        target: AssignmentTarget,
        value: Src<Expression>,
        operator: Option<BinaryOperator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        try_block: Vec<Src<Statement>>,
        error_identifier: PlainIdentifier,
        catch_block: Vec<Src<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement { error_expression: Src<Expression> },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        effect: Vec<Src<Statement>>,
        until: Option<Src<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InvocationStatement(Invocation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    LocalIdentifier(LocalIdentifier),
    PropertyAccessor(Src<PropertyAccessor>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Destination {
    NameAndType(PlainIdentifier, Option<Src<TypeExpression>>),
    Destructure {
        properties: Vec<PlainIdentifier>,
        spread: Option<PlainIdentifier>,
        is_object: bool, // false -> is_array
    },
}
