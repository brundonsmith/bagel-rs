use enum_variant_type::EnumVariantType;

use super::{
    BinaryOperator, Expression, Invocation, LocalIdentifier, Node, PlainIdentifier,
    PropertyAccessor, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Statement {
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        destination: Node<Destination>,
        value: Node<Expression>,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        cases: Vec<Node<(Node<Expression>, Vec<Node<Statement>>)>>,
        default_case: Option<Vec<Node<Statement>>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        item_identifier: PlainIdentifier,
        iterator: Node<Expression>,
        body: Vec<Node<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        condition: Node<Expression>,
        body: Vec<Node<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        target: AssignmentTarget,
        value: Node<Expression>,
        operator: Option<BinaryOperator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        try_block: Vec<Node<Statement>>,
        error_identifier: PlainIdentifier,
        catch_block: Vec<Node<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement { error_expression: Node<Expression> },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        effect: Vec<Node<Statement>>,
        until: Option<Node<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InvocationStatement(Invocation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    LocalIdentifier(LocalIdentifier),
    PropertyAccessor(Node<PropertyAccessor>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Destination {
    NameAndType(PlainIdentifier, Option<Node<TypeExpression>>),
    Destructure {
        properties: Vec<PlainIdentifier>,
        spread: Option<PlainIdentifier>,
        is_object: bool, // false -> is_array
    },
}
