use std::collections::HashSet;

use enum_variant_type::EnumVariantType;

use super::{
    ExactStringLiteral, Expression, Func, Invocation, LocalIdentifier, PlainIdentifier, Proc, Src,
    Statement, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Declaration {
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        name: Src<PlainIdentifier>,
        path: Src<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        imports: Vec<(Src<PlainIdentifier>, Option<Src<PlainIdentifier>>)>,
        path: Src<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        name: Src<PlainIdentifier>,
        declared_type: Src<TypeExpression>,
        exported: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        name: Src<PlainIdentifier>,
        func: Src<Func>, // TODO:  | JsFunc
        exported: bool,
        platforms: HashSet<Platform>,
        decorators: Vec<Src<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        name: Src<PlainIdentifier>,
        proc: Src<Proc>, // TODO:  | JsProc
        exported: bool,
        platforms: HashSet<Platform>,
        decorators: Vec<Src<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        name: Src<PlainIdentifier>,
        type_annotation: Option<Src<TypeExpression>>,
        value: Src<Expression>,
        is_const: bool,
        exported: bool,
        platforms: HashSet<Platform>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        name: Src<ExactStringLiteral>,
        expr: Src<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        name: Src<ExactStringLiteral>,
        block: Vec<Statement>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        name: Src<ExactStringLiteral>,
        destination_type: Src<TypeExpression>,
        value_type: Src<TypeExpression>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    Node,
    Deno,
    Browser,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decorator {
    LocalIdentifier(LocalIdentifier),
    Invocation(Invocation),
}
