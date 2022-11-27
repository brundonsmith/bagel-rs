use enum_variant_type::EnumVariantType;

use super::{
    ExactStringLiteral, Expression, Func, Invocation, LocalIdentifier, PlainIdentifier, Proc, Src,
    Statement, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Declaration {
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        name: PlainIdentifier,
        path: Src<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        imports: Vec<(PlainIdentifier, Option<PlainIdentifier>)>,
        path: Src<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        name: PlainIdentifier,
        declared_type: Src<TypeExpression>,
        exported: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        name: PlainIdentifier,
        func: Src<Func>, // TODO:  | JsFunc
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<Src<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        name: PlainIdentifier,
        proc: Src<Proc>, // TODO:  | JsProc
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<Src<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        name: PlainIdentifier,
        type_annotation: Option<Src<TypeExpression>>,
        value: Src<Expression>,
        is_const: bool,
        exported: bool,
        platforms: PlatformSet,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        name: Src<ExactStringLiteral>,
        expr: Src<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        name: Src<ExactStringLiteral>,
        block: Vec<Src<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        name: Src<ExactStringLiteral>,
        destination_type: Src<TypeExpression>,
        value_type: Src<TypeExpression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decorator {
    LocalIdentifier(LocalIdentifier),
    Invocation(Invocation),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlatformSet {
    pub node: bool,
    pub deno: bool,
    pub browser: bool,
}

impl PlatformSet {
    pub fn all() -> Self {
        PlatformSet {
            node: true,
            deno: true,
            browser: true,
        }
    }
}
