use std::collections::HashSet;

use enum_variant_type::EnumVariantType;

use crate::slice::Slice;

use super::{
    ExactStringLiteral, Expression, Func, Invocation, LocalIdentifier, PlainIdentifier, Proc,
    Sourced, Statement, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Declaration {
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        src: Option<Slice>,
        name: PlainIdentifier,
        path: ExactStringLiteral,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        src: Option<Slice>,
        imports: Vec<ImportItem>,
        path: ExactStringLiteral,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        src: Option<Slice>,
        name: PlainIdentifier,
        declared_type: TypeExpression,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        src: Option<Slice>,
        name: PlainIdentifier,
        func: Func, // TODO:  | JsFunc
        platforms: HashSet<Platform>,
        decorators: Vec<Decorator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        src: Option<Slice>,
        name: PlainIdentifier,
        proc: Proc, // TODO:  | JsProc
        platforms: HashSet<Platform>,
        decorators: Vec<Decorator>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        src: Option<Slice>,
        name: PlainIdentifier,
        type_annotation: Option<TypeExpression>,
        value: Expression,
        is_const: bool,
        exported: bool,
        platforms: HashSet<Platform>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        src: Option<Slice>,
        name: ExactStringLiteral,
        expr: Expression,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        src: Option<Slice>,
        name: ExactStringLiteral,
        block: Vec<Statement>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        src: Option<Slice>,
        name: ExactStringLiteral,
        destination_type: TypeExpression,
        value_type: TypeExpression,
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

#[derive(Clone, Debug, PartialEq)]
pub struct ImportItem {
    pub name: PlainIdentifier,
    pub alias: Option<PlainIdentifier>,
}

impl Sourced for Declaration {
    fn src(&self) -> Option<Slice> {
        match self {
            Declaration::ValueDeclaration {
                src,
                name: _,
                type_annotation: _,
                value: _,
                is_const: _,
                exported: _,
                platforms: _,
            } => src.clone(),
            Declaration::ImportAllDeclaration { src, name, path } => todo!(),
            Declaration::ImportDeclaration { src, imports, path } => todo!(),
            Declaration::TypeDeclaration {
                src,
                name,
                declared_type,
            } => todo!(),
            Declaration::FuncDeclaration {
                src,
                name,
                func,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                src,
                name,
                proc,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { src, name, expr } => todo!(),
            Declaration::TestBlockDeclaration { src, name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                src,
                name,
                destination_type,
                value_type,
            } => todo!(),
        }
    }
}
