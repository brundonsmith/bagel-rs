use std::{collections::HashSet, ops::Range};

use enum_variant_type::EnumVariantType;

use crate::slice::Slice;

use super::{
    ExactStringLiteral, Expression, Func, Invocation, LocalIdentifier, PlainIdentifier, Proc,
    Sourced, Statement, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Declaration<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        src: Option<Slice<'a>>,
        name: PlainIdentifier<'a>,
        path: ExactStringLiteral<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        src: Option<Slice<'a>>,
        imports: Vec<ImportItem<'a>>,
        path: ExactStringLiteral<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        src: Option<Slice<'a>>,
        name: PlainIdentifier<'a>,
        declared_type: TypeExpression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        src: Option<Slice<'a>>,
        name: PlainIdentifier<'a>,
        func: Func<'a>, // TODO:  | JsFunc
        platforms: HashSet<Platform>,
        decorators: Vec<Decorator<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        src: Option<Slice<'a>>,
        name: PlainIdentifier<'a>,
        proc: Proc<'a>, // TODO:  | JsProc
        platforms: HashSet<Platform>,
        decorators: Vec<Decorator<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        src: Option<Slice<'a>>,
        name: PlainIdentifier<'a>,
        type_annotation: Option<TypeExpression<'a>>,
        value: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        src: Option<Slice<'a>>,
        name: ExactStringLiteral<'a>,
        expr: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        src: Option<Slice<'a>>,
        name: ExactStringLiteral<'a>,
        block: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        src: Option<Slice<'a>>,
        name: ExactStringLiteral<'a>,
        destination_type: TypeExpression<'a>,
        value_type: TypeExpression<'a>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    Node,
    Deno,
    Browser,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decorator<'a> {
    LocalIdentifier(LocalIdentifier<'a>),
    Invocation(Invocation<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportItem<'a> {
    pub name: PlainIdentifier<'a>,
    pub alias: Option<PlainIdentifier<'a>>,
}

impl<'a> Sourced<'a> for Declaration<'a> {
    fn src(&self) -> Option<Slice<'a>> {
        match self {
            Declaration::ValueDeclaration {
                src,
                name: _,
                type_annotation: _,
                value: _,
            } => *src,
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
