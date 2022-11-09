use std::{collections::HashSet, ops::Range};

use enum_variant_type::EnumVariantType;

use super::{
    ExactStringLiteral, Expression, Func, Invocation, LocalIdentifier, PlainIdentifier, Proc, Span,
    Statement, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Declaration<'a> {
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        span: Option<Range<usize>>,
        name: PlainIdentifier<'a>,
        path: ExactStringLiteral<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        span: Option<Range<usize>>,
        imports: Vec<ImportItem<'a>>,
        path: ExactStringLiteral<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        span: Option<Range<usize>>,
        name: PlainIdentifier<'a>,
        declared_type: TypeExpression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        span: Option<Range<usize>>,
        name: PlainIdentifier<'a>,
        func: Func<'a>, // TODO:  | JsFunc
        platforms: HashSet<Platform>,
        decorators: Vec<Decorator<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        span: Option<Range<usize>>,
        name: PlainIdentifier<'a>,
        proc: Proc<'a>, // TODO:  | JsProc
        platforms: HashSet<Platform>,
        decorators: Vec<Decorator<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        span: Option<Range<usize>>,
        name: PlainIdentifier<'a>,
        type_annotation: Option<TypeExpression<'a>>,
        value: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        span: Option<Range<usize>>,
        name: ExactStringLiteral<'a>,
        expr: Expression<'a>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        span: Option<Range<usize>>,
        name: ExactStringLiteral<'a>,
        block: Vec<Statement<'a>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        span: Option<Range<usize>>,
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

impl<'a> Span for Declaration<'a> {
    fn span(&self) -> Option<&Range<usize>> {
        match self {
            Declaration::ValueDeclaration {
                span,
                name: _,
                type_annotation: _,
                value: _,
            } => span.as_ref(),
            Declaration::ImportAllDeclaration { span, name, path } => todo!(),
            Declaration::ImportDeclaration {
                span,
                imports,
                path,
            } => todo!(),
            Declaration::TypeDeclaration {
                span,
                name,
                declared_type,
            } => todo!(),
            Declaration::FuncDeclaration {
                span,
                name,
                func,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                span,
                name,
                proc,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { span, name, expr } => todo!(),
            Declaration::TestBlockDeclaration { span, name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                span,
                name,
                destination_type,
                value_type,
            } => todo!(),
        }
    }
}
