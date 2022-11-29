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

impl Declaration {
    pub fn get_type_and_value(&self) -> Option<(Src<Expression>, Option<Src<TypeExpression>>)> {
        match self {
            Declaration::FuncDeclaration {
                name: _,
                func,
                exported: _,
                platforms: _,
                decorators: _,
            } => Some((
                func.clone().map(Expression::from),
                Some(func.node.type_annotation.clone().map(TypeExpression::from)),
            )),
            Declaration::ProcDeclaration {
                name: _,
                proc,
                exported: _,
                platforms: _,
                decorators: _,
            } => Some((
                proc.clone().map(Expression::from),
                Some(proc.node.type_annotation.clone().map(TypeExpression::from)),
            )),
            Declaration::ValueDeclaration {
                name: _,
                type_annotation,
                value,
                is_const: _,
                exported: _,
                platforms: _,
            } => Some((value.clone(), type_annotation.clone())),
            _ => None,
        }
    }

    pub fn get_declared_type(&self) -> Option<&Src<TypeExpression>> {
        if let Declaration::TypeDeclaration {
            name: _,
            declared_type,
            exported: _,
        } = self
        {
            Some(declared_type)
        } else {
            None
        }
    }
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
