use enum_variant_type::EnumVariantType;

use super::{
    ExactStringLiteral, Expression, Func, Invocation, LocalIdentifier, Node, PlainIdentifier, Proc,
    Statement, TypeExpression,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Declaration {
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        name: PlainIdentifier,
        path: Node<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        imports: Vec<(PlainIdentifier, Option<PlainIdentifier>)>,
        path: Node<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        name: PlainIdentifier,
        declared_type: Node<TypeExpression>,
        exported: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        name: PlainIdentifier,
        func: Node<Func>, // TODO:  | JsFunc
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<Node<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        name: PlainIdentifier,
        proc: Node<Proc>, // TODO:  | JsProc
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<Node<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        name: PlainIdentifier,
        type_annotation: Option<Node<TypeExpression>>,
        value: Node<Expression>,
        is_const: bool,
        exported: bool,
        platforms: PlatformSet,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        name: Node<ExactStringLiteral>,
        expr: Node<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        name: Node<ExactStringLiteral>,
        block: Vec<Node<Statement>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        name: Node<ExactStringLiteral>,
        destination_type: Node<TypeExpression>,
        value_type: Node<TypeExpression>,
    },
}

impl Declaration {
    pub fn get_value_and_type(&self) -> Option<(Node<Expression>, Option<Node<TypeExpression>>)> {
        match self {
            Declaration::FuncDeclaration {
                name: _,
                func,
                exported: _,
                platforms: _,
                decorators: _,
            } => Some((
                func.map_deep(Expression::from),
                Some(func.this().type_annotation.map_deep(TypeExpression::from)),
            )),
            Declaration::ProcDeclaration {
                name: _,
                proc,
                exported: _,
                platforms: _,
                decorators: _,
            } => Some((
                proc.map_deep(Expression::from),
                Some(proc.this().type_annotation.map_deep(TypeExpression::from)),
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

    pub fn get_declared_type(&self) -> Option<&Node<TypeExpression>> {
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
