use std::{
    fmt::Display,
    rc::{Rc, Weak},
};

use super::{expressions::Expression, type_expressions::TypeExpression};

#[derive(Clone, Debug)]
pub struct SourceInfo {
    pub parent: Option<Weak<ASTEnum>>,
    pub module: Option<ModuleName>,
    pub start_index: Option<usize>,
    pub end_index: Option<usize>,
}

impl SourceInfo {
    pub fn empty() -> Self {
        Self {
            parent: None,
            module: None,
            start_index: None,
            end_index: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModuleName(pub String);

pub type AST<T> = Result<Rc<WithSourceInfo<T>>, ()>;

#[derive(Clone, Debug)]
pub struct WithSourceInfo<T: Display> {
    pub source_info: SourceInfo,
    pub node: T,
}

impl<T: Display> Display for WithSourceInfo<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

impl<T: Display> WithSourceInfo<T> {
    pub fn empty(node: T) -> Self {
        Self {
            source_info: SourceInfo::empty(),
            node,
        }
    }
}

pub fn visit_ast<F: FnMut(&ASTEnum)>(ast: &ASTEnum, cb: &mut F) {
    cb(ast);

    match &ast {
        ASTEnum::Expression(Ok(x)) => match &x.node {
            Expression::NilLiteral => {}
            Expression::NumberLiteral { value: _ } => {}
            Expression::BinaryOperator { left, op: _, right } => {
                visit_ast(&left.clone().into(), cb);
                visit_ast(&right.clone().into(), cb);
            }
            Expression::Parenthesis { inner } => {
                visit_ast(&inner.clone().into(), cb);
            }
        },
        ASTEnum::TypeExpression(Ok(x)) => match &x.node {
            TypeExpression::UnknownType => {}
            TypeExpression::NilType => {}
            TypeExpression::BooleanType => {}
            TypeExpression::NumberType => {}
            TypeExpression::StringType => {}
        },
        ASTEnum::PlainIdentifier(_) => {}
        ASTEnum::NameAndType { name, typ } => {
            visit_ast(&name.clone().into(), cb);

            if let Some(typ) = typ {
                visit_ast(&typ.clone().into(), cb);
            }
        }
        _ => {}
    };
}

#[derive(Clone, Debug)]
pub enum ASTEnum {
    // Module {
    //     module_type: ModuleType,
    //     declarations: Vec<AST>,
    // },

    // Declaration(Declaration),
    Expression(AST<Expression>),
    TypeExpression(AST<TypeExpression>),
    // Statement(Statement),

    // Attribute,
    PlainIdentifier(AST<PlainIdentifier>),
    // Block {
    //     statements: Vec<AST>,
    // },
    // Operator,
    // Case,
    // SwitchCase,
    // CaseBlock,
    // Args,
    // Arg,
    // SpreadArgs,
    // ImportItem,
    // Spread,
    // InlineDeclaration,
    // ObjectEntry,
    NameAndType {
        name: AST<PlainIdentifier>,
        typ: Option<AST<TypeExpression>>,
    },
    // Destructure {
    //     properties: Vec<AST>,
    //     spread: Option<AST>,
    // },
    // Decorator,
}

#[derive(Clone, Debug)]
pub struct PlainIdentifier(pub String);

impl Display for PlainIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ModuleType {
    Bgl,
    Json,
    Text,
}

impl Display for ASTEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTEnum::Expression(Ok(x)) => Display::fmt(x, f),
            ASTEnum::TypeExpression(Ok(x)) => Display::fmt(x, f),
            ASTEnum::PlainIdentifier(Ok(x)) => Display::fmt(x, f),
            ASTEnum::NameAndType {
                name: Ok(name),
                typ: Some(Ok(typ)),
            } => {
                f.write_str(name.node.0.as_str())?;
                f.write_str(": ")?;
                Display::fmt(&typ.node, f)
            }
            ASTEnum::NameAndType {
                name: Ok(name),
                typ: None,
            } => f.write_str(name.node.0.as_str()),
            _ => f.write_str("<parse error>"),
        }
    }
}

pub fn ast_from(expr: Expression) -> AST<Expression> {
    Ok(Rc::new(WithSourceInfo::empty(expr)))
}

impl From<AST<Expression>> for ASTEnum {
    fn from(expr: AST<Expression>) -> Self {
        Self::Expression(expr)
    }
}

impl From<AST<TypeExpression>> for ASTEnum {
    fn from(expr: AST<TypeExpression>) -> Self {
        Self::TypeExpression(expr)
    }
}

impl From<AST<PlainIdentifier>> for ASTEnum {
    fn from(expr: AST<PlainIdentifier>) -> Self {
        Self::PlainIdentifier(expr)
    }
}
