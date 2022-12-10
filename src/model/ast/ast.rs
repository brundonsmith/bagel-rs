use std::rc::{Rc, Weak};

use super::*;
use crate::model::{module::Module, slice::Slice};

#[derive(Clone, Debug)]
pub enum ASTRef {
    Module(Weak<Module>),
    PlainIdentifier(PlainIdentifier),
    Declaration(Weak<Node<Declaration>>),
    Expression(Weak<Node<Expression>>),
    Statement(Weak<Node<Statement>>),
    TypeExpression(Weak<Node<TypeExpression>>),
}

impl ASTRef {
    // pub fn visit<R, F>(self, mut visit: F) -> Option<R>
    // where
    //     F: FnMut(ASTRef) -> Option<R>,
    // {
    //     visit(self)?;

    //     match &self {
    //         ASTRef::Module(x) => {
    //             for decl in x.declarations.iter() {
    //                 visit(decl.into())?;
    //             }
    //         }
    //         ASTRef::PlainIdentifier(x) => {}
    //         ASTRef::Declaration(x) => match &x.node {
    //             Declaration::ImportAllDeclaration { name, path } => todo!(),
    //             Declaration::ImportDeclaration { imports, path } => todo!(),
    //             Declaration::TypeDeclaration {
    //                 name,
    //                 declared_type,
    //                 exported,
    //             } => todo!(),
    //             Declaration::FuncDeclaration {
    //                 name,
    //                 func,
    //                 exported,
    //                 platforms,
    //                 decorators,
    //             } => todo!(),
    //             Declaration::ProcDeclaration {
    //                 name,
    //                 proc,
    //                 exported,
    //                 platforms,
    //                 decorators,
    //             } => todo!(),
    //             Declaration::ValueDeclaration {
    //                 name,
    //                 type_annotation,
    //                 value,
    //                 is_const: _,
    //                 exported: _,
    //                 platforms: _,
    //             } => {
    //                 visit(name.into())?;
    //                 if let Some(type_annotation) = type_annotation {
    //                     visit(type_annotation.into())?;
    //                 }
    //                 visit(value.into())?;
    //             }
    //             Declaration::TestExprDeclaration { name, expr } => todo!(),
    //             Declaration::TestBlockDeclaration { name, block } => todo!(),
    //             Declaration::TestTypeDeclaration {
    //                 name,
    //                 destination_type,
    //                 value_type,
    //             } => todo!(),
    //         },
    //         ASTRef::Expression(_) => todo!(),
    //         ASTRef::Statement(_) => todo!(),
    //         ASTRef::TypeExpression(_) => todo!(),
    //     }

    //     None
    // }
}

impl PartialEq for ASTRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Module(l0), Self::Module(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::PlainIdentifier(l0), Self::PlainIdentifier(r0)) => l0 == r0,
            (Self::Declaration(l0), Self::Declaration(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::Expression(l0), Self::Expression(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::Statement(l0), Self::Statement(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::TypeExpression(l0), Self::TypeExpression(r0)) => l0.as_ptr() == r0.as_ptr(),
            _ => false,
        }
    }
}

impl From<Weak<Module>> for ASTRef {
    fn from(x: Weak<Module>) -> Self {
        ASTRef::Module(x)
    }
}

impl From<PlainIdentifier> for ASTRef {
    fn from(x: PlainIdentifier) -> Self {
        ASTRef::PlainIdentifier(x)
    }
}

impl From<Weak<Node<Declaration>>> for ASTRef {
    fn from(x: Weak<Node<Declaration>>) -> Self {
        ASTRef::Declaration(x)
    }
}

impl From<Weak<Node<Expression>>> for ASTRef {
    fn from(x: Weak<Node<Expression>>) -> Self {
        ASTRef::Expression(x)
    }
}

impl From<Weak<Node<Statement>>> for ASTRef {
    fn from(x: Weak<Node<Statement>>) -> Self {
        ASTRef::Statement(x)
    }
}

impl From<Weak<Node<TypeExpression>>> for ASTRef {
    fn from(x: Weak<Node<TypeExpression>>) -> Self {
        ASTRef::TypeExpression(x)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node<T: Clone> {
    parent: Option<ASTRef>,
    pub slice: Slice,
    this: Rc<T>,
}

impl<T: Clone> Node<T> {
    pub fn set_parent(&mut self, parent: ASTRef) {
        self.parent = Some(parent);
    }

    // pub fn get_parent_expression(&self) -> Option<Node<Expression>> {
    //     if let Some(ASTRef::Expression(expr)) = &self.parent {
    //         expr.upgrade()
    //     } else {
    //         None
    //     }
    // }

    pub fn this(&self) -> &T {
        &self.this.as_ref()
    }

    pub fn contains(&self, other: &Slice) -> bool {
        self.slice.contains(other)
    }

    pub fn spanning<O: Clone>(&self, other: &Node<O>) -> Slice {
        self.slice.clone().spanning(&other.slice)
    }

    pub fn map<O: Clone, F: Fn(Rc<T>) -> Rc<O>>(self, f: F) -> Node<O> {
        Node {
            parent: self.parent,
            slice: self.slice,
            this: f(self.this),
        }
    }

    pub fn map_deep<O: Clone, F: Fn(T) -> O>(&self, f: F) -> Node<O> {
        Node {
            parent: self.parent.clone(),
            slice: self.slice.clone(),
            this: Rc::new(f(self.this.as_ref().clone())),
        }
    }
}

pub trait Nodeable: Clone + std::fmt::Debug + PartialEq {
    fn with_slice(self, slice: Slice) -> Node<Self> {
        Node {
            parent: None,
            slice,
            this: Rc::new(self),
        }
    }
}

impl<T: Clone + std::fmt::Debug + PartialEq> Nodeable for T {}
