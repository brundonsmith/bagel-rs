use crate::ast::{Declaration, Expression, InlineConstDeclaration, Module, Span, ValueDeclaration};

pub trait Resolve {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding>;
}

impl Resolve for Module {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding> {
        // look inside nested contexts
        for decl in &self.declarations {
            let inner = decl.resolve_symbol_within(symbol, index);

            if inner.is_some() {
                return inner;
            }
        }

        // look in module declarations
        for decl in &self.declarations {
            match decl {
                Declaration::ValueDeclaration {
                    span: _,
                    name,
                    type_annotation: _,
                    value: _,
                } => {
                    if name.name == symbol {
                        return Some(Binding::ValueDeclaration(
                            ValueDeclaration::try_from(decl.clone()).unwrap(),
                        ));
                    }
                }
            }
        }

        None
    }
}

impl Resolve for Declaration {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding> {
        match self {
            Declaration::ValueDeclaration {
                span: _,
                name: _,
                type_annotation: _,
                value,
            } => {
                if value.span().contains(index) {
                    return value.resolve_symbol_within(symbol, index);
                }
            }
        }

        None
    }
}

impl Resolve for Expression {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding> {
        match self {
            Expression::BinaryOperation {
                span: _,
                left,
                op: _,
                right,
            } => {
                if left.span().contains(index) {
                    return left.resolve_symbol_within(symbol, index);
                } else if right.span().contains(index) {
                    return right.resolve_symbol_within(symbol, index);
                }
            }
            Expression::Parenthesis { span: _, inner } => {
                if inner.span().contains(index) {
                    return inner.resolve_symbol_within(symbol, index);
                }
            }

            Expression::LocalIdentifier { span: _, name: _ } => {}
            Expression::NilLiteral { span: _ } => {}
            Expression::NumberLiteral { span: _, value: _ } => {}
            Expression::InlineConstGroup {
                span: _,
                declarations,
                inner: _,
            } => {
                for decl in declarations {
                    let inner = decl.resolve_symbol_within(symbol, index);

                    if inner.is_some() {
                        return inner;
                    }
                }
            }
        };

        None
    }
}

impl Resolve for InlineConstDeclaration {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding> {
        if self.name.name == symbol {
            Some(Binding::InlineConstDeclaration(self.clone()))
        } else {
            None
        }
    }
}

pub enum Binding {
    ValueDeclaration(ValueDeclaration),
    InlineConstDeclaration(InlineConstDeclaration),
}
