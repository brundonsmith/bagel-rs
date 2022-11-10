use crate::ast::{
    Declaration, Expression, InlineConstDeclaration, Module, Sourced, ValueDeclaration,
};

pub trait Resolve {
    fn resolve_symbol_within(&self, symbol: &str, slice: &str) -> Option<Binding>;
}

impl<'a> Resolve for Module<'a> {
    fn resolve_symbol_within(&self, symbol: &str, slice: &str) -> Option<Binding> {
        // look inside nested contexts
        for decl in &self.declarations {
            let inner = decl.resolve_symbol_within(symbol, slice);

            if inner.is_some() {
                return inner;
            }
        }

        // look in module declarations
        for decl in &self.declarations {
            match decl {
                Declaration::ValueDeclaration {
                    src: _,
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

        None
    }
}

impl<'a> Resolve for Declaration<'a> {
    fn resolve_symbol_within(&self, symbol: &str, slice: &str) -> Option<Binding> {
        match self {
            Declaration::ValueDeclaration {
                src: _,
                name: _,
                type_annotation: _,
                value,
            } => {
                if value.contains(slice) {
                    return value.resolve_symbol_within(symbol, slice);
                }
            }
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

        None
    }
}

impl<'a> Resolve for Expression<'a> {
    fn resolve_symbol_within(&self, symbol: &str, slice: &str) -> Option<Binding> {
        match self {
            Expression::BinaryOperation {
                src: _,
                left,
                op: _,
                right,
            } => {
                if left.contains(slice) {
                    return left.resolve_symbol_within(symbol, slice);
                } else if right.contains(slice) {
                    return right.resolve_symbol_within(symbol, slice);
                }
            }
            Expression::Parenthesis { src: _, inner } => {
                if inner.contains(slice) {
                    return inner.resolve_symbol_within(symbol, slice);
                }
            }

            Expression::LocalIdentifier { src: _, name: _ } => {}
            Expression::NilLiteral { src: _ } => {}
            Expression::NumberLiteral { src: _, value: _ } => {}
            Expression::InlineConstGroup {
                src: _,
                declarations,
                inner: _,
            } => {
                for decl in declarations {
                    let inner = decl.resolve_symbol_within(symbol, slice);

                    if inner.is_some() {
                        return inner;
                    }
                }
            }
            Expression::BooleanLiteral { src, value } => todo!(),
            Expression::StringLiteral { src, value } => todo!(),
            Expression::ExactStringLiteral { src, tag, segments } => todo!(),
            Expression::ArrayLiteral { src, entries } => todo!(),
            Expression::ObjectLiteral { src, entries } => todo!(),
            Expression::NegationOperation { src, inner } => todo!(),
            Expression::Func {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                src,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { src, start, end } => todo!(),
            Expression::Invocation {
                src,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                src,
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                src,
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                src,
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                src,
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast {
                src,
                inner,
                as_type,
            } => todo!(),
            Expression::ErrorExpression { src, inner } => todo!(),
            Expression::RegularExpression { src, expr, flags } => todo!(),
        };

        None
    }
}

impl<'a> Resolve for InlineConstDeclaration<'a> {
    fn resolve_symbol_within(&self, symbol: &str, slice: &str) -> Option<Binding<'a>> {
        if self.name.name == symbol {
            Some(Binding::InlineConstDeclaration(self.clone()))
        } else {
            None
        }
    }
}

pub enum Binding<'a> {
    ValueDeclaration(ValueDeclaration<'a>),
    InlineConstDeclaration(InlineConstDeclaration<'a>),
}
