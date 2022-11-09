use crate::ast::{Declaration, Expression, InlineConstDeclaration, Module, Span, ValueDeclaration};

pub trait Resolve {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding>;
}

impl<'a> Resolve for Module<'a> {
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

        None
    }
}

impl<'a> Resolve for Declaration<'a> {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding> {
        match self {
            Declaration::ValueDeclaration {
                span: _,
                name: _,
                type_annotation: _,
                value,
            } => {
                if value.contains(index) {
                    return value.resolve_symbol_within(symbol, index);
                }
            }
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

        None
    }
}

impl<'a> Resolve for Expression<'a> {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding> {
        match self {
            Expression::BinaryOperation {
                span: _,
                left,
                op: _,
                right,
            } => {
                if left.contains(index) {
                    return left.resolve_symbol_within(symbol, index);
                } else if right.contains(index) {
                    return right.resolve_symbol_within(symbol, index);
                }
            }
            Expression::Parenthesis { span: _, inner } => {
                if inner.contains(index) {
                    return inner.resolve_symbol_within(symbol, index);
                }
            }

            Expression::LocalIdentifier { span: _, name: _ } => {}
            Expression::NilLiteral { span: _, p: _ } => {}
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
            Expression::BooleanLiteral { span, p: _, value } => todo!(),
            Expression::StringLiteral { span, value } => todo!(),
            Expression::ExactStringLiteral {
                span,
                tag,
                segments,
            } => todo!(),
            Expression::ArrayLiteral { span, entries } => todo!(),
            Expression::ObjectLiteral { span, entries } => todo!(),
            Expression::NegationOperation { span, inner } => todo!(),
            Expression::Func {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsProc {
                span,
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { span, start, end } => todo!(),
            Expression::Invocation {
                span,
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                span,
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                span,
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                span,
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                span,
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast {
                span,
                inner,
                as_type,
            } => todo!(),
            Expression::ErrorExpression { span, inner } => todo!(),
            Expression::RegularExpression { span, expr, flags } => todo!(),
        };

        None
    }
}

impl<'a> Resolve for InlineConstDeclaration<'a> {
    fn resolve_symbol_within(&self, symbol: &str, index: &usize) -> Option<Binding<'a>> {
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
