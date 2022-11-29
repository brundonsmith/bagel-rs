use crate::{model::ast::*, model::slice::Slice};

pub trait Resolve {
    fn resolve_symbol(&self, symbol: &str, at: &Slice) -> Option<Binding>;
}

impl Resolve for Module {
    fn resolve_symbol(&self, symbol: &str, at: &Slice) -> Option<Binding> {
        // find the declaration that this index is within, and see if anything lower resolves
        for decl in &self.declarations {
            if decl.src.contains(&at) {
                let inner = decl.resolve_symbol(symbol, &at);

                if inner.is_some() {
                    return inner;
                }
            }
        }

        // look in module declarations
        for decl in &self.declarations {
            match &decl.node {
                Declaration::ValueDeclaration {
                    name,
                    type_annotation: _,
                    value: _,
                    is_const: _,
                    exported: _,
                    platforms: _,
                } => {
                    if name.0.as_str() == symbol {
                        return Some(Binding::Declaration(decl));
                    }
                }
                Declaration::ImportAllDeclaration { name, path } => todo!(),
                Declaration::ImportDeclaration { imports, path } => todo!(),
                Declaration::TypeDeclaration {
                    name,
                    declared_type: _,
                    exported: _,
                } => {
                    if name.0.as_str() == symbol {
                        return Some(Binding::Declaration(decl));
                    }
                }
                Declaration::FuncDeclaration {
                    name,
                    func: _,
                    exported: _,
                    platforms: _,
                    decorators: _,
                } => {
                    if name.0.as_str() == symbol {
                        return Some(Binding::Declaration(decl));
                    }
                }
                Declaration::ProcDeclaration {
                    name,
                    proc: _,
                    exported: _,
                    platforms: _,
                    decorators: _,
                } => {
                    if name.0.as_str() == symbol {
                        return Some(Binding::Declaration(decl));
                    }
                }
                Declaration::TestExprDeclaration { name, expr } => todo!(),
                Declaration::TestBlockDeclaration { name, block } => todo!(),
                Declaration::TestTypeDeclaration {
                    name,
                    destination_type,
                    value_type,
                } => todo!(),
            }
        }

        None
    }
}

impl Resolve for Src<Declaration> {
    fn resolve_symbol(&self, symbol: &str, at: &Slice) -> Option<Binding> {
        match &self.node {
            Declaration::ValueDeclaration {
                name: _,
                type_annotation: _,
                value,
                is_const: _,
                exported: _,
                platforms: _,
            } => {
                if value.contains(&at) {
                    return value.resolve_symbol(symbol, &at);
                }
            }
            Declaration::ImportAllDeclaration { name, path } => todo!(),
            Declaration::ImportDeclaration { imports, path } => todo!(),
            Declaration::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => todo!(),
            Declaration::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => todo!(),
            Declaration::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => todo!(),
            Declaration::TestExprDeclaration { name, expr } => todo!(),
            Declaration::TestBlockDeclaration { name, block } => todo!(),
            Declaration::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => todo!(),
        }

        None
    }
}

impl Resolve for Src<Expression> {
    fn resolve_symbol(&self, symbol: &str, at: &Slice) -> Option<Binding> {
        match &self.node {
            Expression::BinaryOperation { left, op: _, right } => {
                if left.contains(at) {
                    return left.resolve_symbol(symbol, at);
                } else if right.contains(at) {
                    return right.resolve_symbol(symbol, at);
                }
            }
            Expression::Parenthesis(inner) => {
                if inner.contains(at) {
                    return inner.resolve_symbol(symbol, at);
                }
            }

            Expression::LocalIdentifier(_) => {}
            Expression::NilLiteral => {}
            Expression::NumberLiteral { value: _ } => {}
            Expression::InlineConstGroup {
                declarations,
                inner: _,
            } => {
                for decl in declarations {
                    let inner = decl.resolve_symbol(symbol, at);

                    if inner.is_some() {
                        return inner;
                    }
                }
            }
            Expression::BooleanLiteral { value } => todo!(),
            Expression::StringLiteral { tag, segments } => todo!(),
            Expression::ExactStringLiteral { tag, value } => todo!(),
            Expression::ArrayLiteral { entries } => todo!(),
            Expression::ObjectLiteral { entries } => todo!(),
            Expression::NegationOperation(inner) => todo!(),
            Expression::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JavascriptEscapeExpression(_) => todo!(),
            Expression::RangeExpression { start, end } => todo!(),
            Expression::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => todo!(),
            Expression::PropertyAccessor {
                subject,
                property,
                optional,
            } => todo!(),
            Expression::IfElseExpression {
                cases,
                default_case,
            } => todo!(),
            Expression::SwitchExpression {
                value,
                cases,
                default_case,
            } => todo!(),
            Expression::ElementTag {
                tag_name,
                attributes,
                children,
            } => todo!(),
            Expression::AsCast { inner, as_type } => todo!(),
            Expression::InstanceOf {
                inner,
                possible_type,
            } => todo!(),
            Expression::ErrorExpression { inner } => todo!(),
            Expression::RegularExpression { expr, flags } => todo!(),
        };

        None
    }
}

impl Resolve for Src<InlineConstDeclaration> {
    fn resolve_symbol(&self, symbol: &str, at: &Slice) -> Option<Binding> {
        if self.node.name.0.as_str() == symbol {
            Some(Binding::InlineConstDeclaration(self))
        } else {
            None
        }
    }
}

pub enum Binding<'a> {
    Declaration(&'a Src<Declaration>),
    InlineConstDeclaration(&'a Src<InlineConstDeclaration>),
}
