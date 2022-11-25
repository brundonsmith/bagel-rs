use crate::{ast::*, slice::Slice};

pub trait Resolve {
    fn resolve_symbol_within(&self, symbol: &str, slice: &Slice) -> Option<Binding>;
}

impl Resolve for Module {
    fn resolve_symbol_within(&self, symbol: &str, slice: &Slice) -> Option<Binding> {
        // look inside nested contexts
        for decl in &self.declarations {
            let inner = decl.resolve_symbol_within(symbol, &slice);

            if inner.is_some() {
                return inner;
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
                    if name.node.name.as_str() == symbol {
                        return Some(Binding::ValueDeclaration(
                            ValueDeclaration::try_from(decl.node.clone()).unwrap(),
                        ));
                    }
                }
                Declaration::ImportAllDeclaration { name, path } => todo!(),
                Declaration::ImportDeclaration { imports, path } => todo!(),
                Declaration::TypeDeclaration {
                    name,
                    declared_type,
                    exported,
                } => {
                    if name.node.name.as_str() == symbol {
                        return Some(Binding::TypeDeclaration(
                            TypeDeclaration::try_from(decl.node.clone()).unwrap(),
                        ));
                    }
                }
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
        }

        None
    }
}

impl Resolve for Src<Declaration> {
    fn resolve_symbol_within(&self, symbol: &str, slice: &Slice) -> Option<Binding> {
        match &self.node {
            Declaration::ValueDeclaration {
                name: _,
                type_annotation: _,
                value,
                is_const: _,
                exported: _,
                platforms: _,
            } => {
                if value.contains(&slice) {
                    return value.resolve_symbol_within(symbol, &slice);
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
    fn resolve_symbol_within(&self, symbol: &str, slice: &Slice) -> Option<Binding> {
        match &self.node {
            Expression::BinaryOperation { left, op: _, right } => {
                if left.contains(slice) {
                    return left.resolve_symbol_within(symbol, slice);
                } else if right.contains(slice) {
                    return right.resolve_symbol_within(symbol, slice);
                }
            }
            Expression::Parenthesis { inner } => {
                if inner.contains(slice) {
                    return inner.resolve_symbol_within(symbol, slice);
                }
            }

            Expression::LocalIdentifier { name: _ } => {}
            Expression::NilLiteral => {}
            Expression::NumberLiteral { value: _ } => {}
            Expression::InlineConstGroup {
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
            Expression::BooleanLiteral { value } => todo!(),
            Expression::StringLiteral { tag, segments } => todo!(),
            Expression::ExactStringLiteral { tag, value } => todo!(),
            Expression::ArrayLiteral { entries } => todo!(),
            Expression::ObjectLiteral { entries } => todo!(),
            Expression::NegationOperation { inner } => todo!(),
            Expression::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            Expression::JsFunc {
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
            Expression::JsProc {
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
            Expression::ErrorExpression { inner } => todo!(),
            Expression::RegularExpression { expr, flags } => todo!(),
        };

        None
    }
}

impl Resolve for Src<InlineConstDeclaration> {
    fn resolve_symbol_within(&self, symbol: &str, slice: &Slice) -> Option<Binding> {
        if self.node.name.name.as_str() == symbol {
            Some(Binding::InlineConstDeclaration(self.node.clone()))
        } else {
            None
        }
    }
}

pub enum Binding {
    ValueDeclaration(ValueDeclaration),
    InlineConstDeclaration(InlineConstDeclaration),
    TypeDeclaration(TypeDeclaration),
}
