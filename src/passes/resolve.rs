use crate::{
    model::ast::*,
    model::{bgl_type::Type, module::Module, slice::Slice},
};

use super::typeinfer::InferTypeContext;

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
            Expression::NumberLiteral(_) => {}
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
            Expression::BooleanLiteral(_) => todo!(),
            Expression::StringLiteral { tag, segments } => todo!(),
            Expression::ExactStringLiteral { tag, value } => todo!(),
            Expression::ArrayLiteral(_) => todo!(),
            Expression::ObjectLiteral(_) => todo!(),
            Expression::NegationOperation(_) => todo!(),
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
            } => {
                for (condition, outcome) in cases {
                    if condition.contains(at) {
                        return condition.resolve_symbol(symbol, at);
                    } else if outcome.contains(at) {
                        return outcome.resolve_symbol(symbol, at);
                    }
                }

                if let Some(default_case) = default_case {
                    if default_case.contains(at) {
                        return default_case.resolve_symbol(symbol, at);
                    }
                }
            }
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
            Expression::ErrorExpression(_) => todo!(),
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

impl<'a> Binding<'a> {
    pub fn contents(self, ctx: InferTypeContext<'a>) -> BindingContents {
        match self {
            Binding::Declaration(decl) => match &decl.node {
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
                Declaration::ValueDeclaration {
                    name,
                    type_annotation,
                    value,
                    is_const,
                    exported,
                    platforms,
                } => todo!(),
                Declaration::TestExprDeclaration { name, expr } => todo!(),
                Declaration::TestBlockDeclaration { name, block } => todo!(),
                Declaration::TestTypeDeclaration {
                    name,
                    destination_type,
                    value_type,
                } => todo!(),
            },
            Binding::InlineConstDeclaration(decl) => BindingContents::ValueWithType(
                decl.node
                    .type_annotation
                    .as_ref()
                    .map(|type_annotation| type_annotation.resolve(ctx.into()))
                    .unwrap_or_else(|| decl.node.value.infer_type(ctx)),
            ),
        }
    }
}

pub enum BindingContents {
    DeclaredType(Type),
    ValueWithType(Type),
}
