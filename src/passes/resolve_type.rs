use crate::model::{
    ast::*,
    bgl_type::{Arg, Type},
    module::{Module, ModulesStore},
};

use super::{check::CheckContext, typeinfer::InferTypeContext};

impl ASTAny {
    pub fn resolve_type<'a>(&self, ctx: ResolveContext<'a>) -> Type {
        match self.details() {
            ASTDetails::GenericParamType { name, extends } => todo!(),
            ASTDetails::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => Type::ProcType {
                args: args
                    .into_iter()
                    .map(|a| {
                        let a = a.downcast();
                        Arg {
                            name: a.name.downcast().0.as_str().to_owned(),
                            type_annotation: a
                                .type_annotation
                                .as_ref()
                                .map(|s| s.resolve_type(ctx)),
                            optional: a.optional,
                        }
                    })
                    .collect(),
                args_spread: args_spread
                    .as_ref()
                    .map(|s| s.resolve_type(ctx))
                    .map(Box::new),
                is_pure: *is_pure,
                is_async: *is_async,
                throws: throws.as_ref().map(|s| s.resolve_type(ctx)).map(Box::new),
            },
            ASTDetails::FuncType {
                args,
                args_spread,
                is_pure,
                is_async: _,
                returns,
            } => Type::FuncType {
                args: args
                    .into_iter()
                    .map(|a| {
                        let a = a.downcast();
                        Arg {
                            name: a.name.downcast().0.as_str().to_owned(),
                            type_annotation: a
                                .type_annotation
                                .as_ref()
                                .map(|s| s.resolve_type(ctx)),
                            optional: a.optional,
                        }
                    })
                    .collect(),
                args_spread: args_spread
                    .as_ref()
                    .map(|s| s.resolve_type(ctx))
                    .map(Box::new),
                is_pure: *is_pure,
                returns: returns
                    .as_ref()
                    .map(|s| s.resolve_type(ctx))
                    .map(Box::new)
                    .unwrap(),
            },
            ASTDetails::GenericType { type_params, inner } => todo!(),
            ASTDetails::BoundGenericType { type_args, generic } => todo!(),
            ASTDetails::ObjectType {
                entries,
                is_interface,
            } => todo!(),
            //  match inner.resolve_type(ctx) {
            //     Type::RecordType {
            //         key_type,
            //         value_type,
            //         mutability,
            //     } => key_type.as_ref().clone(),
            //     Type::ObjectType {
            //         entries,
            //         mutability,
            //         is_interface,
            //     } => Type::UnionType(
            //         entries
            //             .into_iter()
            //             .map(|(key, _)| Type::StringType(Some(key)))
            //             .collect(),
            //     ),
            //     _ => Type::PoisonedType,
            // },
            ASTDetails::ModifierType { kind, inner } => {
                let inner = Box::new(inner.resolve_type(ctx));

                match kind {
                    ModifierTypeKind::Readonly => Type::ReadonlyType(inner),
                    ModifierTypeKind::Keyof => Type::KeyofType(inner),
                    ModifierTypeKind::Valueof => Type::ValueofType(inner),
                    ModifierTypeKind::Elementof => Type::ElementofType(inner),
                }
            }
            ASTDetails::SpecialType { kind, inner } => {
                let inner = Box::new(inner.resolve_type(ctx));

                match kind {
                    SpecialTypeKind::Iterator => Type::IteratorType(inner),
                    SpecialTypeKind::Plan => Type::PlanType(inner),
                    SpecialTypeKind::Error => Type::ErrorType(inner),
                }
            }
            // match inner.resolve_type(ctx) {
            //     Type::ArrayType(element) => element.as_ref().clone(),
            //     Type::TupleType(members) => Type::UnionType(members),
            //     _ => Type::PoisonedType,
            // },
            ASTDetails::UnionType(members) => {
                Type::UnionType(members.iter().map(|m| m.resolve_type(ctx)).collect())
            }
            ASTDetails::MaybeType(inner) => inner.resolve_type(ctx).union(Type::NilType),
            ASTDetails::NamedType(name) => Type::NamedType {
                module_id: ctx.current_module.module_id.clone(),
                name: name.slice().clone(),
            },
            ASTDetails::RegularExpressionType {} => Type::RegularExpressionType {},
            ASTDetails::RecordType {
                key_type,
                value_type,
            } => Type::RecordType {
                key_type: Box::new(key_type.resolve_type(ctx)),
                value_type: Box::new(value_type.resolve_type(ctx)),
            },
            ASTDetails::ArrayType(element) => Type::ArrayType(Box::new(element.resolve_type(ctx))),
            ASTDetails::TupleType(members) => {
                Type::TupleType(members.iter().map(|x| x.resolve_type(ctx)).collect())
            }
            ASTDetails::StringType => Type::ANY_STRING,
            ASTDetails::NumberType => Type::ANY_NUMBER,
            ASTDetails::BooleanType => Type::ANY_BOOLEAN,
            ASTDetails::StringLiteralType(value) => Type::StringType(Some(value.clone())),
            ASTDetails::NumberLiteralType(value) => {
                let n = Some(value.as_str().parse().unwrap());
                Type::NumberType { min: n, max: n }
            }
            ASTDetails::BooleanLiteralType(value) => Type::BooleanType(Some(*value)),
            ASTDetails::NilType => Type::NilType,
            ASTDetails::ParenthesizedType(inner) => inner.resolve_type(ctx),
            ASTDetails::TypeofType(expression) => expression.infer_type(ctx.into()),
            ASTDetails::UnknownType => Type::UnknownType,
            ASTDetails::PropertyType {
                subject,
                property,
                optional: _,
            } => {
                let subject_type = subject.resolve_type(ctx);

                match subject_type {
                    Type::ObjectType {
                        entries,
                        is_interface: _,
                    } => entries
                        .into_iter()
                        .find(|(key, value)| key.as_str() == property.slice().as_str())
                        .map(|(key, value)| value.as_ref().clone())
                        .unwrap_or(Type::PoisonedType),
                    _ => Type::PoisonedType,
                }
            }

            ASTDetails::Arg {
                name,
                type_annotation,
                optional,
            } => unreachable!(),
            ASTDetails::ExactStringLiteral { tag: _, value: _ } => unreachable!(),
            ASTDetails::NumberLiteral(_) => unreachable!(),
            ASTDetails::BooleanLiteral(_) => unreachable!(),
            ASTDetails::Module { declarations } => unreachable!(),
            ASTDetails::ImportAllDeclaration { name, path } => unreachable!(),
            ASTDetails::ImportDeclaration { imports, path } => unreachable!(),
            ASTDetails::ImportItem { name, alias } => unreachable!(),
            ASTDetails::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => unreachable!(),
            ASTDetails::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => unreachable!(),
            ASTDetails::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => unreachable!(),
            ASTDetails::Decorator { name } => unreachable!(),
            ASTDetails::ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            } => unreachable!(),
            ASTDetails::TestExprDeclaration { name, expr } => unreachable!(),
            ASTDetails::TestBlockDeclaration { name, block } => unreachable!(),
            ASTDetails::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => unreachable!(),
            ASTDetails::NilLiteral => unreachable!(),
            ASTDetails::StringLiteral { tag, segments } => unreachable!(),
            ASTDetails::ArrayLiteral(_) => unreachable!(),
            ASTDetails::ObjectLiteral(_) => unreachable!(),
            ASTDetails::BinaryOperation { left, op, right } => unreachable!(),
            ASTDetails::BinaryOperator(_) => unreachable!(),
            ASTDetails::NegationOperation(_) => unreachable!(),
            ASTDetails::Parenthesis(_) => unreachable!(),
            ASTDetails::LocalIdentifier(_) => unreachable!(),
            ASTDetails::InlineConstGroup {
                declarations,
                inner,
            } => unreachable!(),
            ASTDetails::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => unreachable!(),
            ASTDetails::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => unreachable!(),
            ASTDetails::Block(_) => unreachable!(),
            ASTDetails::JavascriptEscape(_) => unreachable!(),
            ASTDetails::RangeExpression { start, end } => unreachable!(),
            ASTDetails::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => unreachable!(),
            ASTDetails::PropertyAccessor {
                subject,
                property,
                optional,
            } => unreachable!(),
            ASTDetails::IfElseExpression {
                cases,
                default_case,
            } => unreachable!(),
            ASTDetails::IfElseExpressionCase { condition, outcome } => unreachable!(),
            ASTDetails::SwitchExpression {
                value,
                cases,
                default_case,
            } => unreachable!(),
            ASTDetails::SwitchExpressionCase {
                type_filter,
                outcome,
            } => unreachable!(),
            ASTDetails::ElementTag {
                tag_name,
                attributes,
                children,
            } => unreachable!(),
            ASTDetails::AsCast { inner, as_type } => unreachable!(),
            ASTDetails::InstanceOf {
                inner,
                possible_type,
            } => unreachable!(),
            ASTDetails::ErrorExpression(_) => unreachable!(),
            ASTDetails::RegularExpression { expr, flags } => unreachable!(),
            ASTDetails::TypeParam { name, extends } => unreachable!(),
            ASTDetails::DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            } => unreachable!(),
            ASTDetails::IfElseStatement {
                cases,
                default_case,
            } => unreachable!(),
            ASTDetails::IfElseStatementCase { condition, outcome } => unreachable!(),
            ASTDetails::ForLoop {
                item_identifier,
                iterator,
                body,
            } => unreachable!(),
            ASTDetails::WhileLoop { condition, body } => unreachable!(),
            ASTDetails::Assignment {
                target,
                value,
                operator,
            } => unreachable!(),
            ASTDetails::TryCatch {
                try_block,
                error_identifier,
                catch_block,
            } => unreachable!(),
            ASTDetails::ThrowStatement { error_expression } => unreachable!(),
            ASTDetails::Autorun {
                effect_block,
                until,
            } => unreachable!(),
            ASTDetails::PlainIdentifier(_) => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ResolveContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
}

impl<'a> From<InferTypeContext<'a>> for ResolveContext<'a> {
    fn from(
        InferTypeContext {
            modules,
            current_module,
        }: InferTypeContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a> From<CheckContext<'a>> for ResolveContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
        }: CheckContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}
