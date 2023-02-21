use std::rc::Rc;

use crate::{
    model::ast::*,
    model::{
        ast::Any,
        bgl_type::{Mutability, Type},
        module::Module,
        slice::Slice,
    },
    passes::check::CheckContext,
    ModulesStore,
};

use super::resolve_type::ResolveContext;

impl AST<Expression> {
    pub fn infer_type<'a>(&self, ctx: InferTypeContext<'a>) -> Type {
        self.refine(
            ctx,
            self,
            match self.downcast() {
                Expression::BinaryOperation(op) => binary_operation_type(ctx, &op),
                Expression::Parenthesis(Parenthesis(inner)) => inner.infer_type(ctx),
                Expression::LocalIdentifier(LocalIdentifier(name)) => {
                    let resolved = self.resolve_symbol(name.as_str());

                    if let Some(resolved) = resolved {
                        match resolved.details() {
                            Any::ImportAllDeclaration(ImportAllDeclaration {
                                platforms,
                                name,
                                path,
                            }) => {
                                todo!()
                            }
                            Any::ImportDeclaration(ImportDeclaration {
                                platforms,
                                imports: _,
                                path,
                            }) => ctx
                                .modules
                                .import(
                                    ctx.current_module.module_id(),
                                    path.downcast().value.as_str(),
                                )
                                .map(|other_module| {
                                    other_module.get_declaration(name.as_str(), true)
                                })
                                .flatten()
                                .map(|decl| decl.declaration_type(ctx))
                                .flatten()
                                .unwrap_or(Type::PoisonedType),
                            Any::ProcDeclaration(ProcDeclaration {
                                name: _,
                                proc,
                                exported: _,
                                platforms: _,
                                decorators: _,
                            }) => proc.clone().recast::<Expression>().infer_type(ctx),
                            Any::FuncDeclaration(FuncDeclaration {
                                name: _,
                                func,
                                exported: _,
                                platforms: _,
                                decorators: _,
                            }) => func.clone().recast::<Expression>().infer_type(ctx),
                            Any::ValueDeclaration(ValueDeclaration {
                                destination,
                                value,
                                is_const,
                                exported: _,
                                platforms: _,
                            }) => match destination {
                                DeclarationDestination::NameAndType(NameAndType {
                                    name: _,
                                    type_annotation,
                                }) => type_annotation
                                    .as_ref()
                                    .map(|x| x.resolve_type(ctx.into()))
                                    .unwrap_or_else(|| {
                                        let base_type = value.infer_type(ctx);

                                        if *is_const {
                                            base_type
                                        } else {
                                            base_type.broaden_for_mutation()
                                        }
                                    }),
                                DeclarationDestination::Destructure(_) => todo!(),
                            },
                            Any::Arg(Arg {
                                name,
                                type_annotation,
                                optional,
                            }) => type_annotation
                                .as_ref()
                                .map(|t| t.resolve_type(ctx.into()))
                                .unwrap_or(Type::PoisonedType),
                            Any::InlineDeclaration(InlineDeclaration { destination, value }) => {
                                match destination {
                                    DeclarationDestination::NameAndType(NameAndType {
                                        name: _,
                                        type_annotation,
                                    }) => type_annotation
                                        .as_ref()
                                        .map(|x| x.resolve_type(ctx.into()))
                                        .unwrap_or_else(|| value.infer_type(ctx)),
                                    DeclarationDestination::Destructure(Destructure {
                                        properties,
                                        spread,
                                        destructure_kind,
                                    }) => {
                                        if let Some((index, property)) = properties
                                            .iter()
                                            .enumerate()
                                            .find(|(_, p)| p.downcast().0.as_str() == name.as_str())
                                        {
                                            Type::PropertyType {
                                                subject: Rc::new(value.infer_type(ctx)),
                                                property: match destructure_kind {
                                                    DestructureKind::Array => {
                                                        Rc::new(Type::NumberType {
                                                            min: Some(index as i32),
                                                            max: Some(index as i32),
                                                        })
                                                    }
                                                    DestructureKind::Object => Rc::new(
                                                        identifier_to_string_type(property.clone())
                                                            .recast::<TypeExpression>()
                                                            .resolve_type(ctx.into()),
                                                    ),
                                                },
                                            }
                                        } else {
                                            // spread
                                            todo!()
                                        }
                                    }
                                }
                            }
                            Any::TryCatch(TryCatch {
                                try_block,
                                error_identifier: _,
                                catch_block: _,
                            }) => {
                                let try_block_throws =
                                    try_block.clone().recast::<Statement>().throws(ctx.into());

                                match try_block_throws {
                                    Some(try_block_throws) => try_block_throws,
                                    None => Type::PoisonedType,
                                }
                            }
                            _ => Type::PoisonedType,
                        }
                    } else {
                        Type::PoisonedType
                    }
                }
                Expression::InlineConstGroup(InlineConstGroup {
                    declarations: _,
                    inner,
                }) => inner.infer_type(ctx),
                Expression::NilLiteral(_) => Type::NilType,
                Expression::NumberLiteral(NumberLiteral(value)) => {
                    let n = Some(value.as_str().parse().unwrap());

                    Type::NumberType { min: n, max: n }
                }
                Expression::BooleanLiteral(BooleanLiteral(value)) => Type::BooleanType(Some(value)),
                Expression::ExactStringLiteral(ExactStringLiteral { value, tag: _ }) => {
                    Type::StringType(Some(value.clone()))
                }
                Expression::StringLiteral(StringLiteral {
                    tag: _,
                    segments: _,
                }) => Type::ANY_STRING,
                Expression::ArrayLiteral(ArrayLiteral(members)) => Type::TupleType {
                    mutability: Mutability::Literal,
                    members: members
                        .into_iter()
                        .map(|member| match member {
                            ElementOrSpread::Element(element) => {
                                ElementOrSpread::Element(element.infer_type(ctx))
                            }
                            ElementOrSpread::Spread(spread) => {
                                ElementOrSpread::Spread(spread.infer_type(ctx))
                            }
                        })
                        .collect(),
                },
                Expression::ObjectLiteral(ObjectLiteral(entries)) => Type::ObjectType {
                    mutability: Mutability::Literal,
                    entries: entries
                        .into_iter()
                        .map(|entry| match entry {
                            KeyValueOrSpread::KeyValue(key, value, _) => {
                                KeyValueOrSpread::KeyValue(
                                    key.infer_type(ctx),
                                    value.infer_type(ctx),
                                    false,
                                )
                            }
                            KeyValueOrSpread::Spread(expr) => {
                                KeyValueOrSpread::Spread(expr.infer_type(ctx))
                            }
                        })
                        .collect(),
                    is_interface: false,
                },
                Expression::NegationOperation(NegationOperation(_)) => Type::BooleanType(None),
                Expression::Func(Func {
                    type_annotation,
                    is_async: _,
                    is_pure,
                    body,
                }) => {
                    let type_annotation = type_annotation.downcast();

                    Type::FuncType {
                        args: type_annotation
                            .args
                            .into_iter()
                            .map(|a| {
                                a.downcast()
                                    .type_annotation
                                    .map(|a| a.resolve_type(ctx.into()))
                            })
                            .collect(),
                        args_spread: type_annotation
                            .args_spread
                            .as_ref()
                            .map(|s| s.downcast().type_annotation.clone())
                            .flatten()
                            .map(|s| s.resolve_type(ctx.into()))
                            .map(Rc::new),
                        is_pure,
                        returns: Rc::new(
                            type_annotation
                                .returns
                                .map(|r| r.resolve_type(ctx.into()))
                                .unwrap_or_else(|| body.infer_type(ctx)),
                        ),
                    }
                }
                Expression::Proc(Proc {
                    type_annotation,
                    is_async,
                    is_pure,
                    body,
                }) => {
                    let type_annotation = type_annotation.downcast();

                    Type::ProcType {
                        args: type_annotation
                            .args
                            .into_iter()
                            .map(|a| {
                                a.downcast()
                                    .type_annotation
                                    .map(|a| a.resolve_type(ctx.into()))
                            })
                            .collect(),
                        args_spread: type_annotation
                            .args_spread
                            .as_ref()
                            .map(|s| s.downcast().type_annotation.clone())
                            .flatten()
                            .map(|s| s.resolve_type(ctx.into()))
                            .map(Rc::new),
                        is_async,
                        is_pure,
                        throws: type_annotation
                            .throws
                            .map(|throws| throws.resolve_type(ctx.into()))
                            .or_else(|| body.throws(ctx))
                            .map(Rc::new),
                    }
                }
                Expression::JavascriptEscape(_) => Type::AnyType,
                Expression::RangeExpression(RangeExpression { start, end }) => {
                    let min = start.infer_type(ctx).to_exact_number();
                    let max = end.infer_type(ctx).to_exact_number();

                    Type::SpecialType {
                        kind: SpecialTypeKind::Iterator,
                        inner: Rc::new(Type::NumberType { min, max }),
                    }
                }
                Expression::AwaitExpression(AwaitExpression(inner)) => Type::InnerType {
                    kind: SpecialTypeKind::Plan,
                    inner: Rc::new(inner.infer_type(ctx)),
                },
                Expression::Invocation(Invocation {
                    subject,
                    args,
                    spread_args,
                    type_args,
                    bubbles,
                    awaited_or_detached,
                }) => {
                    let subject_type = subject.infer_type(ctx);

                    if let Type::FuncType {
                        args: _,
                        args_spread: _,
                        is_pure: _,
                        returns,
                    } = subject_type
                    {
                        returns.as_ref().clone()
                    } else {
                        Type::PoisonedType
                    }
                }
                Expression::PropertyAccessor(PropertyAccessor {
                    subject,
                    property,
                    optional,
                }) => {
                    // TODO: optional

                    println!(
                        "{}",
                        Type::PropertyType {
                            subject: Rc::new(subject.infer_type(ctx)),
                            property: Rc::new(match property.clone() {
                                Property::Expression(expr) => expr.infer_type(ctx.into()),
                                Property::PlainIdentifier(ident) => {
                                    Type::StringType(Some(ident.downcast().0.clone()))
                                }
                            }),
                        }
                    );

                    Type::PropertyType {
                        subject: Rc::new(subject.infer_type(ctx)),
                        property: Rc::new(match property {
                            Property::Expression(expr) => expr.infer_type(ctx.into()),
                            Property::PlainIdentifier(ident) => {
                                Type::StringType(Some(ident.downcast().0.clone()))
                            }
                        }),
                    }
                }
                Expression::IfElseExpression(IfElseExpression {
                    cases,
                    default_case,
                }) => Type::UnionType(
                    cases
                        .iter()
                        .map(|case| case.downcast().outcome.infer_type(ctx))
                        .chain(
                            default_case
                                .as_ref()
                                .map(|case| case.infer_type(ctx))
                                .into_iter(),
                        )
                        .collect(),
                ),
                Expression::SwitchExpression(SwitchExpression {
                    value,
                    cases,
                    default_case,
                }) => Type::UnionType(
                    cases
                        .iter()
                        .map(|case| case.downcast().outcome.infer_type(ctx))
                        .chain(
                            default_case
                                .as_ref()
                                .map(|case| case.infer_type(ctx))
                                .into_iter(),
                        )
                        .collect(),
                ),
                Expression::ElementTag(ElementTag {
                    tag_name,
                    attributes,
                    children,
                }) => Type::ObjectType {
                    mutability: Mutability::Literal,
                    entries: vec![
                        KeyValueOrSpread::KeyValue(
                            Type::StringType(Some(Slice::new(Rc::new(String::from("tag"))))),
                            Type::StringType(Some(tag_name.downcast().0.clone())),
                            false,
                        ),
                        KeyValueOrSpread::KeyValue(
                            Type::StringType(Some(Slice::new(Rc::new(String::from("attributes"))))),
                            Type::ObjectType {
                                mutability: Mutability::Literal,
                                entries: attributes
                                    .into_iter()
                                    .map(|(key, value)| {
                                        KeyValueOrSpread::KeyValue(
                                            identifier_to_string_type(key)
                                                .recast::<TypeExpression>()
                                                .resolve_type(ctx.into()),
                                            value.infer_type(ctx),
                                            false,
                                        )
                                    })
                                    .collect(),
                                is_interface: false,
                            },
                            false,
                        ),
                        KeyValueOrSpread::KeyValue(
                            Type::StringType(Some(Slice::new(Rc::new(String::from("children"))))),
                            Type::TupleType {
                                mutability: Mutability::Literal,
                                members: children
                                    .into_iter()
                                    .map(|child| ElementOrSpread::Element(child.infer_type(ctx)))
                                    .collect(),
                            },
                            false,
                        ),
                    ],
                    is_interface: false,
                },
                Expression::AsCast(AsCast { inner: _, as_type }) => {
                    as_type.resolve_type(ctx.into())
                }
                Expression::InstanceOf(InstanceOf {
                    inner: _,
                    possible_type: _,
                }) => Type::ANY_BOOLEAN,
                Expression::ErrorExpression(ErrorExpression(inner)) => Type::SpecialType {
                    kind: SpecialTypeKind::Error,
                    inner: Rc::new(inner.infer_type(ctx)),
                },
                Expression::RegularExpression(RegularExpression { expr, flags }) => {
                    Type::RegularExpressionType
                }
                Expression::AnyLiteral(_) => Type::AnyType,
            },
        )
    }
}

pub fn binary_operation_type<'a>(
    ctx: InferTypeContext<'a>,
    BinaryOperation { left, op, right }: &BinaryOperation,
) -> Type {
    match op.downcast().0 {
        BinaryOperatorOp::NullishCoalescing => todo!(),
        BinaryOperatorOp::Or => left
            .infer_type(ctx)
            .narrow(ctx.into(), &Type::BooleanType(Some(true)))
            .union(right.infer_type(ctx)),
        BinaryOperatorOp::And => left
            .infer_type(ctx)
            .narrow(ctx.into(), &Type::BooleanType(Some(false)))
            .union(right.infer_type(ctx)),
        BinaryOperatorOp::Equals => Type::ANY_BOOLEAN,
        BinaryOperatorOp::NotEquals => Type::ANY_BOOLEAN,
        BinaryOperatorOp::LessEqual => Type::ANY_BOOLEAN,
        BinaryOperatorOp::GreaterEqual => Type::ANY_BOOLEAN,
        BinaryOperatorOp::Less => Type::ANY_BOOLEAN,
        BinaryOperatorOp::Greater => Type::ANY_BOOLEAN,
        BinaryOperatorOp::Plus => {
            let left_type = left.infer_type(ctx);
            let right_type = right.infer_type(ctx);

            if let (Some(left), Some(right)) =
                (left_type.to_exact_number(), right_type.to_exact_number())
            {
                let result = Some(left + right);
                Type::NumberType {
                    min: result,
                    max: result,
                }
            } else if Type::ANY_NUMBER.subsumes(ctx.into(), &left_type) {
                if Type::ANY_NUMBER.subsumes(ctx.into(), &right_type) {
                    Type::ANY_NUMBER
                } else if Type::ANY_STRING.subsumes(ctx.into(), &right_type) {
                    Type::ANY_STRING
                } else {
                    Type::UnknownType(Mutability::Mutable)
                }
            } else if Type::ANY_STRING.subsumes(ctx.into(), &left_type) {
                if Type::ANY_NUMBER.subsumes(ctx.into(), &right_type)
                    || Type::ANY_STRING.subsumes(ctx.into(), &right_type)
                {
                    Type::ANY_STRING
                } else {
                    Type::UnknownType(Mutability::Mutable)
                }
            } else {
                Type::UnknownType(Mutability::Mutable)
            }
        }
        BinaryOperatorOp::Minus => {
            let left_type = left.infer_type(ctx);
            let right_type = right.infer_type(ctx);

            if let (Some(left), Some(right)) =
                (left_type.to_exact_number(), right_type.to_exact_number())
            {
                let result = Some(left - right);
                Type::NumberType {
                    min: result,
                    max: result,
                }
            } else {
                Type::ANY_NUMBER
            }
        }
        BinaryOperatorOp::Times => {
            let left_type = left.infer_type(ctx);
            let right_type = right.infer_type(ctx);

            if let (Some(left), Some(right)) =
                (left_type.to_exact_number(), right_type.to_exact_number())
            {
                let result = Some(left * right);
                Type::NumberType {
                    min: result,
                    max: result,
                }
            } else {
                Type::ANY_NUMBER
            }
        }
        BinaryOperatorOp::Divide => {
            let left_type = left.infer_type(ctx);
            let right_type = right.infer_type(ctx);

            if let (Some(left), Some(right)) =
                (left_type.to_exact_number(), right_type.to_exact_number())
            {
                let result = Some(left / right);
                Type::NumberType {
                    min: result,
                    max: result,
                }
            } else {
                Type::ANY_NUMBER
            }
        }
    }
}

impl ASTAny {
    pub fn refine<'a>(
        &self,
        ctx: InferTypeContext<'a>,
        expr_to_refine: &AST<Expression>,
        type_to_refine: Type,
    ) -> Type {
        // short-circuit expressions that can never be refined
        match expr_to_refine.downcast() {
            Expression::NilLiteral(_) => return type_to_refine,
            Expression::StringLiteral(_) => return type_to_refine,
            Expression::NumberLiteral(_) => return type_to_refine,
            Expression::BooleanLiteral(_) => return type_to_refine,
            _ => {}
        };

        match self.parent() {
            Some(parent) => parent.refine(
                ctx,
                expr_to_refine,
                match parent.details() {
                    Any::IfElseExpressionCase(IfElseExpressionCase { condition, outcome }) => {
                        if self.ptr_eq(outcome) {
                            match condition.details() {
                                Any::BinaryOperation(BinaryOperation { left, op, right }) => {
                                    match op.downcast().0 {
                                        BinaryOperatorOp::Equals => {
                                            if left.details() == expr_to_refine.details() {
                                                type_to_refine
                                                    .narrow(ctx.into(), &right.infer_type(ctx))
                                            } else if right.details() == expr_to_refine.details() {
                                                type_to_refine
                                                    .narrow(ctx.into(), &left.infer_type(ctx))
                                            } else {
                                                type_to_refine
                                            }
                                        }
                                        BinaryOperatorOp::NotEquals => {
                                            if left.details() == expr_to_refine.details() {
                                                type_to_refine
                                                    .subtract(ctx.into(), &right.infer_type(ctx))
                                            } else if right.details() == expr_to_refine.details() {
                                                type_to_refine
                                                    .subtract(ctx.into(), &left.infer_type(ctx))
                                            } else {
                                                type_to_refine
                                            }
                                        }
                                        _ => type_to_refine,
                                    }
                                }
                                _ => type_to_refine,
                            }
                        } else {
                            type_to_refine
                        }
                    }
                    _ => type_to_refine,
                },
            ),
            None => type_to_refine,
        }
    }
}

impl AST<Expression> {
    pub fn refine<'a>(
        &self,
        ctx: InferTypeContext<'a>,
        expr_to_refine: &AST<Expression>,
        type_to_refine: Type,
    ) -> Type {
        self.clone()
            .upcast()
            .refine(ctx, expr_to_refine, type_to_refine)
    }
}

impl AST<Declaration> {
    pub fn declaration_type<'a>(&self, ctx: InferTypeContext<'a>) -> Option<Type> {
        match self.downcast() {
            Declaration::FuncDeclaration(FuncDeclaration {
                name: _,
                func,
                exported: _,
                platforms: _,
                decorators: _,
            }) => Some(func.recast::<Expression>().infer_type(ctx)),
            Declaration::ProcDeclaration(ProcDeclaration {
                name: _,
                proc,
                exported: _,
                platforms: _,
                decorators: _,
            }) => Some(proc.recast::<Expression>().infer_type(ctx)),
            Declaration::ValueDeclaration(ValueDeclaration {
                destination,
                value,
                is_const: _,
                exported: _,
                platforms: _,
            }) => match destination {
                DeclarationDestination::NameAndType(NameAndType {
                    name: _,
                    type_annotation,
                }) => Some(
                    type_annotation
                        .map(|t| t.resolve_type(ctx.into()))
                        .unwrap_or_else(|| value.infer_type(ctx)),
                ),
                DeclarationDestination::Destructure(_) => None,
            },
            Declaration::SymbolDeclaration(SymbolDeclaration { name, exported: _ }) => {
                Some(Type::SymbolType {
                    module_id: self.clone().upcast().module_id().unwrap(),
                    name: name.downcast().0.clone(),
                })
            }
            Declaration::ImportAllDeclaration(_) => None,
            Declaration::ImportDeclaration(_) => None,
            Declaration::TypeDeclaration(_) => None,
            Declaration::TestExprDeclaration(_) => None,
            Declaration::TestBlockDeclaration(_) => None,
            Declaration::TestTypeDeclaration(_) => None,
        }
    }
}

impl AST<Statement> {
    pub fn throws<'a>(&self, ctx: InferTypeContext<'a>) -> Option<Type> {
        match self.downcast() {
            Statement::ValueDeclaration(_) => None,
            Statement::IfElseStatement(IfElseStatement {
                cases,
                default_case,
            }) => {
                let mut all = Vec::new();

                for case in cases {
                    if let Some(throws) = case.downcast().outcome.recast::<Statement>().throws(ctx)
                    {
                        all.push(throws);
                    }
                }
                if let Some(default_case) = default_case {
                    if let Some(throws) = default_case.recast::<Statement>().throws(ctx) {
                        all.push(throws);
                    }
                }

                match all.len() {
                    0 => None,
                    1 => Some(all.remove(0)),
                    _ => Some(Type::UnionType(all)),
                }
            }
            Statement::ForLoop(ForLoop {
                item_identifier: _,
                iterator: _,
                body,
            }) => body.recast::<Statement>().throws(ctx),
            Statement::WhileLoop(WhileLoop { condition: _, body }) => {
                body.recast::<Statement>().throws(ctx)
            }
            Statement::Assignment(_) => None,
            Statement::TryCatch(_) => todo!(),
            Statement::ThrowStatement(ThrowStatement { error_expression }) => {
                Some(error_expression.infer_type(ctx))
            }
            Statement::Autorun(_) => None,
            Statement::Invocation(Invocation {
                subject,
                args: _,
                spread_args: _,
                type_args: _,
                bubbles: _,
                awaited_or_detached: _,
            }) => match subject.infer_type(ctx) {
                Type::ProcType {
                    args: _,
                    args_spread: _,
                    is_pure: _,
                    is_async: _,
                    throws,
                } => throws.map(|rc| rc.as_ref().clone()),
                _ => None,
            },
            Statement::Block(Block(statements)) => {
                let mut all = Vec::new();

                for stmt in statements {
                    if let Some(throws) = stmt.throws(ctx) {
                        all.push(throws);
                    }
                }

                match all.len() {
                    0 => None,
                    1 => Some(all.remove(0)),
                    _ => Some(Type::UnionType(all)),
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InferTypeContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
}

impl<'a> From<&CheckContext<'a>> for InferTypeContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            nearest_func_or_proc: _,
        }: &CheckContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}

impl<'a> From<ResolveContext<'a>> for InferTypeContext<'a> {
    fn from(
        ResolveContext {
            modules,
            current_module,
        }: ResolveContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
        }
    }
}
