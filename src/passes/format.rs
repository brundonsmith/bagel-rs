use crate::model::{
    ast::{self, *},
    module::Module,
};
use std::fmt::{Display, Formatter, Result, Write};

impl Module {
    pub fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        self.ast.format(f, opts)
    }
}

impl Display for ASTAny {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, FormatOptions::DEFAULT)
    }
}

pub trait Formattable {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result;
}

impl<TKind> Formattable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        match self.details() {
            Any::Module(ast::Module { declarations }) => {
                for decl in declarations {
                    decl.format(f, opts)?;
                    f.write_str("\n\n")?;
                }

                Ok(())
            }
            Any::ImportAllDeclaration(ImportAllDeclaration { name, path }) => todo!(),
            Any::ImportDeclaration(ImportDeclaration { imports, path }) => todo!(),
            Any::ImportItem(ImportItem { name, alias }) => todo!(),
            Any::TypeDeclaration(TypeDeclaration {
                name,
                declared_type,
                exported,
            }) => todo!(),
            Any::FuncDeclaration(FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            }) => todo!(),
            Any::ProcDeclaration(ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            }) => todo!(),
            Any::Decorator(Decorator { name }) => todo!(),
            Any::ValueDeclaration(ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            }) => {
                if *exported {
                    f.write_str("export ")?;
                }

                f.write_str(if *is_const { "const " } else { "let " })?;
                name.format(f, opts)?;
                if let Some(type_annotation) = type_annotation {
                    f.write_str(": ")?;
                    type_annotation.format(f, opts)?;
                }
                f.write_str(" = ")?;
                value.format(f, opts)
            }
            Any::TestExprDeclaration(TestExprDeclaration { name, expr }) => todo!(),
            Any::TestBlockDeclaration(TestBlockDeclaration { name, block }) => todo!(),
            Any::TestTypeDeclaration(TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            }) => todo!(),
            Any::NilLiteral(_) => f.write_str("nil"),
            Any::BooleanLiteral(BooleanLiteral(value)) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            Any::NumberLiteral(NumberLiteral(value)) => f.write_str(value.as_str()),
            Any::StringLiteral(StringLiteral { tag, segments }) => todo!(),
            Any::ExactStringLiteral(ExactStringLiteral { tag, value }) => {
                f.write_str("\"")?;
                f.write_str(value.as_str())?;
                f.write_str("\"")
            }
            Any::ArrayLiteral(ArrayLiteral(entries)) => {
                f.write_char('[')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    match entry {
                        ElementOrSpread::Element(element) => {
                            element.format(f, opts)?;
                        }
                        ElementOrSpread::Spread(spread) => {
                            f.write_str("...")?;
                            spread.format(f, opts)?;
                        }
                    }
                }
                f.write_str(" ]")
            }
            Any::ObjectLiteral(ObjectLiteral(entries)) => {
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;

                    match entry {
                        KeyValueOrSpread::KeyValue(key, value) => {
                            key.format(f, opts)?;
                            f.write_str(": ")?;
                            value.format(f, opts)?;
                        }
                        KeyValueOrSpread::Spread(expr) => {
                            f.write_str("...")?;
                            expr.format(f, opts)?;
                        }
                    }
                }
                f.write_str(" }")
            }
            Any::SpreadExpression(SpreadExpression(inner)) => {
                f.write_str("...")?;
                inner.format(f, opts)
            }
            Any::BinaryOperation(BinaryOperation { left, op, right }) => {
                left.format(f, opts)?;
                f.write_char(' ')?;
                op.format(f, opts)?;
                f.write_char(' ')?;
                right.format(f, opts)
            }
            Any::BinaryOperator(BinaryOperator(op)) => f.write_str(op.into()),
            Any::NegationOperation(NegationOperation(_)) => todo!(),
            Any::Parenthesis(Parenthesis(inner)) => {
                f.write_char('(')?;
                inner.format(f, opts)?;
                f.write_char(')')
            }
            Any::LocalIdentifier(LocalIdentifier(name)) => f.write_str(name.as_str()),
            Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner,
            }) => todo!(),
            Any::InlineDeclaration(InlineDeclaration { destination, value }) => todo!(),
            Any::Func(Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                f.write_char('(')?;
                type_annotation.args.format(f, opts)?;
                f.write_char(')')?;
                format_type_annotation(f, opts, type_annotation.returns.as_ref())?;
                f.write_str(" => ")?;

                body.format(f, opts)
            }
            Any::Proc(Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => todo!(),
            Any::Block(Block(_)) => todo!(),
            Any::JavascriptEscape(JavascriptEscape(_)) => todo!(),
            Any::RangeExpression(RangeExpression { start, end }) => {
                start.format(f, opts)?;
                f.write_str("..")?;
                end.format(f, opts)
            }
            Any::AwaitExpression(AwaitExpression(inner)) => {
                f.write_str("await ")?;
                inner.format(f, opts)
            }
            Any::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            }) => todo!(),
            Any::PropertyAccessor(PropertyAccessor {
                subject,
                property,
                optional,
            }) => todo!(),
            Any::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            }) => todo!(),
            Any::IfElseExpressionCase(IfElseExpressionCase { condition, outcome }) => {
                todo!()
            }
            Any::SwitchExpression(SwitchExpression {
                value,
                cases,
                default_case,
            }) => todo!(),
            Any::SwitchExpressionCase(SwitchExpressionCase {
                type_filter,
                outcome,
            }) => todo!(),
            Any::ElementTag(ElementTag {
                tag_name,
                attributes,
                children,
            }) => todo!(),
            Any::AsCast(AsCast { inner, as_type }) => todo!(),
            Any::InstanceOf(InstanceOf {
                inner,
                possible_type,
            }) => todo!(),
            Any::ErrorExpression(ErrorExpression(_)) => todo!(),
            Any::RegularExpression(RegularExpression { expr, flags }) => todo!(),
            Any::UnionType(UnionType(members)) => {
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }

                    member.format(f, opts)?;
                }

                Ok(())
            }
            Any::MaybeType(MaybeType(_)) => todo!(),
            Any::NamedType(NamedType(_)) => todo!(),
            Any::GenericParamType(GenericParamType { name, extends }) => todo!(),
            Any::ProcType(ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            }) => todo!(),
            Any::FuncType(FuncType {
                args,
                args_spread,
                is_pure,
                is_async,
                returns,
            }) => {
                f.write_char('(')?;
                for arg in args {
                    arg.format(f, opts)?;
                }
                f.write_char(')')?;

                f.write_str(" => ")?;

                if let Some(returns) = returns {
                    returns.format(f, opts)?;
                } else {
                    f.write_str("<none>")?;
                }

                Ok(())
            }
            Any::Arg(Arg {
                name,
                type_annotation,
                optional,
            }) => {
                name.format(f, opts)?;
                if *optional {
                    f.write_char('?')?;
                }
                format_type_annotation(f, opts, type_annotation.as_ref())
            }
            Any::GenericType(GenericType { type_params, inner }) => todo!(),
            Any::TypeParam(TypeParam { name, extends }) => todo!(),
            Any::BoundGenericType(BoundGenericType { type_args, generic }) => todo!(),
            Any::ObjectType(ObjectType {
                entries,
                is_interface,
            }) => todo!(),
            Any::RecordType(RecordType {
                key_type,
                value_type,
            }) => todo!(),
            Any::ArrayType(ArrayType(_)) => todo!(),
            Any::TupleType(TupleType(_)) => todo!(),
            Any::SpecialType(SpecialType { kind, inner }) => {
                f.write_str(kind.into())?;
                f.write_char('<')?;
                inner.format(f, opts)?;
                f.write_char('>')
            }
            Any::ModifierType(ModifierType { kind, inner }) => {
                f.write_str(kind.into())?;
                f.write_char(' ')?;
                inner.format(f, opts)
            }
            Any::StringLiteralType(StringLiteralType(value)) => {
                f.write_char('\'')?;
                f.write_str(value.as_str())?;
                f.write_char('\'')
            }
            Any::NumberLiteralType(NumberLiteralType(value)) => f.write_str(value.as_str()),
            Any::BooleanLiteralType(BooleanLiteralType(value)) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            Any::StringType(_) => f.write_str("string"),
            Any::NumberType(_) => f.write_str("number"),
            Any::BooleanType(_) => f.write_str("boolean"),
            Any::NilType(_) => f.write_str("nil"),
            Any::ParenthesizedType(ParenthesizedType(_)) => todo!(),
            Any::TypeofType(TypeofType(_)) => todo!(),
            Any::UnknownType(_) => todo!(),
            Any::RegularExpressionType(_) => todo!(),
            Any::PropertyType(PropertyType {
                subject,
                property,
                optional,
            }) => todo!(),
            Any::DeclarationStatement(DeclarationStatement {
                destination,
                value,
                is_const,
            }) => todo!(),
            Any::IfElseStatement(IfElseStatement {
                cases,
                default_case,
            }) => todo!(),
            Any::IfElseStatementCase(IfElseStatementCase { condition, outcome }) => todo!(),
            Any::ForLoop(ForLoop {
                item_identifier,
                iterator,
                body,
            }) => todo!(),
            Any::WhileLoop(WhileLoop { condition, body }) => todo!(),
            Any::Assignment(Assignment {
                target,
                value,
                operator,
            }) => todo!(),
            Any::TryCatch(TryCatch {
                try_block,
                error_identifier,
                catch_block,
            }) => todo!(),
            Any::ThrowStatement(ThrowStatement { error_expression }) => todo!(),
            Any::Autorun(Autorun {
                effect_block,
                until,
            }) => todo!(),
            Any::PlainIdentifier(PlainIdentifier(name)) => f.write_str(name.as_str()),
        }
    }
}

impl Formattable for Arg {
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        let Arg {
            name,
            type_annotation,
            optional,
        } = self;

        name.format(f, opts)?;
        if *optional {
            f.write_char('?')?;
        }
        format_type_annotation(f, opts, type_annotation.as_ref())?;

        Ok(())
    }
}

impl<T> Formattable for Option<T>
where
    T: Formattable,
{
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        if let Some(sel) = self {
            sel.format(f, opts);
        }

        Ok(())
    }
}

impl<T> Formattable for Vec<T>
where
    T: Formattable,
{
    fn format<W: Write>(&self, f: &mut W, opts: FormatOptions) -> Result {
        for el in self.iter() {
            el.format(f, opts);
        }

        Ok(())
    }
}

fn format_type_annotation<W: Write>(
    f: &mut W,
    opts: FormatOptions,
    type_annotation: Option<&AST<TypeExpression>>,
) -> Result {
    if let Some(type_annotation) = type_annotation {
        f.write_str(": ")?;
        type_annotation.format(f, opts)?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {}

impl FormatOptions {
    pub const DEFAULT: FormatOptions = FormatOptions {};
}
