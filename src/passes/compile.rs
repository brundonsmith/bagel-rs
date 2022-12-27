use crate::model::{ast::*, module::Module};
use std::fmt::{Result, Write};

impl Module {
    pub fn compile<W: Write>(&self, f: &mut W) -> Result {
        self.ast.compile(f)
    }
}

pub trait Compilable {
    fn compile<W: Write>(&self, f: &mut W) -> Result;
}

impl<TKind> Compilable for AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match self.details() {
            ASTDetails::Module { declarations } => {
                for decl in declarations {
                    decl.compile(f)?;
                    f.write_str("\n\n")?;
                }

                Ok(())
            }
            ASTDetails::ImportAllDeclaration { name, path } => todo!(),
            ASTDetails::ImportDeclaration { imports, path } => todo!(),
            ASTDetails::ImportItem { name, alias } => todo!(),
            ASTDetails::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => todo!(),
            ASTDetails::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => {
                let func = func.downcast();
                let type_annotation = func.type_annotation.downcast();

                if *exported {
                    f.write_str("export ")?;
                }
                f.write_str("const ")?;
                f.write_str(name.slice().as_str())?;
                f.write_str(" = ")?;
                compile_function(
                    f,
                    Some(name.slice().as_str()),
                    &type_annotation.args,
                    false,
                    type_annotation.returns.as_ref(),
                    &func.body,
                )?;
                f.write_str(";")
            }
            ASTDetails::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => {
                let proc = proc.downcast();
                let type_annotation = proc.type_annotation.downcast();

                if *exported {
                    f.write_str("export ")?;
                }
                f.write_str("const ")?;
                f.write_str(name.slice().as_str())?;
                f.write_str(" = ")?;
                compile_function(
                    f,
                    Some(name.slice().as_str()),
                    &type_annotation.args,
                    true,
                    None,
                    &proc.body,
                )?;
                f.write_str(";")
            }
            ASTDetails::Decorator { name } => todo!(),
            ASTDetails::ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            } => {
                if *exported {
                    f.write_str("export ")?;
                }
                f.write_str("const ")?;
                f.write_str(name.slice().as_str())?;
                compile_type_annotation(f, type_annotation.as_ref())?;
                f.write_str(" = ")?;
                value.compile(f)?;
                f.write_str(";")
            }
            ASTDetails::TestExprDeclaration { name, expr } => todo!(),
            ASTDetails::TestBlockDeclaration { name, block } => todo!(),
            ASTDetails::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => todo!(),
            ASTDetails::NilLiteral => f.write_str("undefined"),
            ASTDetails::BooleanLiteral(_) => todo!(),
            ASTDetails::NumberLiteral(value) => f.write_str(value.as_str()),
            ASTDetails::StringLiteral { tag, segments } => todo!(),
            ASTDetails::ExactStringLiteral { tag, value } => {
                f.write_char('`')?;
                f.write_str(value.as_str())?;
                f.write_char('`')
            }
            ASTDetails::ArrayLiteral(entries) => {
                f.write_char('[')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    entry.compile(f)?;
                }
                f.write_str(" ]")
            }
            ASTDetails::ObjectLiteral(entries) => {
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }
                    f.write_char(' ')?;

                    match entry {
                        ObjectLiteralEntry::KeyValue(KeyValue { key, value }) => {
                            key.compile(f)?;
                            f.write_str(": ")?;
                            value.compile(f)?;
                        }
                        ObjectLiteralEntry::Spread(Spread(expr)) => {
                            f.write_str("...")?;
                            expr.compile(f)?;
                        }
                    }
                }
                f.write_str(" }")
            }
            ASTDetails::BinaryOperation { left, op, right } => {
                f.write_char('(')?;
                left.compile(f)?;
                f.write_char(' ')?;
                op.compile(f)?;
                f.write_char(' ')?;
                right.compile(f)?;
                f.write_char(')')
            }
            ASTDetails::BinaryOperator(op) => f.write_str(op.into()),
            ASTDetails::NegationOperation(_) => todo!(),
            ASTDetails::Parenthesis(inner) => {
                f.write_char('(')?;
                inner.compile(f)?;
                f.write_char(')')
            }
            ASTDetails::LocalIdentifier(name) => f.write_str(name.as_str()),
            ASTDetails::InlineConstGroup {
                declarations,
                inner,
            } => todo!(),
            ASTDetails::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => {
                let type_annotation = type_annotation.downcast();

                compile_function(
                    f,
                    None,
                    &type_annotation.args,
                    false,
                    type_annotation.returns.as_ref(),
                    body,
                )
            }
            ASTDetails::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => todo!(),
            ASTDetails::Block(statements) => {
                f.write_str("{\n")?;
                for stmt in statements {
                    stmt.compile(f)?;
                }
                f.write_str("\n}")
            }
            ASTDetails::JavascriptEscape(_) => todo!(),
            ASTDetails::RangeExpression { start, end } => todo!(),
            ASTDetails::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => {
                subject.compile(f)?;

                f.write_char('(')?;
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    arg.compile(f)?;
                }
                f.write_char(')')?;

                Ok(())
            }
            ASTDetails::PropertyAccessor {
                subject,
                property,
                optional,
            } => {
                f.write_str(INT)?;
                f.write_str("observe(")?;
                subject.compile(f)?;
                f.write_str(", ")?;

                match property.details() {
                    ASTDetails::PlainIdentifier(_) => {
                        f.write_char('\'')?;
                        property.compile(f)?;
                        f.write_char('\'')?;
                    }
                    _ => {
                        property.compile(f)?;
                    }
                }

                f.write_str(")")?;

                Ok(())
            }
            ASTDetails::IfElseExpression {
                cases,
                default_case,
            } => todo!(),
            ASTDetails::IfElseExpressionCase { condition, outcome } => todo!(),
            ASTDetails::SwitchExpression {
                value,
                cases,
                default_case,
            } => todo!(),
            ASTDetails::SwitchExpressionCase {
                type_filter,
                outcome,
            } => todo!(),
            ASTDetails::ElementTag {
                tag_name,
                attributes,
                children,
            } => todo!(),
            ASTDetails::AsCast { inner, as_type } => todo!(),
            ASTDetails::InstanceOf {
                inner,
                possible_type,
            } => todo!(),
            ASTDetails::ErrorExpression(_) => todo!(),
            ASTDetails::RegularExpression { expr, flags } => todo!(),
            ASTDetails::UnionType(_) => todo!(),
            ASTDetails::MaybeType(_) => todo!(),
            ASTDetails::NamedType(_) => todo!(),
            ASTDetails::GenericParamType { name, extends } => todo!(),
            ASTDetails::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            ASTDetails::FuncType {
                args,
                args_spread,
                is_pure,
                is_async,
                returns,
            } => todo!(),
            ASTDetails::GenericType { type_params, inner } => todo!(),
            ASTDetails::TypeParam { name, extends } => todo!(),
            ASTDetails::BoundGenericType { type_args, generic } => todo!(),
            ASTDetails::ObjectType {
                entries,
                is_interface,
            } => todo!(),
            ASTDetails::RecordType {
                key_type,
                value_type,
            } => todo!(),
            ASTDetails::ArrayType(_) => todo!(),
            ASTDetails::TupleType(_) => todo!(),
            ASTDetails::StringLiteralType(_) => todo!(),
            ASTDetails::NumberLiteralType(_) => todo!(),
            ASTDetails::BooleanLiteralType(_) => todo!(),
            ASTDetails::StringType => f.write_str("string"),
            ASTDetails::NumberType => f.write_str("number"),
            ASTDetails::BooleanType => f.write_str("boolean"),
            ASTDetails::NilType => f.write_str("null | undefined"),
            ASTDetails::SpecialType { kind, inner } => todo!(),
            ASTDetails::ParenthesizedType(_) => todo!(),
            ASTDetails::TypeofType(_) => todo!(),
            ASTDetails::ModifierType { kind, inner } => todo!(),
            ASTDetails::UnknownType => f.write_str("unknown"),
            ASTDetails::RegularExpressionType => todo!(),
            ASTDetails::PropertyType {
                subject,
                property,
                optional,
            } => todo!(),
            ASTDetails::DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            } => todo!(),
            ASTDetails::IfElseStatement {
                cases,
                default_case,
            } => todo!(),
            ASTDetails::IfElseStatementCase { condition, outcome } => todo!(),
            ASTDetails::ForLoop {
                item_identifier,
                iterator,
                body,
            } => todo!(),
            ASTDetails::WhileLoop { condition, body } => todo!(),
            ASTDetails::Assignment {
                target,
                value,
                operator,
            } => todo!(),
            ASTDetails::TryCatch {
                try_block,
                error_identifier,
                catch_block,
            } => todo!(),
            ASTDetails::ThrowStatement { error_expression } => todo!(),
            ASTDetails::Autorun {
                effect_block,
                until,
            } => todo!(),
            ASTDetails::PlainIdentifier(name) => f.write_str(name.as_str()),
        }
    }
}

// impl<'a, TKind> Compilable for AST<TKind>
// where
//     TKind: 'a,
//     &'a TKind: From<&'a ASTDetails>,
//     ASTDetails: TryInto<TKind>,
// {
//     fn compile<W: Write>(&self, f: &mut W) -> Result {
//         self.into().compile(f)
//     }
// }

impl Compilable for Arg {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        let Arg {
            name,
            type_annotation,
            optional,
        } = self;

        name.compile(f)?;
        if *optional {
            f.write_char('?')?;
        }
        compile_type_annotation(f, type_annotation.as_ref())?;

        Ok(())
    }
}

impl<T> Compilable for Option<T>
where
    T: Compilable,
{
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        if let Some(sel) = self {
            sel.compile(f);
        }

        Ok(())
    }
}

impl<T> Compilable for Vec<T>
where
    T: Compilable,
{
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        for el in self.iter() {
            el.compile(f);
        }

        Ok(())
    }
}

pub const INT: &str = "___";
pub const INT_FN: &str = "___fn_";

// --- Util functions ---

fn compile_function<W: Write>(
    f: &mut W,
    name: Option<&str>,
    args: &Vec<Arg>,
    return_type_void: bool, // HACK
    return_type: Option<&ASTAny>,
    body: &ASTAny,
) -> Result {
    f.write_str("function ")?;

    if let Some(name) = name {
        f.write_str("___fn_")?;
        f.write_str(name)?;
    }

    f.write_char('(')?;
    for Arg {
        name,
        type_annotation,
        optional,
    } in args
    {
        name.compile(f)?;
        compile_type_annotation(f, type_annotation.as_ref())?;
    }
    f.write_char(')')?;

    if return_type_void {
        f.write_str(": void")?;
    } else {
        compile_type_annotation(f, return_type)?;
    }

    f.write_char(' ')?;

    if let ASTDetails::Block(_) = body.details() {
        body.compile(f)?;
    } else {
        f.write_str("{ return ")?;
        body.compile(f)?;
        f.write_str("; }")?;
    }

    Ok(())
}

fn compile_type_annotation<W: Write>(f: &mut W, type_annotation: Option<&ASTAny>) -> Result {
    if let Some(type_annotation) = type_annotation {
        f.write_str(": ")?;
        type_annotation.compile(f)?;
    }

    Ok(())
}
