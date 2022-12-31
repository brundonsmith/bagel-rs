use crate::model::{
    ast::{self, *},
    module::Module,
};
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
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match self.details() {
            Any::Module(ast::Module { declarations }) => {
                for decl in declarations {
                    decl.compile(f)?;
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
            }) => {
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
                    &func.body.upcast(),
                )?;
                f.write_str(";")
            }
            Any::ProcDeclaration(ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            }) => {
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
                    &proc.body.upcast(),
                )?;
                f.write_str(";")
            }
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
                f.write_str("const ")?;
                f.write_str(name.slice().as_str())?;
                compile_type_annotation(f, type_annotation.as_ref())?;
                f.write_str(" = ")?;
                value.compile(f)?;
                f.write_str(";")
            }
            Any::TestExprDeclaration(TestExprDeclaration { name, expr }) => todo!(),
            Any::TestBlockDeclaration(TestBlockDeclaration { name, block }) => todo!(),
            Any::TestTypeDeclaration(TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            }) => todo!(),
            Any::NilLiteral(_) => f.write_str("undefined"),
            Any::BooleanLiteral(BooleanLiteral(value)) => f.write_str(match value {
                true => "true",
                false => "false",
            }),
            Any::NumberLiteral(NumberLiteral(value)) => f.write_str(value.as_str()),
            Any::StringLiteral(StringLiteral { tag: _, segments }) => {
                f.write_char('`')?;
                for segment in segments {
                    match segment {
                        StringLiteralSegment::Slice(s) => f.write_str(s.as_str())?,
                        StringLiteralSegment::AST(insert) => {
                            f.write_str("${")?;
                            insert.compile(f)?;
                            f.write_str("}")?;
                        }
                    };
                }
                f.write_char('`')
            }
            Any::ExactStringLiteral(ExactStringLiteral { tag, value }) => {
                f.write_char('`')?;
                f.write_str(value.as_str())?;
                f.write_char('`')
            }
            Any::ArrayLiteral(ArrayLiteral(entries)) => {
                f.write_char('[')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    match entry {
                        ArrayLiteralEntry::Expression(expr) => {
                            expr.compile(f)?;
                        }
                        ArrayLiteralEntry::Spread(spread_expr) => {
                            spread_expr.compile(f)?;
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
                        ObjectLiteralEntry::KeyValue(KeyValue { key, value }) => {
                            key.compile(f)?;
                            f.write_str(": ")?;
                            value.compile(f)?;
                        }
                        ObjectLiteralEntry::SpreadExpression(SpreadExpression(expr)) => {
                            f.write_str("...")?;
                            expr.compile(f)?;
                        }
                    }
                }
                f.write_str(" }")
            }
            Any::SpreadExpression(SpreadExpression(inner)) => {
                f.write_str("...")?;
                inner.compile(f)
            }
            Any::BinaryOperation(BinaryOperation { left, op, right }) => {
                f.write_char('(')?;
                left.compile(f)?;
                f.write_char(' ')?;
                op.compile(f)?;
                f.write_char(' ')?;
                right.compile(f)?;
                f.write_char(')')
            }
            Any::BinaryOperator(BinaryOperator(op)) => f.write_str(op.into()),
            Any::NegationOperation(NegationOperation(inner)) => {
                f.write_char('!')?;
                inner.compile(f)
            }
            Any::Parenthesis(Parenthesis(inner)) => {
                f.write_char('(')?;
                inner.compile(f)?;
                f.write_char(')')
            }
            Any::LocalIdentifier(LocalIdentifier(name)) => f.write_str(name.as_str()),
            Any::InlineConstGroup(InlineConstGroup {
                declarations,
                inner,
            }) => {
                f.write_str("(() => {\n")?;
                for decl in declarations {
                    decl.compile(f)?;
                    f.write_char('\n')?;
                }
                f.write_str("return ")?;
                inner.compile(f)?;
                f.write_str(";\n})()")
            }
            Any::InlineDeclaration(InlineDeclaration {
                destination,
                awaited,
                value,
            }) => {
                f.write_str("const ")?;
                destination.compile(f)?;
                f.write_str(" = ")?;
                value.compile(f)?;
                f.write_char(';')
            }
            Any::Func(Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                compile_function(
                    f,
                    None,
                    &type_annotation.args,
                    false,
                    type_annotation.returns.as_ref(),
                    &body.clone().upcast(),
                )
            }
            Any::Proc(Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            }) => {
                let type_annotation = type_annotation.downcast();

                compile_function(
                    f,
                    None,
                    &type_annotation.args,
                    true,
                    None,
                    &body.clone().upcast(),
                )
            }
            Any::Block(Block(statements)) => {
                f.write_str("{\n")?;
                for stmt in statements {
                    stmt.compile(f)?;
                }
                f.write_str("\n}")
            }
            Any::JavascriptEscape(_) => todo!(),
            Any::RangeExpression(RangeExpression { start, end }) => todo!(),
            Any::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            }) => {
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
            Any::PropertyAccessor(PropertyAccessor {
                subject,
                property,
                optional,
            }) => {
                f.write_str(INT)?;
                f.write_str("observe(")?;
                subject.compile(f)?;
                f.write_str(", ")?;

                match property {
                    Property::Expression(expr) => expr.compile(f)?,
                    Property::PlainIdentifier(ident) => {
                        f.write_char('\'')?;
                        ident.compile(f)?;
                        f.write_char('\'')?;
                    }
                }

                f.write_str(")")?;

                Ok(())
            }
            Any::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            }) => {
                f.write_char('(')?;
                for case in cases {
                    case.compile(f)?;
                }
                if let Some(default_case) = default_case {
                    default_case.compile(f)?;
                } else {
                    f.write_str("undefined")?;
                }
                f.write_char(')')
            }
            Any::IfElseExpressionCase(IfElseExpressionCase { condition, outcome }) => {
                condition.compile(f)?;
                f.write_str(" ? ")?;
                outcome.compile(f)?;
                f.write_str(" : ")
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
            Any::ErrorExpression(_) => todo!(),
            Any::RegularExpression(RegularExpression { expr, flags }) => todo!(),
            Any::UnionType(_) => todo!(),
            Any::MaybeType(_) => todo!(),
            Any::NamedType(_) => todo!(),
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
            }) => todo!(),
            Any::Arg(Arg {
                name,
                type_annotation,
                optional,
            }) => {
                name.compile(f)?;
                compile_type_annotation(f, type_annotation.as_ref())
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
            Any::ArrayType(_) => todo!(),
            Any::TupleType(_) => todo!(),
            Any::StringLiteralType(_) => todo!(),
            Any::NumberLiteralType(_) => todo!(),
            Any::BooleanLiteralType(_) => todo!(),
            Any::StringType(_) => f.write_str("string"),
            Any::NumberType(_) => f.write_str("number"),
            Any::BooleanType(_) => f.write_str("boolean"),
            Any::NilType(_) => f.write_str("null | undefined"),
            Any::SpecialType(SpecialType { kind, inner }) => todo!(),
            Any::ParenthesizedType(_) => todo!(),
            Any::TypeofType(_) => todo!(),
            Any::ModifierType(ModifierType { kind, inner }) => todo!(),
            Any::UnknownType(_) => f.write_str("unknown"),
            Any::RegularExpressionType(_) => todo!(),
            Any::PropertyType(PropertyType {
                subject,
                property,
                optional,
            }) => todo!(),
            Any::DeclarationStatement(DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            }) => todo!(),
            Any::IfElseStatement(IfElseStatement {
                cases,
                default_case,
            }) => {
                for (index, case) in cases.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" else ")?;
                    }

                    case.compile(f)?;
                }

                if let Some(default_case) = default_case {
                    f.write_str(" else ")?;
                    default_case.compile(f)?;
                }

                Ok(())
            }
            Any::IfElseStatementCase(IfElseStatementCase { condition, outcome }) => {
                f.write_str("if (")?;
                condition.compile(f)?;
                f.write_str(") ")?;
                outcome.compile(f)
            }
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

// impl<'a, TKind> Compilable for AST<TKind>
// where
//     TKind: 'a,
//     &'a TKind: From<&'a Any>,
//     Any: TryInto<TKind>,
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

impl Compilable for DeclarationDestination {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match self {
            DeclarationDestination::NameAndType(NameAndType {
                name,
                type_annotation,
            }) => {
                name.compile(f)?;
                if let Some(type_annotation) = type_annotation {
                    f.write_str(": ")?;
                    type_annotation.compile(f)?;
                }

                Ok(())
            }
            DeclarationDestination::Destructure(_) => todo!(),
        }
    }
}

pub const INT: &str = "___";
pub const INT_FN: &str = "___fn_";

// --- Util functions ---

fn compile_function<W: Write>(
    f: &mut W,
    name: Option<&str>,
    args: &Vec<AST<Arg>>,
    return_type_void: bool, // HACK
    return_type: Option<&AST<TypeExpression>>,
    body: &ASTAny,
) -> Result {
    f.write_str("function ")?;

    if let Some(name) = name {
        f.write_str("___fn_")?;
        f.write_str(name)?;
    }

    f.write_char('(')?;
    for arg in args {
        arg.compile(f)?;
    }
    f.write_char(')')?;

    if return_type_void {
        f.write_str(": void")?;
    } else {
        compile_type_annotation(f, return_type)?;
    }

    f.write_char(' ')?;

    if let Any::Block(_) = body.details() {
        body.compile(f)?;
    } else {
        f.write_str("{ return ")?;
        body.compile(f)?;
        f.write_str(" }")?;
    }

    Ok(())
}

fn compile_type_annotation<W: Write>(
    f: &mut W,
    type_annotation: Option<&AST<TypeExpression>>,
) -> Result {
    if let Some(type_annotation) = type_annotation {
        f.write_str(": ")?;
        type_annotation.compile(f)?;
    }

    Ok(())
}
