use std::fmt::{Result, Write};

use crate::model::{ast::*, slice::Slice};

pub trait Compile {
    fn compile<W: Write>(&self, f: &mut W) -> Result;
}

impl Compile for Module {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        for decl in &self.declarations {
            decl.compile(f)?;
        }

        Ok(())
    }
}

impl Compile for Src<Declaration> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            Declaration::ImportAllDeclaration { name, path } => todo!(),
            Declaration::ImportDeclaration { imports, path } => todo!(),
            Declaration::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => todo!(),
            Declaration::ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms: _,
            } => {
                if *exported {
                    f.write_str("export ")?;
                }
                f.write_str("const ")?;
                f.write_str(name.0.as_str())?;
                compile_type_annotation(f, type_annotation.as_ref())?;
                f.write_str(" = ")?;
                value.compile(f)?;
                f.write_str(";")
            }
            Declaration::FuncDeclaration {
                name,
                func,
                exported,
                platforms: _,
                decorators,
            } => {
                if *exported {
                    f.write_str("export ")?;
                }
                f.write_str("const ")?;
                f.write_str(name.0.as_str())?;
                f.write_str(" = ")?;
                compile_function(
                    f,
                    Some(name.0.as_str()),
                    &func.node.type_annotation.node.args,
                    func.node
                        .type_annotation
                        .node
                        .returns
                        .as_ref()
                        .map(|x| x.as_ref()),
                    &func.node.body,
                )?;
                f.write_str(";")
            }
            Declaration::ProcDeclaration {
                name,
                proc,
                exported,
                platforms: _,
                decorators,
            } => {
                if *exported {
                    f.write_str("export ")?;
                }
                f.write_str("const ")?;
                f.write_str(name.0.as_str())?;
                f.write_str(" = ")?;
                compile_function(
                    f,
                    None,
                    &proc.node.type_annotation.node.args,
                    None,
                    &proc.node.body,
                )?;
                f.write_str(";")
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
}

impl Compile for Src<Expression> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            Expression::NilLiteral => f.write_str("undefined")?,
            Expression::BooleanLiteral(value) => todo!(),
            Expression::NumberLiteral(value) => f.write_str(value.as_str())?,
            Expression::StringLiteral { tag, segments } => todo!(),
            Expression::ExactStringLiteral { tag, value } => {
                f.write_char('`')?;
                f.write_str(value.as_str())?;
                f.write_char('`')?;
            }
            Expression::ArrayLiteral(entries) => {
                f.write_char('[')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    entry.compile(f)?;
                }
                f.write_str(" ]")?;
            }
            Expression::ObjectLiteral(entries) => {
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_char(',')?;
                    }

                    f.write_char(' ')?;
                    entry.compile(f)?;
                }
                f.write_str(" }")?;
            }
            Expression::BinaryOperation { left, op, right } => {
                f.write_char('(')?;
                left.compile(f)?;
                f.write_char(' ')?;
                op.compile(f)?;
                f.write_char(' ')?;
                right.compile(f)?;
                f.write_char(')')?;
            }
            Expression::Parenthesis(inner) => {
                f.write_char('(')?;
                inner.compile(f)?;
                f.write_char(')')?;
            }
            Expression::LocalIdentifier(name) => f.write_str(name.as_str())?,
            Expression::InlineConstGroup {
                declarations,
                inner,
            } => todo!(),
            Expression::NegationOperation(inner) => todo!(),
            Expression::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => {
                compile_function(
                    f,
                    None,
                    &type_annotation.node.args,
                    type_annotation.node.returns.as_ref().map(|x| x.as_ref()),
                    body,
                )?;
            }
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
            Expression::ErrorExpression(inner) => todo!(),
            Expression::RegularExpression { expr, flags } => todo!(),
        };

        Ok(())
    }
}

pub const INT: &str = "___";
pub const INT_FN: &str = "___fn_";

impl Compile for FuncBody {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match self {
            FuncBody::Expression(expr) => {
                f.write_str(" return ")?;
                expr.compile(f)?;
                f.write_char(' ')
            }
            FuncBody::Js(js) => f.write_str(js),
        }
    }
}

impl Compile for ProcBody {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match self {
            ProcBody::Statements(stmts) => {
                for statement in stmts {
                    statement.compile(f)?;
                    f.write_str(";\n")?;
                }

                Ok(())
            }
            ProcBody::Js(js) => f.write_str(js),
        }
    }
}

impl Compile for Src<Statement> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            Statement::DeclarationStatement {
                destination,
                value,
                awaited,
                is_const,
            } => todo!(),
            Statement::IfElseStatement {
                cases,
                default_case,
            } => todo!(),
            Statement::ForLoop {
                item_identifier,
                iterator,
                body,
            } => todo!(),
            Statement::WhileLoop { condition, body } => todo!(),
            Statement::Assignment {
                target,
                value,
                operator,
            } => todo!(),
            Statement::TryCatch {
                try_block,
                error_identifier,
                catch_block,
            } => todo!(),
            Statement::ThrowStatement { error_expression } => todo!(),
            Statement::Autorun { effect, until } => todo!(),
            Statement::InvocationStatement(_) => todo!(),
        }
    }
}

impl Compile for Src<BinaryOperator> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        f.write_str(self.node.into())
    }
}

impl Compile for Src<ArrayLiteralEntry> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            ArrayLiteralEntry::Spread(spread) => {
                f.write_str("...")?;
                f.write_str(spread.0.as_str())
            }
            ArrayLiteralEntry::Element(element) => Src {
                src: self.src.clone(),
                node: element.clone(),
            }
            .compile(f),
        }
    }
}

impl Compile for Src<ObjectLiteralEntry> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            ObjectLiteralEntry::Variable(x) => x.compile(f),
            ObjectLiteralEntry::Spread(spread) => {
                f.write_str("...")?;
                f.write_str(spread.0.as_str())
            }
            ObjectLiteralEntry::KeyAndValue(key, value) => {
                key.compile(f)?;
                f.write_str(": ")?;
                value.compile(f)
            }
        }
    }
}

impl Compile for LocalIdentifier {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        f.write_str(self.0.as_str())
    }
}

impl Compile for PlainIdentifier {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        f.write_str(self.0.as_str())
    }
}

impl Compile for Src<TypeExpression> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            TypeExpression::UnknownType => f.write_str("unknown"),
            TypeExpression::NilType => f.write_str("null | undefined"),
            TypeExpression::BooleanType => f.write_str("boolean"),
            TypeExpression::NumberType => f.write_str("number"),
            TypeExpression::StringType => f.write_str("string"),
            TypeExpression::UnionType(members) => todo!(),
            TypeExpression::MaybeType(inner) => todo!(),
            TypeExpression::NamedType(name) => todo!(),
            TypeExpression::GenericParamType { name, extends } => todo!(),
            TypeExpression::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            TypeExpression::FuncType {
                args,
                args_spread,
                is_pure,
                returns,
            } => todo!(),
            TypeExpression::GenericType { type_params, inner } => todo!(),
            TypeExpression::BoundGenericType { type_args, generic } => todo!(),
            TypeExpression::ObjectType(entries) => todo!(),
            TypeExpression::InterfaceType(entries) => todo!(),
            TypeExpression::RecordType {
                key_type,
                value_type,
            } => todo!(),
            TypeExpression::ArrayType(element) => todo!(),
            TypeExpression::TupleType(members) => todo!(),
            TypeExpression::ReadonlyType(inner) => todo!(),
            TypeExpression::LiteralType(value) => todo!(),
            TypeExpression::IteratorType(inner) => todo!(),
            TypeExpression::PlanType(inner) => todo!(),
            TypeExpression::ErrorType(inner) => todo!(),
            TypeExpression::ParenthesizedType(inner) => todo!(),
            TypeExpression::TypeofType(expression) => todo!(),
            TypeExpression::KeyofType(inner) => todo!(),
            TypeExpression::ValueofType(inner) => todo!(),
            TypeExpression::ElementofType(inner) => todo!(),
            TypeExpression::PoisonedType => todo!(),
            TypeExpression::AnyType => todo!(),
            TypeExpression::RegularExpressionType {} => todo!(),
            TypeExpression::PropertyType {
                subject,
                property,
                optional,
            } => todo!(),
        }
    }
}

impl Compile for IdentifierOrExpression {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        todo!()
        // match &self.node {
        //     IdentifierOrExpression::PlainIdentifier(x) => f.write_str(x.0.as_str()),
        //     IdentifierOrExpression::Expression(x) => {
        //         f.write_char('[')?;
        //         let expr: Src<Expression> = x.with_opt_src(self.src);
        //         expr.compile(f)?;
        //         f.write_char(']')
        //     }
        // }
    }
}

fn compile_function<W: Write, B: Compile>(
    f: &mut W,
    name: Option<&str>,
    args: &Vec<Src<Arg>>,
    return_type: Option<&Src<TypeExpression>>,
    body: &B,
) -> Result {
    f.write_str("function ")?;

    if let Some(name) = name {
        f.write_str("___fn_")?;
        f.write_str(name)?;
    }

    f.write_char('(')?;

    for Src {
        src: _,
        node: Arg {
            name,
            type_annotation,
            optional,
        },
    } in args.iter()
    {
        f.write_str(name.0.as_str())?;
        if *optional {
            f.write_char('?')?;
        }
        compile_type_annotation(f, type_annotation.as_ref())?;
    }

    f.write_char(')')?;

    compile_type_annotation(f, return_type)?;

    f.write_char(' ')?;
    f.write_char('{')?;
    body.compile(f)?;
    f.write_char('}')?;

    Ok(())
}

fn compile_type_annotation<W: Write>(
    f: &mut W,
    type_annotation: Option<&Src<TypeExpression>>,
) -> Result {
    if let Some(type_annotation) = type_annotation {
        f.write_str(": ")?;
        type_annotation.compile(f)?;
    }

    Ok(())
}
