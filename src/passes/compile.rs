use std::fmt::{Result, Write};

use crate::model::ast::*;

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
            Declaration::ValueDeclaration {
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
                f.write_str(name.node.0.as_str())?;
                if let Some(type_annotation) = type_annotation {
                    type_annotation.compile(f)?;
                }
                f.write_str(" = ")?;
                value.compile(f)?;
                f.write_str(";")
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
            } => {
                f.write_str("func ")?;
                f.write_str(&name.node.0)?;
                func.compile(f)
            }
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
}

impl Compile for Src<Expression> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            Expression::NilLiteral => f.write_str("undefined")?,
            Expression::BooleanLiteral { value } => todo!(),
            Expression::NumberLiteral { value } => f.write_str(value.as_str())?,
            Expression::StringLiteral { tag, segments } => todo!(),
            Expression::ExactStringLiteral { tag, value } => {
                f.write_char('`')?;
                f.write_str(value.as_str())?;
                f.write_char('`')?;
            }
            Expression::ArrayLiteral { entries } => {
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
            Expression::ObjectLiteral { entries } => {
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
                left.compile(f)?;
                f.write_char(' ')?;
                op.compile(f)?;
                f.write_char(' ')?;
                right.compile(f)?;
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

        Ok(())
    }
}

pub const INT: &str = "___";
pub const INT_FN: &str = "___fn_";

impl Compile for Src<Func> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        todo!()
        // f.write_str("function ")?;

        // // TODO: name?

        // f.write_char('(')?;
        // for arg in self.node.type_annotation.node.args {
        //     arg.format(f, opts)?;
        // }
        // if let Some(spread) = self.node.type_annotation.node.args_spread {
        //     f.write_str("...")?;
        //     spread.format(f, opts)?;
        // }
        // f.write_char(')')?;
        // if let Some(return_type) = self.node.type_annotation.node.returns {
        //     f.write_str(": ")?;
        //     return_type.format(f, opts)?;
        // }
        // f.write_str(" => ")?;
        // self.node.body.format(f, opts)
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
                src: self.src,
                node: element.clone(),
            }
            .compile(f),
        }
    }
}

impl Compile for Src<ObjectLiteralEntry> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        match &self.node {
            ObjectLiteralEntry::Variable(x) => Src {
                src: self.src,
                node: x.clone(),
            }
            .compile(f),
            ObjectLiteralEntry::Spread(spread) => {
                f.write_str("...")?;
                f.write_str(spread.0.as_str())
            }
            ObjectLiteralEntry::KeyValue(key, value) => {
                Src {
                    src: self.src,
                    node: key.clone(),
                }
                .compile(f)?;
                f.write_str(": ")?;
                Src {
                    src: self.src,
                    node: value.clone(),
                }
                .compile(f)
            }
        }
    }
}

impl Compile for Src<LocalIdentifier> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        f.write_str(self.node.0.as_str())
    }
}

impl Compile for Src<PlainIdentifier> {
    fn compile<W: Write>(&self, f: &mut W) -> Result {
        f.write_str(self.node.0.as_str())
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
            TypeExpression::UnionType { members } => todo!(),
            TypeExpression::MaybeType { inner } => todo!(),
            TypeExpression::NamedType { name } => todo!(),
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
            TypeExpression::ObjectType { entries } => todo!(),
            TypeExpression::InterfaceType { entries } => todo!(),
            TypeExpression::RecordType {
                key_type,
                value_type,
            } => todo!(),
            TypeExpression::ArrayType { element } => todo!(),
            TypeExpression::TupleType { members } => todo!(),
            TypeExpression::ReadonlyType { inner } => todo!(),
            TypeExpression::LiteralType { value } => todo!(),
            TypeExpression::NominalType {
                module_id,
                name,
                inner,
            } => todo!(),
            TypeExpression::IteratorType { inner } => todo!(),
            TypeExpression::PlanType { inner } => todo!(),
            TypeExpression::ErrorType { inner } => todo!(),
            TypeExpression::ParenthesizedType { inner } => todo!(),
            TypeExpression::TypeofType { expression } => todo!(),
            TypeExpression::KeyofType { inner } => todo!(),
            TypeExpression::ValueofType { inner } => todo!(),
            TypeExpression::ElementofType { inner } => todo!(),
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
