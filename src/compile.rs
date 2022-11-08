use std::fmt::Result;

use crate::ast::{Declaration, Expression, Module, TypeExpression};

pub trait Compile {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result;
}

impl Compile for Module {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        for decl in &self.declarations {
            decl.compile(f)?;
        }

        Ok(())
    }
}

impl Compile for Declaration {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            Declaration::ValueDeclaration {
                span,
                name,
                type_annotation,
                value,
            } => {
                f.write_str("const ")?;
                f.write_str(&name.name)?;
                if let Some(type_annotation) = type_annotation {
                    type_annotation.compile(f)?;
                }
                f.write_str(" = ")?;
                value.compile(f)?;
                f.write_str(";")?;

                Ok(())
            }
        }
    }
}

impl Compile for Expression {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            Expression::NilLiteral { span } => f.write_str("undefined"),
            Expression::NumberLiteral { span, value } => f.write_str(value),
            Expression::BinaryOperation {
                span,
                left,
                op,
                right,
            } => todo!(),
            Expression::Parenthesis { span, inner } => todo!(),
            Expression::LocalIdentifier { span, name } => todo!(),
            Expression::InlineConstGroup {
                span,
                declarations,
                inner,
            } => todo!(),
        }
    }
}

impl Compile for TypeExpression {
    fn compile(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            TypeExpression::UnknownType => f.write_str("unknown"),
            TypeExpression::NilType => f.write_str("null | undefined"),
            TypeExpression::BooleanType => f.write_str("boolean"),
            TypeExpression::NumberType => f.write_str("number"),
            TypeExpression::StringType => f.write_str("string"),
        }
    }
}
