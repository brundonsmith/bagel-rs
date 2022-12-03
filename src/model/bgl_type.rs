use std::fmt::{Display, Write};

use enum_variant_type::EnumVariantType;

use crate::{
    model::ast::Mutability,
    passes::{check::CheckContext, typeinfer::InferTypeContext},
    ModulesStore,
};

use super::{module::ModuleID, slice::Slice};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Type {
    ElementofType(Box<Type>),

    ValueofType(Box<Type>),

    KeyofType(Box<Type>),

    ReadonlyType(Box<Type>),

    PropertyType {
        subject: Box<Type>,
        property: Slice,
        // optional: bool,
    },

    UnionType(Vec<Type>),

    ProcType {
        args: Vec<Arg>,
        args_spread: Option<Box<Type>>,
        is_pure: bool,
        is_async: bool,
        throws: Option<Box<Type>>,
    },

    FuncType {
        args: Vec<Arg>,
        args_spread: Option<Box<Type>>,
        is_pure: bool,
        returns: Option<Box<Type>>,
    },

    ObjectType {
        entries: Vec<(Slice, Box<Type>)>,
        is_interface: bool,
    },

    RecordType {
        key_type: Box<Type>,
        value_type: Box<Type>,
    },

    ArrayType(Box<Type>),

    TupleType(Vec<Type>),

    StringType(Option<Slice>),

    NumberType {
        min: Option<i32>,
        max: Option<i32>,
    },

    BooleanType(Option<bool>),

    NilType,

    NamedType {
        module_id: ModuleID,
        name: Slice,
    },

    IteratorType(Box<Type>),

    PlanType(Box<Type>),

    ErrorType(Box<Type>),

    RegularExpressionType, // TODO: Number of match groups?

    UnknownType,

    PoisonedType,

    AnyType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arg {
    pub name: String,
    pub type_annotation: Option<Type>,
    pub optional: bool,
}

impl Type {
    pub const ANY_STRING: Type = Type::StringType(None);
    pub const ANY_NUMBER: Type = Type::NumberType {
        min: None,
        max: None,
    };
    pub const ANY_BOOLEAN: Type = Type::BooleanType(None);

    pub fn subsumes<'a>(&self, ctx: SubsumationContext<'a>, other: &Self) -> bool {
        self.subsumation_issues(ctx, other).is_none()
    }

    pub fn subsumation_issues<'a>(
        &self,
        ctx: SubsumationContext<'a>,
        other: &Self,
    ) -> Option<SubsumationIssue> {
        let destination = self;
        let value = other;

        if destination == value {
            return None;
        }

        if ctx.dest_mutability.is_mutable() && !ctx.val_mutability.is_mutable() {
            return Some(SubsumationIssue::Mutability(
                destination.clone(),
                value.clone(),
            ));
        }

        match (destination, value) {
            (Type::ReadonlyType(inner), value) => {
                return inner
                    .subsumation_issues(ctx.with_dest_mutability(Mutability::Readonly), value);
            }
            (dest, Type::ReadonlyType(inner)) => {
                return dest
                    .subsumation_issues(ctx.with_val_mutability(Mutability::Readonly), inner);
            }
            (Type::UnionType(members), value) => {
                if members.iter().any(|member| member.subsumes(ctx, &value)) {
                    return None;
                }
            }
            (Type::StringType(None), Type::StringType(_)) => {
                return None;
            }
            (Type::StringType(Some(dest)), Type::StringType(Some(val)))
                if dest.as_str() == val.as_str() =>
            {
                return None;
            }
            (
                Type::NumberType {
                    min: None,
                    max: None,
                },
                Type::NumberType { min: _, max: _ },
            ) => {
                return None;
            }
            (
                Type::NumberType {
                    min: dest_min,
                    max: dest_max,
                },
                Type::NumberType {
                    min: val_min,
                    max: val_max,
                },
            ) if dest_min
                .map(|dest_min| val_min.map(|val_min| val_min >= dest_min).unwrap_or(false))
                .unwrap_or(true)
                && dest_max
                    .map(|dest_max| val_max.map(|val_max| val_max <= dest_max).unwrap_or(false))
                    .unwrap_or(true) =>
            {
                return None;
            }
            (Type::BooleanType(None), Type::BooleanType(_)) => {
                return None;
            }
            (
                Type::ProcType {
                    args: args_1,
                    args_spread: args_spread_1,
                    is_pure: is_pure_1,
                    is_async: is_async_1,
                    throws: throws_1,
                },
                Type::ProcType {
                    args: args_2,
                    args_spread: args_spread_2,
                    is_pure: is_pure_2,
                    is_async: is_async_2,
                    throws: throws_2,
                },
            ) => {}
            (
                Type::FuncType {
                    args: args_1,
                    args_spread: args_spread_1,
                    is_pure: is_pure_1,
                    returns: returns_1,
                },
                Type::FuncType {
                    args: args_2,
                    args_spread: args_spread_2,
                    is_pure: is_pure_2,
                    returns: returns_2,
                },
            ) => {}
            (Type::UnknownType, _) => {
                return None;
            }
            (Type::AnyType, _) => {
                return None;
            }
            (_, Type::AnyType) => {
                return None;
            }
            _ => {}
        };

        //     let resolved = name
        //     .src
        //     .map(|src| ctx.module.resolve_symbol_within(&name.node.name, &src))
        //     .flatten();

        // match resolved {
        //     Some(Binding::TypeDeclaration(binding)) => binding.declared_type.resolve(ctx),
        //     _ => Type::PoisonedType,
        // }

        Some(SubsumationIssue::Assignment(vec![(
            destination.clone(),
            value.clone(),
        )]))
    }

    pub fn union(self, other: Self) -> Self {
        Type::UnionType(vec![self, other])
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SubsumationContext<'a> {
    pub modules: &'a ModulesStore,
    pub dest_mutability: Mutability,
    pub val_mutability: Mutability,
}

impl<'a> From<CheckContext<'a>> for SubsumationContext<'a> {
    fn from(ctx: CheckContext<'a>) -> Self {
        Self {
            modules: ctx.modules,
            dest_mutability: Mutability::Mutable,
            val_mutability: Mutability::Mutable,
        }
    }
}

impl<'a> From<InferTypeContext<'a>> for SubsumationContext<'a> {
    fn from(ctx: InferTypeContext<'a>) -> Self {
        Self {
            modules: ctx.modules,
            dest_mutability: Mutability::Mutable,
            val_mutability: Mutability::Mutable,
        }
    }
}

impl<'a> SubsumationContext<'a> {
    pub fn with_dest_mutability(self, dest_mutability: Mutability) -> Self {
        Self {
            modules: self.modules,
            dest_mutability,
            val_mutability: self.val_mutability,
        }
    }

    pub fn with_val_mutability(self, val_mutability: Mutability) -> Self {
        Self {
            modules: self.modules,
            dest_mutability: self.dest_mutability,
            val_mutability,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::ElementofType(inner) => f.write_fmt(format_args!("elementof {}", inner)),
            Type::ValueofType(inner) => f.write_fmt(format_args!("valueof {}", inner)),
            Type::KeyofType(inner) => f.write_fmt(format_args!("keyof {}", inner)),
            Type::ReadonlyType(inner) => f.write_fmt(format_args!("readonly {}", inner)),
            Type::PropertyType { subject, property } => {
                f.write_fmt(format_args!("{}.{}", subject, property.as_str()))
            }
            Type::UnionType(members) => {
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }

                    f.write_fmt(format_args!("{}", member))?;
                }

                Ok(())
            }
            Type::ProcType {
                args,
                args_spread,
                is_pure,
                is_async,
                throws,
            } => todo!(),
            Type::FuncType {
                args,
                args_spread,
                is_pure,
                returns,
            } => todo!(),
            Type::ObjectType {
                entries,
                is_interface,
            } => todo!(),
            Type::RecordType {
                key_type,
                value_type,
            } => f.write_fmt(format_args!("{{[{}]: {}}}", key_type, value_type)),
            Type::ArrayType(element) => f.write_fmt(format_args!("{}[]", element)),
            Type::TupleType(members) => {
                f.write_char('[')?;
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    f.write_fmt(format_args!("{}", member))?;
                }
                f.write_char(']')
            }
            Type::StringType(s) => match s {
                Some(s) => f.write_fmt(format_args!("'{}'", s.as_str())),
                None => f.write_str("string"),
            },
            Type::NumberType { min, max } => match (min, max) {
                (None, None) => f.write_str("number"),
                (None, Some(max)) => f.write_fmt(format_args!("<={}", max)),
                (Some(min), None) => f.write_fmt(format_args!(">={}", min)),
                (Some(min), Some(max)) => {
                    if min == max {
                        f.write_fmt(format_args!("{}", min))
                    } else {
                        f.write_fmt(format_args!("{}-{}", min, max))
                    }
                }
            },
            Type::BooleanType(b) => match b {
                Some(true) => f.write_str("true"),
                Some(false) => f.write_str("false"),
                None => f.write_str("boolean"),
            },
            Type::NilType => f.write_str("nil"),
            Type::NamedType { module_id, name } => f.write_str(name.as_str()),
            Type::IteratorType(inner) => f.write_fmt(format_args!("Iterator<{}>", inner)),
            Type::PlanType(inner) => f.write_fmt(format_args!("Plan<{}>", inner)),
            Type::ErrorType(inner) => f.write_fmt(format_args!("Error<{}>", inner)),
            Type::UnknownType => f.write_str("unknown"),
            Type::PoisonedType => f.write_str("unknown"),
            Type::AnyType => f.write_str("any"),
            Type::RegularExpressionType {} => f.write_str("RegExp"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SubsumationIssue {
    Assignment(Vec<(Type, Type)>),
    Mutability(Type, Type),
}
