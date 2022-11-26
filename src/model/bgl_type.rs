use std::fmt::{Display, Write};

use enum_variant_type::EnumVariantType;

use crate::model::ast::{ModuleID, Mutability};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Type {
    UnionType {
        members: Vec<Type>,
    },

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
        entries: Vec<(String, Box<Type>)>,
        mutability: Mutability,
        is_interface: bool,
    },

    RecordType {
        key_type: Box<Type>,
        value_type: Box<Type>,
        mutability: Mutability,
    },

    ArrayType {
        element: Box<Type>,
        mutability: Mutability,
    },

    TupleType {
        members: Vec<Type>,
        mutability: Mutability,
    },

    StringType,

    NumberType,

    BooleanType,

    NilType,

    LiteralType {
        value: LiteralTypeValue,
    },

    NamedType {
        module_id: ModuleID,
        name: String,
        index: usize,
    },

    NominalType {
        module_id: ModuleID,
        name: String,
        inner: Option<Box<Type>>,
    },

    IteratorType {
        inner: Box<Type>,
    },

    PlanType {
        inner: Box<Type>,
    },

    ErrorType {
        inner: Box<Type>,
    },

    UnknownType {
        mutability: Mutability,
    },

    PoisonedType,

    AnyType,

    RegularExpressionType {
        // TODO: Number of match groups?
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arg {
    pub name: String,
    pub type_annotation: Option<Type>,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralTypeValue {
    ExactString(String),
    NumberLiteral(String),
    BooleanLiteral(bool),
}

impl Type {
    pub fn subsumes(&self, other: &Self) -> bool {
        self.subsumation_issues(other).is_none()
    }

    pub fn subsumation_issues(&self, other: &Self) -> Option<SubsumationIssue> {
        let destination = self;
        let value = other;

        if destination == value {
            return None;
        }

        match (destination, value) {
            (Type::UnionType { members }, value) => {
                if members.iter().any(|member| member.subsumes(&value)) {
                    return None;
                }
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
            (
                Type::UnknownType {
                    mutability: Mutability::Readonly,
                },
                _,
            ) => {
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
        Type::UnionType {
            members: vec![self, other],
        }
    }

    pub fn with_mutability(self, mutability: Mutability) -> Self {
        match self {
            Type::ObjectType {
                mutability: _,
                entries,
                is_interface,
            } => Type::ObjectType {
                mutability,
                entries,
                is_interface,
            },
            Type::RecordType {
                mutability: _,
                key_type,
                value_type,
            } => Type::RecordType {
                mutability,
                key_type,
                value_type,
            },
            Type::ArrayType {
                mutability: _,
                element,
            } => Type::ArrayType {
                mutability,
                element,
            },
            Type::TupleType {
                mutability: _,
                members,
            } => Type::TupleType {
                mutability,
                members,
            },
            Type::UnknownType { mutability: _ } => Type::UnknownType { mutability },
            _ => self,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UnionType { members } => {
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
                mutability,
                is_interface,
            } => todo!(),
            Type::RecordType {
                key_type,
                value_type,
                mutability,
            } => {
                if !mutability.is_mutable() {
                    f.write_str("readonly ")?;
                }

                f.write_fmt(format_args!("{{[{}]: {}}}", key_type, value_type))
            }
            Type::ArrayType {
                element,
                mutability,
            } => {
                if !mutability.is_mutable() {
                    f.write_str("readonly ")?;
                }

                f.write_fmt(format_args!("{}[]", element))
            }
            Type::TupleType {
                members,
                mutability,
            } => {
                if !mutability.is_mutable() {
                    f.write_str("readonly ")?;
                }

                f.write_char('[')?;
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    f.write_fmt(format_args!("{}", member))?;
                }
                f.write_char(']')
            }
            Type::StringType => f.write_str("string"),
            Type::NumberType => f.write_str("number"),
            Type::BooleanType => f.write_str("boolean"),
            Type::NilType => f.write_str("nil"),
            Type::LiteralType { value } => match value {
                LiteralTypeValue::ExactString(s) => f.write_fmt(format_args!("'{}'", s)),
                LiteralTypeValue::NumberLiteral(s) => f.write_str(s),
                LiteralTypeValue::BooleanLiteral(s) => {
                    f.write_str(if *s { "true" } else { "false" })
                }
            },
            Type::NamedType {
                module_id,
                name,
                index,
            } => f.write_str(name),
            Type::NominalType {
                module_id,
                name,
                inner,
            } => f.write_str(name),
            Type::IteratorType { inner } => f.write_fmt(format_args!("Iterator<{}>", inner)),
            Type::PlanType { inner } => f.write_fmt(format_args!("Plan<{}>", inner)),
            Type::ErrorType { inner } => f.write_fmt(format_args!("Error<{}>", inner)),
            Type::UnknownType { mutability } => f.write_fmt(format_args!(
                "{}unknown",
                if mutability.is_mutable() {
                    ""
                } else {
                    "readonly "
                }
            )),
            Type::PoisonedType => f.write_str("unknown"),
            Type::AnyType => f.write_str("any"),
            Type::RegularExpressionType {} => f.write_str("RegExp"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SubsumationIssue {
    Assignment(Vec<(Type, Type)>),
}
