use std::fmt::Display;

use enum_variant_type::EnumVariantType;

use crate::ast::{ModuleID, Mutability};

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
            Type::UnionType { members } => todo!(),
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
            } => todo!(),
            Type::ArrayType {
                element,
                mutability,
            } => todo!(),
            Type::TupleType {
                members,
                mutability,
            } => todo!(),
            Type::StringType => f.write_str("string"),
            Type::NumberType => f.write_str("number"),
            Type::BooleanType => f.write_str("boolean"),
            Type::NilType => f.write_str("nil"),
            Type::LiteralType { value } => todo!(),
            Type::NominalType {
                module_id,
                name,
                inner,
            } => f.write_str(name),
            Type::IteratorType { inner } => todo!(),
            Type::PlanType { inner } => todo!(),
            Type::ErrorType { inner } => todo!(),
            Type::UnknownType { mutability } => todo!(),
            Type::PoisonedType => todo!(),
            Type::AnyType => todo!(),
            Type::RegularExpressionType {} => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SubsumationIssue {
    Assignment(Vec<(Type, Type)>),
}
