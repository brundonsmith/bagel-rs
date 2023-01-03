use std::{
    fmt::{Display, Write},
    rc::Rc,
};

use enum_variant_type::EnumVariantType;
use memoize::memoize;

use crate::{
    passes::{check::CheckContext, resolve_type::ResolveContext, typeinfer::InferTypeContext},
    ModulesStore,
};

use super::{
    ast::{Any, LocalIdentifier, TypeDeclaration, AST},
    errors::BagelError,
    module::Module,
    slice::Slice,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Type {
    ElementofType(Rc<Type>),

    ValueofType(Rc<Type>),

    KeyofType(Rc<Type>),

    ReadonlyType(Rc<Type>),

    PropertyType {
        subject: Rc<Type>,
        property: Rc<Type>,
        // optional: bool,
    },

    UnionType(Vec<Type>),

    ProcType {
        args: Vec<Option<Type>>,
        args_spread: Option<Rc<Type>>,
        is_pure: bool,
        is_async: bool,
        throws: Option<Rc<Type>>,
    },

    FuncType {
        args: Vec<Option<Type>>,
        args_spread: Option<Rc<Type>>,
        is_pure: bool,
        returns: Rc<Type>,
    },

    ObjectType {
        entries: Vec<KeyValueOrSpread>,
        is_interface: bool,
    },

    RecordType {
        key_type: Rc<Type>,
        value_type: Rc<Type>,
    },

    ArrayType(Rc<Type>),

    TupleType(Vec<Type>),

    StringType(Option<Slice>),

    NumberType {
        min: Option<i32>,
        max: Option<i32>,
    },

    BooleanType(Option<bool>),

    NilType,

    NamedType(AST<LocalIdentifier>),

    IteratorType(Rc<Type>),

    PlanType(Rc<Type>),

    ErrorType(Rc<Type>),

    RegularExpressionType, // TODO: Number of match groups?

    UnknownType,

    PoisonedType,

    AnyType,
}

#[memoize]
pub fn truthiness_safe_types() -> Type {
    Type::UnionType(vec![
        Type::ANY_BOOLEAN,
        Type::NilType,
        // RECORD_OF_ANY,
        any_array(),
        any_iterator(),
        any_plan(),
        // PROC,
        // FUNC
    ])
}

#[memoize]
pub fn string_template_safe_types() -> Type {
    Type::UnionType(vec![Type::ANY_STRING, Type::ANY_NUMBER, Type::ANY_BOOLEAN])
}

#[memoize]
pub fn any_array() -> Type {
    Type::ArrayType(Rc::new(Type::AnyType))
}

#[memoize]
pub fn any_iterator() -> Type {
    Type::IteratorType(Rc::new(Type::AnyType))
}

#[memoize]
pub fn any_error() -> Type {
    Type::ErrorType(Rc::new(Type::AnyType))
}

#[memoize]
pub fn any_plan() -> Type {
    Type::PlanType(Rc::new(Type::AnyType))
}

impl Type {
    pub const ANY_STRING: Type = Type::StringType(None);
    pub const ANY_NUMBER: Type = Type::NumberType {
        min: None,
        max: None,
    };
    pub const ANY_BOOLEAN: Type = Type::BooleanType(None);

    // export const FALSY: UnionType = {
    //     kind: "union-type",
    //     members: [
    //         FALSE_TYPE,
    //         NIL_TYPE,
    //         ERROR_OF_ANY
    //     ],
    //     ...TYPE_AST_NOISE
    // }

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
            (Type::NamedType(name), value) => {
                let destination = name
                    .resolve_symbol(name.downcast().0.as_str())
                    .map(|resolved| match resolved.details() {
                        Any::TypeDeclaration(TypeDeclaration {
                            name,
                            declared_type,
                            exported,
                        }) => declared_type.resolve_type(ctx.into()),
                        _ => Type::PoisonedType,
                    })
                    .unwrap_or(Type::PoisonedType);

                return destination.subsumation_issues(ctx, value);
            }
            (destination, Type::NamedType(name)) => {
                let value = name
                    .resolve_symbol(name.downcast().0.as_str())
                    .map(|resolved| match resolved.details() {
                        Any::TypeDeclaration(TypeDeclaration {
                            name,
                            declared_type,
                            exported,
                        }) => declared_type.resolve_type(ctx.into()),
                        _ => Type::PoisonedType,
                    })
                    .unwrap_or(Type::PoisonedType);

                return destination.subsumation_issues(ctx, &value);
            }
            (Type::ReadonlyType(inner), value) => {
                return inner
                    .subsumation_issues(ctx.with_dest_mutability(Mutability::Readonly), value);
            }
            (dest, Type::ReadonlyType(inner)) => {
                return dest
                    .subsumation_issues(ctx.with_val_mutability(Mutability::Readonly), inner);
            }
            (Type::ArrayType(element), Type::TupleType(members)) => {
                if members.iter().all(|member| element.subsumes(ctx, member)) {
                    return None;
                }
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
            (Type::PlanType(destination), Type::PlanType(value)) => {
                return destination.subsumation_issues(ctx, value);
            }
            (Type::IteratorType(destination), Type::IteratorType(value)) => {
                return destination.subsumation_issues(ctx, value);
            }
            (Type::ErrorType(destination), Type::ErrorType(value)) => {
                return destination.subsumation_issues(ctx, value);
            }
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

    pub fn indexed(&self, other: &Self) -> Option<Type> {
        match (self, other) {
            (Type::ArrayType(element), Type::NumberType { min: _, max: _ }) => {
                Some(element.as_ref().clone().union(Type::NilType))
            }
            (Type::TupleType(members), Type::NumberType { min, max }) => match min {
                Some(min) => match max {
                    Some(max) => {
                        if *min as usize > members.len() || *max < 0 {
                            Some(Type::PoisonedType)
                        } else if min == max {
                            members.get(*min as usize).cloned()
                        } else {
                            Some(Type::UnionType(
                                (&members[*min as usize..*max as usize])
                                    .iter()
                                    .map(|m| m.clone())
                                    .collect(),
                            ))
                        }
                    }
                    None => Some(Type::UnionType(
                        (&members[*min as usize..])
                            .iter()
                            .map(|m| m.clone())
                            .chain(std::iter::once(Type::NilType))
                            .collect(),
                    )),
                },
                None => match max {
                    Some(max) => Some(Type::UnionType(
                        (&members[..*max as usize])
                            .iter()
                            .map(|m| m.clone())
                            .chain(std::iter::once(Type::NilType))
                            .collect(),
                    )),
                    None => Some(Type::UnionType(members.clone())),
                },
            },
            // (Type::RecordType { key_type, value_type }, index) =>
            _ => None,
        }
    }

    pub fn broaden_for_mutation(self) -> Type {
        match self {
            Type::BooleanType(_) => Type::ANY_BOOLEAN,
            Type::NumberType { min: _, max: _ } => Type::ANY_NUMBER,
            Type::StringType(_) => Type::ANY_STRING,
            _ => self,
        }
    }

    pub fn to_exact_number(&self) -> Option<i32> {
        match self {
            Type::NumberType { min, max } => {
                if let (Some(min), Some(max)) = (min, max) {
                    if min == max {
                        Some(*min)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SubsumationContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a Module,
    pub dest_mutability: Mutability,
    pub val_mutability: Mutability,
}

impl<'a, F: FnMut(BagelError)> From<&CheckContext<'a, F>> for SubsumationContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            report_error: _,
        }: &CheckContext<'a, F>,
    ) -> Self {
        Self {
            modules,
            current_module,
            dest_mutability: Mutability::Mutable,
            val_mutability: Mutability::Mutable,
        }
    }
}

impl<'a> From<InferTypeContext<'a>> for SubsumationContext<'a> {
    fn from(
        InferTypeContext {
            modules,
            current_module,
        }: InferTypeContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            dest_mutability: Mutability::Mutable,
            val_mutability: Mutability::Mutable,
        }
    }
}

impl<'a> From<ResolveContext<'a>> for SubsumationContext<'a> {
    fn from(
        ResolveContext {
            modules,
            current_module,
        }: ResolveContext<'a>,
    ) -> Self {
        Self {
            modules,
            current_module,
            dest_mutability: Mutability::Mutable,
            val_mutability: Mutability::Mutable,
        }
    }
}

impl<'a> SubsumationContext<'a> {
    pub fn with_dest_mutability(self, dest_mutability: Mutability) -> Self {
        Self {
            modules: self.modules,
            current_module: self.current_module,
            dest_mutability,
            val_mutability: self.val_mutability,
        }
    }

    pub fn with_val_mutability(self, val_mutability: Mutability) -> Self {
        Self {
            modules: self.modules,
            current_module: self.current_module,
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
                if let Type::StringType(Some(exact)) = property.as_ref() {
                    f.write_fmt(format_args!("{}.{}", subject, exact.as_str()))
                } else {
                    f.write_fmt(format_args!("{}[{}]", subject, property))
                }
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
            } => {
                if *is_pure {
                    f.write_str("pure ")?;
                }

                if *is_async {
                    f.write_str("async ")?;
                }

                f.write_char('(')?;
                for arg in args {
                    if let Some(arg) = arg {
                        f.write_fmt(format_args!("{}", arg))?;
                    } else {
                        f.write_str("_")?;
                    }
                }
                f.write_str(") {}")?;

                Ok(())
            }
            Type::FuncType {
                args,
                args_spread,
                is_pure,
                returns,
            } => {
                if *is_pure {
                    f.write_str("pure ")?;
                }

                f.write_char('(')?;
                for arg in args {
                    if let Some(arg) = arg {
                        f.write_fmt(format_args!("{}", arg))?;
                    } else {
                        f.write_str("_")?;
                    }
                }
                f.write_str(") => ")?;
                f.write_fmt(format_args!("{}", returns))?;

                Ok(())
            }
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
            Type::NamedType(name) => f.write_str(name.downcast().0.as_str()),
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
pub enum KeyValueOrSpread {
    KeyValue(Type, Type),
    Spread(Type),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Constant,
    Readonly,
    Mutable,
    Literal,
}

impl Mutability {
    pub fn is_mutable(&self) -> bool {
        match self {
            Mutability::Constant => false,
            Mutability::Readonly => false,
            Mutability::Mutable => true,
            Mutability::Literal => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SubsumationIssue {
    Assignment(Vec<(Type, Type)>),
    Mutability(Type, Type),
}
