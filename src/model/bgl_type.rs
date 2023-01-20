use std::{
    fmt::{Display, Write},
    rc::Rc,
};

use enum_variant_type::EnumVariantType;
use memoize::memoize;

use crate::{
    passes::{check::CheckContext, resolve_type::ResolveContext, typeinfer::InferTypeContext},
    utils::Loggable,
    ModulesStore,
};

use super::{
    ast::{
        Any, ElementOrSpread, KeyValueOrSpread, LocalIdentifier, SpecialTypeKind, TypeDeclaration,
        AST,
    },
    module::Module,
    slice::Slice,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Type {
    // meta types
    ElementofType(Rc<Type>),
    ValueofType(Rc<Type>),
    KeyofType(Rc<Type>),
    ReadonlyType(Rc<Type>),
    InnerType {
        kind: SpecialTypeKind,
        inner: Rc<Type>,
    },
    ReturnType(Rc<Type>), // Funcs
    PropertyType {
        subject: Rc<Type>,
        property: Rc<Type>,
        // optional: bool,
    },

    NamedType(AST<LocalIdentifier>),

    // procs/funcs
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

    UnionType(Vec<Type>),

    // data structures
    SpecialType {
        kind: SpecialTypeKind,
        inner: Rc<Type>,
    },
    ObjectType {
        entries: Vec<KeyValueOrSpread<Type>>,
        is_interface: bool,
    },
    RecordType {
        key_type: Rc<Type>,
        value_type: Rc<Type>,
    },
    ArrayType(Rc<Type>),
    TupleType(Vec<ElementOrSpread<Type>>),

    // promitives
    RegularExpressionType, // TODO: Number of match groups?
    StringType(Option<Slice>),
    NumberType {
        min: Option<i32>,
        max: Option<i32>,
    },
    BooleanType(Option<bool>),
    NilType,

    // special signifiers
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
pub fn falsy_types() -> Type {
    Type::UnionType(vec![Type::BooleanType(Some(false)), Type::NilType])
}

#[memoize]
pub fn truthy_types() -> Type {
    Type::UnionType(vec![
        Type::BooleanType(Some(true)),
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
pub fn any_object() -> Type {
    Type::RecordType {
        key_type: Rc::new(Type::AnyType),
        value_type: Rc::new(Type::AnyType),
    }
}

#[memoize]
pub fn any_iterator() -> Type {
    Type::SpecialType {
        kind: SpecialTypeKind::Iterator,
        inner: Rc::new(Type::AnyType),
    }
}

#[memoize]
pub fn any_error() -> Type {
    Type::SpecialType {
        kind: SpecialTypeKind::Error,
        inner: Rc::new(Type::AnyType),
    }
}

#[memoize]
pub fn any_plan() -> Type {
    Type::SpecialType {
        kind: SpecialTypeKind::Plan,
        inner: Rc::new(Type::AnyType),
    }
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
        value: &Self,
    ) -> Option<SubsumationIssue> {
        let destination = &self.clone().simplify(ctx.into(), &Vec::new());
        let value = &value.clone().simplify(ctx.into(), &Vec::new());

        // println!("-------------");
        // println!("destination: {}", destination);
        // println!("value:       {}", value);

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
                if inner.subsumes(ctx.with_dest_mutability(Mutability::Readonly), value) {
                    return None;
                }
            }
            (dest, Type::ReadonlyType(inner)) => {
                if dest.subsumes(ctx.with_val_mutability(Mutability::Readonly), inner) {
                    return None;
                }
            }
            (Type::ArrayType(destination_element), Type::ArrayType(value_element)) => {
                if ctx.dest_mutability.encompasses(ctx.val_mutability)
                    && destination_element.subsumes(ctx, value_element)
                {
                    return None;
                }
            }
            (Type::ArrayType(destination_element), Type::TupleType(value_members)) => {
                if ctx.dest_mutability.encompasses(ctx.val_mutability)
                    && value_members.iter().all(|member| match member {
                        ElementOrSpread::Element(element) => {
                            destination_element.subsumes(ctx, element)
                        }
                        ElementOrSpread::Spread(spread) => destination.subsumes(ctx, spread),
                    })
                {
                    return None;
                }
            }
            (Type::TupleType(destination_members), Type::TupleType(value_members)) => {
                if ctx.dest_mutability.encompasses(ctx.val_mutability)
                    && destination_members
                        .iter()
                        .zip(value_members.iter())
                        .all(|((destination, value))| todo!())
                {
                    return None;
                }
            }
            (
                Type::RecordType {
                    key_type: destination_key_type,
                    value_type: destination_value_type,
                },
                Type::RecordType {
                    key_type: value_key_type,
                    value_type: value_value_type,
                },
            ) => {
                if ctx.dest_mutability.encompasses(ctx.val_mutability)
                    && destination_key_type.subsumes(ctx, value_key_type)
                    && destination_value_type.subsumes(ctx, value_value_type)
                {
                    return None;
                }
            }
            (
                Type::RecordType {
                    key_type: destination_key_type,
                    value_type: destination_value_type,
                },
                Type::ObjectType {
                    entries: value_entries,
                    is_interface: value_is_interface,
                },
            ) => {
                if ctx.dest_mutability.encompasses(ctx.val_mutability)
                    && !*value_is_interface
                    && value_entries.iter().all(|value_entry| match value_entry {
                        KeyValueOrSpread::KeyValue(key, value) => {
                            destination_key_type.subsumes(ctx, key)
                                && destination_value_type.subsumes(ctx, value)
                        }
                        KeyValueOrSpread::Spread(spread) => destination.subsumes(ctx, spread),
                    })
                {
                    return None;
                }
            }
            (
                Type::ObjectType {
                    entries: destination_entries,
                    is_interface: destination_is_interface,
                },
                Type::ObjectType {
                    entries: value_entries,
                    is_interface: value_is_interface,
                },
            ) => {
                if ctx.dest_mutability.encompasses(ctx.val_mutability)
                    && (*destination_is_interface || !*value_is_interface)
                    && destination_entries
                        .iter()
                        .all(|destination_entry| match destination_entry {
                            KeyValueOrSpread::KeyValue(destination_key, destination_value) => {
                                value_entries.iter().any(|value_entry| match value_entry {
                                    KeyValueOrSpread::KeyValue(value_key, value_value) => {
                                        destination_key.subsumes(ctx, value_key)
                                            && destination_value.subsumes(ctx, value_value)
                                    }
                                    KeyValueOrSpread::Spread(destination_spread) => todo!(),
                                })
                            }
                            KeyValueOrSpread::Spread(destination_spread) => {
                                destination_spread.subsumes(ctx, value)
                            }
                        })
                {
                    return None;
                }
            }
            (Type::UnionType(destination_members), Type::UnionType(value_members)) => {
                if value_members.iter().all(|value_member| {
                    destination_members
                        .iter()
                        .any(|destination_member| destination_member.subsumes(ctx, value_member))
                }) {
                    return None;
                }
            }
            (Type::UnionType(destination_members), value) => {
                if destination_members
                    .iter()
                    .any(|member| member.subsumes(ctx, value))
                {
                    return None;
                }
            }
            (destination, Type::UnionType(value_members)) => {
                if value_members
                    .iter()
                    .all(|member| destination.subsumes(ctx, member))
                {
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
                    args: destination_args,
                    args_spread: destination_args_spread,
                    is_pure: destination_is_pure,
                    is_async: destination_is_async,
                    throws: destination_throws,
                },
                Type::ProcType {
                    args: value_args,
                    args_spread: value_args_spread,
                    is_pure: value_is_pure,
                    is_async: value_is_async,
                    throws: value_throws,
                },
            ) => {
                if (!*destination_is_pure || *value_is_pure)
                    && (!*value_is_async || *destination_is_async)
                    && value_args.iter().enumerate().all(|(index, value_arg)| {
                        value_arg
                            .as_ref()
                            .map(|value_arg| {
                                destination_args.get(index).map(|destination_arg| {
                                    destination_arg.as_ref().map(|destination_arg| {
                                        destination_arg.subsumes(ctx, &value_arg)
                                    })
                                })
                            })
                            .flatten()
                            .flatten()
                            .unwrap_or(false)
                    })
                    && match (destination_throws, value_throws) {
                        (Some(destination_throws), Some(value_throws)) => {
                            destination_throws.subsumes(ctx, &value_throws)
                        }
                        (None, None) => true,
                        _ => false,
                    }
                {
                    return None;
                }
            }
            (
                Type::FuncType {
                    args: destination_args,
                    args_spread: destination_args_spread,
                    is_pure: destination_is_pure,
                    returns: destination_returns,
                },
                Type::FuncType {
                    args: value_args,
                    args_spread: value_args_spread,
                    is_pure: value_is_pure,
                    returns: value_returns,
                },
            ) => {
                if (!*destination_is_pure || *value_is_pure)
                    && value_args.iter().enumerate().all(|(index, value_arg)| {
                        value_arg
                            .as_ref()
                            .map(|value_arg| {
                                destination_args.get(index).map(|destination_arg| {
                                    destination_arg.as_ref().map(|destination_arg| {
                                        destination_arg.subsumes(ctx, &value_arg)
                                    })
                                })
                            })
                            .flatten()
                            .flatten()
                            .unwrap_or(false)
                    })
                    && destination_returns.subsumes(ctx, &value_returns)
                {
                    return None;
                }
            }
            (
                Type::SpecialType {
                    kind: destination_kind,
                    inner: destination_inner,
                },
                Type::SpecialType {
                    kind: value_kind,
                    inner: value_inner,
                },
            ) => {
                if destination_kind == value_kind {
                    return destination_inner.subsumation_issues(ctx, value_inner);
                }
            }
            (
                Type::SpecialType {
                    kind: destination_kind,
                    inner: destination_inner,
                },
                Type::SpecialType {
                    kind: value_kind,
                    inner: value_inner,
                },
            ) => {
                if destination_kind == value_kind {
                    return destination_inner.subsumation_issues(ctx, value_inner);
                }
            }
            (
                destination,
                Type::InnerType {
                    kind: value_kind,
                    inner: value_inner,
                },
            ) => {
                return Type::SpecialType {
                    kind: *value_kind,
                    inner: Rc::new(destination.clone()),
                }
                .subsumation_issues(ctx, value_inner)
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

    pub fn exact_number(n: i32) -> Type {
        Type::NumberType {
            min: Some(n),
            max: Some(n),
        }
    }

    // pub fn indexed(&self, other: &Self) -> Option<Type> {
    //     match (self, other) {
    //         (Type::ArrayType(element), Type::NumberType { min: _, max: _ }) => {
    //             Some(element.as_ref().clone().union(Type::NilType))
    //         }
    //         (Type::TupleType(members), Type::NumberType { min, max }) => match min {
    //             Some(min) => match max {
    //                 Some(max) => {
    //                     if *min as usize > members.len() || *max < 0 {
    //                         Some(Type::PoisonedType)
    //                     } else if min == max {
    //                         members.get(*min as usize).cloned()
    //                     } else {
    //                         Some(Type::UnionType(
    //                             (&members[*min as usize..*max as usize])
    //                                 .iter()
    //                                 .map(|m| m.clone())
    //                                 .collect(),
    //                         ))
    //                     }
    //                 }
    //                 None => Some(Type::UnionType(
    //                     (&members[*min as usize..])
    //                         .iter()
    //                         .map(|m| m.clone())
    //                         .chain(std::iter::once(Type::NilType))
    //                         .collect(),
    //                 )),
    //             },
    //             None => match max {
    //                 Some(max) => Some(Type::UnionType(
    //                     (&members[..*max as usize])
    //                         .iter()
    //                         .map(|m| m.clone())
    //                         .chain(std::iter::once(Type::NilType))
    //                         .collect(),
    //                 )),
    //                 None => Some(Type::UnionType(members.clone())),
    //             },
    //         },
    //         // (Type::RecordType { key_type, value_type }, index) =>
    //         _ => None,
    //     }
    // }

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

    pub fn subtract<'a>(self, ctx: SubsumationContext<'a>, other: &Type) -> Type {
        if &self == other {
            return Type::UnionType(vec![]);
        }

        match self {
            Type::UnionType(members) => Type::UnionType(
                members
                    .into_iter()
                    .map(|m| m.subtract(ctx, other))
                    .collect(),
            ),
            _ => self,
        }
    }

    pub fn narrow<'a>(self, ctx: SubsumationContext<'a>, other: &Type) -> Type {
        if &self == other {
            return self;
        }

        match self {
            Type::UnionType(members) => Type::UnionType(
                members
                    .into_iter()
                    .map(|m| m.narrow(ctx, other))
                    .filter(|member| other.subsumes(ctx, member))
                    .collect(),
            ),
            _ => self,
        }
    }

    fn simplify<'a>(self, ctx: ResolveContext<'a>, symbols_encountered: &Vec<Slice>) -> Type {
        match self {
            Type::NamedType(name) => {
                let name_slice = name.downcast().0;

                if symbols_encountered.contains(&name_slice) {
                    Type::NamedType(name)
                } else {
                    let symbols_encountered = symbols_encountered
                        .iter()
                        .map(Slice::clone)
                        .chain(std::iter::once(name_slice.clone()))
                        .collect();
                    let symbols_encountered = &symbols_encountered;

                    name.resolve_symbol(name_slice.as_str())
                        .map(|resolved| match resolved.details() {
                            Any::TypeDeclaration(TypeDeclaration {
                                name: _,
                                declared_type,
                                exported: _,
                            }) => declared_type.resolve_type(ctx),
                            _ => Type::PoisonedType,
                        })
                        .unwrap_or(Type::PoisonedType)
                        .simplify(ctx, symbols_encountered)
                }
            }
            Type::UnionType(mut members) => {
                if members.len() == 1 {
                    members.remove(0).simplify(ctx, symbols_encountered)
                } else {
                    Type::UnionType(
                        members
                            .into_iter()
                            .filter(|member| {
                                if let Type::UnionType(members) = member {
                                    if members.len() == 0 {
                                        return false;
                                    }
                                }

                                return true;
                            })
                            .map(|t| t.simplify(ctx, symbols_encountered))
                            .collect(),
                    )
                }
            }
            Type::PropertyType { subject, property } => {
                let subject = subject.as_ref().clone().simplify(ctx, symbols_encountered);
                let property = property.as_ref().clone().simplify(ctx, symbols_encountered);

                if let Type::StringType(Some(s)) = &property {
                    if s.as_str() == "length" {
                        match subject {
                            Type::ArrayType(_) => return Type::ANY_NUMBER,
                            Type::TupleType(members) => {
                                return Type::exact_number(members.len() as i32)
                            }
                            _ => {}
                        };
                    }
                }

                match subject {
                    Type::ObjectType {
                        entries,
                        is_interface: _,
                    } => entries
                        .iter()
                        .find_map(|entry| match entry {
                            KeyValueOrSpread::KeyValue(key, value) => {
                                if key.subsumes(ctx.into(), &property) {
                                    Some(value)
                                } else {
                                    None
                                }
                            }
                            KeyValueOrSpread::Spread(_) => None,
                        })
                        .cloned()
                        .unwrap_or(Type::PoisonedType),
                    Type::RecordType {
                        key_type,
                        value_type,
                    } => {
                        if key_type.subsumes(ctx.into(), &property) {
                            value_type.as_ref().clone()
                        } else {
                            Type::PoisonedType
                        }
                    }
                    Type::TupleType(members) => {
                        if let Type::NumberType { min, max } = property {
                            let len = members.len() as i32;
                            let min = min.unwrap_or(0);
                            let max = max.unwrap_or(len - 1);

                            if min == max {
                                if min >= 0 && max < len {
                                    members
                                        .get(min as usize)
                                        .map(|member| match member {
                                            ElementOrSpread::Element(element) => element,
                                            ElementOrSpread::Spread(_) => unreachable!(),
                                        })
                                        .cloned()
                                        .unwrap_or(Type::PoisonedType)
                                } else {
                                    Type::PoisonedType
                                }
                            } else {
                                let mut members_type: Vec<Type> = members
                                    [(min as usize).max(0)..(max as usize).min(members.len() - 1)]
                                    .iter()
                                    .map(|member| match member {
                                        ElementOrSpread::Element(element) => element,
                                        ElementOrSpread::Spread(_) => unreachable!(),
                                    })
                                    .cloned()
                                    .collect();

                                if min < 0 || max > len - 1 {
                                    members_type.push(Type::NilType);
                                }

                                Type::UnionType(members_type)
                            }
                        } else {
                            Type::PoisonedType
                        }
                    }
                    Type::ArrayType(element) => {
                        if let Type::NumberType { min: _, max: _ } = property {
                            element.as_ref().clone().union(Type::NilType)
                        } else {
                            Type::PoisonedType
                        }
                    }
                    _ => Type::PoisonedType,
                }
            }
            Type::ElementofType(inner) => {
                let inner = inner.as_ref().clone().simplify(ctx, symbols_encountered);

                match inner {
                    Type::ArrayType(element) => element.as_ref().clone(),
                    Type::TupleType(members) => Type::UnionType(
                        members
                            .into_iter()
                            .map(|member| match member {
                                ElementOrSpread::Element(element) => element,
                                ElementOrSpread::Spread(_) => unreachable!(),
                            })
                            .collect(),
                    ),
                    _ => Type::PoisonedType,
                }
            }
            Type::KeyofType(inner) => {
                let inner = inner.as_ref().clone().simplify(ctx, symbols_encountered);

                match inner {
                    Type::RecordType {
                        key_type,
                        value_type: _,
                    } => key_type.as_ref().clone(),
                    Type::ObjectType {
                        entries,
                        is_interface: _,
                    } => Type::UnionType(
                        entries
                            .into_iter()
                            .map(|entry| match entry {
                                KeyValueOrSpread::KeyValue(key, _) => key,
                                KeyValueOrSpread::Spread(_) => unreachable!(),
                            })
                            .collect(),
                    ),
                    _ => Type::PoisonedType,
                }
            }
            Type::ValueofType(inner) => {
                let inner = inner.as_ref().clone().simplify(ctx, symbols_encountered);

                match inner {
                    Type::RecordType {
                        key_type: _,
                        value_type,
                    } => value_type.as_ref().clone(),
                    Type::ObjectType {
                        entries,
                        is_interface: _,
                    } => Type::UnionType(
                        entries
                            .into_iter()
                            .map(|entry| match entry {
                                KeyValueOrSpread::KeyValue(_, value) => value,
                                KeyValueOrSpread::Spread(_) => unreachable!(),
                            })
                            .collect(),
                    ),
                    _ => Type::PoisonedType,
                }
            }
            Type::InnerType { kind, inner } => {
                let inner = inner.as_ref().clone().simplify(ctx, symbols_encountered);

                match inner {
                    Type::SpecialType {
                        kind: inner_kind,
                        inner,
                    } => match (inner_kind, kind) {
                        (SpecialTypeKind::Iterator, SpecialTypeKind::Iterator) => {
                            inner.as_ref().clone()
                        }
                        (SpecialTypeKind::Plan, SpecialTypeKind::Plan) => inner.as_ref().clone(),
                        (SpecialTypeKind::Error, SpecialTypeKind::Error) => inner.as_ref().clone(),
                        _ => Type::PoisonedType,
                    },
                    _ => Type::PoisonedType,
                }
            }
            Type::ObjectType {
                entries,
                is_interface,
            } => {
                let mut flattened_entries = Vec::with_capacity(entries.len());

                for entry in entries {
                    match entry {
                        KeyValueOrSpread::KeyValue(key, value) => {
                            flattened_entries.push(KeyValueOrSpread::KeyValue(
                                key.simplify(ctx, symbols_encountered),
                                value.simplify(ctx, symbols_encountered),
                            ))
                        }
                        KeyValueOrSpread::Spread(spread) => {
                            match spread.simplify(ctx, symbols_encountered) {
                                Type::ObjectType {
                                    mut entries,
                                    is_interface: _,
                                } => flattened_entries.append(&mut entries),
                                spread => flattened_entries.push(KeyValueOrSpread::Spread(spread)),
                            }
                        }
                    };
                }

                Type::ObjectType {
                    entries: flattened_entries,
                    is_interface,
                }
            }
            Type::TupleType(members) => {
                let mut simplified_members = Vec::new();
                let mut is_array = false;

                for member in members.into_iter() {
                    match member {
                        ElementOrSpread::Element(element) => {
                            simplified_members.push(element.simplify(ctx, symbols_encountered));
                        }
                        ElementOrSpread::Spread(spread) => {
                            let spread = spread.simplify(ctx, symbols_encountered);

                            match spread {
                                Type::TupleType(spread_members) => {
                                    for spread_member in spread_members {
                                        match spread_member {
                                            ElementOrSpread::Element(element) => {
                                                simplified_members.push(element);
                                            }
                                            ElementOrSpread::Spread(_) => unreachable!(),
                                        }
                                    }
                                }
                                Type::ArrayType(spread_element) => {
                                    is_array = true;
                                    simplified_members.push(spread_element.as_ref().clone());
                                }
                                _ => return Type::PoisonedType,
                            }
                        }
                    };
                }

                match is_array {
                    true => Type::ArrayType(Rc::new(Type::UnionType(simplified_members))),
                    false => Type::TupleType(
                        simplified_members
                            .into_iter()
                            .map(ElementOrSpread::Element)
                            .collect(),
                    ),
                }
            }
            Type::RecordType {
                key_type,
                value_type,
            } => Type::RecordType {
                key_type: Rc::new(key_type.as_ref().clone().simplify(ctx, symbols_encountered)),
                value_type: Rc::new(
                    value_type
                        .as_ref()
                        .clone()
                        .simplify(ctx, symbols_encountered),
                ),
            },
            Type::ArrayType(element) => Type::ArrayType(Rc::new(
                element.as_ref().clone().simplify(ctx, symbols_encountered),
            )),
            Type::SpecialType { kind, inner } => Type::SpecialType {
                kind,
                inner: Rc::new(inner.as_ref().clone().simplify(ctx, symbols_encountered)),
            },
            _ => self,
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

impl<'a> From<&CheckContext<'a>> for SubsumationContext<'a> {
    fn from(
        CheckContext {
            modules,
            current_module,
            nearest_func_or_proc: _,
        }: &CheckContext<'a>,
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
            Type::InnerType { kind, inner } => todo!(),
            Type::ReturnType(inner) => todo!(),
            Type::PropertyType { subject, property } => {
                if let Type::StringType(Some(exact)) = property.as_ref() {
                    f.write_fmt(format_args!("{}.{}", subject, exact.as_str()))
                } else {
                    f.write_fmt(format_args!("{}[{}]", subject, property))
                }
            }

            Type::NamedType(name) => f.write_str(name.downcast().0.as_str()),

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
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

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
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

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

            Type::UnionType(members) => {
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" | ")?;
                    }

                    f.write_fmt(format_args!("{}", member))?;
                }

                Ok(())
            }

            Type::SpecialType { kind, inner } => {
                let kind: &'static str = kind.into();
                f.write_fmt(format_args!("{}<{}>", kind, inner))
            }
            Type::ObjectType {
                entries,
                is_interface,
            } => {
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    match entry {
                        KeyValueOrSpread::KeyValue(key, value) => {
                            f.write_fmt(format_args!("{}: {}", key, value))?;
                        }
                        KeyValueOrSpread::Spread(spread) => {
                            f.write_fmt(format_args!("...{}", spread))?;
                        }
                    }
                }
                f.write_char('}')
            }
            Type::RecordType {
                key_type,
                value_type,
            } => f.write_fmt(format_args!("{{[{}]: {}}}", key_type, value_type)),
            Type::ArrayType(element) => {
                if matches!(element.as_ref(), Type::UnionType(_)) {
                    f.write_fmt(format_args!("({})[]", element))
                } else {
                    f.write_fmt(format_args!("{}[]", element))
                }
            }
            Type::TupleType(members) => {
                f.write_char('[')?;
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    match member {
                        ElementOrSpread::Element(element) => {
                            f.write_fmt(format_args!("{}", element))?;
                        }
                        ElementOrSpread::Spread(spread) => {
                            f.write_fmt(format_args!("...{}", spread))?;
                        }
                    }
                }
                f.write_char(']')
            }

            Type::RegularExpressionType {} => f.write_str("RegExp"),
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
            Type::BooleanType(b) => f.write_str(match b {
                Some(true) => "true",
                Some(false) => "false",
                None => "boolean",
            }),
            Type::NilType => f.write_str("nil"),
            Type::UnknownType => f.write_str("unknown"),
            Type::PoisonedType => f.write_str("unknown"),
            Type::AnyType => f.write_str("any"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Constant,
    Readonly,
    Mutable,
    Literal,
}

impl Mutability {
    pub fn is_mutable(self) -> bool {
        match self {
            Mutability::Constant => false,
            Mutability::Readonly => false,
            Mutability::Mutable => true,
            Mutability::Literal => true,
        }
    }

    pub fn encompasses(self, other: Mutability) -> bool {
        match self {
            Mutability::Constant => other == Mutability::Constant,
            Mutability::Readonly => true,
            Mutability::Mutable => other == Mutability::Mutable || other == Mutability::Literal,
            Mutability::Literal => other == Mutability::Literal,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SubsumationIssue {
    Assignment(Vec<(Type, Type)>),
    Mutability(Type, Type),
}
