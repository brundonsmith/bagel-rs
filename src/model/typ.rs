use std::{
    fmt::{Debug, Display, Write},
    rc::Rc,
};

use enum_variant_type::EnumVariantType;
use memoize::memoize;

#[allow(unused_imports)]
use crate::utils::Loggable;
use crate::{
    cli::ModulesStore,
    model::{
        ast::{
            Any, ElementOrSpread, KeyValueOrSpread, LocalIdentifier, ModifierTypeKind,
            SpecialTypeKind, SymbolDeclaration, TypeDeclaration, AST,
        },
        parsed_module::ParsedModule,
        slice::Slice,
        ModuleID,
    },
    utils::Rcable,
};

#[derive(Clone, Debug, PartialEq, EnumVariantType)]
pub enum Type {
    MetaType {
        kind: MetaTypeKind,
        inner: Rc<Type>,
    },
    PropertyType {
        subject: Rc<Type>,
        property: Rc<Type>,
        // optional: bool,
    },
    GenericType {
        type_params: Vec<TypeParam>,
        inner: Rc<Type>,
    },
    BoundGenericType {
        type_args: Vec<Type>,
        generic: Rc<Type>,
    },

    NamedType {
        mutability: Mutability,
        name: AST<LocalIdentifier>,
    },

    // procs/funcs
    ProcType {
        args: Vec<Option<Type>>,
        args_spread: Option<Rc<Type>>,
        is_async: bool,
        throws: Option<Rc<Type>>,
    },
    FuncType {
        args: Vec<Option<Type>>,
        args_spread: Option<Rc<Type>>,
        returns: Rc<Type>,
    },

    UnionType(Vec<Type>),

    // data structures
    ObjectType {
        mutability: Mutability,
        entries: Vec<KeyValueOrSpread<Type>>,
        is_interface: bool,
    },
    RecordType {
        mutability: Mutability,
        key_type: Rc<Type>,
        value_type: Rc<Type>,
    },
    ArrayType {
        mutability: Mutability,
        element_type: Rc<Type>,
    },
    TupleType {
        mutability: Mutability,
        members: Vec<ElementOrSpread<Type>>,
    },

    // primitives
    SymbolType {
        module_id: ModuleID,
        name: Slice,
    },
    RegularExpressionType, // TODO: Number of match groups?
    StringType(Option<Slice>),
    NumberType {
        min: Option<i32>,
        max: Option<i32>,
    },
    BooleanType(Option<bool>),
    NilType,

    // special signifiers
    UnknownType(Mutability),
    PoisonedType,
    AnyType,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MetaTypeKind {
    Iterable,
    Plan,
    Error,
    Readonly,
    Keyof,
    Valueof,
    Elementof,
    Parameters,
    ReturnType,
    ThrowsType,
    Awaited,
}

impl From<ModifierTypeKind> for MetaTypeKind {
    fn from(value: ModifierTypeKind) -> Self {
        match value {
            ModifierTypeKind::Readonly => MetaTypeKind::Readonly,
            ModifierTypeKind::Keyof => MetaTypeKind::Keyof,
            ModifierTypeKind::Valueof => MetaTypeKind::Valueof,
            ModifierTypeKind::Elementof => MetaTypeKind::Elementof,
        }
    }
}

impl From<SpecialTypeKind> for MetaTypeKind {
    fn from(value: SpecialTypeKind) -> Self {
        match value {
            SpecialTypeKind::Iterable => MetaTypeKind::Iterable,
            SpecialTypeKind::Plan => MetaTypeKind::Plan,
            SpecialTypeKind::Error => MetaTypeKind::Error,
        }
    }
}

impl Eq for Type {}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.order().partial_cmp(&other.order())
    }
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.order().cmp(&other.order())
    }
}

impl From<Slice> for Type {
    fn from(value: Slice) -> Self {
        Type::StringType(Some(value))
    }
}

impl From<i32> for Type {
    fn from(value: i32) -> Self {
        Type::NumberType {
            min: Some(value),
            max: Some(value),
        }
    }
}

impl From<usize> for Type {
    fn from(value: usize) -> Self {
        Type::NumberType {
            min: Some(value as i32),
            max: Some(value as i32),
        }
    }
}

#[memoize]
pub fn string_template_safe_types() -> Type {
    Type::UnionType(vec![Type::ANY_STRING, Type::ANY_NUMBER, Type::ANY_BOOLEAN])
}

#[memoize]
pub fn number_or_string() -> Type {
    Type::ANY_NUMBER.union(Type::ANY_STRING)
}

#[memoize]
pub fn any_array() -> Type {
    Type::ArrayType {
        mutability: Mutability::Readonly,
        element_type: Type::AnyType.rc(),
    }
}

#[memoize]
pub fn unknown_array() -> Type {
    Type::ArrayType {
        mutability: Mutability::Readonly,
        element_type: Type::UnknownType(Mutability::Literal).rc(),
    }
}

#[memoize]
pub fn any_object() -> Type {
    Type::RecordType {
        mutability: Mutability::Readonly,
        key_type: Type::AnyType.rc(),
        value_type: Type::AnyType.rc(),
    }
}

#[memoize]
pub fn any_iterable() -> Type {
    Type::MetaType {
        kind: MetaTypeKind::Iterable,
        inner: Type::AnyType.rc(),
    }
}

#[memoize]
pub fn any_error() -> Type {
    Type::MetaType {
        kind: MetaTypeKind::Error,
        inner: Type::AnyType.rc(),
    }
}

#[memoize]
pub fn any_plan() -> Type {
    Type::MetaType {
        kind: MetaTypeKind::Plan,
        inner: Type::AnyType.rc(),
    }
}

#[memoize]
pub fn any_function() -> Type {
    Type::FuncType {
        args: Vec::new(),
        args_spread: Some(any_array().rc()),
        returns: Type::AnyType.rc(),
    }
}

#[memoize]
pub fn any_procedure() -> Type {
    Type::ProcType {
        args: Vec::new(),
        args_spread: Some(any_array().rc()),
        is_async: false,
        throws: Some(Type::AnyType.rc()),
    }
}

#[memoize]
pub fn any_callable() -> Type {
    any_function().union(any_procedure())
}

#[derive(Clone, Copy, Debug)]
pub struct SubsumationContext<'a> {
    pub modules: &'a ModulesStore,
    pub current_module: &'a ParsedModule,
    pub symbols_encountered: &'a Vec<Slice>,
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
        value: &Self,
    ) -> Option<SubsumationIssue> {
        let destination = &self.clone().simplify(ctx);
        let value = &value.clone().simplify(ctx);

        // println!("-------------");
        // println!("destination: {}", destination);
        // println!("value:       {}", value);

        if destination == value {
            return None;
        }

        match (destination, value) {
            (
                Type::ArrayType {
                    mutability: destination_mutability,
                    element_type: destination_element,
                },
                Type::ArrayType {
                    mutability: value_mutability,
                    element_type: value_element,
                },
            ) => {
                if destination_mutability
                    .encompasses(*value_mutability, || destination_element == value_element)
                    && destination_element.subsumes(ctx, value_element)
                {
                    return None;
                }
            }
            (
                Type::ArrayType {
                    mutability: destination_mutability,
                    element_type: destination_element,
                },
                Type::TupleType {
                    mutability: value_mutability,
                    members: value_members,
                },
            ) => {
                if destination_mutability.encompasses(*value_mutability, || false)
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
            (
                Type::TupleType {
                    mutability: destination_mutability,
                    members: destination_members,
                },
                Type::TupleType {
                    mutability: value_mutability,
                    members: value_members,
                },
            ) => {
                let mut destination_index = 0;
                let mut value_index = 0;

                while destination_index < destination_members.len()
                    && value_index < value_members.len()
                {
                    let destination_member = &destination_members[destination_index];
                    let value_member = &value_members[value_index];

                    match (destination_member, value_member) {
                        (
                            ElementOrSpread::Element(destination_member),
                            ElementOrSpread::Element(value_member),
                        ) => {
                            if let Some(issue) =
                                destination_member.subsumation_issues(ctx, value_member)
                            {
                                return Some(issue);
                            } else {
                                destination_index += 1;
                                value_index += 1;
                            }
                        }
                        (
                            ElementOrSpread::Element(destination_member),
                            ElementOrSpread::Spread(value_member),
                        ) => {
                            if let Some(issue) = destination_member
                                .subsumation_issues(ctx, &value_member.clone().elementof())
                            {
                                return Some(issue);
                            } else {
                                destination_index += 1;
                            }
                        }
                        (
                            ElementOrSpread::Spread(destination_member),
                            ElementOrSpread::Element(value_member),
                        ) => {
                            if let Some(issue) = destination_member
                                .clone()
                                .elementof()
                                .subsumation_issues(ctx, &value_member)
                            {
                                return Some(issue);
                            } else {
                                value_index += 1;
                            }
                        }
                        (
                            ElementOrSpread::Spread(destination_member),
                            ElementOrSpread::Spread(value_member),
                        ) => {
                            if let Some(issue) =
                                destination_member.subsumation_issues(ctx, value_member)
                            {
                                return Some(issue);
                            } else {
                                destination_index += 1;
                                value_index += 1;
                            }
                        }
                    }
                }

                // TODO: Will currently reject any spreads that aren't the last member in the tuple

                if (destination_index == destination_members.len()
                    && value_index == value_members.len())
                    || (destination_index == destination_members.len() - 1
                        && matches!(
                            destination_members[destination_index],
                            ElementOrSpread::Spread(_)
                        ))
                    || (value_index == value_members.len() - 1
                        && matches!(value_members[value_index], ElementOrSpread::Spread(_)))
                {
                    return None;
                }
            }
            (
                Type::RecordType {
                    mutability: destination_mutability,
                    key_type: destination_key_type,
                    value_type: destination_value_type,
                },
                Type::RecordType {
                    mutability: value_mutability,
                    key_type: value_key_type,
                    value_type: value_value_type,
                },
            ) => {
                if destination_mutability.encompasses(*value_mutability, || {
                    destination_key_type == value_key_type
                        && destination_value_type == value_value_type
                }) && destination_key_type.subsumes(ctx, value_key_type)
                    && destination_value_type.subsumes(ctx, value_value_type)
                {
                    return None;
                }
            }
            (
                Type::RecordType {
                    mutability: destination_mutability,
                    key_type: destination_key_type,
                    value_type: destination_value_type,
                },
                Type::ObjectType {
                    mutability: value_mutability,
                    entries: value_entries,
                    is_interface: value_is_interface,
                },
            ) => {
                if *destination_mutability == Mutability::Readonly
                    && !*value_is_interface
                    && value_entries.iter().all(|value_entry| match value_entry {
                        KeyValueOrSpread::KeyValue(key, value, optional) => {
                            destination_key_type.subsumes(ctx, key) // TODO: Optional
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
                    mutability: destination_mutability,
                    entries: destination_entries,
                    is_interface: destination_is_interface,
                },
                Type::ObjectType {
                    mutability: value_mutability,
                    entries: value_entries,
                    is_interface: value_is_interface,
                },
            ) => {
                let properties_match =
                    destination_entries
                        .iter()
                        .all(|destination_entry| match destination_entry {
                            KeyValueOrSpread::KeyValue(
                                destination_key,
                                destination_value,
                                destination_optional,
                            ) => value_entries.iter().any(|value_entry| match value_entry {
                                KeyValueOrSpread::KeyValue(
                                    value_key,
                                    value_value,
                                    value_optional,
                                ) => {
                                    destination_key.subsumes(ctx, value_key)
                                        && destination_value.subsumes(ctx, value_value)
                                }
                                KeyValueOrSpread::Spread(destination_spread) => todo!(),
                            }),
                            KeyValueOrSpread::Spread(destination_spread) => {
                                destination_spread.subsumes(ctx, value)
                            }
                        });

                if destination_mutability.encompasses(*value_mutability, || properties_match)
                    && (*destination_is_interface || !*value_is_interface)
                    && properties_match
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
            (Type::StringType(Some(dest)), Type::StringType(Some(val))) if dest == val => {
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
                    is_async: destination_is_async,
                    throws: destination_throws,
                },
                Type::ProcType {
                    args: value_args,
                    args_spread: value_args_spread,
                    is_async: value_is_async,
                    throws: value_throws,
                },
            ) => {
                if value
                    .clone()
                    .parameters()
                    .subsumes(ctx, &destination.clone().parameters())
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
                    returns: destination_returns,
                },
                Type::FuncType {
                    args: value_args,
                    args_spread: value_args_spread,
                    returns: value_returns,
                },
            ) => {
                if value
                    .clone()
                    .parameters()
                    .subsumes(ctx, &destination.clone().parameters())
                    && destination
                        .clone()
                        .return_type()
                        .subsumes(ctx, &value.clone().return_type())
                {
                    return None;
                }
            }
            (
                Type::MetaType {
                    kind: destination_kind,
                    inner: destination_inner,
                },
                Type::MetaType {
                    kind: value_kind,
                    inner: value_inner,
                },
            ) => {
                if destination_kind == value_kind && destination_inner.subsumes(ctx, value_inner) {
                    return None;
                }
            }
            (
                Type::MetaType {
                    kind: MetaTypeKind::Iterable,
                    inner: destination_inner,
                },
                Type::ArrayType {
                    mutability: value_mutability,
                    element_type: value_element_type,
                },
            ) => {
                return destination_inner.subsumation_issues(ctx, value_element_type);
            }
            (
                Type::MetaType {
                    kind: MetaTypeKind::Iterable,
                    inner: destination_inner,
                },
                Type::TupleType {
                    mutability: value_mutability,
                    members: value_members,
                },
            ) => {
                if value_members.iter().all(|member| match member {
                    ElementOrSpread::Element(element) => destination_inner.subsumes(ctx, element),
                    ElementOrSpread::Spread(spread) => destination.subsumes(ctx, spread),
                }) {
                    return None;
                }
            }
            (Type::UnknownType(destination_mutability), value) => {
                return None;
            }
            (Type::AnyType, _) => {
                return None;
            }
            (_, Type::AnyType) => {
                return None;
            }
            (Type::PoisonedType, _) => {
                return None;
            }
            (_, Type::PoisonedType) => {
                return None;
            }
            _ => {}
        };

        Some(SubsumationIssue::Assignment(vec![(
            destination.clone(),
            value.clone(),
        )]))
    }

    pub fn mutability<'a>(&self, ctx: SubsumationContext<'a>) -> Option<Mutability> {
        match self.clone().simplify(ctx) {
            Type::ObjectType {
                mutability,
                entries: _,
                is_interface: _,
            } => Some(mutability),
            Type::RecordType {
                mutability,
                key_type: _,
                value_type: _,
            } => Some(mutability),
            Type::ArrayType {
                mutability,
                element_type: _,
            } => Some(mutability),
            Type::TupleType {
                mutability,
                members: _,
            } => Some(mutability),
            _ => None,
        }
    }

    pub fn is_poisoned_or_any<'a>(&self, ctx: SubsumationContext<'a>) -> bool {
        let simplified = self.clone().simplify(ctx);

        simplified == Type::PoisonedType || simplified == Type::AnyType
    }

    pub fn union(self, other: Self) -> Self {
        Type::UnionType(vec![self, other])
    }

    pub fn elementof(self) -> Self {
        Type::MetaType {
            kind: MetaTypeKind::Elementof,
            inner: self.rc(),
        }
    }

    pub fn parameters(self) -> Self {
        Type::MetaType {
            kind: MetaTypeKind::Parameters,
            inner: self.rc(),
        }
    }

    pub fn return_type(self) -> Self {
        Type::MetaType {
            kind: MetaTypeKind::ReturnType,
            inner: self.rc(),
        }
    }

    pub fn property(self, property: Self) -> Self {
        Type::PropertyType {
            subject: self.rc(),
            property: property.rc(),
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

    fn simplify<'a>(self, ctx: SubsumationContext<'a>) -> Type {
        // println!("simplify {} {:?}", self, ctx.symbols_encountered);

        match self {
            Type::NamedType { mutability, name } => {
                let name_slice = name.downcast().0;

                if ctx.symbols_encountered.contains(&name_slice) {
                    // encountered cycle, bail out here to avoid infinite loop
                    Type::NamedType { mutability, name }
                } else {
                    // add current symbol to symbols_encountered
                    let symbols_encountered = ctx
                        .symbols_encountered
                        .iter()
                        .cloned()
                        .chain(std::iter::once(name_slice.clone()))
                        .collect();
                    let symbols_encountered = &symbols_encountered;
                    let ctx = SubsumationContext {
                        modules: ctx.modules,
                        current_module: ctx.current_module,
                        symbols_encountered,
                    };

                    name.resolve_symbol(ctx.into(), name_slice.as_str())
                        .map(|resolved| match resolved.details() {
                            Any::TypeDeclaration(TypeDeclaration {
                                name: _,
                                declared_type,
                                exported: _,
                            }) => declared_type.resolve_type(ctx.into()),
                            Any::SymbolDeclaration(SymbolDeclaration { name, exported }) => {
                                Type::SymbolType {
                                    module_id: name.clone().upcast().module_id().unwrap(),
                                    name: name.downcast().0.clone(),
                                }
                            }
                            Any::TypeParam(crate::model::ast::TypeParam { name, extends }) => {
                                extends
                                    .as_ref()
                                    .map(|extends| extends.resolve_type(ctx.into()))
                                    .unwrap_or(Type::UnknownType(Mutability::Mutable))
                            }
                            _ => Type::PoisonedType,
                        })
                        .unwrap_or(Type::PoisonedType)
                        .simplify(ctx)
                        .with_mutability(mutability)
                }
            }
            Type::UnionType(members) => {
                let members: Vec<Type> = members
                    .into_iter()
                    .map(|t| t.simplify(ctx))
                    .map(|member| -> Box<dyn Iterator<Item = Type>> {
                        // flatten nested union types
                        if let Type::UnionType(members) = member {
                            Box::new(members.into_iter())
                        } else {
                            Box::new(std::iter::once(member))
                        }
                    })
                    .flatten()
                    .collect();

                // remove any redundant members of the union
                let mut indexes_to_delete = Vec::new();
                for (a_index, a) in members.iter().enumerate() {
                    for (b_index, b) in members.iter().enumerate() {
                        if a_index != b_index
                            && b.subsumes(ctx, a)
                            && !indexes_to_delete.contains(&b_index)
                            && !matches!(b, Type::UnknownType(_))
                        {
                            indexes_to_delete.push(a_index);
                        }
                    }
                }

                let mut members: Vec<Type> = members
                    .into_iter()
                    .enumerate()
                    .filter_map(|(index, member)| {
                        if indexes_to_delete.contains(&index) {
                            None
                        } else {
                            Some(member)
                        }
                    })
                    .collect();

                members.sort();

                if members.len() == 1 {
                    members.remove(0)
                } else {
                    Type::UnionType(members)
                }
            }
            Type::PropertyType { subject, property } => get_property(ctx, &subject, &property)
                .unwrap_or(Type::PoisonedType)
                .simplify(ctx),
            Type::MetaType { kind, inner } => {
                let inner = inner.as_ref().clone().simplify(ctx);

                match kind {
                    MetaTypeKind::Iterable => Type::MetaType {
                        kind,
                        inner: inner.rc(),
                    },
                    MetaTypeKind::Plan => Type::MetaType {
                        kind,
                        inner: inner.rc(),
                    },
                    MetaTypeKind::Error => Type::MetaType {
                        kind,
                        inner: inner.rc(),
                    },
                    MetaTypeKind::Readonly => inner.with_mutability(Mutability::Readonly),
                    MetaTypeKind::Keyof => match inner {
                        Type::RecordType {
                            mutability,
                            key_type,
                            value_type: _,
                        } => key_type.as_ref().clone().with_mutability(mutability),
                        Type::ObjectType {
                            mutability,
                            entries,
                            is_interface: _,
                        } => Type::UnionType(
                            entries
                                .into_iter()
                                .map(|entry| match entry {
                                    KeyValueOrSpread::KeyValue(key, _, optional) => key,
                                    KeyValueOrSpread::Spread(_) => unreachable!(),
                                })
                                .collect(),
                        )
                        .with_mutability(mutability),
                        _ => Type::PoisonedType,
                    },
                    MetaTypeKind::Valueof => match inner {
                        Type::RecordType {
                            mutability,
                            key_type: _,
                            value_type,
                        } => value_type.as_ref().clone().with_mutability(mutability),
                        Type::ObjectType {
                            mutability,
                            entries,
                            is_interface: _,
                        } => Type::UnionType(
                            entries
                                .into_iter()
                                .map(|entry| match entry {
                                    KeyValueOrSpread::KeyValue(_, value, optional) => value,
                                    KeyValueOrSpread::Spread(_) => unreachable!(),
                                })
                                .collect(),
                        )
                        .with_mutability(mutability),
                        _ => Type::PoisonedType,
                    },
                    MetaTypeKind::Elementof => match inner {
                        Type::ArrayType {
                            mutability,
                            element_type,
                        } => element_type.as_ref().clone().with_mutability(mutability),
                        Type::TupleType {
                            mutability,
                            members,
                        } => Type::UnionType(
                            members
                                .into_iter()
                                .map(|member| match member {
                                    ElementOrSpread::Element(element) => element,
                                    ElementOrSpread::Spread(_) => unreachable!(),
                                })
                                .collect(),
                        )
                        .with_mutability(mutability),
                        _ => Type::PoisonedType,
                    },

                    MetaTypeKind::Parameters => {
                        args_and_spread_tuple(&inner).unwrap_or(unknown_array())
                    }
                    MetaTypeKind::ReturnType => match inner {
                        Type::FuncType {
                            args: _,
                            args_spread: _,
                            returns,
                        } => returns.as_ref().clone(),
                        Type::AnyType => Type::AnyType,
                        _ => Type::PoisonedType,
                    },
                    MetaTypeKind::ThrowsType => match inner {
                        Type::ProcType {
                            args: _,
                            args_spread: _,
                            is_async: _,
                            throws,
                        } => throws
                            .map(|t| t.as_ref().clone())
                            .unwrap_or(Type::PoisonedType),
                        Type::AnyType => Type::AnyType,
                        _ => Type::PoisonedType,
                    },
                    MetaTypeKind::Awaited => match inner {
                        Type::MetaType {
                            kind: MetaTypeKind::Plan,
                            inner,
                        } => inner.as_ref().clone().simplify(ctx),
                        _ => Type::PoisonedType,
                    },
                }
            }
            Type::ObjectType {
                mutability,
                entries,
                is_interface,
            } => {
                let mut flattened_entries = Vec::with_capacity(entries.len());

                for entry in entries {
                    match entry {
                        KeyValueOrSpread::KeyValue(key, value, optional) => {
                            flattened_entries.push(KeyValueOrSpread::KeyValue(
                                key.simplify(ctx),
                                value.simplify(ctx),
                                optional,
                            ))
                        }
                        KeyValueOrSpread::Spread(spread) => match spread.simplify(ctx) {
                            Type::ObjectType {
                                mutability,
                                mut entries,
                                is_interface: _,
                            } => flattened_entries.append(&mut entries),
                            spread => flattened_entries.push(KeyValueOrSpread::Spread(spread)),
                        },
                    };
                }

                Type::ObjectType {
                    mutability,
                    entries: flattened_entries,
                    is_interface,
                }
            }
            Type::TupleType {
                mutability,
                members,
            } => {
                let mut simplified_members = Vec::new();
                let mut is_array = false;

                for member in members.into_iter() {
                    match member {
                        ElementOrSpread::Element(element) => {
                            simplified_members.push(element.simplify(ctx));
                        }
                        ElementOrSpread::Spread(spread) => {
                            let spread = spread.simplify(ctx);

                            match spread {
                                Type::TupleType {
                                    mutability: _,
                                    members: spread_members,
                                } => {
                                    for spread_member in spread_members {
                                        match spread_member {
                                            ElementOrSpread::Element(element) => {
                                                simplified_members.push(element);
                                            }
                                            ElementOrSpread::Spread(_) => unreachable!(),
                                        }
                                    }
                                }
                                Type::ArrayType {
                                    mutability: _,
                                    element_type: spread_element,
                                } => {
                                    is_array = true;
                                    simplified_members.push(spread_element.as_ref().clone());
                                }
                                _ => return Type::PoisonedType,
                            }
                        }
                    };
                }

                match is_array {
                    true => Type::ArrayType {
                        mutability,
                        element_type: Type::UnionType(simplified_members).rc(),
                    },
                    false => Type::TupleType {
                        mutability,
                        members: simplified_members
                            .into_iter()
                            .map(ElementOrSpread::Element)
                            .collect(),
                    },
                }
            }
            Type::RecordType {
                mutability,
                key_type,
                value_type,
            } => Type::RecordType {
                mutability,
                key_type: key_type.as_ref().clone().simplify(ctx).rc(),
                value_type: value_type.as_ref().clone().simplify(ctx).rc(),
            },
            Type::ArrayType {
                mutability,
                element_type,
            } => Type::ArrayType {
                mutability,
                element_type: element_type.as_ref().clone().simplify(ctx).rc(),
            },
            Type::BoundGenericType { type_args, generic } => {
                if let Type::GenericType { type_params, inner } = generic.as_ref() {
                    // let params_to_args = type_params.iter().zip(type_args.iter()).collect();
                    todo!()
                } else {
                    unreachable!()
                }
            }
            Type::FuncType {
                args,
                args_spread,
                returns,
            } => Type::FuncType {
                args: args
                    .into_iter()
                    .map(|a| a.map(|t| t.simplify(ctx)))
                    .collect(),
                args_spread: args_spread.map(|t| t.as_ref().clone().simplify(ctx).rc()),
                returns: returns.as_ref().clone().simplify(ctx).rc(),
            },
            Type::ProcType {
                args,
                args_spread,
                is_async,
                throws,
            } => Type::ProcType {
                args: args
                    .into_iter()
                    .map(|a| a.map(|t| t.simplify(ctx)))
                    .collect(),
                args_spread: args_spread.map(|t| t.as_ref().clone().simplify(ctx).rc()),
                is_async,
                throws: throws.map(|t| t.as_ref().clone().simplify(ctx).rc()),
            },
            _ => self,
        }
    }

    fn order(&self) -> u8 {
        match self {
            Type::MetaType { kind: _, inner: _ } => 3,
            Type::PropertyType {
                subject: _,
                property: _,
            } => 6,
            Type::NamedType {
                mutability: _,
                name: _,
            } => 7,
            Type::ProcType {
                args: _,
                args_spread: _,
                is_async: _,
                throws: _,
            } => 8,
            Type::FuncType {
                args: _,
                args_spread: _,
                returns: _,
            } => 9,
            Type::UnionType(_) => 10,
            Type::ObjectType {
                mutability: _,
                entries: _,
                is_interface: _,
            } => 12,
            Type::RecordType {
                mutability: _,
                key_type: _,
                value_type: _,
            } => 13,
            Type::ArrayType {
                mutability: _,
                element_type: _,
            } => 14,
            Type::TupleType {
                mutability: _,
                members: _,
            } => 15,
            Type::RegularExpressionType => 16,
            Type::StringType(_) => 17,
            Type::NumberType { min: _, max: _ } => 18,
            Type::BooleanType(_) => 19,
            Type::NilType => 20,
            Type::UnknownType(_) => 21,
            Type::PoisonedType => 22,
            Type::AnyType => 23,
            Type::SymbolType {
                module_id: _,
                name: _,
            } => 24,
            Type::GenericType {
                type_params: _,
                inner: _,
            } => 25,
            Type::BoundGenericType {
                type_args: _,
                generic: _,
            } => 26,
        }
    }

    pub fn with_mutability(self, new_mutability: Mutability) -> Self {
        match self {
            // actually have mutability
            Type::UnknownType(mutability) => Type::UnknownType(mutability.or(new_mutability)),
            Type::NamedType { mutability, name } => Type::NamedType {
                mutability: mutability.or(new_mutability),
                name,
            },
            Type::ObjectType {
                mutability,
                entries,
                is_interface,
            } => Type::ObjectType {
                mutability: mutability.or(new_mutability),
                entries,
                is_interface,
            },
            Type::RecordType {
                mutability,
                key_type,
                value_type,
            } => Type::RecordType {
                mutability: mutability.or(new_mutability),
                key_type,
                value_type,
            },
            Type::ArrayType {
                mutability,
                element_type,
            } => Type::ArrayType {
                mutability: mutability.or(new_mutability),
                element_type,
            },
            Type::TupleType {
                mutability,
                members,
            } => Type::TupleType {
                mutability: mutability.or(new_mutability),
                members,
            },

            // recursive
            Type::MetaType { kind, inner } => Type::MetaType {
                kind,
                inner: inner.as_ref().clone().with_mutability(new_mutability).rc(),
            },
            Type::UnionType(members) => Type::UnionType(
                members
                    .into_iter()
                    .map(|m| m.with_mutability(new_mutability))
                    .collect(),
            ),
            Type::GenericType { type_params, inner } => Type::GenericType {
                type_params,
                inner: inner.as_ref().clone().with_mutability(new_mutability).rc(),
            },
            // Type::PropertyType { subject, property } => todo!(),
            // Type::ProcType {
            //     args,
            //     args_spread,
            //     is_async,
            //     throws,
            // } => todo!(),
            // Type::FuncType {
            //     args,
            //     args_spread,
            //     returns,
            // } => todo!(),
            _ => self,
        }
    }

    pub fn property_exists<'a>(&self, ctx: SubsumationContext<'a>, property: &Type) -> bool {
        get_property(ctx, self, property).is_some()
    }
}

fn get_property<'a>(ctx: SubsumationContext<'a>, subject: &Type, property: &Type) -> Option<Type> {
    let subject = subject.clone().simplify(ctx);

    if subject == Type::AnyType || property == &Type::AnyType {
        return Some(Type::AnyType);
    }

    if subject == Type::PoisonedType || property == &Type::PoisonedType {
        return Some(Type::PoisonedType);
    }

    if let Type::UnionType(members) = subject {
        let property_type_members: Vec<Option<Type>> = members
            .into_iter()
            .map(|member| get_property(ctx, &member, property))
            .collect();

        if property_type_members.iter().any(|member| member.is_some()) {
            return Some(Type::UnionType(
                property_type_members
                    .into_iter()
                    .map(|member| member.unwrap_or(Type::NilType))
                    .collect(),
            ));
        } else {
            return None;
        }
    }

    let property = property.clone().simplify(ctx);

    // specific named properties
    if let Type::StringType(Some(s)) = &property {
        if s == "length" {
            match subject {
                Type::ArrayType {
                    mutability: _,
                    element_type: _,
                } => return Some(Type::ANY_NUMBER),
                Type::TupleType {
                    mutability: _,
                    members,
                } => return Some(members.len().into()),
                Type::StringType(string) => {
                    return match string {
                        Some(string) => Some(string.len().into()),
                        None => Some(Type::ANY_NUMBER),
                    }
                }
                _ => {}
            };
        }
        if s == "value" {
            match subject {
                Type::MetaType {
                    kind: MetaTypeKind::Error,
                    inner,
                } => return Some(inner.as_ref().clone()),
                _ => {}
            }
        }
    }

    match subject {
        Type::ObjectType {
            mutability,
            entries,
            is_interface: _,
        } => entries
            .iter()
            .find_map(|entry| match entry {
                KeyValueOrSpread::KeyValue(key, value, optional) => {
                    if key.subsumes(ctx, &property) {
                        if *optional {
                            Some(value.clone().union(Type::NilType))
                        } else {
                            Some(value.clone())
                        }
                    } else {
                        None
                    }
                }
                KeyValueOrSpread::Spread(_) => None,
            })
            .map(|t| t.with_mutability(mutability)),
        Type::RecordType {
            mutability,
            key_type,
            value_type,
        } => {
            if key_type.subsumes(ctx, &property) {
                Some(value_type.as_ref().clone())
            } else {
                None
            }
        }
        Type::TupleType {
            mutability,
            members,
        } => if let Type::NumberType { min, max } = property {
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
                } else {
                    None
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

                Some(Type::UnionType(members_type))
            }
        } else {
            None
        }
        .map(|t| t.with_mutability(mutability)),
        Type::StringType(Some(string)) => {
            if let Type::NumberType { min, max } = property {
                let len = string.len() as i32;
                let min = min.unwrap_or(0);
                let max = max.unwrap_or(len - 1);

                if min == max {
                    if min >= 0 && max < len {
                        Some(Type::StringType(Some(
                            string
                                .clone()
                                .slice_range(min as usize, Some(min as usize + 1)),
                        )))
                    } else {
                        None
                    }
                } else {
                    let mut members_type: Vec<Type> = string
                        .clone()
                        .as_str()
                        .char_indices()
                        .skip((min as usize).max(0))
                        .take((max as usize).min(string.len() - 1))
                        .map(move |(index, _)| {
                            Type::StringType(Some(
                                string.clone().slice_range(index, Some(index + 1)),
                            ))
                        })
                        .collect();

                    if min < 0 || max > len - 1 {
                        members_type.push(Type::NilType);
                    }

                    Some(Type::UnionType(members_type))
                }
            } else {
                None
            }
        }
        Type::StringType(None) => {
            if let Type::NumberType { min: _, max: _ } = property {
                Some(Type::ANY_STRING.union(Type::NilType))
            } else {
                None
            }
        }
        Type::ArrayType {
            mutability,
            element_type,
        } => {
            if let Type::NumberType { min: _, max: _ } = property {
                Some(
                    element_type
                        .as_ref()
                        .clone()
                        .union(Type::NilType)
                        .with_mutability(mutability),
                )
            } else {
                None
            }
        }
        _ => None,
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::MetaType { kind, inner } => match kind {
                MetaTypeKind::Iterable => f.write_fmt(format_args!("Iterable<{}>", inner)),
                MetaTypeKind::Plan => f.write_fmt(format_args!("Plan<{}>", inner)),
                MetaTypeKind::Error => f.write_fmt(format_args!("Error<{}>", inner)),
                MetaTypeKind::Readonly => f.write_fmt(format_args!("readonly {}", inner)),
                MetaTypeKind::Keyof => f.write_fmt(format_args!("keyof {}", inner)),
                MetaTypeKind::Valueof => f.write_fmt(format_args!("valueof {}", inner)),
                MetaTypeKind::Elementof => f.write_fmt(format_args!("elementof {}", inner)),
                MetaTypeKind::Parameters => f.write_fmt(format_args!("Parameters<{}>", inner)),
                MetaTypeKind::ReturnType => f.write_fmt(format_args!("ReturnType<{}>", inner)),
                MetaTypeKind::ThrowsType => f.write_fmt(format_args!("ThrowsType<{}>", inner)),
                MetaTypeKind::Awaited => f.write_fmt(format_args!("Awaited<{}>", inner)),
            },
            Type::PropertyType { subject, property } => {
                if let Type::StringType(Some(exact)) = property.as_ref() {
                    f.write_fmt(format_args!("{}.{}", subject, exact.as_str()))
                } else {
                    f.write_fmt(format_args!("{}[{}]", subject, property))
                }
            }
            Type::GenericType { type_params, inner } => {
                if type_params.len() > 0 {
                    f.write_char('<')?;
                    for (index, param) in type_params.iter().enumerate() {
                        if index > 0 {
                            f.write_str(", ")?;
                        }

                        f.write_str(param.name.as_str())?;
                        if let Some(extends) = &param.extends {
                            f.write_fmt(format_args!(" extends {}", extends))?;
                        }
                    }
                    f.write_char('>')?;
                }

                f.write_fmt(format_args!("{}", inner))
            }
            Type::BoundGenericType { type_args, generic } => {
                f.write_char('<')?;
                for (index, arg) in type_args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    f.write_fmt(format_args!("{}", arg))?;
                }
                f.write_char('>')?;

                f.write_fmt(format_args!("{}", generic))
            }

            Type::NamedType {
                mutability: _,
                name,
            } => f.write_str(name.downcast().0.as_str()),

            Type::ProcType {
                args,
                args_spread,
                is_async,
                throws,
            } => {
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
                returns,
            } => {
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
            Type::ObjectType {
                mutability,
                entries,
                is_interface,
            } => {
                f.write_str(mutability.display_prefix())?;
                f.write_char('{')?;
                for (index, entry) in entries.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }

                    match entry {
                        KeyValueOrSpread::KeyValue(key, value, optional) => {
                            f.write_fmt(format_args!(
                                "{}{}: {}",
                                key,
                                if *optional { "?" } else { "" },
                                value
                            ))?;
                        }
                        KeyValueOrSpread::Spread(spread) => {
                            f.write_fmt(format_args!("...{}", spread))?;
                        }
                    }
                }
                f.write_char('}')
            }
            Type::RecordType {
                mutability,
                key_type,
                value_type,
            } => {
                f.write_str(mutability.display_prefix())?;
                f.write_fmt(format_args!("{{[{}]: {}}}", key_type, value_type))
            }
            Type::ArrayType {
                mutability,
                element_type,
            } => {
                f.write_str(mutability.display_prefix())?;

                if matches!(element_type.as_ref(), Type::UnionType(_)) {
                    f.write_fmt(format_args!("({})[]", element_type))
                } else {
                    f.write_fmt(format_args!("{}[]", element_type))
                }
            }
            Type::TupleType {
                mutability,
                members,
            } => {
                f.write_str(mutability.display_prefix())?;

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

            Type::SymbolType { module_id: _, name } => f.write_str(name.as_str()),
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
            Type::UnknownType(mutability) => {
                f.write_str(mutability.display_prefix())?;
                f.write_str("unknown")
            }
            Type::PoisonedType => f.write_str("poisoned"),
            Type::AnyType => f.write_str("any"),
        }
    }
}

fn args_and_spread_tuple(callable: &Type) -> Option<Type> {
    let args_and_spread = match callable {
        Type::FuncType {
            args,
            args_spread,
            returns: _,
        } => Some((args, args_spread.clone())),
        Type::ProcType {
            args,
            args_spread,
            is_async: _,
            throws: _,
        } => Some((args, args_spread.clone())),
        Type::GenericType { type_params, inner } => match inner.as_ref() {
            Type::FuncType {
                args,
                args_spread,
                returns: _,
            } => Some((args, args_spread.clone())),
            Type::ProcType {
                args,
                args_spread,
                is_async: _,
                throws: _,
            } => Some((args, args_spread.clone())),
            _ => None,
        },
        _ => None,
    };

    args_and_spread.map(|(args, spread)| Type::TupleType {
        mutability: Mutability::Literal,
        members: args
            .into_iter()
            .map(|t| {
                ElementOrSpread::Element(
                    t.clone().unwrap_or(Type::UnknownType(Mutability::Literal)),
                )
            })
            .chain(spread.map(|s| ElementOrSpread::Spread(s.as_ref().clone())))
            .collect(),
    })
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Constant,
    Readonly,
    Mutable,
    Literal,
}

impl Mutability {
    pub fn encompasses<F: Fn() -> bool>(self, other: Mutability, exactly_equal: F) -> bool {
        match (self, other) {
            (Mutability::Readonly, _) => true,
            (_, Mutability::Literal) => true,
            (Mutability::Mutable, Mutability::Mutable) => exactly_equal(),
            (Mutability::Constant, Mutability::Constant) => true,
            _ => false,
        }
    }

    pub fn or(self, other: Mutability) -> Mutability {
        match self {
            Mutability::Constant => self,
            Mutability::Readonly => self,
            Mutability::Mutable => other,
            Mutability::Literal => other,
        }
    }

    pub fn display_prefix(&self) -> &'static str {
        match self {
            Mutability::Constant => "const ",
            Mutability::Readonly => "readonly ",
            Mutability::Mutable => "",
            Mutability::Literal => "",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SubsumationIssue {
    Assignment(Vec<(Type, Type)>),
    Mutability(Type, Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: Slice,
    pub extends: Option<Type>,
}
