use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};
use strum_macros::{EnumString, IntoStaticStr};

use crate::model::{ModuleID, Slice, Type};
use crate::passes::InferTypeContext;
use crate::utils::Rcable;

macro_rules! union_type {
    ($name:ident = $( $s:ident )|*) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum $name {
            $($s($s)),*
        }

        $(
            impl From<$s> for $name {
                fn from(variant: $s) -> Self {
                    $name::$s(variant)
                }
            }

            impl TryFrom<$name> for $s {
                type Error = ();

                fn try_from(un: $name) -> Result<Self, Self::Error> {
                    match un {
                        $name::$s(variant) => Ok(variant),
                        _ => Err(())
                    }
                }
            }
        )*
    };
}

macro_rules! union_subtype {
    ($name:ident = $( $s:ident )|*) => {
        union_type!($name = $($s)|*);

        impl From<$name> for Any {
            fn from(sub: $name) -> Self {
                match sub {
                    $(
                        $name::$s(s) => Any::$s(s),
                    )*
                }
            }
        }

        impl TryFrom<Any> for $name {
            type Error = ();

            fn try_from(det: Any) -> Result<Self, Self::Error> {
                match det {
                    $(
                        Any::$s(s) => Ok($name::$s(s)),
                    )*
                    _ => Err(())
                }
            }
        }
    };
}

#[derive(Clone, PartialEq, Eq)]
pub struct AST<TKind>(Rc<ASTInner>, PhantomData<TKind>)
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>;

impl<TKind> std::hash::Hash for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<TKind> Debug for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("AST").field(&self.0).finish()
    }
}

pub type ASTAny = AST<Any>;

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    pub fn details(&self) -> &Any {
        &self.0.details
    }

    pub fn ptr_eq<TOtherKind>(&self, other: &AST<TOtherKind>) -> bool
    where
        TOtherKind: Clone + TryFrom<Any>,
        Any: From<TOtherKind>,
    {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn parent(&self) -> Option<ASTAny> {
        self.0
            .parent
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade())
            .flatten()
            .map(|node| AST::<Any>(node, PhantomData))
    }

    pub fn downcast(&self) -> TKind {
        match TKind::try_from(self.0.details.clone()) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }

    pub fn contains(&self, other: &AST<Any>) -> bool {
        let mut current = Some(other.clone());

        while let Some(some_current) = current {
            if self.ptr_eq::<Any>(&some_current) {
                return true;
            }

            current = some_current.parent();
        }

        return false;
    }

    pub fn try_downcast<TExpected>(&self) -> Option<TExpected>
    where
        TExpected: TryFrom<Any>,
        Any: From<TExpected>,
    {
        TExpected::try_from(self.0.details.clone()).ok()
    }

    pub fn upcast(self) -> ASTAny {
        AST::<Any>(self.0, PhantomData)
    }

    pub fn recast<TExpected>(self) -> AST<TExpected>
    where
        TExpected: Clone + TryFrom<Any> + From<TKind>,
        Any: From<TExpected>,
    {
        AST::<TExpected>(self.0, PhantomData)
    }

    pub fn try_recast<TExpected>(self) -> Option<AST<TExpected>>
    where
        TExpected: Clone + TryFrom<Any> + TryFrom<TKind>,
        Any: From<TExpected>,
    {
        TExpected::try_from(self.0.details.clone())
            .ok()
            .map(|_| AST::<TExpected>(self.0, PhantomData))
    }
}

pub trait Slicable {
    fn slice(&self) -> &Slice;

    fn spanning<T: Slicable>(&self, other: &T) -> Slice {
        let this_slice = self.slice().clone();
        let other_slice = other.slice();

        this_slice.join(other_slice)
    }
}

impl Slicable for Slice {
    fn slice(&self) -> &Slice {
        self
    }
}

impl<TKind> Slicable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn slice(&self) -> &Slice {
        &self.0.slice
    }
}

pub trait Parentable {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        // do nothing by default
    }
}

impl<TKind> Parentable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        self.0
            .as_ref()
            .parent
            .replace(Some(Rc::downgrade(&parent.0)));
    }
}

impl<T> Parentable for Option<T>
where
    T: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        if let Some(s) = self {
            s.set_parent(parent);
        }
    }
}

impl<T> Parentable for Vec<T>
where
    T: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        for ast in self.iter_mut() {
            ast.set_parent(parent);
        }
    }
}

// just for the sake of make_node!() macros
impl Parentable for bool {}
impl Parentable for Slice {}
impl Parentable for PlatformSet {}
impl Parentable for ModifierTypeKind {}
impl Parentable for SpecialTypeKind {}
impl Parentable for AwaitOrDetach {}
impl Parentable for RegularExpressionFlag {}

#[derive(Clone)]
pub struct ASTInner {
    pub parent: RefCell<Option<Weak<ASTInner>>>,
    pub slice: Slice,
    pub details: Any,
}

impl Debug for ASTInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.details))
    }
}

impl PartialEq for ASTInner {
    fn eq(&self, other: &Self) -> bool {
        self.parent.borrow().as_ref().map(|w| w.as_ptr())
            == other.parent.borrow().as_ref().map(|w| w.as_ptr())
            && self.slice == other.slice
            && self.details == other.details
    }
}

impl Eq for ASTInner {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub module_id: ModuleID,
    pub declarations: Vec<AST<Declaration>>,
}

// --- Declarations ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportAllDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<PlainIdentifier>,
    pub path: AST<ExactStringLiteral>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub imports: Vec<AST<ImportItem>>,
    pub path: AST<ExactStringLiteral>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportItem {
    pub name: AST<PlainIdentifier>,
    pub alias: Option<AST<PlainIdentifier>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub name: AST<PlainIdentifier>,
    pub declared_type: AST<TypeExpression>,
    pub exported: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<PlainIdentifier>,
    pub func: AST<Func>,
    pub exported: bool,
    pub decorators: Vec<AST<Decorator>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<PlainIdentifier>,
    pub proc: AST<Proc>,
    pub exported: bool,
    pub decorators: Vec<AST<Decorator>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decorator {
    pub name: AST<PlainIdentifier>,
    pub arguments: Option<Vec<AST<Expression>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub destination: DeclarationDestination,
    pub value: AST<Expression>,
    pub is_const: bool,
    pub exported: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolDeclaration {
    pub name: AST<PlainIdentifier>,
    pub exported: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestExprDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<ExactStringLiteral>,
    pub expr: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestBlockDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<ExactStringLiteral>,
    pub block: AST<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestTypeDeclaration {
    pub name: AST<ExactStringLiteral>,
    pub destination_type: AST<TypeExpression>,
    pub value_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclarationPlatforms {
    pub platforms: Vec<AST<PlainIdentifier>>,
}

impl TryFrom<&AST<DeclarationPlatforms>> for PlatformSet {
    type Error = ();

    fn try_from(value: &AST<DeclarationPlatforms>) -> Result<Self, Self::Error> {
        let mut set = PlatformSet::new();

        for platform in value.downcast().platforms {
            match platform.downcast().0.as_str() {
                "deno" => set.deno = true,
                "node" => set.node = true,
                "bun" => set.bun = true,
                "browser" => set.browser = true,
                _ => return Err(()),
            }
        }

        Ok(set)
    }
}

pub const JS_GLOBAL_IDENTIFIER: &str = "jsGlobal";
pub const JS_FILE_EXTENSIONS: [&str; 2] = ["js", "ts"];
pub const VALID_PLATFORMS: [&str; 4] = ["deno", "node", "bun", "browser"];

// --- Expressions ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NilLiteral;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanLiteral(pub bool);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberLiteral(pub Slice);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    pub tag: Option<AST<PlainIdentifier>>,
    pub segments: Vec<StringLiteralSegment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExactStringLiteral {
    pub tag: Option<AST<PlainIdentifier>>,
    pub value: Slice,
}

impl From<Slice> for ExactStringLiteral {
    fn from(value: Slice) -> Self {
        Self { tag: None, value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayLiteral(pub Vec<ElementOrSpread<AST<Expression>>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectLiteral(pub Vec<KeyValueOrSpread<AST<Expression>>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryOperation {
    pub left: AST<Expression>,
    pub op: AST<BinaryOperator>,
    pub right: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryOperator(pub BinaryOperatorOp);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NegationOperation(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parenthesis(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalIdentifier(pub Slice);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlineConstGroup {
    pub declarations: Vec<AST<InlineDeclaration>>,
    pub inner: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlineDeclaration {
    pub destination: DeclarationDestination,
    pub value: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub type_annotation: AST<FuncType>,
    pub is_async: bool,
    pub body: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Proc {
    pub type_annotation: AST<ProcType>,
    pub is_async: bool,
    pub body: AST<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block(pub Vec<AST<Statement>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RangeExpression {
    pub start: AST<Expression>,
    pub end: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AwaitExpression(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Invocation {
    pub subject: AST<Expression>,
    pub args: Vec<AST<Expression>>,
    pub spread_args: Option<AST<Expression>>,
    pub type_args: Vec<AST<TypeExpression>>,
    pub bubbles: bool,
    pub awaited_or_detached: Option<AwaitOrDetach>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropertyAccessor {
    pub subject: AST<Expression>,
    pub property: Property,
    pub optional: bool,
}

pub fn method_call_as_invocation<'a>(
    ctx: InferTypeContext<'a>,
    invocation: AST<Invocation>,
) -> Option<AST<Expression>> {
    let Invocation {
        subject,
        args,
        spread_args,
        type_args,
        bubbles,
        awaited_or_detached,
    } = invocation.downcast();

    match subject.downcast() {
        Expression::PropertyAccessor(PropertyAccessor {
            subject: property_subject,
            property,
            optional,
        }) => {
            let property_type = match &property {
                Property::Expression(expr) => expr.infer_type(ctx.into()),
                Property::PlainIdentifier(ident) => {
                    Type::StringType(Some(ident.downcast().0.clone()))
                }
            };

            if !property_subject
                .infer_type(ctx)
                .property_exists(ctx.into(), &property_type)
            {
                if !optional {
                    if let Property::PlainIdentifier(identifier) = property {
                        let subject: AST<LocalIdentifier> = identifier.into();

                        if subject
                            .resolve_symbol(ctx.into(), subject.downcast().0.as_str())
                            .is_some()
                        {
                            return Some(
                                Expression::Invocation(Invocation {
                                    subject: subject.recast::<Expression>(),
                                    args: std::iter::once(property_subject)
                                        .chain(args.into_iter())
                                        .collect(),
                                    spread_args,
                                    type_args,
                                    bubbles,
                                    awaited_or_detached,
                                })
                                .as_ast(invocation.slice().clone()),
                            );
                        }
                    }
                }
            }
        }
        _ => {}
    }

    None
}

impl From<AST<PlainIdentifier>> for AST<LocalIdentifier> {
    fn from(plain_identifier: AST<PlainIdentifier>) -> Self {
        let mut local =
            LocalIdentifier(plain_identifier.downcast().0).as_ast(plain_identifier.slice().clone());

        local.set_parent(plain_identifier.parent().as_ref().unwrap());

        local
    }
}

impl From<AST<LocalIdentifier>> for AST<PlainIdentifier> {
    fn from(plain_identifier: AST<LocalIdentifier>) -> Self {
        let mut local =
            PlainIdentifier(plain_identifier.downcast().0).as_ast(plain_identifier.slice().clone());

        local.set_parent(plain_identifier.parent().as_ref().unwrap());

        local
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfElseExpression {
    pub cases: Vec<AST<IfElseExpressionCase>>,
    pub default_case: Option<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfElseExpressionCase {
    pub condition: AST<Expression>,
    pub outcome: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchExpression {
    pub value: AST<Expression>,
    pub cases: Vec<AST<SwitchExpressionCase>>,
    pub default_case: Option<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchExpressionCase {
    pub type_filter: AST<TypeExpression>,
    pub outcome: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpreadExpression(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementTag {
    pub tag_name: AST<PlainIdentifier>,
    pub attributes: Vec<(AST<PlainIdentifier>, AST<Expression>)>,
    pub children: Vec<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsCast {
    pub inner: AST<Expression>,
    pub as_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceOf {
    pub inner: AST<Expression>,
    pub possible_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorExpression(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegularExpression {
    pub expr: Slice,
    pub flags: Vec<RegularExpressionFlag>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnyLiteral;

// --- Type expressions ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionType(pub Vec<AST<TypeExpression>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaybeType(pub AST<TypeExpression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedType(pub AST<LocalIdentifier>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParamType {
    pub name: AST<PlainIdentifier>,
    pub extends: Option<AST<TypeExpression>>,
}

// TODO: Args as an AST node

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcType {
    pub type_params: Vec<AST<TypeParam>>,
    pub args: Vec<AST<Arg>>,
    pub args_spread: Option<AST<Arg>>,
    pub is_async: bool,
    pub throws: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub type_params: Vec<AST<TypeParam>>,
    pub args: Vec<AST<Arg>>,
    pub args_spread: Option<AST<Arg>>,
    pub returns: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arg {
    pub name: AST<PlainIdentifier>,
    pub type_annotation: Option<AST<TypeExpression>>,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub type_params: Vec<AST<TypeParam>>,
    pub inner: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam {
    pub name: AST<PlainIdentifier>,
    pub extends: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundGenericType {
    pub type_args: Vec<AST<TypeExpression>>,
    pub generic: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectType {
    pub entries: Vec<KeyValueOrSpread<AST<TypeExpression>>>,
    pub is_interface: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordType {
    pub key_type: AST<TypeExpression>,
    pub value_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType(pub AST<TypeExpression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleType(pub Vec<ElementOrSpread<AST<TypeExpression>>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteralType(pub Slice);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberLiteralType(pub Slice);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanLiteralType(pub bool);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NilType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenthesizedType(pub AST<TypeExpression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpecialType {
    pub kind: SpecialTypeKind,
    pub inner: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModifierType {
    pub kind: ModifierTypeKind,
    pub inner: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeofType(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegularExpressionType; // TODO: Number of match groups?

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropertyType {
    pub subject: AST<TypeExpression>,
    pub property: AST<TypeExpression>,
    pub optional: bool,
}

// --- Statements ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfElseStatement {
    pub cases: Vec<AST<IfElseStatementCase>>,
    pub default_case: Option<AST<Block>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfElseStatementCase {
    pub condition: AST<Expression>,
    pub outcome: AST<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForLoop {
    pub item_identifier: AST<PlainIdentifier>,
    pub iterable: AST<Expression>,
    pub body: AST<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileLoop {
    pub condition: AST<Expression>,
    pub body: AST<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
    pub target: AST<Expression>,
    pub value: AST<Expression>,
    pub operator: Option<AST<BinaryOperator>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TryCatch {
    pub try_block: AST<Block>,
    pub error_identifier: AST<PlainIdentifier>,
    pub catch_block: AST<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ThrowStatement {
    pub error_expression: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Autorun {
    pub effect_block: AST<Block>,
    pub until: Option<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlainIdentifier(pub Slice);

// --- Pieces of AST nodes ---

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString, IntoStaticStr)]
pub enum SpecialTypeKind {
    Iterable,
    Plan,
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString, IntoStaticStr)]
pub enum ModifierTypeKind {
    #[strum(serialize = "readonly")]
    Readonly,
    #[strum(serialize = "keyof")]
    Keyof,
    #[strum(serialize = "valueof")]
    Valueof,
    #[strum(serialize = "elementof")]
    Elementof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StringLiteralSegment {
    Slice(Slice),
    AST(AST<Expression>),
}
impl From<Slice> for StringLiteralSegment {
    fn from(s: Slice) -> Self {
        Self::Slice(s)
    }
}
impl From<AST<Expression>> for StringLiteralSegment {
    fn from(s: AST<Expression>) -> Self {
        Self::AST(s)
    }
}
impl Parentable for StringLiteralSegment {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            StringLiteralSegment::Slice(_) => {}
            StringLiteralSegment::AST(expr) => expr.set_parent(parent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValueType {
    pub key: ASTAny,
    pub value: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpreadType(pub AST<TypeExpression>);

union_type!(DeclarationDestination = NameAndType | Destructure);

impl DeclarationDestination {
    pub fn slice(&self) -> Slice {
        match self {
            DeclarationDestination::NameAndType(NameAndType {
                name,
                type_annotation,
            }) => todo!(),
            DeclarationDestination::Destructure(Destructure {
                properties,
                spread,
                destructure_kind,
            }) => todo!(),
        }
    }
}

impl Parentable for DeclarationDestination {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            DeclarationDestination::NameAndType(NameAndType {
                name,
                type_annotation,
            }) => {
                name.set_parent(parent);
                type_annotation.set_parent(parent);
            }
            DeclarationDestination::Destructure(Destructure {
                properties,
                spread,
                destructure_kind,
            }) => {
                properties.set_parent(parent);
                spread.set_parent(parent);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameAndType {
    pub name: AST<PlainIdentifier>,
    pub type_annotation: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Destructure {
    pub properties: Vec<AST<PlainIdentifier>>,
    pub spread: Option<AST<PlainIdentifier>>,
    pub destructure_kind: DestructureKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DestructureKind {
    Array,
    Object,
}

// --- Utils ---

pub fn covering<TKind>(vec: &Vec<AST<TKind>>) -> Option<Slice>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    vec.get(0)
        .map(|first| first.slice().clone().join(vec[vec.len() - 1].slice()))
}

pub trait WithSlice: Sized
where
    Self: Clone + TryFrom<Any>,
    Any: From<Self>,
{
    fn as_ast(self, src: Slice) -> AST<Self>;
}

impl<TKind> WithSlice for TKind
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn as_ast(self, src: Slice) -> AST<TKind> {
        AST(
            ASTInner {
                parent: RefCell::new(None),
                slice: src,
                details: self.into(),
            }
            .rc(),
            PhantomData,
        )
    }
}

// --- Misc data ---

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString, IntoStaticStr)]
pub enum RegularExpressionFlag {
    #[strum(serialize = "d")]
    D,
    #[strum(serialize = "g")]
    G,
    #[strum(serialize = "i")]
    I,
    #[strum(serialize = "m")]
    M,
    #[strum(serialize = "s")]
    S,
    #[strum(serialize = "u")]
    U,
    #[strum(serialize = "y")]
    Y,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString, IntoStaticStr)]
pub enum AwaitOrDetach {
    #[strum(serialize = "await")]
    Await,
    #[strum(serialize = "detach")]
    Detach,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString, IntoStaticStr)]
pub enum BinaryOperatorOp {
    #[strum(serialize = "??")]
    NullishCoalescing,
    #[strum(serialize = "||")]
    Or,
    #[strum(serialize = "&&")]
    And,
    #[strum(serialize = "==")]
    Equals,
    #[strum(serialize = "!=")]
    NotEquals,
    #[strum(serialize = "<=")]
    LessEqual,
    #[strum(serialize = ">=")]
    GreaterEqual,
    #[strum(serialize = "<")]
    Less,
    #[strum(serialize = ">")]
    Greater,
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "*")]
    Times,
    #[strum(serialize = "/")]
    Divide,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlatformSet {
    pub node: bool,
    pub deno: bool,
    pub bun: bool,
    pub browser: bool,
}

impl PlatformSet {
    pub fn new() -> Self {
        PlatformSet {
            node: false,
            deno: false,
            bun: false,
            browser: false,
        }
    }

    pub fn subset_of(self, other: PlatformSet) -> bool {
        (self.node == false || other.node == true)
            && (self.deno == false || other.deno == true)
            && (self.bun == false || other.bun == true)
            && (self.browser == false || other.browser == true)
    }
}

impl Display for PlatformSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;

        if self.node {
            if !first {
                f.write_str(", ")?;
            }

            f.write_str("node")?;

            first = true;
        }

        if self.deno {
            if !first {
                f.write_str(", ")?;
            }

            f.write_str("deno")?;

            first = true;
        }

        if self.bun {
            if !first {
                f.write_str(", ")?;
            }

            f.write_str("bun")?;

            first = true;
        }

        if self.browser {
            if !first {
                f.write_str(", ")?;
            }

            f.write_str("browser")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Property {
    Expression(AST<Expression>),
    PlainIdentifier(AST<PlainIdentifier>),
}

impl Parentable for Property {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            Property::Expression(expr) => expr.set_parent(parent),
            Property::PlainIdentifier(ident) => ident.set_parent(parent),
        }
    }
}

impl<T, U> Parentable for (T, U)
where
    T: Parentable,
    U: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        self.0.set_parent(parent);
        self.1.set_parent(parent);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyValueOrSpread<T> {
    KeyValue(T, T, bool),
    Spread(T),
}

impl<T> Parentable for KeyValueOrSpread<T>
where
    T: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            KeyValueOrSpread::KeyValue(key, value, _optional) => {
                key.set_parent(parent);
                value.set_parent(parent);
            }
            KeyValueOrSpread::Spread(spread) => {
                spread.set_parent(parent);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElementOrSpread<T> {
    Element(T),
    Spread(T),
}

impl<T> Parentable for ElementOrSpread<T>
where
    T: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            ElementOrSpread::Element(element) => {
                element.set_parent(parent);
            }
            ElementOrSpread::Spread(spread) => {
                spread.set_parent(parent);
            }
        }
    }
}

impl From<AST<PlainIdentifier>> for AST<ExactStringLiteral> {
    fn from(value: AST<PlainIdentifier>) -> Self {
        ExactStringLiteral {
            tag: None,
            value: value.downcast().0.clone(),
        }
        .as_ast(value.slice().clone())
    }
}

impl From<AST<LocalIdentifier>> for AST<ExactStringLiteral> {
    fn from(value: AST<LocalIdentifier>) -> Self {
        ExactStringLiteral {
            tag: None,
            value: value.downcast().0.clone(),
        }
        .as_ast(value.slice().clone())
    }
}

impl From<AST<PlainIdentifier>> for AST<StringLiteralType> {
    fn from(value: AST<PlainIdentifier>) -> Self {
        StringLiteralType(value.downcast().0.clone()).as_ast(value.slice().clone())
    }
}

// --- AST groups ---

union_type! {
    Any = Module
        | ImportAllDeclaration
        | ImportDeclaration
        | ImportItem
        | TypeDeclaration
        | FuncDeclaration
        | ProcDeclaration
        | Decorator
        | ValueDeclaration
        | SymbolDeclaration
        | TestExprDeclaration
        | TestBlockDeclaration
        | TestTypeDeclaration
        | DeclarationPlatforms
        | NilLiteral
        | BooleanLiteral
        | NumberLiteral
        | StringLiteral
        | ExactStringLiteral
        | ArrayLiteral
        | ObjectLiteral
        | BinaryOperation
        | BinaryOperator
        | NegationOperation
        | Parenthesis
        | LocalIdentifier
        | InlineConstGroup
        | InlineDeclaration
        | Func
        | Proc
        | Block
        | RangeExpression
        | AwaitExpression
        | Invocation
        | PropertyAccessor
        | IfElseExpression
        | IfElseExpressionCase
        | SwitchExpression
        | SwitchExpressionCase
        | SpreadExpression
        | ElementTag
        | AsCast
        | InstanceOf
        | ErrorExpression
        | RegularExpression
        | AnyLiteral
        | UnionType
        | MaybeType
        | NamedType
        | GenericParamType
        | ProcType
        | FuncType
        | Arg
        | GenericType
        | TypeParam
        | BoundGenericType
        | ObjectType
        | RecordType
        | ArrayType
        | TupleType
        | StringLiteralType
        | NumberLiteralType
        | BooleanLiteralType
        | StringType
        | NumberType
        | BooleanType
        | NilType
        | ParenthesizedType
        | SpecialType
        | ModifierType
        | TypeofType
        | UnknownType
        | RegularExpressionType
        | PropertyType
        | IfElseStatement
        | IfElseStatementCase
        | ForLoop
        | WhileLoop
        | Assignment
        | TryCatch
        | ThrowStatement
        | Autorun
        | PlainIdentifier
}

union_subtype!(
    Declaration = ImportAllDeclaration
        | ImportDeclaration
        | TypeDeclaration
        | FuncDeclaration
        | ProcDeclaration
        | ValueDeclaration
        | SymbolDeclaration
        | TestExprDeclaration
        | TestBlockDeclaration
        | TestTypeDeclaration
);

union_subtype!(
    Expression = NilLiteral
        | BooleanLiteral
        | NumberLiteral
        | StringLiteral
        | ExactStringLiteral
        | ArrayLiteral
        | ObjectLiteral
        | BinaryOperation
        | NegationOperation
        | Parenthesis
        | LocalIdentifier
        | InlineConstGroup
        | Func
        | Proc
        | RangeExpression
        | AwaitExpression
        | Invocation
        | PropertyAccessor
        | IfElseExpression
        | SwitchExpression
        | ElementTag
        | AsCast
        | InstanceOf
        | ErrorExpression
        | RegularExpression
        | AnyLiteral
);

union_subtype!(
    TypeExpression = UnionType
        | MaybeType
        | NamedType
        | GenericParamType
        | ProcType
        | FuncType
        | GenericType
        | BoundGenericType
        | ObjectType
        | RecordType
        | ArrayType
        | TupleType
        | StringLiteralType
        | NumberLiteralType
        | BooleanLiteralType
        | StringType
        | NumberType
        | BooleanType
        | NilType
        | ParenthesizedType
        | SpecialType
        | ModifierType
        | TypeofType
        | UnknownType
        | RegularExpressionType
        | PropertyType
);

union_subtype!(
    Statement = ValueDeclaration
        | IfElseStatement
        | ForLoop
        | WhileLoop
        | Assignment
        | TryCatch
        | ThrowStatement
        | Autorun
        | Invocation
        | Block
);

impl AST<Any> {
    pub fn find_parent<F: Fn(&AST<Any>) -> bool>(&self, f: F) -> Option<AST<Any>> {
        let mut current: Option<AST<Any>> = Some(self.clone());

        while let Some(parent) = current {
            if f(&parent) {
                return Some(parent.clone());
            }

            current = parent.parent();
        }

        return None;
    }

    pub fn find_parent_of_type<TExpected>(&self) -> Option<AST<TExpected>>
    where
        TExpected: TryFrom<Any> + Clone,
        Any: From<TExpected>,
    {
        self.find_parent(|p| p.try_downcast::<TExpected>().is_some())
            .map(|module| module.try_recast::<TExpected>().unwrap())
    }

    pub fn module_id(&self) -> Option<ModuleID> {
        self.find_parent_of_type::<Module>()
            .map(|module| module.downcast().module_id)
    }
}
