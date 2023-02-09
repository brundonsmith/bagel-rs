use crate::model::slice::Slice;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};
use strum_macros::{EnumString, IntoStaticStr};

use super::module::ModuleID;

macro_rules! union_type {
    ($name:ident = $( $s:ident )|*) => {
        #[derive(Clone, Debug, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct AST<TKind>(Rc<ASTInner>, PhantomData<TKind>)
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>;

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

#[derive(Debug, Clone)]
pub struct ASTInner {
    pub parent: RefCell<Option<Weak<ASTInner>>>,
    pub slice: Slice,
    pub details: Any,
}

impl PartialEq for ASTInner {
    fn eq(&self, other: &Self) -> bool {
        self.parent.borrow().as_ref().map(|w| w.as_ptr())
            == other.parent.borrow().as_ref().map(|w| w.as_ptr())
            && self.slice == other.slice
            && self.details == other.details
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub module_id: ModuleID,
    pub declarations: Vec<AST<Declaration>>,
}

// --- Declarations ---
#[derive(Debug, Clone, PartialEq)]
pub struct ImportAllDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<PlainIdentifier>,
    pub path: AST<ExactStringLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub imports: Vec<AST<ImportItem>>,
    pub path: AST<ExactStringLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub name: AST<PlainIdentifier>,
    pub alias: Option<AST<PlainIdentifier>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDeclaration {
    pub name: AST<PlainIdentifier>,
    pub declared_type: AST<TypeExpression>,
    pub exported: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<PlainIdentifier>,
    pub func: AST<Func>,
    pub exported: bool,
    pub decorators: Vec<AST<Decorator>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<PlainIdentifier>,
    pub proc: AST<Proc>,
    pub exported: bool,
    pub decorators: Vec<AST<Decorator>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator {
    pub name: AST<PlainIdentifier>,
    pub arguments: Option<Vec<AST<Expression>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub destination: DeclarationDestination,
    pub value: AST<Expression>,
    pub is_const: bool,
    pub exported: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolDeclaration {
    pub name: AST<PlainIdentifier>,
    pub exported: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestExprDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<ExactStringLiteral>,
    pub expr: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestBlockDeclaration {
    pub platforms: Option<AST<DeclarationPlatforms>>,
    pub name: AST<ExactStringLiteral>,
    pub block: AST<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestTypeDeclaration {
    pub name: AST<ExactStringLiteral>,
    pub destination_type: AST<TypeExpression>,
    pub value_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct NilLiteral;

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral(pub bool);

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral(pub Slice);

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub tag: Option<AST<PlainIdentifier>>,
    pub segments: Vec<StringLiteralSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExactStringLiteral {
    pub tag: Option<AST<PlainIdentifier>>,
    pub value: Slice,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral(pub Vec<ElementOrSpread<AST<Expression>>>);

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectLiteral(pub Vec<KeyValueOrSpread<AST<Expression>>>);

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub left: AST<Expression>,
    pub op: AST<BinaryOperator>,
    pub right: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperator(pub BinaryOperatorOp);

#[derive(Debug, Clone, PartialEq)]
pub struct NegationOperation(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct Parenthesis(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct LocalIdentifier(pub Slice);

#[derive(Debug, Clone, PartialEq)]
pub struct InlineConstGroup {
    pub declarations: Vec<AST<InlineDeclaration>>,
    pub inner: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineDeclaration {
    pub destination: DeclarationDestination,
    pub value: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub type_annotation: AST<FuncType>,
    pub is_async: bool,
    pub is_pure: bool,
    pub body: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Proc {
    pub type_annotation: AST<ProcType>,
    pub is_async: bool,
    pub is_pure: bool,
    pub body: AST<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<AST<Statement>>);

#[derive(Debug, Clone, PartialEq)]
pub struct JavascriptEscape(pub Slice);

#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpression {
    pub start: AST<Expression>,
    pub end: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AwaitExpression(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct Invocation {
    pub subject: AST<Expression>,
    pub args: Vec<AST<Expression>>,
    pub spread_args: Option<AST<Expression>>,
    pub type_args: Vec<AST<TypeExpression>>,
    pub bubbles: bool,
    pub awaited_or_detached: Option<AwaitOrDetach>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyAccessor {
    pub subject: AST<Expression>,
    pub property: Property,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfElseExpression {
    pub cases: Vec<AST<IfElseExpressionCase>>,
    pub default_case: Option<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfElseExpressionCase {
    pub condition: AST<Expression>,
    pub outcome: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchExpression {
    pub value: AST<Expression>,
    pub cases: Vec<AST<SwitchExpressionCase>>,
    pub default_case: Option<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchExpressionCase {
    pub type_filter: AST<TypeExpression>,
    pub outcome: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpreadExpression(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct ElementTag {
    pub tag_name: AST<PlainIdentifier>,
    pub attributes: Vec<KeyValue>,
    pub children: Vec<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsCast {
    pub inner: AST<Expression>,
    pub as_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceOf {
    pub inner: AST<Expression>,
    pub possible_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorExpression(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct RegularExpression {
    pub expr: Slice,
    pub flags: Vec<RegularExpressionFlag>,
}

// --- Type expressions ---
#[derive(Debug, Clone, PartialEq)]
pub struct UnionType(pub Vec<AST<TypeExpression>>);

#[derive(Debug, Clone, PartialEq)]
pub struct MaybeType(pub AST<TypeExpression>);

#[derive(Debug, Clone, PartialEq)]
pub struct NamedType(pub AST<LocalIdentifier>);

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParamType {
    pub name: AST<PlainIdentifier>,
    pub extends: Option<AST<TypeExpression>>,
}

// TODO: Args as an AST node

#[derive(Debug, Clone, PartialEq)]
pub struct ProcType {
    pub args: Vec<AST<Arg>>,
    pub args_spread: Option<AST<Arg>>,
    pub is_pure: bool,
    pub is_async: bool,
    pub throws: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub args: Vec<AST<Arg>>,
    pub args_spread: Option<AST<Arg>>,
    pub is_pure: bool,
    pub is_async: bool,
    pub returns: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub name: AST<PlainIdentifier>,
    pub type_annotation: Option<AST<TypeExpression>>,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericType {
    pub type_params: Vec<AST<TypeParam>>,
    pub inner: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: AST<PlainIdentifier>,
    pub extends: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoundGenericType {
    pub type_args: Vec<AST<TypeExpression>>,
    pub generic: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectType {
    pub entries: Vec<KeyValueOrSpread<AST<TypeExpression>>>,
    pub is_interface: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordType {
    pub key_type: AST<TypeExpression>,
    pub value_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType(pub AST<TypeExpression>);

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType(pub Vec<ElementOrSpread<AST<TypeExpression>>>);

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteralType(pub Slice);

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteralType(pub Slice);

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteralType(pub bool);

#[derive(Debug, Clone, PartialEq)]
pub struct StringType;

#[derive(Debug, Clone, PartialEq)]
pub struct NumberType;

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanType;

#[derive(Debug, Clone, PartialEq)]
pub struct NilType;

#[derive(Debug, Clone, PartialEq)]
pub struct ParenthesizedType(pub AST<TypeExpression>);

#[derive(Debug, Clone, PartialEq)]
pub struct SpecialType {
    pub kind: SpecialTypeKind,
    pub inner: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModifierType {
    pub kind: ModifierTypeKind,
    pub inner: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeofType(pub AST<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct UnknownType;

#[derive(Debug, Clone, PartialEq)]
pub struct RegularExpressionType; // TODO: Number of match groups?

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyType {
    pub subject: AST<TypeExpression>,
    pub property: AST<TypeExpression>,
    pub optional: bool,
}

// --- Statements ---
#[derive(Debug, Clone, PartialEq)]
pub struct IfElseStatement {
    pub cases: Vec<AST<IfElseStatementCase>>,
    pub default_case: Option<AST<Block>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfElseStatementCase {
    pub condition: AST<Expression>,
    pub outcome: AST<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForLoop {
    pub item_identifier: AST<PlainIdentifier>,
    pub iterator: AST<Expression>,
    pub body: AST<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    pub condition: AST<Expression>,
    pub body: AST<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: AST<Expression>,
    pub value: AST<Expression>,
    pub operator: Option<AST<BinaryOperator>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryCatch {
    pub try_block: AST<Block>,
    pub error_identifier: AST<PlainIdentifier>,
    pub catch_block: AST<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThrowStatement {
    pub error_expression: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Autorun {
    pub effect_block: AST<Block>,
    pub until: Option<AST<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PlainIdentifier(pub Slice);

// --- Pieces of AST nodes ---

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumString, IntoStaticStr)]
pub enum SpecialTypeKind {
    Iterator,
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

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValue {
    pub key: ASTAny,
    pub value: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValueType {
    pub key: ASTAny,
    pub value: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct NameAndType {
    pub name: AST<PlainIdentifier>,
    pub type_annotation: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
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
            Rc::new(ASTInner {
                parent: RefCell::new(None),
                slice: src,
                details: self.into(),
            }),
            PhantomData,
        )
    }
}

// --- Misc data ---

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegularExpressionFlag {
    D,
    G,
    I,
    M,
    S,
    U,
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

            first = true;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum KeyValueOrSpread<T> {
    KeyValue(T, T),
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
            KeyValueOrSpread::KeyValue(key, value) => {
                key.set_parent(parent);
                value.set_parent(parent);
            }
            KeyValueOrSpread::Spread(spread) => {
                spread.set_parent(parent);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

pub fn identifier_to_string(ast: AST<PlainIdentifier>) -> AST<ExactStringLiteral> {
    ExactStringLiteral {
        tag: None,
        value: ast.downcast().0.clone(),
    }
    .as_ast(ast.slice().clone())
}

pub fn identifier_to_string_type(ast: AST<PlainIdentifier>) -> AST<StringLiteralType> {
    StringLiteralType(ast.downcast().0.clone()).as_ast(ast.slice().clone())
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
        | JavascriptEscape
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
        | JavascriptEscape
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
