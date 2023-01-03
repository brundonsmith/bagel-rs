use crate::model::slice::Slice;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};
use strum_macros::{EnumString, IntoStaticStr};

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
    pub declarations: Vec<AST<Declaration>>,
}

// --- Declarations ---
#[derive(Debug, Clone, PartialEq)]
pub struct ImportAllDeclaration {
    pub name: AST<PlainIdentifier>,
    pub path: AST<ExactStringLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDeclaration {
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
    pub name: AST<PlainIdentifier>,
    pub func: AST<Func>,
    pub exported: bool,
    pub platforms: PlatformSet,
    pub decorators: Vec<AST<Decorator>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcDeclaration {
    pub name: AST<PlainIdentifier>,
    pub proc: AST<Proc>,
    pub exported: bool,
    pub platforms: PlatformSet,
    pub decorators: Vec<AST<Decorator>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator {
    pub name: AST<PlainIdentifier>,
    // TODO: arguments
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDeclaration {
    pub name: AST<PlainIdentifier>,
    pub type_annotation: Option<AST<TypeExpression>>,
    pub value: AST<Expression>,
    pub is_const: bool,
    pub exported: bool,
    pub platforms: PlatformSet,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestExprDeclaration {
    pub name: AST<ExactStringLiteral>,
    pub expr: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestBlockDeclaration {
    pub name: AST<ExactStringLiteral>,
    pub block: AST<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestTypeDeclaration {
    pub name: AST<ExactStringLiteral>,
    pub destination_type: AST<TypeExpression>,
    pub value_type: AST<TypeExpression>,
}

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
pub struct ArrayLiteral(pub Vec<ArrayLiteralEntry>);

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectLiteral(pub Vec<ObjectLiteralEntry>);

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
    pub awaited: bool,
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
    pub body: AST<Block>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct ProcType {
    pub args: Vec<AST<Arg>>,
    pub args_spread: Option<AST<TypeExpression>>,
    pub is_pure: bool,
    pub is_async: bool,
    pub throws: Option<AST<TypeExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub args: Vec<AST<Arg>>,
    pub args_spread: Option<AST<TypeExpression>>,
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
    pub entries: Vec<ObjectTypeEntry>,
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
pub struct TupleType(pub Vec<AST<TypeExpression>>);

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
pub struct DeclarationStatement {
    pub destination: DeclarationDestination,
    pub value: AST<Expression>,
    pub awaited: bool,
    pub is_const: bool,
}

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

union_type!(ObjectLiteralEntry = KeyValue | SpreadExpression);

impl Parentable for ObjectLiteralEntry {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            ObjectLiteralEntry::KeyValue(KeyValue { key, value }) => {
                key.set_parent(parent);
                value.set_parent(parent);
            }
            ObjectLiteralEntry::SpreadExpression(SpreadExpression(spread)) => {
                spread.set_parent(parent);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValue {
    pub key: ASTAny,
    pub value: AST<Expression>,
}

union_type!(ObjectTypeEntry = KeyValueType | SpreadType);

impl Parentable for ObjectTypeEntry {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            ObjectTypeEntry::KeyValueType(KeyValueType { key, value }) => {
                key.set_parent(parent);
                value.set_parent(parent);
            }
            ObjectTypeEntry::SpreadType(SpreadType(spread)) => {
                spread.set_parent(parent);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValueType {
    pub key: ASTAny,
    pub value: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpreadType(pub AST<TypeExpression>);

union_type!(DeclarationDestination = NameAndType | Destructure);

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
    pub properties: Vec<ASTAny>,
    pub spread: Option<ASTAny>,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AwaitOrDetach {
    Await,
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
    pub fn all() -> Self {
        PlatformSet {
            node: true,
            deno: true,
            bun: true,
            browser: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayLiteralEntry {
    Expression(AST<Expression>),
    Spread(AST<SpreadExpression>),
}

impl Parentable for ArrayLiteralEntry {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        match self {
            ArrayLiteralEntry::Expression(expr) => expr.set_parent(parent),
            ArrayLiteralEntry::Spread(spread) => spread.set_parent(parent),
        }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum ObjectLiteralEntry {
//     KeyValue(AST<Expression>),
//     Spread(AST<SpreadExpression>),
// }

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
        | TestExprDeclaration
        | TestBlockDeclaration
        | TestTypeDeclaration
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
        | DeclarationStatement
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
    Statement = DeclarationStatement
        | IfElseStatement
        | ForLoop
        | WhileLoop
        | Assignment
        | TryCatch
        | ThrowStatement
        | Autorun
        | Invocation
);
