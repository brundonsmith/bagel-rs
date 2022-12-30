use crate::model::slice::Slice;
use enum_variant_type::EnumVariantType;
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
                fn from(s: $s) -> Self {
                    Self::$s(s)
                }
            }
            impl TryFrom<$name> for $s {
                type Error = ();

                fn try_from(s: $name) -> Result<Self, ()> {
                    match s {
                        $name::$s(s) => Ok(s),
                        _ => Err(())
                    }
                }
            }
        )*
    };
}

macro_rules! ast_union_type {
    ($name:ident = $( $s:ident )|*) => {
        union_type!($name = $($s)|*);

        impl From<$name> for ASTDetails {
            fn from(un: $name) -> Self {
                match un {
                    $(
                        $name::$s(inner) => ASTDetails::from(inner),
                    )*
                }
            }
        }
    }
}

macro_rules! simple_enum {
    ($name:ident = $( $s:ident )|*) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum $name {
            $($s),*
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST<TKind>(Rc<ASTInner>, PhantomData<TKind>)
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>;

pub type ASTAny = AST<ASTDetails>;

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    pub fn slice(&self) -> &Slice {
        &self.0.slice
    }

    pub fn details(&self) -> &ASTDetails {
        &self.0.details
    }

    pub fn parent(&self) -> Option<ASTAny> {
        self.0
            .parent
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade())
            .flatten()
            .map(|node| AST::<ASTDetails>(node, PhantomData))
    }

    pub fn downcast(&self) -> TKind {
        match TKind::try_from(self.0.details.clone()) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }

    pub fn try_downcast<TExpected>(&self) -> Option<TExpected>
    where
        TExpected: TryFrom<ASTDetails>,
        ASTDetails: From<TExpected>,
    {
        TExpected::try_from(self.0.details.clone()).ok()
    }

    pub fn upcast(self) -> ASTAny {
        AST::<ASTDetails>(self.0, PhantomData)
    }

    pub fn recast<TExpected>(self) -> AST<TExpected>
    where
        TExpected: Clone + TryFrom<ASTDetails> + From<TKind>,
        ASTDetails: From<TExpected>,
    {
        AST::<TExpected>(self.0, PhantomData)
    }
}

pub trait Parentable {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<ASTDetails>,
        ASTDetails: From<TParentKind>;
}

impl<TKind> Parentable for AST<TKind>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<ASTDetails>,
        ASTDetails: From<TParentKind>,
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
        TParentKind: Clone + TryFrom<ASTDetails>,
        ASTDetails: From<TParentKind>,
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
        TParentKind: Clone + TryFrom<ASTDetails>,
        ASTDetails: From<TParentKind>,
    {
        for ast in self.iter_mut() {
            ast.set_parent(parent);
        }
    }
}

impl Parentable for ObjectTypeEntry {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<ASTDetails>,
        ASTDetails: From<TParentKind>,
    {
        match self {
            ObjectTypeEntry::KeyValueType(KeyValueType { key, value }) => {
                key.set_parent(parent);
                value.set_parent(parent);
            }
            ObjectTypeEntry::SpreadType(SpreadType(expr)) => expr.set_parent(parent),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTInner {
    parent: RefCell<Option<Weak<ASTInner>>>,
    slice: Slice,
    details: ASTDetails,
}

impl PartialEq for ASTInner {
    fn eq(&self, other: &Self) -> bool {
        self.parent.borrow().as_ref().map(|w| w.as_ptr())
            == other.parent.borrow().as_ref().map(|w| w.as_ptr())
            && self.slice == other.slice
            && self.details == other.details
    }
}

#[derive(Debug, Clone, PartialEq, EnumVariantType)]
pub enum ASTDetails {
    #[evt(derive(Debug, Clone, PartialEq))]
    Module {
        declarations: Vec<AST<Declaration>>,
    },

    // --- Declarations ---
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        name: AST<PlainIdentifier>,
        path: AST<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        imports: Vec<AST<ImportItem>>,
        path: AST<ExactStringLiteral>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportItem {
        name: AST<PlainIdentifier>,
        alias: Option<AST<PlainIdentifier>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        name: AST<PlainIdentifier>,
        declared_type: ASTAny,
        exported: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        name: AST<PlainIdentifier>,
        func: AST<Func>,
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<AST<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        name: AST<PlainIdentifier>,
        proc: AST<Proc>,
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<AST<Decorator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Decorator {
        name: AST<PlainIdentifier>,
        // TODO: arguments
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        name: AST<PlainIdentifier>,
        type_annotation: Option<ASTAny>,
        value: AST<Expression>,
        is_const: bool,
        exported: bool,
        platforms: PlatformSet,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        name: AST<ExactStringLiteral>,
        expr: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        name: AST<ExactStringLiteral>,
        block: AST<Block>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        name: AST<ExactStringLiteral>,
        destination_type: ASTAny,
        value_type: ASTAny,
    },

    // --- Expressions ---
    #[evt(derive(Debug, Clone, PartialEq))]
    NilLiteral,

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteral(bool),

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteral(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteral {
        tag: Option<AST<PlainIdentifier>>,
        segments: Vec<StringLiteralSegment>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ExactStringLiteral {
        tag: Option<AST<PlainIdentifier>>,
        value: Slice,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayLiteral(Vec<ASTAny>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectLiteral(Vec<ObjectLiteralEntry>),

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperation {
        left: AST<Expression>,
        op: AST<BinaryOperator>,
        right: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperator(BinaryOperatorOp),

    #[evt(derive(Debug, Clone, PartialEq))]
    NegationOperation(AST<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    Parenthesis(AST<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    LocalIdentifier(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineConstGroup {
        declarations: Vec<AST<InlineDeclaration>>,
        inner: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineDeclaration {
        destination: DeclarationDestination,
        awaited: bool,
        value: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Func {
        type_annotation: AST<FuncType>,
        is_async: bool,
        is_pure: bool,
        body: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Proc {
        type_annotation: AST<ProcType>,
        is_async: bool,
        is_pure: bool,
        body: AST<Block>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Block(Vec<ASTAny>),

    #[evt(derive(Debug, Clone, PartialEq))]
    JavascriptEscape(String),

    #[evt(derive(Debug, Clone, PartialEq))]
    RangeExpression {
        start: AST<Expression>,
        end: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Invocation {
        subject: AST<Expression>,
        args: Vec<AST<Expression>>,
        spread_args: Option<ASTAny>,
        type_args: Vec<ASTAny>,
        bubbles: bool,
        awaited_or_detached: Option<AwaitOrDetach>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyAccessor {
        subject: ASTAny,
        property: ASTAny,
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpression {
        cases: Vec<AST<IfElseExpressionCase>>,
        default_case: Option<AST<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpressionCase {
        condition: AST<Expression>,
        outcome: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpression {
        value: AST<Expression>,
        cases: Vec<AST<SwitchExpressionCase>>,
        default_case: Option<AST<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpressionCase {
        type_filter: ASTAny,
        outcome: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SpreadExpression(AST<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementTag {
        tag_name: AST<PlainIdentifier>,
        attributes: Vec<KeyValue>,
        children: Vec<AST<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AsCast {
        inner: AST<Expression>,
        as_type: ASTAny,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InstanceOf {
        inner: AST<Expression>,
        possible_type: ASTAny,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorExpression(AST<Expression>),

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpression {
        expr: String,
        flags: Vec<RegularExpressionFlag>,
    },

    // --- Type expressions ---
    #[evt(derive(Debug, Clone, PartialEq))]
    UnionType(Vec<ASTAny>),

    #[evt(derive(Debug, Clone, PartialEq))]
    MaybeType(ASTAny),

    #[evt(derive(Debug, Clone, PartialEq))]
    NamedType(AST<PlainIdentifier>),

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericParamType {
        name: AST<PlainIdentifier>,
        extends: Option<ASTAny>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcType {
        args: Vec<AST<Arg>>,
        args_spread: Option<ASTAny>,
        is_pure: bool,
        is_async: bool,
        throws: Option<ASTAny>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncType {
        args: Vec<AST<Arg>>,
        args_spread: Option<ASTAny>,
        is_pure: bool,
        is_async: bool,
        returns: Option<ASTAny>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Arg {
        name: AST<PlainIdentifier>,
        type_annotation: Option<ASTAny>,
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericType {
        type_params: Vec<AST<TypeParam>>,
        inner: ASTAny,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeParam {
        name: AST<PlainIdentifier>,
        extends: Option<ASTAny>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BoundGenericType {
        type_args: Vec<ASTAny>,
        generic: ASTAny,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectType {
        entries: Vec<ObjectTypeEntry>,
        is_interface: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        key_type: ASTAny,
        value_type: ASTAny,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType(ASTAny),

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType(Vec<ASTAny>),

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteralType(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteralType(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteralType(bool),

    StringType,

    NumberType,

    BooleanType,

    NilType,

    #[evt(derive(Debug, Clone, PartialEq))]
    ParenthesizedType(ASTAny),

    #[evt(derive(Debug, Clone, PartialEq))]
    SpecialType {
        kind: SpecialTypeKind,
        inner: ASTAny,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ModifierType {
        kind: ModifierTypeKind,
        inner: ASTAny,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeofType(AST<Expression>),

    UnknownType,

    RegularExpressionType, // TODO: Number of match groups?

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyType {
        subject: ASTAny,
        property: ASTAny,
        optional: bool,
    },

    // --- Statements ---
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        destination: DeclarationDestination,
        value: AST<Expression>,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        cases: Vec<AST<IfElseStatementCase>>,
        default_case: Option<AST<Block>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatementCase {
        condition: AST<Expression>,
        outcome: AST<Block>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        item_identifier: AST<PlainIdentifier>,
        iterator: AST<Expression>,
        body: AST<Block>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        condition: AST<Expression>,
        body: AST<Block>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        target: ASTAny,
        value: AST<Expression>,
        operator: Option<AST<BinaryOperator>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        try_block: AST<Block>,
        error_identifier: AST<PlainIdentifier>,
        catch_block: AST<Block>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement {
        error_expression: AST<Expression>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        effect_block: AST<Block>,
        until: Option<AST<Expression>>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PlainIdentifier(Slice),
}

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

// union_type!(StringLiteralSegment = Slice | ASTAny);

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

union_type!(ObjectLiteralEntry = KeyValue | SpreadExpression);

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValue {
    pub key: ASTAny,
    pub value: AST<Expression>,
}

union_type!(ObjectTypeEntry = KeyValueType | SpreadType);

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValueType {
    pub key: ASTAny,
    pub value: ASTAny,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpreadType(pub ASTAny);

union_type!(DeclarationDestination = NameAndType | Destructure);

impl Parentable for DeclarationDestination {
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<ASTDetails>,
        ASTDetails: From<TParentKind>,
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
    pub type_annotation: Option<ASTAny>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Destructure {
    pub properties: Vec<ASTAny>,
    pub spread: Option<ASTAny>,
    pub destructure_kind: DestructureKind,
}

simple_enum!(DestructureKind = Array | Object);

// --- Utils ---

pub fn covering<TKind>(vec: &Vec<AST<TKind>>) -> Option<Slice>
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    vec.get(0)
        .map(|first| first.slice().clone().spanning(vec[vec.len() - 1].slice()))
}

pub trait WithSlice: Sized
where
    Self: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<Self>,
{
    fn with_slice(self, src: Slice) -> AST<Self>;
}

impl<TKind> WithSlice for TKind
where
    TKind: Clone + TryFrom<ASTDetails>,
    ASTDetails: From<TKind>,
{
    fn with_slice(self, src: Slice) -> AST<TKind> {
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

simple_enum!(RegularExpressionFlag = D | G | I | M | S | U | Y);

simple_enum!(AwaitOrDetach = Await | Detach);

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
    pub browser: bool,
}

impl PlatformSet {
    pub fn all() -> Self {
        PlatformSet {
            node: true,
            deno: true,
            browser: true,
        }
    }
}

// --- AST subgroups ---

ast_union_type!(
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

impl TryFrom<ASTDetails> for Declaration {
    type Error = ();

    fn try_from(value: ASTDetails) -> Result<Self, Self::Error> {
        match value {
            ASTDetails::ImportAllDeclaration { name, path } => {
                Ok(Declaration::ImportAllDeclaration(ImportAllDeclaration {
                    name,
                    path,
                }))
            }
            ASTDetails::ImportDeclaration { imports, path } => {
                Ok(Declaration::ImportDeclaration(ImportDeclaration {
                    imports,
                    path,
                }))
            }
            ASTDetails::TypeDeclaration {
                name,
                declared_type,
                exported,
            } => Ok(Declaration::TypeDeclaration(TypeDeclaration {
                name,
                declared_type,
                exported,
            })),
            ASTDetails::FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            } => Ok(Declaration::FuncDeclaration(FuncDeclaration {
                name,
                func,
                exported,
                platforms,
                decorators,
            })),
            ASTDetails::ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            } => Ok(Declaration::ProcDeclaration(ProcDeclaration {
                name,
                proc,
                exported,
                platforms,
                decorators,
            })),
            ASTDetails::ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            } => Ok(Declaration::ValueDeclaration(ValueDeclaration {
                name,
                type_annotation,
                value,
                is_const,
                exported,
                platforms,
            })),
            ASTDetails::TestExprDeclaration { name, expr } => {
                Ok(Declaration::TestExprDeclaration(TestExprDeclaration {
                    name,
                    expr,
                }))
            }
            ASTDetails::TestBlockDeclaration { name, block } => {
                Ok(Declaration::TestBlockDeclaration(TestBlockDeclaration {
                    name,
                    block,
                }))
            }
            ASTDetails::TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            } => Ok(Declaration::TestTypeDeclaration(TestTypeDeclaration {
                name,
                destination_type,
                value_type,
            })),
            _ => Err(()),
        }
    }
}

ast_union_type!(
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

impl TryFrom<ASTDetails> for Expression {
    type Error = ();

    fn try_from(value: ASTDetails) -> Result<Self, Self::Error> {
        match value {
            ASTDetails::NilLiteral => Ok(Expression::NilLiteral(NilLiteral)),
            ASTDetails::BooleanLiteral(value) => {
                Ok(Expression::BooleanLiteral(BooleanLiteral(value)))
            }
            ASTDetails::StringLiteral { tag, segments } => {
                Ok(Expression::StringLiteral(StringLiteral { tag, segments }))
            }
            ASTDetails::ExactStringLiteral { tag, value } => {
                Ok(Expression::ExactStringLiteral(ExactStringLiteral {
                    tag,
                    value,
                }))
            }
            ASTDetails::ArrayLiteral(entries) => {
                Ok(Expression::ArrayLiteral(ArrayLiteral(entries)))
            }
            ASTDetails::ObjectLiteral(entries) => {
                Ok(Expression::ObjectLiteral(ObjectLiteral(entries)))
            }
            ASTDetails::BinaryOperation { left, op, right } => {
                Ok(Expression::BinaryOperation(BinaryOperation {
                    left,
                    op,
                    right,
                }))
            }
            ASTDetails::NegationOperation(inner) => {
                Ok(Expression::NegationOperation(NegationOperation(inner)))
            }
            ASTDetails::Parenthesis(inner) => Ok(Expression::Parenthesis(Parenthesis(inner))),
            ASTDetails::LocalIdentifier(inner) => {
                Ok(Expression::LocalIdentifier(LocalIdentifier(inner)))
            }
            ASTDetails::InlineConstGroup {
                declarations,
                inner,
            } => Ok(Expression::InlineConstGroup(InlineConstGroup {
                declarations,
                inner,
            })),
            ASTDetails::Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => Ok(Expression::Func(Func {
                type_annotation,
                is_async,
                is_pure,
                body,
            })),
            ASTDetails::Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            } => Ok(Expression::Proc(Proc {
                type_annotation,
                is_async,
                is_pure,
                body,
            })),
            ASTDetails::JavascriptEscape(contents) => {
                Ok(Expression::JavascriptEscape(JavascriptEscape(contents)))
            }
            ASTDetails::RangeExpression { start, end } => {
                Ok(Expression::RangeExpression(RangeExpression { start, end }))
            }
            ASTDetails::Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            } => Ok(Expression::Invocation(Invocation {
                subject,
                args,
                spread_args,
                type_args,
                bubbles,
                awaited_or_detached,
            })),
            ASTDetails::PropertyAccessor {
                subject,
                property,
                optional,
            } => Ok(Expression::PropertyAccessor(PropertyAccessor {
                subject,
                property,
                optional,
            })),
            ASTDetails::IfElseExpression {
                cases,
                default_case,
            } => Ok(Expression::IfElseExpression(IfElseExpression {
                cases,
                default_case,
            })),
            ASTDetails::SwitchExpression {
                value,
                cases,
                default_case,
            } => Ok(Expression::SwitchExpression(SwitchExpression {
                value,
                cases,
                default_case,
            })),
            ASTDetails::ElementTag {
                tag_name,
                attributes,
                children,
            } => Ok(Expression::ElementTag(ElementTag {
                tag_name,
                attributes,
                children,
            })),
            ASTDetails::AsCast { inner, as_type } => {
                Ok(Expression::AsCast(AsCast { inner, as_type }))
            }
            ASTDetails::InstanceOf {
                inner,
                possible_type,
            } => Ok(Expression::InstanceOf(InstanceOf {
                inner,
                possible_type,
            })),
            ASTDetails::ErrorExpression(inner) => {
                Ok(Expression::ErrorExpression(ErrorExpression(inner)))
            }
            ASTDetails::RegularExpression { expr, flags } => {
                Ok(Expression::RegularExpression(RegularExpression {
                    expr,
                    flags,
                }))
            }
            _ => Err(()),
        }
    }
}
