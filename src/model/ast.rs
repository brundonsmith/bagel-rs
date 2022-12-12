use crate::model::slice::Slice;
use enum_variant_type::EnumVariantType;
use std::fmt::Debug;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};
use strum_macros::{EnumString, IntoStaticStr};

#[derive(Debug, Clone, PartialEq)]
pub struct AST(Rc<ASTInner>);

impl AST {
    pub fn slice(&self) -> &Slice {
        &self.0.slice
    }

    pub fn details(&self) -> &ASTDetails {
        &self.0.details
    }

    pub fn parent(&self) -> Option<AST> {
        self.0
            .parent
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade())
            .flatten()
            .map(AST)
    }

    pub fn set_parent(&mut self, parent: &AST) {
        self.0
            .as_ref()
            .parent
            .replace(Some(Rc::downgrade(&parent.0)));
    }

    pub fn expect<T: Debug + TryFrom<ASTDetails, Error = impl Debug>>(&self) -> T {
        T::try_from(self.details().clone()).expect("Broken AST")
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
        declarations: Vec<AST>,
    },

    // --- Declarations ---
    #[evt(derive(Debug, Clone, PartialEq))]
    ImportAllDeclaration {
        name: AST,
        path: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportDeclaration {
        imports: Vec<AST>,
        path: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ImportItem {
        name: AST,
        alias: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeDeclaration {
        name: AST,
        declared_type: AST,
        exported: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncDeclaration {
        name: AST,
        func: AST,
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcDeclaration {
        name: AST,
        proc: AST,
        exported: bool,
        platforms: PlatformSet,
        decorators: Vec<AST>,
    },

    Decorator {
        name: AST,
        // TODO: arguments
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueDeclaration {
        name: AST,
        type_annotation: Option<AST>,
        value: AST,
        is_const: bool,
        exported: bool,
        platforms: PlatformSet,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestExprDeclaration {
        name: AST,
        expr: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestBlockDeclaration {
        name: AST,
        block: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TestTypeDeclaration {
        name: AST,
        destination_type: AST,
        value_type: AST,
    },

    // --- Expressions ---
    NilLiteral,

    #[evt(derive(Debug, Clone, PartialEq))]
    BooleanLiteral(bool),

    #[evt(derive(Debug, Clone, PartialEq))]
    NumberLiteral(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteral {
        tag: Option<AST>,
        segments: Vec<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    StringLiteralRawSegment(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    ExactStringLiteral {
        tag: Option<AST>,
        value: Slice,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayLiteral(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectLiteral(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    Spread(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyValue {
        key: AST,
        value: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperation {
        left: AST,
        op: AST,
        right: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BinaryOperator(BinaryOperatorOp),

    #[evt(derive(Debug, Clone, PartialEq))]
    NegationOperation(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    Parenthesis(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    LocalIdentifier(Slice),

    #[evt(derive(Debug, Clone, PartialEq))]
    InlineConstGroup {
        declarations: Vec<AST>,
        inner: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Func {
        type_annotation: AST,
        is_async: bool,
        is_pure: bool,
        body: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Proc {
        type_annotation: AST,
        is_async: bool,
        is_pure: bool,
        body: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Block(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ArgsSeries(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    Arg {
        name: AST,
        type_annotation: Option<AST>,
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    JavascriptEscapeExpression(String),

    #[evt(derive(Debug, Clone, PartialEq))]
    RangeExpression {
        start: AST,
        end: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Invocation {
        subject: AST,
        args: Vec<AST>,
        spread_args: Option<AST>,
        type_args: Vec<AST>,
        bubbles: bool,
        awaited_or_detached: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyAccessor {
        subject: AST,
        property: AST,
        optional: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpression {
        cases: Vec<AST>,
        default_case: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseExpressionCase {
        condition: AST,
        outcome: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpression {
        value: AST,
        cases: Vec<AST>,
        default_case: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    SwitchExpressionCase {
        type_filter: AST,
        outcome: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementTag {
        tag_name: AST,
        attributes: Vec<AST>,
        children: Vec<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    AsCast {
        inner: AST,
        as_type: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    InstanceOf {
        inner: AST,
        possible_type: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorExpression(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    RegularExpression {
        expr: String,
        flags: Vec<RegularExpressionFlag>,
    },

    // --- Type expressions ---
    #[evt(derive(Debug, Clone, PartialEq))]
    UnionType(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    MaybeType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    NamedType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericParamType {
        name: AST,
        extends: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ProcType {
        args: AST,
        args_spread: Option<AST>,
        is_pure: bool,
        is_async: bool,
        throws: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    FuncType {
        args: AST,
        args_spread: Option<AST>,
        is_pure: bool,
        returns: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    GenericType {
        type_params: Vec<AST>,
        inner: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeParam {
        name: AST,
        extends: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    BoundGenericType {
        type_args: Vec<AST>,
        generic: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ObjectType(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    InterfaceType(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    SpreadType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyValueType {
        key: AST,
        value: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    RecordType {
        key_type: AST,
        value_type: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ArrayType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    TupleType(Vec<AST>),

    #[evt(derive(Debug, Clone, PartialEq))]
    ReadonlyType(AST),

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
    IteratorType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    PlanType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    ErrorType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    ParenthesizedType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    TypeofType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    KeyofType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    ValueofType(AST),

    #[evt(derive(Debug, Clone, PartialEq))]
    ElementofType(AST),

    UnknownType,

    RegularExpressionType, // TODO: Number of match groups?

    #[evt(derive(Debug, Clone, PartialEq))]
    PropertyType {
        subject: AST,
        property: AST,
        optional: bool,
    },

    // --- Statements ---
    #[evt(derive(Debug, Clone, PartialEq))]
    DeclarationStatement {
        destination: AST,
        value: AST,
        awaited: bool,
        is_const: bool,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatement {
        cases: Vec<AST>,
        default_case: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    IfElseStatementCase {
        condition: AST,
        outcome: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ForLoop {
        item_identifier: AST,
        iterator: AST,
        body: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    WhileLoop {
        condition: AST,
        body: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Assignment {
        target: AST,
        value: AST,
        operator: Option<AST>,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    TryCatch {
        try_block: AST,
        error_identifier: AST,
        catch_block: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    ThrowStatement {
        error_expression: AST,
    },

    #[evt(derive(Debug, Clone, PartialEq))]
    Autorun {
        effect_block: AST,
        until: Option<AST>,
    },

    PlainIdentifier(Slice),
}

pub fn covering(vec: &Vec<AST>) -> Option<Slice> {
    vec.get(0)
        .map(|first| first.slice().clone().spanning(vec[vec.len() - 1].slice()))
}

impl ASTDetails {
    pub fn with_slice(self, src: Slice) -> AST {
        AST(Rc::new(ASTInner {
            parent: RefCell::new(None),
            slice: src,
            details: self,
        }))
    }
}

// --- Misc data ---

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegularExpressionFlag {
    D,
    G,
    I,
    M,
    S,
    U,
    Y,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
