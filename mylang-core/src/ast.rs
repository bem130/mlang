//! パーサーが生成する抽象構文木(AST)のデータ構造を定義します。
//! RawASTは意味解析前、TypedASTは意味解析・型解決後の構造を表します。

extern crate alloc;
use crate::span::Span;
use crate::token::Token;
use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;

// 1パス目の構文解析で生成される、未解決の構文木
#[derive(Debug, PartialEq, Clone)]
pub enum RawAstNode {
    // 式の構成要素。まだS式呼び出しなのか変数なのか確定していない。
    Expr(Vec<RawExprPart>),
    // 関数定義 (fn ... は let hoist ... に脱糖されるため、FnDefは不要)
    StructDef {
        name: (String, Span),
        fields: Vec<RawStructField>,
        span: Span,
    },
    EnumDef {
        name: (String, Span),
        variants: Vec<RawEnumVariant>,
        span: Span,
    },
    // let束縛（イミュータブル）
    Let {
        name: (String, Span),
        value: Box<RawAstNode>,
        span: Span,
    },
    // let mut束縛（ミュータブル）
    LetMut {
        name: (String, Span),
        value: Box<RawAstNode>,
        span: Span,
    },
    // let hoist束縛（自己再帰関数用）
    LetHoist {
        name: (String, Span),
        type_params: Vec<RawTypeParam>,
        value: Box<RawAstNode>,
        span: Span,
    },
    TraitDef {
        name: (String, Span),
        type_params: Vec<RawTypeParam>,
        methods: Vec<RawTraitMethod>,
        span: Span,
    },
    ImplDef {
        trait_name: (String, Span),
        trait_args: Vec<(String, Span)>,
        self_type: (String, Span),
        methods: Vec<RawTraitMethod>,
        span: Span,
    },
    // 代入 (set 識別子 式)
    Set {
        name: (String, Span),
        value: Box<RawAstNode>,
        span: Span,
    },
    // ラムダ式 |arg: type, ...|->type body
    Lambda {
        params: Vec<((String, Span), (String, Span))>, // ((name, span), (type, span))
        body: Box<RawAstNode>,
        return_type: (String, Span),
        span: Span,
    },
    // ブロック
    Block {
        statements: Vec<RawAstNode>,
        span: Span,
    },
    While {
        condition: Box<RawAstNode>,
        body: Box<RawAstNode>,
        span: Span,
    },
    Match {
        value: Box<RawAstNode>,
        arms: Vec<RawMatchArm>,
        span: Span,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct RawStructField {
    pub name: (String, Span),
    pub type_name: (String, Span),
}

#[derive(Debug, PartialEq, Clone)]
pub enum RawEnumVariantKind {
    Unit,
    Tuple(Vec<(String, Span)>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RawEnumVariant {
    pub name: (String, Span),
    pub kind: RawEnumVariantKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RawMatchArm {
    pub pattern: RawPattern,
    pub body: Box<RawAstNode>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RawTraitBound {
    pub trait_name: (String, Span),
    pub trait_args: Vec<(String, Span)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RawTypeParam {
    pub name: (String, Span),
    pub bounds: Vec<RawTraitBound>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RawTraitMethod {
    pub name: (String, Span),
    pub type_params: Vec<RawTypeParam>,
    pub params: Vec<((String, Span), (String, Span))>,
    pub return_type: (String, Span),
    pub body: Option<Box<RawAstNode>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RawPattern {
    Wildcard(Span),
    Identifier((String, Span)),
    Tuple(Vec<RawPattern>, Span),
    Path {
        segments: Vec<(String, Span)>,
        subpatterns: Vec<RawPattern>,
        span: Span,
    },
    Literal(Token, Span),
}

/// `RawAstNode::Expr`を構成する、意味が未解決の部品。
/// パーサーはソースコードの構造のみを認識し、この単位に分解する。
#[derive(Debug, PartialEq, Clone)]
pub enum RawExprPart {
    /// 数値リテラル、識別子などの単一トークン。
    Token(Token, Span),
    /// S式グループ `( ... )`。式の評価順序を制御する。
    /// `(` の前に空白がある場合に生成される。
    Group(Vec<RawExprPart>, Span),
    /// C-style呼び出し `f(...)` の引数リスト。
    /// 識別子と `(` がソースコード上で隣接している場合に生成される。
    CStyleArgs(Vec<RawAstNode>, Span),
    /// `$$` で囲まれた数式ブロック。内部ではPrattパーサーにより中置記法が解析される。
    MathBlock(MathAstNode, Span),
    /// 型注釈 `: i32`。(ラムダ式以外での利用)
    TypeAnnotation(String, Span),
    /// if式。S式の一部として扱われる。
    IfExpr {
        condition: Box<RawAstNode>,
        then_branch: Box<RawAstNode>,
        else_branch: Box<RawAstNode>,
        span: Span,
    },
    /// match式。
    MatchExpr {
        value: Box<RawAstNode>,
        arms: Vec<RawMatchArm>,
        span: Span,
    },
    TupleLiteral(Vec<RawAstNode>, Span),
    /// ラムダ式 `|arg: type, ...|->type body`
    Lambda {
        params: Vec<((String, Span), (String, Span))>, // ((name, span), (type, span))
        body: Box<RawAstNode>,
        return_type: (String, Span),
        span: Span,
    },
}

// 数式リテラルの種類
#[derive(Debug, PartialEq, Clone)]
pub enum MathLiteral {
    Int(i64),
    Float(f64),
    Bool(bool),
}

// 数式パーサー(Pratt)が生成する、より構造化されたAST
#[derive(Debug, PartialEq, Clone)]
pub enum MathAstNode {
    Literal(MathLiteral, Span),
    Variable(String, Span),
    // 中置演算
    InfixOp {
        op: Token,
        left: Box<MathAstNode>,
        right: Box<MathAstNode>,
        span: Span,
    },
    PrefixOp {
        op: Token,
        expr: Box<MathAstNode>,
        span: Span,
    },
    // 数式ブロック内での関数呼び出しは `()` が必須
    Call {
        name: (String, Span),
        // 引数はS式として解析されるため、RawAstNodeのリストになる。
        // これにより `my_func(add 1 2)` のような引数を記述できる。
        args: Vec<RawAstNode>,
        span: Span,
    },
}

// 意味解析後のリテラル値
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    I32(i64),
    F64(f64),
    Bool(bool),
}

// 2パス目の意味解析・型チェックを経て生成される、意味が確定した構文木
// これはトップレベルの項目を表す
#[derive(Debug, Clone)]
pub enum TypedAstNode {
    FnDef {
        name: String,
        params: Vec<(String, DataType)>,
        body: TypedExpr,
        return_type: DataType,
        span: Span,
    },
    StructDef {
        name: String,
        fields: Vec<(String, DataType)>,
        span: Span,
    },
    EnumDef {
        name: String,
        variants: Vec<TypedEnumVariant>,
        span: Span,
    },
    TraitDef {
        name: String,
        type_params: Vec<String>,
        span: Span,
    },
    ImplDef {
        trait_name: String,
        trait_args: Vec<DataType>,
        self_type: DataType,
        span: Span,
    },
    // 将来的にトップレベルの`const`などもここに追加できる
}

#[derive(Debug, Clone)]
pub struct TypedEnumVariant {
    pub name: String,
    pub field_types: Vec<DataType>,
    pub span: Span,
}

// 型付きの「式」を表すデータ構造
#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub data_type: DataType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Literal(LiteralValue),
    StringLiteral {
        header_offset: u32,
    },
    VariableRef {
        name: String,        // 元の名前 (エラーメッセージ用)
        unique_name: String, // WAT内でのユニークな名前
    },
    LetBinding {
        // name: (元の名前, ユニークな名前)
        name: (String, String),
        value: Box<TypedExpr>,
        is_mutable: bool,
        name_span: Span,
    },
    LetHoistBinding {
        name: (String, String),
        value: Box<TypedExpr>,
        name_span: Span,
    },
    Assignment {
        name: (String, String),
        value: Box<TypedExpr>,
    },
    Lambda {
        params: Vec<(String, DataType)>,
        body: Box<TypedExpr>,
    },
    FunctionCall {
        name: String,
        args: Vec<TypedExpr>,
    },
    IfExpr {
        condition: Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Box<TypedExpr>,
    },
    Block {
        statements: Vec<TypedExpr>,
    },
    While {
        condition: Box<TypedExpr>,
        body: Box<TypedExpr>,
    },
    TupleLiteral {
        elements: Vec<TypedExpr>,
    },
    Match {
        value: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },
}

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub body: TypedExpr,
}

#[derive(Debug, Clone)]
pub enum TypedPattern {
    Wildcard,
    Binding {
        name: (String, String),
        data_type: DataType,
    },
    Tuple(Vec<TypedPattern>),
    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: Vec<TypedPattern>,
    },
    Literal(LiteralValue),
}

// プログラム内で扱われるデータ型
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    I32,
    F64,
    Bool,
    String,
    TypeVar(String),
    Vector(Box<DataType>),
    Unit, // 値を返さないことを示す型
    Tuple(Vec<DataType>),
    Struct(String),
    Enum(String),
    Function {
        params: Vec<DataType>,
        return_type: Box<DataType>,
    },
    /// Refined type: `<binder: Base | predicate>`
    Refined {
        base: Box<DataType>,
        binder: String,
        predicate_id: usize,
    },
}

// エラーメッセージで型名を綺麗に表示するためのDisplay実装
impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DataType::I32 => write!(f, "i32"),
            DataType::F64 => write!(f, "f64"),
            DataType::Bool => write!(f, "bool"),
            DataType::String => write!(f, "string"),
            DataType::TypeVar(name) => write!(f, "{}", name),
            DataType::Vector(inner) => write!(f, "Vec<{}>", inner),
            DataType::Unit => write!(f, "()"),
            DataType::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            DataType::Struct(name) => write!(f, "{}", name),
            DataType::Enum(name) => write!(f, "{}", name),
            DataType::Refined { base, binder, predicate_id } => write!(f, "<{}: {} | {}#{}>", binder, base, binder, predicate_id),
            DataType::Function {
                params,
                return_type,
            } => {
                write!(
                    f,
                    "({}) -> {}",
                    params
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                )
            }
        }
    }
}

impl DataType {
    /// Return the base (non-refined) type by recursively unwrapping `Refined`.
    ///
    /// This is a convenience used throughout the analyzer and codegen when
    /// refinement annotations should be treated as their underlying base types.
    pub fn core_type(&self) -> DataType {
        match self {
            DataType::Refined { base, .. } => base.core_type(),
            // All other cases return a clone of self (owned DataType)
            other => other.clone(),
        }
    }
}

/// RawAstNodeにSpanを返すメソッドを実装
impl RawAstNode {
    pub fn span(&self) -> Span {
        match self {
            RawAstNode::Expr(parts) => parts.first().map_or(Span::default(), |p| p.span()),
            RawAstNode::StructDef { span, .. } => *span,
            RawAstNode::EnumDef { span, .. } => *span,
            RawAstNode::Let { span, .. } => *span,
            RawAstNode::LetMut { span, .. } => *span,
            RawAstNode::LetHoist { span, .. } => *span,
            RawAstNode::TraitDef { span, .. } => *span,
            RawAstNode::ImplDef { span, .. } => *span,
            RawAstNode::Set { span, .. } => *span,
            RawAstNode::Lambda { span, .. } => *span,
            RawAstNode::Block { span, .. } => *span,
            RawAstNode::While { span, .. } => *span,
            RawAstNode::Match { span, .. } => *span,
        }
    }
}
impl RawExprPart {
    pub fn span(&self) -> Span {
        match self {
            RawExprPart::Token(_, span) => *span,
            RawExprPart::Group(_, span) => *span,
            RawExprPart::CStyleArgs(_, span) => *span,
            RawExprPart::MathBlock(_, span) => *span,
            RawExprPart::TypeAnnotation(_, span) => *span,
            RawExprPart::IfExpr { span, .. } => *span,
            RawExprPart::MatchExpr { span, .. } => *span,
            RawExprPart::TupleLiteral(_, span) => *span,
            RawExprPart::Lambda { span, .. } => *span,
        }
    }
}
impl MathAstNode {
    pub fn span(&self) -> Span {
        match self {
            MathAstNode::Literal(_, span) => *span,
            MathAstNode::Variable(_, span) => *span,
            MathAstNode::InfixOp { span, .. } => *span,
            MathAstNode::PrefixOp { span, .. } => *span,
            MathAstNode::Call { span, .. } => *span,
        }
    }
}

impl RawPattern {
    pub fn span(&self) -> Span {
        match self {
            RawPattern::Wildcard(span) => *span,
            RawPattern::Identifier((_, span)) => *span,
            RawPattern::Tuple(_, span) => *span,
            RawPattern::Path { span, .. } => *span,
            RawPattern::Literal(_, span) => *span,
        }
    }
}
