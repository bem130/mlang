//! パーサーが生成する抽象構文木(AST)のデータ構造を定義します。
//! RawASTは意味解析前、TypedASTは意味解析・型解決後の構造を表します。

use crate::span::Span;
use crate::token::Token;
use std::fmt;

// 1パス目の構文解析で生成される、未解決の構文木
#[derive(Debug, PartialEq, Clone)]
pub enum RawAstNode {
    // 式の構成要素。まだS式呼び出しなのか変数なのか確定していない。
    Expr(Vec<RawExprPart>),
    // 関数定義
    FnDef {
        name: (String, Span),
        params: Vec<((String, Span), (String, Span))>, // ((param_name, span), (type_name, span))
        return_type: Option<(String, Span)>,
        body: Box<RawAstNode>,
        span: Span,
    },
    // let束縛
    LetDef {
        name: (String, Span),
        value: Box<RawAstNode>,
        span: Span,
    },
    // if式
    IfExpr {
        condition: Box<RawAstNode>,
        then_branch: Box<RawAstNode>,
        else_branch: Box<RawAstNode>,
        span: Span,
    },
    // `{}` で囲まれたブロック
    Block {
        statements: Vec<RawAstNode>,
        span: Span,
    },
    // print文
    PrintStmt {
        value: Box<RawAstNode>,
        span: Span,
    }
}

// RawAstNode::Exprを構成する部品
#[derive(Debug, PartialEq, Clone)]
pub enum RawExprPart {
    Token(Token, Span),
    // `()` で囲まれたグループ
    Group(Vec<RawExprPart>, Span),
    // `$$` で囲まれた数式ブロック
    MathBlock(MathAstNode, Span),
    // 型注釈 `: i32`
    TypeAnnotation(String, Span),
}

// 数式リテラルの種類
#[derive(Debug, PartialEq, Clone)]
pub enum MathLiteral {
    Int(i64),
    Float(f64),
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
    // 数式ブロック内での関数呼び出しは `()` が必須
    Call {
        name: (String, Span),
        // 引数部分は再びLL1パーサーがパースするためRawExprPartのリストになる
        args: Vec<Vec<RawExprPart>>,
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
#[derive(Debug, Clone)]
pub struct TypedAstNode {
    pub kind: TypedNodeKind,
    pub data_type: DataType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypedNodeKind {
    Literal(LiteralValue),
    StringLiteral { header_offset: u32 },
    VariableRef(String),
    LetBinding {
        name: String,
        value: Box<TypedAstNode>,
    },
    FunctionCall {
        name: String,
        args: Vec<TypedAstNode>,
    },
    IfExpr {
        condition: Box<TypedAstNode>,
        then_branch: Box<TypedAstNode>,
        else_branch: Box<TypedAstNode>,
    },
    Block {
        statements: Vec<TypedAstNode>,
    },
    PrintStmt {
        value: Box<TypedAstNode>,
    },
}

// プログラム内で扱われるデータ型
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    I32,
    F64,
    Bool,
    String,
    Unit, // 値を返さないことを示す型
}

// エラーメッセージで型名を綺麗に表示するためのDisplay実装
impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DataType::I32 => write!(f, "i32"),
            DataType::F64 => write!(f, "f64"),
            DataType::Bool => write!(f, "bool"),
            DataType::String => write!(f, "string"),
            DataType::Unit => write!(f, "()"),
        }
    }
}

/// RawAstNodeにSpanを返すメソッドを実装
impl RawAstNode {
    pub fn span(&self) -> Span {
        match self {
            RawAstNode::Expr(parts) => parts.first().unwrap().span(), // 簡易
            RawAstNode::FnDef { span, .. } => *span,
            RawAstNode::LetDef { span, .. } => *span,
            RawAstNode::IfExpr { span, .. } => *span,
            RawAstNode::Block { span, .. } => *span,
            RawAstNode::PrintStmt { span, .. } => *span,
        }
    }
}
impl RawExprPart {
    pub fn span(&self) -> Span {
        match self {
            RawExprPart::Token(_, span) => *span,
            RawExprPart::Group(_, span) => *span,
            RawExprPart::MathBlock(_, span) => *span,
            RawExprPart::TypeAnnotation(_, span) => *span,
        }
    }
}
impl MathAstNode {
    pub fn span(&self) -> Span {
        match self {
            MathAstNode::Literal(_, span) => *span,
            MathAstNode::Variable(_, span) => *span,
            MathAstNode::InfixOp { span, .. } => *span,
            MathAstNode::Call { span, .. } => *span,
        }
    }
}