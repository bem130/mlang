// src/lang/parser.rs

use thiserror::Error;

/// パース処理中に発生する可能性のあるエラーを定義します。
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("予期せず入力の終わりに到達しました")]
    UnexpectedEndOfInput,
    #[error("無効なトークンです: {0}")]
    InvalidToken(String),
    #[error("数値への変換に失敗しました: {0}")]
    NotANumber(String),
}

/// 四則演算の種類を表すenum（列挙型）
#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

/// 数式の構造を表す「抽象構文木 (AST)」
/// 再帰的なデータ構造になっています。
#[derive(Debug, Clone)]
pub enum Expr {
    // 数値を表すノード（構文木の葉）
    Number(f64),
    // 二項演算を表すノード。演算子と左右の式を持つ。
    // Box<Expr>は、再帰的なデータ構造で無限のサイズになるのを防ぐためのもの。
    BinaryOp {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    }
}

/// ポーランド記法のトークン列から再帰的にASTを構築する関数
/// イテレータを引数に取ることで、トークンの消費状態を管理しやすくします。
fn build_ast_from_tokens(tokens: &mut std::slice::Iter<'_, &str>) -> Result<Expr, ParseError> {
    // 次のトークンを取り出す。もし無ければ、入力が足りないのでエラー。
    let token = tokens.next().ok_or(ParseError::UnexpectedEndOfInput)?;

    // トークンが演算子か数値かを判断する
    match *token {
        "add" | "sub" | "mul" | "div" => {
            let op = match *token {
                "add" => Operator::Add,
                "sub" => Operator::Sub,
                "mul" => Operator::Mul,
                "div" => Operator::Div,
                _ => unreachable!(), // このケースはありえない
            };

            // 演算子の場合、再帰的に左辺と右辺の式をパースする
            let left_expr = build_ast_from_tokens(tokens)?;
            let right_expr = build_ast_from_tokens(tokens)?;

            // 左右の式を子に持つ二項演算ノードを作成して返す
            Ok(Expr::BinaryOp {
                op,
                left: Box::new(left_expr),
                right: Box::new(right_expr),
            })
        }
        // それ以外のトークンは数値として解釈を試みる
        numeric_token => {
            numeric_token
                .parse::<f64>()
                .map(Expr::Number) // パース成功なら Expr::Number で包む
                .map_err(|_| ParseError::NotANumber(numeric_token.to_string())) // 失敗ならエラー
        }
    }
}

/// 文字列のソースコードを受け取り、パースしてASTを返すトップレベル関数
pub fn parse(source: &str) -> Result<Expr, ParseError> {
    // ソースコードを空白で区切ってトークンのベクタ（可変長配列）を作成
    let tokens: Vec<&str> = source.split_whitespace().collect();
    
    // トークンのイテレータを作成
    let mut tokens_iter = tokens.iter();

    // ASTの構築を開始
    let ast = build_ast_from_tokens(&mut tokens_iter)?;

    // もし全てのトークンを消費しきっていない場合、式が不正（オペランドが多すぎるなど）
    if tokens_iter.next().is_some() {
        return Err(ParseError::InvalidToken("式が長すぎます".to_string()));
    }

    Ok(ast)
}