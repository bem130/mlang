//! コンパイル処理中に発生するエラーを定義します。

use crate::span::Span;
use thiserror::Error;

/// ライブラリ全体で発生しうるエラーの集約
#[derive(Debug, Error)]
pub enum LangError {
    #[error("Parse Error: {0}")]
    Parse(#[from] ParseError),
    #[error("Compile Error: {0}")]
    Compile(#[from] CompileError),
}

/// 構文解析エラー
#[derive(Debug, Error)]
#[error("{message} at {span}")]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

/// コンパイルエラー（意味解析、型チェックなど）
#[derive(Debug, Error)]
#[error("{message} at {span}")]
pub struct CompileError {
    pub message: String,
    pub span: Span,
}

impl CompileError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}