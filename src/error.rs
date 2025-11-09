//! コンパイル処理中に発生するエラーを定義します。

use crate::span::Span;
use thiserror::Error;
use std::fmt;

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
#[derive(Debug)]
pub struct CompileError {
    pub message: String,
    pub span: Span,
    pub notes: Vec<(String, Span)>,
}

impl CompileError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            notes: Vec::new(),
        }
    }

    pub fn with_note(mut self, message: impl Into<String>, span: Span) -> Self {
        self.notes.push((message.into(), span));
        self
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message, self.span)?;
        for (note, span) in &self.notes {
            if span.line > 0 {
                write!(f, "\n  note: {} at {}", note, span)?;
            } else {
                write!(f, "\n  note: {}", note)?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for CompileError {}