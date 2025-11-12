//! コンパイル処理中に発生するエラーを定義します。

extern crate alloc;
use crate::span::Span;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

/// ライブラリ全体で発生しうるエラーの集約
#[derive(Debug)]
pub enum LangError {
    Parse(ParseError),
    Compile(CompileError),
}

impl fmt::Display for LangError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LangError::Parse(e) => write!(f, "Parse Error: {}", e),
            LangError::Compile(e) => write!(f, "Compile Error: {}", e),
        }
    }
}

/// 構文解析エラー
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message, self.span)
    }
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

// --- From trait implementations ---

impl From<ParseError> for LangError {
    fn from(err: ParseError) -> Self {
        LangError::Parse(err)
    }
}

impl From<CompileError> for LangError {
    fn from(err: CompileError) -> Self {
        LangError::Compile(err)
    }
}
