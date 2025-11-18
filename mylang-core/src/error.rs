//! コンパイル処理中に発生するエラーを定義します。

extern crate alloc;
use crate::span::Span;
use alloc::format;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;
use core::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub message: String,
    pub span: Span,
    pub help: Option<String>,
    pub highlight: Option<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            kind: DiagnosticKind::Error,
            message: message.into(),
            span,
            help: None,
            highlight: None,
        }
    }

    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            kind: DiagnosticKind::Warning,
            message: message.into(),
            span,
            help: None,
            highlight: None,
        }
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_highlight(mut self, highlight: impl Into<String>) -> Self {
        self.highlight = Some(highlight.into());
        self
    }

    fn color_code(&self) -> (&'static str, &'static str) {
        match self.kind {
            DiagnosticKind::Error => ("error", "\x1b[31m"),
            DiagnosticKind::Warning => ("warning", "\x1b[33m"),
        }
    }

    pub fn to_colored_string(&self) -> String {
        let (label, color) = self.color_code();
        let reset = "\x1b[0m";
        let mut message = format!("{color}{label}{reset} at {}: {}", self.span, self.message);
        if let Some(ref highlight) = self.highlight {
            message.push_str(&format!("\n  --> {}", highlight));
        }
        if let Some(ref help) = self.help {
            message.push_str(&format!("\n  help: {}", help));
        }
        message
    }
}

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
#[derive(Debug, Clone)]
pub struct ParseError {
    pub diagnostics: Vec<Diagnostic>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.diagnostics.is_empty() {
            return write!(f, "no diagnostics emitted");
        }
        for (idx, diagnostic) in self.diagnostics.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", diagnostic.to_colored_string())?;
        }
        Ok(())
    }
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            diagnostics: vec![Diagnostic::error(message, span)],
        }
    }

    pub fn from_diagnostics(diagnostics: Vec<Diagnostic>) -> Self {
        Self { diagnostics }
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::Error)
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
