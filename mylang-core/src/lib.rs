#![no_std]
//! mylangのコンパイラライブラリのクレート。
//! 字句解析、構文解析、意味解析のパイプラインを管理します。

extern crate alloc;

pub mod ast;
pub mod compiler;
pub mod error;
pub mod span;
pub mod token;

mod lexer;
mod parser;

use crate::ast::TypedAstNode;
use crate::compiler::{Analyzer, FunctionSignature};
use crate::error::LangError;
use crate::error::ParseError;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

/// `analyze_source`関数の成功時の戻り値。
/// コード生成に必要な情報をすべて含みます。
pub struct AnalysisResult {
    pub typed_ast: Vec<TypedAstNode>,
    pub function_table: BTreeMap<String, FunctionSignature>,
    pub string_headers: BTreeMap<String, (u32, u32)>, // (data_offset, header_offset)
    pub static_offset: u32,
}

/// ソースコード文字列を解析し、型付きのASTと解析結果を生成するメイン関数。
/// これがこのライブラリの公開APIとなります。
pub fn analyze_source(source: &str) -> Result<AnalysisResult, LangError> {
    // 1. 字句解析: ソースコードをトークン列に変換
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.tokenize_all().map_err(|msg| {
        // Lexerから返された単純なエラーメッセージを、位置情報を持つParseErrorに変換する
        ParseError::new(msg, span::Span::default())
    })?;

    // 2. 構文解析: トークン列を未解決のAST(RawAST)に変換
    let mut parser = parser::Parser::new(tokens);
    let raw_ast = parser.parse_toplevel()?;

    // 3. 意味解析: RawASTを意味解析・型チェックし、TypedASTを生成
    let mut analyzer = Analyzer::new();
    let typed_ast = analyzer.analyze(&raw_ast)?;

    // 解析結果を構造体にまとめて返す
    Ok(AnalysisResult {
        typed_ast,
        function_table: analyzer.function_table,
        string_headers: analyzer.string_headers,
        static_offset: analyzer.static_offset,
    })
}
