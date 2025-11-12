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

use crate::ast::{RawAstNode, RawExprPart, TypedAstNode};
use crate::compiler::{Analyzer, FunctionSignature};
use crate::error::{CompileError, LangError, ParseError};
use crate::span::{combine_spans, Span};
use alloc::collections::BTreeMap;
use alloc::string::{String, ToString};
use alloc::boxed::Box;
use alloc::vec;
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
pub fn analyze_source(source: &str, is_library: bool) -> Result<AnalysisResult, LangError> {
    // 1. 字句解析: ソースコードをトークン列に変換
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.tokenize_all().map_err(|msg| {
        // Lexerから返された単純なエラーメッセージを、位置情報を持つParseErrorに変換する
        ParseError::new(msg, span::Span::default())
    })?;

    // 2. 構文解析: トークン列を未解決のAST(RawAST)に変換
    let mut parser = parser::Parser::new(tokens);
    let raw_ast_nodes = parser.parse_toplevel()?;

    // is_libraryフラグに基づいて、暗黙のmain関数をASTレベルで生成するか判断する
    let final_ast_nodes = if is_library {
        raw_ast_nodes
    } else {
        let mut definitions = Vec::new();
        let mut statements = Vec::new();
        let mut has_explicit_main = false;

        for node in raw_ast_nodes {
            match &node {
                RawAstNode::FnDef { name, .. } | RawAstNode::LetHoist { name, .. }
                    if name.0 == "main" =>
                {
                    has_explicit_main = true;
                    definitions.push(node);
                }
                RawAstNode::FnDef { .. }
                | RawAstNode::LetHoist { .. }
                | RawAstNode::StructDef { .. }
                | RawAstNode::EnumDef { .. } => {
                    definitions.push(node);
                }
                _ => {
                    statements.push(node);
                }
            }
        }

        if has_explicit_main && !statements.is_empty() {
            return Err(LangError::Compile(CompileError::new(
                "Cannot have top-level statements when a 'main' function is explicitly defined",
                statements[0].span(),
            )));
        }

        if !statements.is_empty() {
            let first_span = statements.first().unwrap().span();
            let last_span = statements.last().unwrap().span();
            let body_span = combine_spans(first_span, last_span);

            let main_body_block = RawAstNode::Block {
                statements,
                span: body_span,
            };

            let main_fn_span = main_body_block.span();
            let unit_type_span = Span::default(); // Span情報がないのでデフォルトで
            let main_name_span = Span::default();

            // fn main || { ... } : () -> () と同等のASTを構築
            let main_fn = RawAstNode::LetHoist {
                name: ("main".to_string(), main_name_span),
                value: Box::new(RawAstNode::Expr(vec![RawExprPart::Lambda {
                    params: vec![],
                    body: Box::new(main_body_block),
                    return_type: Some(("()".to_string(), unit_type_span)),
                    span: main_fn_span,
                }])),
                span: main_fn_span,
            };
            definitions.push(main_fn);
        }

        definitions
    };

    // 3. 意味解析: RawASTを意味解析・型チェックし、TypedASTを生成
    let mut analyzer = Analyzer::new();
    let typed_ast = analyzer.analyze(&final_ast_nodes)?;

    // 解析結果を構造体にまとめて返す
    Ok(AnalysisResult {
        typed_ast,
        function_table: analyzer.function_table,
        string_headers: analyzer.string_headers,
        static_offset: analyzer.static_offset,
    })
}