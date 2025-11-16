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

use crate::ast::{RawAstNode, TypedAstNode};
use crate::compiler::{Analyzer, FunctionSignature};
use crate::error::{CompileError, LangError, ParseError};
use crate::span::{Span, combine_spans};
use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;

/// `analyze_source`関数の成功時の戻り値。
/// コード生成に必要な情報をすべて含みます。
pub struct AnalysisResult {
    pub typed_ast: Vec<TypedAstNode>,
    pub function_table: BTreeMap<String, Vec<FunctionSignature>>,
    pub string_headers: BTreeMap<String, (u32, u32)>, // (data_offset, header_offset)
    pub static_offset: u32,
}

/// 字句解析と構文解析の結果を保持する構造体。
pub struct ParsedModule {
    pub tokens: Vec<(token::Token, Span)>,
    pub raw_ast: Vec<RawAstNode>,
    pub prepared_ast: Vec<RawAstNode>,
}

/// IDE向けに字句解析・構文解析・意味解析の全情報をまとめた構造体。
pub struct IdeAnalysis {
    pub parsed: ParsedModule,
    pub analysis: AnalysisResult,
}

/// ソースコードを字句解析するヘルパー。
pub fn lex_source(source: &str) -> Result<Vec<(token::Token, Span)>, LangError> {
    let mut lexer = lexer::Lexer::new(source);
    lexer
        .tokenize_all()
        .map_err(|msg| LangError::Parse(ParseError::new(msg, Span::default())))
}

/// トークン列を構文解析してRawASTへ変換するヘルパー。
pub fn parse_tokens(tokens: &[(token::Token, Span)]) -> Result<Vec<RawAstNode>, LangError> {
    let mut parser = parser::Parser::new(tokens.to_vec());
    parser.parse_toplevel()
}

/// is_libraryフラグに基づいてASTを整形し、暗黙のmain関数を生成する。
pub fn prepare_ast(
    raw_ast_nodes: &[RawAstNode],
    is_library: bool,
) -> Result<Vec<RawAstNode>, LangError> {
    if is_library {
        return Ok(raw_ast_nodes.to_vec());
    }

    let mut definitions = Vec::new();
    let mut statements = Vec::new();
    let mut has_explicit_main = false;

    for node in raw_ast_nodes.iter().cloned() {
        match &node {
            RawAstNode::LetHoist { name, .. } if name.0 == "main" => {
                has_explicit_main = true;
                definitions.push(node);
            }
            RawAstNode::LetHoist { .. }
            | RawAstNode::StructDef { .. }
            | RawAstNode::EnumDef { .. }
            | RawAstNode::TraitDef { .. }
            | RawAstNode::ImplDef { .. } => {
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
        let unit_type_span = Span::default();
        let main_name_span = Span::default();

        let main_fn = RawAstNode::LetHoist {
            name: ("main".to_string(), main_name_span),
            type_params: Vec::new(),
            value: Box::new(RawAstNode::Lambda {
                params: vec![],
                body: Box::new(main_body_block),
                return_type: ("()".to_string(), unit_type_span),
                span: main_fn_span,
            }),
            span: main_fn_span,
        };
        definitions.push(main_fn);
    }

    Ok(definitions)
}

/// 字句解析と構文解析の両方を行い、解析結果を返すヘルパー。
pub fn parse_source(source: &str, is_library: bool) -> Result<ParsedModule, LangError> {
    let tokens = lex_source(source)?;
    let raw_ast = parse_tokens(&tokens)?;
    let prepared_ast = prepare_ast(&raw_ast, is_library)?;
    Ok(ParsedModule {
        tokens,
        raw_ast,
        prepared_ast,
    })
}

/// ソースコード文字列を解析し、型付きのASTと解析結果を生成するメイン関数。
/// これがこのライブラリの公開APIとなります。
pub fn analyze_source(source: &str, is_library: bool) -> Result<AnalysisResult, LangError> {
    analyze_for_ide(source, is_library).map(|analysis| analysis.analysis)
}

/// IDEで利用するために、字句解析から意味解析までをまとめて実行する。
pub fn analyze_for_ide(source: &str, is_library: bool) -> Result<IdeAnalysis, LangError> {
    let parsed = parse_source(source, is_library)?;
    let mut analyzer = Analyzer::new();
    let typed_ast = analyzer.analyze(&parsed.prepared_ast)?;
    let analysis = AnalysisResult {
        typed_ast,
        function_table: analyzer.function_table,
        string_headers: analyzer.string_headers,
        static_offset: analyzer.static_offset,
    };
    Ok(IdeAnalysis { parsed, analysis })
}
