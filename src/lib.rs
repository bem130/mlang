//! minilangのコンパイラライブラリのクレート。
//! 字句解析、構文解析、意味解析、コード生成のパイプラインを管理します。

mod ast;
mod compiler;
pub mod error;
mod lexer;
mod parser;
mod span;
mod token;

use error::LangError;

/// ソースコード文字列をWAT文字列に直接コンパイルするメイン関数。
/// これがこのライブラリの公開APIとなります。
pub fn compile_source(source: &str) -> Result<String, LangError> {
    // 1. 字句解析: ソースコードをトークン列に変換
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.tokenize_all().map_err(|msg| {
        // Lexerから返された単純なエラーメッセージを、位置情報を持つParseErrorに変換する
        // (より詳細なエラー位置を特定するにはLexerの改良が必要)
        error::ParseError::new(msg, span::Span::default())
    })?;

    // 2. 構文解析: トークン列を未解決のAST(RawAST)に変換
    let mut parser = parser::Parser::new(tokens);
    let raw_ast = parser.parse_toplevel()?;
    
    // 3. コンパイル: RawASTを意味解析・型チェックし、WATを生成
    let mut compiler = compiler::Compiler::new();
    let wat = compiler.compile(&raw_ast)?;

    Ok(wat)
}