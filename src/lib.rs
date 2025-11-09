// src/lib.rs

pub mod lang;

use thiserror::Error;

// 複数の種類のエラーを一つにまとめるためのenum
#[derive(Debug, Error)]
pub enum LangError {
    #[error("パースエラー: {0}")]
    Parse(#[from] lang::parser::ParseError),
    #[error("コンパイルエラー: {0}")]
    Compile(#[from] lang::compiler::CompileError),
}

/// ソースコード文字列を直接WATにコンパイルする便利な関数
/// これがこのライブラリのメインの公開関数となる
pub fn compile_source(source: &str) -> Result<String, LangError> {
    // 1. ソースコードをパースしてASTを作成
    let ast = lang::parser::parse(source)?;
    // 2. ASTをコンパイルしてWATを生成
    let wat = lang::compiler::compile(&ast)?;
    Ok(wat)
}