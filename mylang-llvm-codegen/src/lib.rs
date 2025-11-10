//! mylangのTypedASTを受け取り、LLVM IRを生成するコードジェネレーター。

use mylang_core::error::LangError;
use mylang_core::AnalysisResult;

// プレースホルダーのエラー型
#[derive(Debug, thiserror::Error)]
pub enum LlvmCodegenError {
    #[error("LLVM code generation is not yet implemented.")]
    NotImplemented,
}

impl From<LlvmCodegenError> for LangError {
    fn from(err: LlvmCodegenError) -> Self {
        // LangErrorに新しいバリアントを追加するか、CompileErrorでラップする必要がある
        // ここでは仮にCompileErrorを使用
        use mylang_core::error::CompileError;
        use mylang_core::span::Span;
        LangError::Compile(CompileError::new(err.to_string(), Span::default()))
    }
}

/// 解析結果を受け取り、LLVM IR文字列を生成するメイン関数。
pub fn generate(analysis_result: &AnalysisResult) -> Result<String, LangError> {
    // TODO: inkwellを使用してLLVM IRを構築するロジックを実装
    let _ = analysis_result; // analysis_resultを使用していることをコンパイラに伝える
    Err(LlvmCodegenError::NotImplemented.into())
}