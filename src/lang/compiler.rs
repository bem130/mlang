// src/lang/compiler.rs

use super::parser::{Expr, Operator};
use thiserror::Error;

/// コンパイル処理中に発生する可能性のあるエラーを定義します。
#[derive(Debug, Error)]
pub enum CompileError {
    // 現在はエラーケースがないが、将来の拡張のために用意
}

/// ASTのノードを再帰的に辿り、WATの命令を文字列として生成する関数
fn build_wat_from_ast(ast: &Expr, builder: &mut String) -> Result<(), CompileError> {
    match ast {
        // 数値ノードの場合
        Expr::Number(n) => {
            // f64.const 命令を追加して、数値をスタックに積む
            builder.push_str(&format!("    f64.const {}\n", n));
        }
        // 二項演算ノードの場合
        Expr::BinaryOp { op, left, right } => {
            // WebAssemblyはスタックマシンなので、後置記法（逆ポーランド記法）の順で命令を生成する
            // 1. 左辺の式をコンパイル（結果がスタックに積まれる）
            build_wat_from_ast(left, builder)?;
            // 2. 右辺の式をコンパイル（結果がスタックに積まれる）
            build_wat_from_ast(right, builder)?;
            // 3. 演算子の命令を追加（スタックから2つ値を取り、計算結果を積む）
            let op_inst = match op {
                Operator::Add => "f64.add",
                Operator::Sub => "f64.sub",
                Operator::Mul => "f64.mul",
                Operator::Div => "f64.div",
            };
            builder.push_str(&format!("    {}\n", op_inst));
        }
    }
    Ok(())
}

/// ASTを受け取り、完全なWATモジュールを文字列として返すトップレベル関数
pub fn compile(ast: &Expr) -> Result<String, CompileError> {
    let mut wat_body = String::new();
    // ASTからWATの関数本体部分を生成
    build_wat_from_ast(ast, &mut wat_body)?;

    // WATのモジュール全体を定義するテンプレート
    let wat_module = format!(
r#"(module
  (func $execute (result f64)
{body}  )
  (export "execute" (func $execute))
)
"#,
        body = wat_body
    );

    Ok(wat_module)
}