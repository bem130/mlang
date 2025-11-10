//! TypedASTを受け取り、WATを生成するコードジェネレータ。

use super::Compiler;
use crate::ast::*;
use crate::error::{CompileError, LangError};
use std::collections::HashMap;

pub fn generate(compiler: &mut Compiler, node: &TypedAstNode) -> Result<(), LangError> {
    match node {
        TypedAstNode::FnDef { name, params, body, return_type, .. } => {
            compiler.enter_scope();
            compiler.wat_buffer.push_str(&format!("\n  ;; --- Function Definition: {} ---\n", name));
            compiler.wat_buffer.push_str(&format!("  (func ${}", name));

            for (param_name, param_type) in params {
                compiler.wat_buffer.push_str(&format!(" (param ${} {})", param_name, compiler.type_to_wat(param_type)));
            }

            if *return_type != DataType::Unit {
                compiler.wat_buffer.push_str(&format!(" (result {})", compiler.type_to_wat(return_type)));
            }
            compiler.wat_buffer.push_str("\n");

            collect_and_declare_locals(compiler, body);
            
            generate_expr(compiler, body)?;

            if name == "main" && body.data_type != DataType::Unit {
                compiler.wat_buffer.push_str("    drop ;; Drop last expression value in main\n");
            }

            compiler.wat_buffer.push_str("  )\n");
            compiler.leave_scope();
        }
    }
    Ok(())
}

/// スタックのトップにある文字列（へのポインタ）を標準出力に書き出すWATコードを生成する。
fn generate_print_logic(compiler: &mut Compiler) {
    compiler.wat_buffer.push_str("    local.set $__print_temp\n");
    compiler.wat_buffer.push_str("    i32.const 0 ;; iovec[0].buf\n");
    compiler.wat_buffer.push_str("    local.get $__print_temp\n");
    compiler.wat_buffer.push_str("    i32.load offset=0 ;; load data_ptr from header\n");
    compiler.wat_buffer.push_str("    i32.store\n");
    compiler.wat_buffer.push_str("    i32.const 4 ;; iovec[0].buf_len\n");
    compiler.wat_buffer.push_str("    local.get $__print_temp\n");
    compiler.wat_buffer.push_str("    i32.load offset=4 ;; load data_len from header\n");
    compiler.wat_buffer.push_str("    i32.store\n");
    compiler.wat_buffer.push_str("    i32.const 1   ;; fd (1=stdout)\n");
    compiler.wat_buffer.push_str("    i32.const 0   ;; iovecs_ptr\n");
    compiler.wat_buffer.push_str("    i32.const 1   ;; iovecs_len\n");
    compiler.wat_buffer.push_str("    i32.const 16  ;; nwritten_ptr\n");
    compiler.wat_buffer.push_str("    call $fd_write\n    drop\n");
}

fn generate_expr(compiler: &mut Compiler, node: &TypedExpr) -> Result<(), LangError> {
    match &node.kind {
        TypedExprKind::Literal(val) => {
            let (wat_type, const_val) = match val {
                LiteralValue::I32(i) => ("i32", i.to_string()),
                LiteralValue::F64(f) => ("f64", f.to_string()),
                LiteralValue::Bool(b) => ("i32", if *b { "1" } else { "0" }.to_string()),
            };
            compiler.wat_buffer.push_str(&format!("    {}.const {} ;; Literal\n", wat_type, const_val));
        }
        TypedExprKind::StringLiteral { header_offset } => {
            compiler.wat_buffer.push_str(&format!("    i32.const {} ;; String literal header\n", header_offset));
        }
        TypedExprKind::VariableRef { unique_name, name, .. } => {
            compiler.wat_buffer.push_str(&format!("    local.get ${} ;; Var: {}\n", unique_name, name));
        }
        TypedExprKind::FunctionCall { name, args } => {
            if name.contains('.') {
                 compiler.wat_buffer.push_str(&format!("    ;; Built-in Op: {}\n", name));
            } else {
                 compiler.wat_buffer.push_str(&format!("    ;; Call: {}\n", name));
            }

            // --- 組み込み関数の特別処理 ---
            match name.as_str() {
                "string_concat" => {
                    generate_expr(compiler, &args[0])?;
                    generate_expr(compiler, &args[1])?;
                    compiler.wat_buffer.push_str("    call $__string_concat\n");
                    return Ok(());
                }
                "i32_to_string" => {
                    generate_expr(compiler, &args[0])?;
                    compiler.wat_buffer.push_str("    call $__i32_to_string\n");
                    return Ok(());
                }
                "print" => {
                    generate_expr(compiler, &args[0])?;
                    generate_print_logic(compiler);
                    return Ok(());
                }
                "println" => {
                    // First: print the actual value.
                    generate_expr(compiler, &args[0])?;
                    generate_print_logic(compiler);
                    // Second: print the newline character.
                    let newline_header_offset = compiler.ensure_string_is_statically_allocated(&"\n".to_string());
                    compiler.wat_buffer.push_str(&format!("    i32.const {} ;; newline string header\n", newline_header_offset));
                    generate_print_logic(compiler);
                    return Ok(());
                }
                _ => {}
            }
            
            // --- 通常の関数呼び出し ---
            for arg in args {
                generate_expr(compiler, arg)?;
            }
            if name.contains('.') { // 中置演算子
                compiler.wat_buffer.push_str(&format!("    {}\n", name));
            } else { // ユーザー定義 or その他の組み込み関数
                compiler.wat_buffer.push_str(&format!("    call ${}\n", name));
            }
        }
        TypedExprKind::LetBinding { name, value } => {
            compiler.wat_buffer.push_str(&format!("    ;; Let: {}\n", name.0));
            generate_expr(compiler, value)?;
            // 値を持つ型の場合のみ local.set を行う
            if value.data_type != DataType::Unit {
                compiler.wat_buffer.push_str(&format!("    local.set ${}\n", name.1));
            }
        }
        TypedExprKind::IfExpr { condition, then_branch, else_branch } => {
            compiler.wat_buffer.push_str("    ;; If\n");
            generate_expr(compiler, condition)?;
            compiler.wat_buffer.push_str(&format!("    (if (result {})\n", compiler.type_to_wat(&node.data_type)));
            compiler.wat_buffer.push_str("      (then\n");
            generate_expr(compiler, then_branch)?;
            compiler.wat_buffer.push_str("      )\n");
            compiler.wat_buffer.push_str("      (else\n");
            generate_expr(compiler, else_branch)?;
            compiler.wat_buffer.push_str("      )\n");
            compiler.wat_buffer.push_str("    )\n");
        }
        TypedExprKind::Block { statements } => {
             compiler.wat_buffer.push_str("    ;; Block\n");
            if statements.is_empty() {
                if node.data_type != DataType::Unit {
                    return Err(LangError::Compile(CompileError::new(format!("Empty block must have type '()' but found '{}'", node.data_type), node.span)));
                }
                return Ok(());
            }
            for (i, stmt) in statements.iter().enumerate() {
                generate_expr(compiler, stmt)?;
                // ブロックの最後の式でなければ、スタックに残った値を捨てる
                if i < statements.len() - 1 && stmt.data_type != DataType::Unit {
                    compiler.wat_buffer.push_str("    drop\n");
                }
            }
        }
    }
    Ok(())
}

fn body_uses_print(expr: &TypedExpr) -> bool {
    let mut uses = false;
    fn find_prints(expr: &TypedExpr, uses: &mut bool) {
        if *uses { return; }
        match &expr.kind {
            TypedExprKind::FunctionCall { name, args, .. } => {
                if name == "print" || name == "println" {
                    *uses = true;
                    // Note: We don't return early, as args might also contain prints.
                }
                for arg in args { find_prints(arg, uses); }
            }
            TypedExprKind::LetBinding { value, .. } => find_prints(value, uses),
            TypedExprKind::IfExpr { condition, then_branch, else_branch, .. } => {
                find_prints(condition, uses); find_prints(then_branch, uses); find_prints(else_branch, uses);
            }
            TypedExprKind::Block { statements, .. } => { for stmt in statements { find_prints(stmt, uses); } }
            _ => {}
        }
    }
    find_prints(expr, &mut uses);
    uses
}

pub fn collect_and_declare_locals(compiler: &mut Compiler, typed_body: &TypedExpr) {
    let mut locals: HashMap<String, DataType> = HashMap::new();

    fn find_lets(expr: &TypedExpr, locals: &mut HashMap<String, DataType>) {
        match &expr.kind {
            TypedExprKind::LetBinding { name, value } => {
                locals.insert(name.1.clone(), value.data_type.clone());
                find_lets(value, locals);
            }
            TypedExprKind::IfExpr { condition, then_branch, else_branch } => {
                find_lets(condition, locals);
                find_lets(then_branch, locals);
                find_lets(else_branch, locals);
            }
            TypedExprKind::Block { statements } => {
                for stmt in statements {
                    find_lets(stmt, locals);
                }
            }
            TypedExprKind::FunctionCall { args, .. } => {
                for arg in args {
                    find_lets(arg, locals);
                }
            }
            _ => {}
        }
    }
    find_lets(typed_body, &mut locals);

    if !locals.is_empty() {
        compiler.wat_buffer.push_str("    ;; Local variables\n");
        for (unique_name, data_type) in locals {
            // Unit型の変数はWasmのローカル変数として宣言しない
            if data_type != DataType::Unit {
                compiler.wat_buffer.push_str(&format!("    (local ${} {})\n", unique_name, compiler.type_to_wat(&data_type)));
            }
        }
    }

    if body_uses_print(typed_body) {
        compiler.wat_buffer.push_str("    (local $__print_temp i32) ;; for print/println iovec\n");
    }
}

pub fn generate_builtin_helpers(compiler: &mut Compiler) {
    let builtins_wat = include_str!("builtins.wat");
    compiler.wat_buffer.push_str(builtins_wat);
}