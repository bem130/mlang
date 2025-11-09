//! TypedASTを受け取り、WATを生成するコードジェネレータ。

use super::Compiler;
use crate::ast::*;
use crate::error::{CompileError, LangError};

pub fn generate(compiler: &mut Compiler, node: &TypedAstNode) -> Result<(), LangError> {
    match node {
        TypedAstNode::FnDef { name, params, body, return_type, .. } => {
            compiler.enter_scope();
            compiler.wat_buffer.push_str(&format!("  (func ${}", name));

            for (param_name, param_type) in params {
                compiler.wat_buffer.push_str(&format!(" (param ${} {})", param_name, compiler.type_to_wat(param_type)));
                compiler.variable_table.push((param_name.clone(), param_type.clone(), compiler.scope_depth));
            }

            if *return_type != DataType::Unit {
                compiler.wat_buffer.push_str(&format!(" (result {})", compiler.type_to_wat(return_type)));
            }
            compiler.wat_buffer.push_str("\n");

            collect_and_declare_locals(compiler, body);
            
            generate_expr(compiler, body)?;

            // main関数の本体が値をスタックに残す場合、それを破棄して_startの規約(スタックを汚さない)を遵守する
            if name == "main" && body.data_type != DataType::Unit {
                compiler.wat_buffer.push_str("    drop\n");
            }

            compiler.wat_buffer.push_str("  )\n");
            compiler.leave_scope();
        }
    }
    Ok(())
}


fn generate_expr(compiler: &mut Compiler, node: &TypedExpr) -> Result<(), LangError> {
    match &node.kind {
        TypedExprKind::Literal(val) => {
            let (wat_type, const_val) = match val {
                LiteralValue::I32(i) => ("i32", i.to_string()),
                LiteralValue::F64(f) => ("f64", f.to_string()),
                LiteralValue::Bool(b) => ("i32", if *b { "1" } else { "0" }.to_string()),
            };
            compiler.wat_buffer.push_str(&format!("    {}.const {}\n", wat_type, const_val));
        }
        TypedExprKind::StringLiteral { header_offset } => {
            compiler.wat_buffer.push_str(&format!("    i32.const {}\n", header_offset));
        }
        TypedExprKind::VariableRef(name) => {
            compiler.wat_buffer.push_str(&format!("    local.get ${}\n", name));
        }
        TypedExprKind::FunctionCall { name, args } => {
             if name == "string_concat" {
                generate_expr(compiler, &args[0])?;
                generate_expr(compiler, &args[1])?;
                compiler.wat_buffer.push_str("    call $__string_concat\n");
                return Ok(());
            }
            for arg in args {
                generate_expr(compiler, arg)?;
            }
            if name.contains('.') {
                compiler.wat_buffer.push_str(&format!("    {}\n", name));
            } else {
                compiler.wat_buffer.push_str(&format!("    call ${}\n", name));
            }
        }
        TypedExprKind::LetBinding { name, value } => {
            generate_expr(compiler, value)?;
            compiler.wat_buffer.push_str(&format!("    local.set ${}\n", name));
            // let束縛は文なので、スタックに値を残さない (Unit型だがWATレベルでの表現はない)
            // しかし、束縛される値がUnitでない場合、local.setは値を消費しないためスタックに残る。
            // これをdropする必要があるか？ -> local.teeを使うと値を残せるが、setは消費する。
            // いや、let a = 1; はUnitを返すが、1はスタックに積まれる。setがそれを消費する。
            // let a = if b {1} else {2}; の場合、ifの結果がスタックに積まれ、setが消費。
            // よってdropは不要。
        }
        TypedExprKind::IfExpr { condition, then_branch, else_branch } => {
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
            if statements.is_empty() {
                // Unitを返す空のブロックは何もしない
                if node.data_type != DataType::Unit {
                    return Err(LangError::Compile(CompileError::new(format!("Empty block must have type '()' but found '{}'", node.data_type), node.span)));
                }
                return Ok(());
            }
            for (i, stmt) in statements.iter().enumerate() {
                generate_expr(compiler, stmt)?;
                // ブロックの最後の式以外で、かつUnitを返さない文の結果はスタックから破棄する
                if i < statements.len() - 1 && stmt.data_type != DataType::Unit {
                    compiler.wat_buffer.push_str("    drop\n");
                }
            }
        }
        TypedExprKind::PrintStmt { value } => {
            compiler.wat_buffer.push_str("    i32.const 0 ;; iovec addr\n");
            generate_expr(compiler, value)?;
            compiler.wat_buffer.push_str("    i32.load offset=0 ;; load data_ptr\n");
            compiler.wat_buffer.push_str("    i32.store\n");
            compiler.wat_buffer.push_str("    i32.const 4 ;; iovec addr + 4\n");
            generate_expr(compiler, value)?;
            compiler.wat_buffer.push_str("    i32.load offset=4 ;; load data_len\n");
            compiler.wat_buffer.push_str("    i32.store\n");
            compiler.wat_buffer.push_str("    i32.const 1\n    i32.const 0\n    i32.const 1\n    i32.const 16\n");
            compiler.wat_buffer.push_str("    call $fd_write\n    drop\n");
        }
    }
    Ok(())
}

pub fn collect_and_declare_locals(compiler: &mut Compiler, typed_body: &TypedExpr) {
    // 再帰的にlet束縛を探してローカル変数を宣言する
    fn find_lets(compiler: &mut Compiler, expr: &TypedExpr) {
        match &expr.kind {
            TypedExprKind::LetBinding { name, value } => {
                compiler.wat_buffer.push_str(&format!("    (local ${} {})\n", name, compiler.type_to_wat(&value.data_type)));
                find_lets(compiler, value);
            }
            TypedExprKind::IfExpr { condition, then_branch, else_branch } => {
                find_lets(compiler, condition);
                find_lets(compiler, then_branch);
                find_lets(compiler, else_branch);
            }
            TypedExprKind::Block { statements } => {
                for stmt in statements {
                    find_lets(compiler, stmt);
                }
            }
            TypedExprKind::FunctionCall { args, .. } => {
                for arg in args {
                    find_lets(compiler, arg);
                }
            }
            _ => {}
        }
    }
    find_lets(compiler, typed_body);
}

pub fn generate_builtin_helpers(compiler: &mut Compiler) {
    // --- __alloc ---
    compiler.wat_buffer.push_str("  (func $__alloc (param $size i32) (result i32)\n");
    compiler.wat_buffer.push_str("    global.get $heap_ptr\n");
    compiler.wat_buffer.push_str("    global.get $heap_ptr\n");
    compiler.wat_buffer.push_str("    local.get $size\n");
    compiler.wat_buffer.push_str("    i32.add\n");
    compiler.wat_buffer.push_str("    global.set $heap_ptr\n");
    compiler.wat_buffer.push_str("  )\n");
    
    // --- __string_concat ---
    compiler.wat_buffer.push_str("  (func $__string_concat (param $s1_header_ptr i32) (param $s2_header_ptr i32) (result i32)\n");
    compiler.wat_buffer.push_str("    (local $s1_data_ptr i32) (local $s1_len i32)\n");
    compiler.wat_buffer.push_str("    (local $s2_data_ptr i32) (local $s2_len i32)\n");
    compiler.wat_buffer.push_str("    (local $new_len i32) (local $new_cap i32)\n");
    compiler.wat_buffer.push_str("    (local $new_data_ptr i32) (local $new_header_ptr i32)\n");
    compiler.wat_buffer.push_str("    local.get $s1_header_ptr\n    i32.load offset=0\n    local.set $s1_data_ptr\n");
    compiler.wat_buffer.push_str("    local.get $s1_header_ptr\n    i32.load offset=4\n    local.set $s1_len\n");
    compiler.wat_buffer.push_str("    local.get $s2_header_ptr\n    i32.load offset=0\n    local.set $s2_data_ptr\n");
    compiler.wat_buffer.push_str("    local.get $s2_header_ptr\n    i32.load offset=4\n    local.set $s2_len\n");
    compiler.wat_buffer.push_str("    local.get $s1_len\n    local.get $s2_len\n    i32.add\n    local.set $new_len\n");
    compiler.wat_buffer.push_str("    local.get $new_len\n    i32.const 2\n    i32.mul\n    local.set $new_cap\n");
    compiler.wat_buffer.push_str("    local.get $new_cap\n    call $__alloc\n    local.set $new_data_ptr\n");
    compiler.wat_buffer.push_str("    i32.const 12\n    call $__alloc\n    local.set $new_header_ptr\n");
    compiler.wat_buffer.push_str("    local.get $new_data_ptr\n    local.get $s1_data_ptr\n    local.get $s1_len\n    memory.copy\n");
    compiler.wat_buffer.push_str("    local.get $new_data_ptr\n    local.get $s1_len\n    i32.add\n");
    compiler.wat_buffer.push_str("    local.get $s2_data_ptr\n    local.get $s2_len\n    memory.copy\n");
    compiler.wat_buffer.push_str("    local.get $new_header_ptr\n    local.get $new_data_ptr\n    i32.store offset=0\n");
    compiler.wat_buffer.push_str("    local.get $new_header_ptr\n    local.get $new_len\n    i32.store offset=4\n");
    compiler.wat_buffer.push_str("    local.get $new_header_ptr\n    local.get $new_cap\n    i32.store offset=8\n");
    compiler.wat_buffer.push_str("    local.get $new_header_ptr\n");
    compiler.wat_buffer.push_str("  )\n");
}