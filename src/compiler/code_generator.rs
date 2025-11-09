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
            // 特別に処理される組み込み関数
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
                _ => {}
            }
            
            // 一般の関数呼び出し
            for arg in args {
                generate_expr(compiler, arg)?;
            }
            if name.contains('.') { // Wasmの組み込み命令 (例: i32.add)
                compiler.wat_buffer.push_str(&format!("    {}\n", name));
            } else { // ユーザー定義関数
                compiler.wat_buffer.push_str(&format!("    call ${}\n", name));
            }
        }
        TypedExprKind::LetBinding { name, value } => {
            generate_expr(compiler, value)?;
            compiler.wat_buffer.push_str(&format!("    local.set ${}\n", name));
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
                if node.data_type != DataType::Unit {
                    return Err(LangError::Compile(CompileError::new(format!("Empty block must have type '()' but found '{}'", node.data_type), node.span)));
                }
                return Ok(());
            }
            for (i, stmt) in statements.iter().enumerate() {
                generate_expr(compiler, stmt)?;
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

    // --- __i32_to_string ---
    compiler.wat_buffer.push_str("  (func $__i32_to_string (param $n i32) (result i32)\n");
    compiler.wat_buffer.push_str("    (local $is_neg i32) (local $len i32) (local $temp_ptr i32) (local $write_ptr i32)\n");
    compiler.wat_buffer.push_str("    (local $char_ptr i32) (local $cap i32) (local $header_ptr i32) (local $i i32)\n");
    compiler.wat_buffer.push_str("    i32.const 16\n    call $__alloc\n    local.set $temp_ptr\n");
    compiler.wat_buffer.push_str("    local.get $temp_ptr\n    local.set $write_ptr\n");
    compiler.wat_buffer.push_str("    local.get $n\n    i32.const 0\n    i32.lt_s\n    local.set $is_neg\n");
    compiler.wat_buffer.push_str("    local.get $is_neg\n    if\n");
    compiler.wat_buffer.push_str("      local.get $n\n      i32.const -1\n      i32.mul\n      local.set $n\n");
    compiler.wat_buffer.push_str("    end\n");
    compiler.wat_buffer.push_str("    local.get $n\n    i32.const 0\n    i32.eq\n    if\n");
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      i32.const 48\n      i32.store8\n");
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      i32.const 1\n      i32.add\n      local.set $write_ptr\n");
    compiler.wat_buffer.push_str("    else\n");
    compiler.wat_buffer.push_str("      (loop $digits\n");
    compiler.wat_buffer.push_str("        local.get $write_ptr\n");
    compiler.wat_buffer.push_str("        local.get $n\n        i32.const 10\n        i32.rem_u\n");
    compiler.wat_buffer.push_str("        i32.const 48\n        i32.add\n");
    compiler.wat_buffer.push_str("        i32.store8\n");
    compiler.wat_buffer.push_str("        local.get $write_ptr\n        i32.const 1\n        i32.add\n        local.set $write_ptr\n");
    compiler.wat_buffer.push_str("        local.get $n\n        i32.const 10\n        i32.div_u\n        local.set $n\n");
    compiler.wat_buffer.push_str("        local.get $n\n        i32.const 0\n        i32.ne\n");
    compiler.wat_buffer.push_str("        br_if $digits\n");
    compiler.wat_buffer.push_str("      )\n");
    compiler.wat_buffer.push_str("    end\n");
    compiler.wat_buffer.push_str("    local.get $is_neg\n    if\n");
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      i32.const 45\n      i32.store8\n");
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      i32.const 1\n      i32.add\n      local.set $write_ptr\n");
    compiler.wat_buffer.push_str("    end\n");
    compiler.wat_buffer.push_str("    local.get $write_ptr\n    local.get $temp_ptr\n    i32.sub\n    local.set $len\n");
    compiler.wat_buffer.push_str("    local.get $len\n    local.set $cap\n");
    compiler.wat_buffer.push_str("    local.get $cap\n    call $__alloc\n    local.set $char_ptr\n");
    compiler.wat_buffer.push_str("    (loop $reverse\n");
    compiler.wat_buffer.push_str("      local.get $char_ptr\n      local.get $i\n      i32.add\n");
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      local.get $i\n      i32.sub\n      i32.const 1\n      i32.sub\n");
    compiler.wat_buffer.push_str("      i32.load8_u\n");
    compiler.wat_buffer.push_str("      i32.store8\n");
    compiler.wat_buffer.push_str("      local.get $i\n      i32.const 1\n      i32.add\n      local.set $i\n");
    compiler.wat_buffer.push_str("      local.get $i\n      local.get $len\n      i32.lt_s\n");
    compiler.wat_buffer.push_str("      br_if $reverse\n");
    compiler.wat_buffer.push_str("    )\n");
    compiler.wat_buffer.push_str("    i32.const 12\n    call $__alloc\n    local.set $header_ptr\n");
    compiler.wat_buffer.push_str("    local.get $header_ptr\n    local.get $char_ptr\n    i32.store offset=0\n");
    compiler.wat_buffer.push_str("    local.get $header_ptr\n    local.get $len\n    i32.store offset=4\n");
    compiler.wat_buffer.push_str("    local.get $header_ptr\n    local.get $cap\n    i32.store offset=8\n");
    compiler.wat_buffer.push_str("    local.get $header_ptr\n");
    compiler.wat_buffer.push_str("  )\n");
}