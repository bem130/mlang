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
        TypedExprKind::PrintStmt { value } => {
            compiler.wat_buffer.push_str("    ;; print\n");
            generate_expr(compiler, value)?;
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
        TypedExprKind::PrintlnStmt { value } => {
            compiler.wat_buffer.push_str("    ;; println (implemented as two separate prints for portability)\n");

            // First: print the actual value.
            generate_expr(compiler, value)?;
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

            // Second: print the newline character.
            let newline_header_offset = compiler.ensure_string_is_statically_allocated(&"\n".to_string());
            compiler.wat_buffer.push_str(&format!("    i32.const {} ;; newline string header\n", newline_header_offset));
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
    }
    Ok(())
}

fn body_uses_print(expr: &TypedExpr) -> bool {
    let mut uses = false;
    fn find_prints(expr: &TypedExpr, uses: &mut bool) {
        if *uses { return; }
        match &expr.kind {
            TypedExprKind::PrintStmt {..} | TypedExprKind::PrintlnStmt {..} => { *uses = true; },
            TypedExprKind::LetBinding { value, .. } => find_prints(value, uses),
            TypedExprKind::IfExpr { condition, then_branch, else_branch, .. } => {
                find_prints(condition, uses); find_prints(then_branch, uses); find_prints(else_branch, uses);
            }
            TypedExprKind::Block { statements, .. } => { for stmt in statements { find_prints(stmt, uses); } }
            TypedExprKind::FunctionCall { args, .. } => { for arg in args { find_prints(arg, uses); } }
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
            // 【修正点】Unit型の変数はWasmのローカル変数として宣言しない
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
    compiler.wat_buffer.push_str("\n  ;; --- Built-in Helper Functions ---\n");
    // --- __alloc (simple bump allocator) ---
    compiler.wat_buffer.push_str("  ;; Allocates memory by incrementing a heap pointer.\n");
    compiler.wat_buffer.push_str("  ;; Does not implement freeing.\n");
    compiler.wat_buffer.push_str("  (func $__alloc (param $size i32) (result i32)\n");
    compiler.wat_buffer.push_str("    global.get $heap_ptr         ;; Return current heap pointer\n");
    compiler.wat_buffer.push_str("    global.get $heap_ptr         ;; Push heap pointer again to calculate next\n");
    compiler.wat_buffer.push_str("    local.get $size\n");
    compiler.wat_buffer.push_str("    i32.add\n");
    compiler.wat_buffer.push_str("    global.set $heap_ptr         ;; Update heap pointer\n");
    compiler.wat_buffer.push_str("  )\n");
    
    // --- __string_concat ---
    compiler.wat_buffer.push_str("\n  ;; Concatenates two strings, returns a new string.\n");
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
    compiler.wat_buffer.push_str("\n  ;; Converts an i32 to a new string.\n");
    compiler.wat_buffer.push_str("  (func $__i32_to_string (param $n i32) (result i32)\n");
    compiler.wat_buffer.push_str("    (local $is_neg i32) (local $len i32) (local $temp_ptr i32) (local $write_ptr i32)\n");
    compiler.wat_buffer.push_str("    (local $char_ptr i32) (local $cap i32) (local $header_ptr i32) (local $i i32)\n");
    compiler.wat_buffer.push_str("    i32.const 16\n    call $__alloc\n    local.set $temp_ptr ;; Temp buffer for reversed digits\n");
    compiler.wat_buffer.push_str("    local.get $temp_ptr\n    local.set $write_ptr\n");
    compiler.wat_buffer.push_str("    local.get $n\n    i32.const 0\n    i32.lt_s\n    local.set $is_neg\n");
    compiler.wat_buffer.push_str("    local.get $is_neg\n    if\n");
    compiler.wat_buffer.push_str("      local.get $n\n      i32.const -1\n      i32.mul\n      local.set $n\n");
    compiler.wat_buffer.push_str("    end\n");
    compiler.wat_buffer.push_str("    local.get $n\n    i32.const 0\n    i32.eq\n    if\n");
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      i32.const 48 ;; '0'\n      i32.store8\n");
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
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      i32.const 45 ;; '-'\n      i32.store8\n");
    compiler.wat_buffer.push_str("      local.get $write_ptr\n      i32.const 1\n      i32.add\n      local.set $write_ptr\n");
    compiler.wat_buffer.push_str("    end\n");
    compiler.wat_buffer.push_str("    local.get $write_ptr\n    local.get $temp_ptr\n    i32.sub\n    local.set $len\n");
    compiler.wat_buffer.push_str("    local.get $len\n    local.set $cap\n");
    compiler.wat_buffer.push_str("    local.get $cap\n    call $__alloc\n    local.set $char_ptr ;; Final buffer for correct-order string\n");
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