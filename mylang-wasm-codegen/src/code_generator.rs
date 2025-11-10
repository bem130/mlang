//! TypedASTを受け取り、WATを生成するコードジェネレータ。

extern crate alloc;
use super::WasmGenerator;
use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::{String, ToString};
use mylang_core::ast::*;
use mylang_core::error::{CompileError, LangError};

pub fn generate(generator: &mut WasmGenerator, node: &TypedAstNode) -> Result<(), LangError> {
    match node {
        TypedAstNode::FnDef {
            name,
            params,
            body,
            return_type,
            ..
        } => {
            generator
                .wat_buffer
                .push_str(&format!("\n  ;; --- Function Definition: {} ---\n", name));
            generator.wat_buffer.push_str(&format!("  (func ${}", name));

            for (param_name, param_type) in params {
                generator.wat_buffer.push_str(&format!(
                    " (param ${} {})",
                    param_name,
                    generator.type_to_wat(param_type)
                ));
            }

            if *return_type != DataType::Unit {
                generator
                    .wat_buffer
                    .push_str(&format!(" (result {})", generator.type_to_wat(return_type)));
            }
            generator.wat_buffer.push_str("\n");

            collect_and_declare_locals(generator, body);

            generate_expr(generator, body)?;

            if name == "main" && body.data_type != DataType::Unit {
                generator
                    .wat_buffer
                    .push_str("    drop ;; Drop last expression value in main\n");
            }

            generator.wat_buffer.push_str("  )\n");
        }
    }
    Ok(())
}

/// スタックのトップにある文字列（へのポインタ）を標準出力に書き出すWATコードを生成する。
fn generate_print_logic(generator: &mut WasmGenerator) {
    generator
        .wat_buffer
        .push_str("    local.set $__print_temp\n");
    generator
        .wat_buffer
        .push_str("    i32.const 0 ;; iovec[0].buf\n");
    generator
        .wat_buffer
        .push_str("    local.get $__print_temp\n");
    generator
        .wat_buffer
        .push_str("    i32.load offset=0 ;; load data_ptr from header\n");
    generator.wat_buffer.push_str("    i32.store\n");
    generator
        .wat_buffer
        .push_str("    i32.const 4 ;; iovec[0].buf_len\n");
    generator
        .wat_buffer
        .push_str("    local.get $__print_temp\n");
    generator
        .wat_buffer
        .push_str("    i32.load offset=4 ;; load data_len from header\n");
    generator.wat_buffer.push_str("    i32.store\n");
    generator
        .wat_buffer
        .push_str("    i32.const 1   ;; fd (1=stdout)\n");
    generator
        .wat_buffer
        .push_str("    i32.const 0   ;; iovecs_ptr\n");
    generator
        .wat_buffer
        .push_str("    i32.const 1   ;; iovecs_len\n");
    generator
        .wat_buffer
        .push_str("    i32.const 16  ;; nwritten_ptr\n");
    generator
        .wat_buffer
        .push_str("    call $fd_write\n    drop\n");
}

fn generate_expr(generator: &mut WasmGenerator, node: &TypedExpr) -> Result<(), LangError> {
    match &node.kind {
        TypedExprKind::Literal(val) => {
            let (wat_type, const_val) = match val {
                LiteralValue::I32(i) => ("i32", i.to_string()),
                LiteralValue::F64(f) => ("f64", f.to_string()),
                LiteralValue::Bool(b) => ("i32", if *b { "1" } else { "0" }.to_string()),
            };
            generator
                .wat_buffer
                .push_str(&format!("    {}.const {} ;; Literal\n", wat_type, const_val));
        }
        TypedExprKind::StringLiteral { header_offset } => {
            generator.wat_buffer.push_str(&format!(
                "    i32.const {} ;; String literal header\n",
                header_offset
            ));
        }
        TypedExprKind::VariableRef { unique_name, name, .. } => {
            generator.wat_buffer.push_str(&format!(
                "    local.get ${} ;; Var: {}\n",
                unique_name, name
            ));
        }
        TypedExprKind::FunctionCall { name, args } => {
            if name.contains('.') {
                generator
                    .wat_buffer
                    .push_str(&format!("    ;; Built-in Op: {}\n", name));
            } else {
                generator
                    .wat_buffer
                    .push_str(&format!("    ;; Call: {}\n", name));
            }

            // --- 組み込み関数の特別処理 ---
            match name.as_str() {
                "string_concat" => {
                    generate_expr(generator, &args[0])?;
                    generate_expr(generator, &args[1])?;
                    generator
                        .wat_buffer
                        .push_str("    call $__string_concat\n");
                    return Ok(());
                }
                "i32_to_string" => {
                    generate_expr(generator, &args[0])?;
                    generator
                        .wat_buffer
                        .push_str("    call $__i32_to_string\n");
                    return Ok(());
                }
                "print" => {
                    generate_expr(generator, &args[0])?;
                    generate_print_logic(generator);
                    return Ok(());
                }
                "println" => {
                    generate_expr(generator, &args[0])?;
                    generate_print_logic(generator);
                    // 改行文字は静的に確保済みのはず
                    let newline_header_offset = generator
                        .string_headers
                        .get("\n")
                        .map(|(_, header_offset)| *header_offset) // (data, header)タプルからheader_offsetを取り出す
                        .expect("Newline string should be pre-allocated");
                    generator.wat_buffer.push_str(&format!(
                        "    i32.const {} ;; newline string header\n",
                        newline_header_offset
                    ));
                    generate_print_logic(generator);
                    return Ok(());
                }
                _ => {}
            }

            // --- 通常の関数呼び出し ---
            for arg in args {
                generate_expr(generator, arg)?;
            }
            if name.contains('.') {
                generator.wat_buffer.push_str(&format!("    {}\n", name));
            } else {
                generator
                    .wat_buffer
                    .push_str(&format!("    call ${}\n", name));
            }
        }
        TypedExprKind::LetBinding { name, value } => {
            generator
                .wat_buffer
                .push_str(&format!("    ;; Let: {}\n", name.0));
            generate_expr(generator, value)?;
            if value.data_type != DataType::Unit {
                generator
                    .wat_buffer
                    .push_str(&format!("    local.set ${}\n", name.1));
            }
        }
        TypedExprKind::IfExpr {
            condition,
            then_branch,
            else_branch,
        } => {
            generator.wat_buffer.push_str("    ;; If\n");
            generate_expr(generator, condition)?;
            generator.wat_buffer.push_str(&format!(
                "    (if (result {})\n",
                generator.type_to_wat(&node.data_type)
            ));
            generator.wat_buffer.push_str("      (then\n");
            generate_expr(generator, then_branch)?;
            generator.wat_buffer.push_str("      )\n");
            generator.wat_buffer.push_str("      (else\n");
            generate_expr(generator, else_branch)?;
            generator.wat_buffer.push_str("      )\n");
            generator.wat_buffer.push_str("    )\n");
        }
        TypedExprKind::Block { statements } => {
            generator.wat_buffer.push_str("    ;; Block\n");
            if statements.is_empty() {
                if node.data_type != DataType::Unit {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Empty block must have type '()' but found '{}'",
                            node.data_type
                        ),
                        node.span,
                    )));
                }
                return Ok(());
            }
            for (i, stmt) in statements.iter().enumerate() {
                generate_expr(generator, stmt)?;
                if i < statements.len() - 1 && stmt.data_type != DataType::Unit {
                    generator.wat_buffer.push_str("    drop\n");
                }
            }
        }
    }
    Ok(())
}

fn body_uses_print(expr: &TypedExpr) -> bool {
    let mut uses = false;
    fn find_prints(expr: &TypedExpr, uses: &mut bool) {
        if *uses {
            return;
        }
        match &expr.kind {
            TypedExprKind::FunctionCall { name, args, .. } => {
                if name == "print" || name == "println" {
                    *uses = true;
                }
                for arg in args {
                    find_prints(arg, uses);
                }
            }
            TypedExprKind::LetBinding { value, .. } => find_prints(value, uses),
            TypedExprKind::IfExpr {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                find_prints(condition, uses);
                find_prints(then_branch, uses);
                find_prints(else_branch, uses);
            }
            TypedExprKind::Block { statements, .. } => {
                for stmt in statements {
                    find_prints(stmt, uses);
                }
            }
            _ => {}
        }
    }
    find_prints(expr, &mut uses);
    uses
}

pub fn collect_and_declare_locals(generator: &mut WasmGenerator, typed_body: &TypedExpr) {
    let mut locals: BTreeMap<String, DataType> = BTreeMap::new();

    fn find_lets(expr: &TypedExpr, locals: &mut BTreeMap<String, DataType>) {
        match &expr.kind {
            TypedExprKind::LetBinding { name, value } => {
                locals.insert(name.1.clone(), value.data_type.clone());
                find_lets(value, locals);
            }
            TypedExprKind::IfExpr {
                condition,
                then_branch,
                else_branch,
            } => {
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
        generator.wat_buffer.push_str("    ;; Local variables\n");
        for (unique_name, data_type) in locals {
            if data_type != DataType::Unit {
                generator.wat_buffer.push_str(&format!(
                    "    (local ${} {})\n",
                    unique_name,
                    generator.type_to_wat(&data_type)
                ));
            }
        }
    }

    if body_uses_print(typed_body) {
        generator
            .wat_buffer
            .push_str("    (local $__print_temp i32) ;; for print/println iovec\n");
    }
}

pub fn generate_builtin_helpers(generator: &mut WasmGenerator) {
    let builtins_wat = include_str!("builtins.wat");
    generator.wat_buffer.push_str(builtins_wat);
}