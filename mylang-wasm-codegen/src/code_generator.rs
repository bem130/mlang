//! TypedASTを受け取り、WATを生成するコードジェネレータ。

extern crate alloc;
use super::WasmGenerator;
use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use mylang_core::ast::*;
use mylang_core::error::{CompileError, LangError};
use mylang_core::span::Span;

pub fn generate(generator: &mut WasmGenerator, node: &TypedAstNode) -> Result<(), LangError> {
    match node {
        TypedAstNode::FnDef {
            name,
            params,
            body,
            return_type,
            ..
        } => {
            let param_types = params.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();
            let mangled_name = generator
                .resolve_function_label(name, &param_types)
                .expect("function label should be registered before codegen")
                .to_string();
            generator
                .wat_buffer
                .push_str(&format!("\n  ;; --- Function Definition: {} ---\n", name));
            generator
                .wat_buffer
                .push_str(&format!("  (func ${}", mangled_name));

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

            generator.clear_temp_state();
            collect_and_declare_locals(generator, body);

            generate_expr(generator, body)?;

            if name == "main" && body.data_type != DataType::Unit {
                generator
                    .wat_buffer
                    .push_str("    drop ;; Drop last expression value in main\n");
            }

            generator.wat_buffer.push_str("  )\n");
        }
        TypedAstNode::StructDef { .. } | TypedAstNode::EnumDef { .. } => {}
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

fn describe_argument_types(types: &[DataType]) -> String {
    types
        .iter()
        .map(|ty| ty.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

fn generate_expr(generator: &mut WasmGenerator, node: &TypedExpr) -> Result<(), LangError> {
    match &node.kind {
        TypedExprKind::Literal(val) => {
            let (wat_type, const_val) = match val {
                LiteralValue::I32(i) => ("i32", i.to_string()),
                LiteralValue::F64(f) => ("f64", f.to_string()),
                LiteralValue::Bool(b) => ("i32", if *b { "1" } else { "0" }.to_string()),
            };
            generator.wat_buffer.push_str(&format!(
                "    {}.const {} ;; Literal\n",
                wat_type, const_val
            ));
        }
        TypedExprKind::StringLiteral { header_offset } => {
            generator.wat_buffer.push_str(&format!(
                "    i32.const {} ;; String literal header\n",
                header_offset
            ));
        }
        TypedExprKind::VariableRef {
            unique_name, name, ..
        } => {
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
                    generator.wat_buffer.push_str("    call $__string_concat\n");
                    return Ok(());
                }
                "i32_to_string" => {
                    generate_expr(generator, &args[0])?;
                    generator.wat_buffer.push_str("    call $__i32_to_string\n");
                    return Ok(());
                }
                "f64_to_string" => {
                    generate_expr(generator, &args[0])?;
                    generator.wat_buffer.push_str("    call $__f64_to_string\n");
                    return Ok(());
                }
                "string_char_at" => {
                    generate_expr(generator, &args[0])?;
                    generate_expr(generator, &args[1])?;
                    generator
                        .wat_buffer
                        .push_str("    call $__string_char_at\n");
                    return Ok(());
                }
                "vec_new_i32" => {
                    generator.wat_buffer.push_str("    call $__vec_new_i32\n");
                    return Ok(());
                }
                "vec_push_i32" => {
                    generate_expr(generator, &args[0])?;
                    generate_expr(generator, &args[1])?;
                    generator.wat_buffer.push_str("    call $__vec_push_i32\n");
                    return Ok(());
                }
                "vec_get_i32" => {
                    generate_expr(generator, &args[0])?;
                    generate_expr(generator, &args[1])?;
                    generator.wat_buffer.push_str("    call $__vec_get_i32\n");
                    return Ok(());
                }
                "vec_len_i32" => {
                    generate_expr(generator, &args[0])?;
                    generator.wat_buffer.push_str("    call $__vec_len_i32\n");
                    return Ok(());
                }
                "read_line" => {
                    generator.wat_buffer.push_str("    call $__read_line\n");
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
                let arg_types = args
                    .iter()
                    .map(|arg| arg.data_type.clone())
                    .collect::<Vec<_>>();
                let label = generator
                    .resolve_function_label(name, &arg_types)
                    .ok_or_else(|| {
                        LangError::Compile(CompileError::new(
                            format!(
                                "No matching function label for '{}' with argument types ({})",
                                name,
                                describe_argument_types(&arg_types)
                            ),
                            node.span,
                        ))
                    })?;
                generator
                    .wat_buffer
                    .push_str(&format!("    call ${}\n", label));
            }
        }
        TypedExprKind::LetBinding { name, value, .. } => {
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
        TypedExprKind::LetHoistBinding { name, value, .. } => {
            // For now, generate the same code as a normal let binding.
            // Proper hoisting semantics (declare before evaluating value) will be
            // implemented in the analyzer; here we emit code like a normal let.
            generator
                .wat_buffer
                .push_str(&format!("    ;; LetHoist: {}\n", name.0));
            generate_expr(generator, value)?;
            if value.data_type != DataType::Unit {
                generator
                    .wat_buffer
                    .push_str(&format!("    local.set ${}\n", name.1));
            }
        }
        TypedExprKind::Assignment { name, value } => {
            generator
                .wat_buffer
                .push_str(&format!("    ;; Assign: {}\n", name.0));
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
        TypedExprKind::While { condition, body } => {
            generator.wat_buffer.push_str("    ;; While\n");
            generator.wat_buffer.push_str("    block ;; while exit\n");
            generator.wat_buffer.push_str("      loop ;; while loop\n");
            generate_expr(generator, condition)?;
            generator.wat_buffer.push_str("        i32.eqz\n");
            generator.wat_buffer.push_str("        br_if 1\n");
            generate_expr(generator, body)?;
            generator.wat_buffer.push_str("        br 0\n");
            generator.wat_buffer.push_str("      end\n");
            generator.wat_buffer.push_str("    end\n");
        }
        TypedExprKind::Lambda { params, body } => {
            generator.lambda_count += 1;
            let lambda_name = format!("__lambda_{}", generator.lambda_count);

            // ラムダ関数を生成するために、ジェネレータのメインバッファを一時的に退避
            let original_buffer = core::mem::take(&mut generator.wat_buffer);

            // --- 新しいバッファにラムダ関数を生成 ---
            generator.wat_buffer.push_str(&format!(
                "\n  ;; --- Lambda Function: {} ---\n",
                lambda_name
            ));
            generator
                .wat_buffer
                .push_str(&format!("  (func ${}", lambda_name));

            for (param_name, param_type) in params {
                generator.wat_buffer.push_str(&format!(
                    " (param ${} {})",
                    param_name,
                    generator.type_to_wat(param_type)
                ));
            }

            if node.data_type != DataType::Unit {
                if let DataType::Function { return_type, .. } = &node.data_type {
                    if **return_type != DataType::Unit {
                        generator
                            .wat_buffer
                            .push_str(&format!(" (result {})", generator.type_to_wat(return_type)));
                    }
                }
            }
            generator.wat_buffer.push_str("\n");

            generator.clear_temp_state();
            collect_and_declare_locals(generator, body);
            generate_expr(generator, body)?;
            generator.wat_buffer.push_str("  )\n");

            // 生成したラムダコードを専用バッファに移動し、元のバッファを復元
            let generated_lambda_code = core::mem::take(&mut generator.wat_buffer);
            generator.wat_buffer = original_buffer;
            generator.lambdas_buffer.push_str(&generated_lambda_code);

            // ラムダ式を評価した結果（関数ポインタ）をスタックに積む
            generator
                .wat_buffer
                .push_str("    i32.const 0 ;; Lambda function index (placeholder)\n");
        }
        TypedExprKind::TupleLiteral { elements } => {
            let tuple_types = match &node.data_type {
                DataType::Tuple(types) => types,
                other => {
                    return Err(LangError::Compile(CompileError::new(
                        format!("Tuple literal has unexpected type '{}'", other),
                        node.span,
                    )));
                }
            };
            let layout = compute_tuple_layout(tuple_types);
            let tuple_ptr_local =
                generator.tuple_temp_local_for(node as *const _ as usize, &node.data_type);
            let total_size = layout.total_size.max(1);
            generator.wat_buffer.push_str(&format!(
                "    ;; Tuple literal allocate {} bytes\n",
                total_size
            ));
            generator
                .wat_buffer
                .push_str(&format!("    i32.const {}\n", total_size));
            generator.wat_buffer.push_str("    call $__alloc\n");
            generator
                .wat_buffer
                .push_str(&format!("    local.set ${}\n", tuple_ptr_local));

            for (idx, element) in elements.iter().enumerate() {
                let offset = layout.offsets[idx];
                generator
                    .wat_buffer
                    .push_str(&format!("    local.get ${}\n", tuple_ptr_local));
                if offset != 0 {
                    generator
                        .wat_buffer
                        .push_str(&format!("    i32.const {}\n", offset));
                    generator.wat_buffer.push_str("    i32.add\n");
                }
                generate_expr(generator, element)?;
                emit_store_for_type(generator, &element.data_type)?;
            }

            generator
                .wat_buffer
                .push_str(&format!("    local.get ${}\n", tuple_ptr_local));
        }
        TypedExprKind::Match { value, arms } => {
            let (scrutinee_local, value_type) = generator
                .scrutinee_local_for(value.as_ref() as *const _ as usize, &value.data_type);
            generator.wat_buffer.push_str("    ;; Match expression\n");
            generate_expr(generator, value)?;
            generator
                .wat_buffer
                .push_str(&format!("    local.set ${}\n", scrutinee_local));

            let match_label = format!("$__match_end_{}", node as *const _ as usize);
            let result_clause = if node.data_type == DataType::Unit {
                String::new()
            } else {
                format!(" (result {})", generator.type_to_wat(&node.data_type))
            };
            generator.wat_buffer.push_str(&format!(
                "    (block {}\n",
                format!("{}{}", match_label, result_clause)
            ));

            for (arm_index, arm) in arms.iter().enumerate() {
                let arm_label = format!(
                    "$__match_arm_fail_{}_{}",
                    node as *const _ as usize, arm_index
                );
                generator
                    .wat_buffer
                    .push_str(&format!("      (block {}\n", arm_label));

                generate_pattern_condition(
                    generator,
                    &arm.pattern,
                    &scrutinee_local,
                    &value_type,
                    node.span,
                )?;
                generator.wat_buffer.push_str("        i32.eqz\n");
                generator
                    .wat_buffer
                    .push_str(&format!("        br_if {}\n", arm_label));

                bind_pattern_values(
                    generator,
                    &arm.pattern,
                    &scrutinee_local,
                    &value_type,
                    node.span,
                )?;
                generate_expr(generator, &arm.body)?;
                generator
                    .wat_buffer
                    .push_str(&format!("        br {}\n", match_label));
                generator.wat_buffer.push_str("      )\n");
            }

            generator
                .wat_buffer
                .push_str("      unreachable ;; Analyzer enforces exhaustiveness\n");
            generator.wat_buffer.push_str("    )\n");
        }
    }
    Ok(())
}

struct TupleLayout {
    offsets: Vec<u32>,
    total_size: u32,
}

fn type_size_and_align(data_type: &DataType) -> (u32, u32) {
    match data_type {
        DataType::I32
        | DataType::Bool
        | DataType::String
        | DataType::Vector(_)
        | DataType::Tuple(_)
        | DataType::Struct(_)
        | DataType::Enum(_) => (4, 4),
        DataType::Function { .. } => (4, 4),
        DataType::F64 => (8, 8),
        DataType::Unit => (0, 1),
        DataType::TypeVar(name) => {
            panic!("Type variable '{}' cannot be lowered in wasm codegen", name)
        }
    }
}

fn compute_tuple_layout(types: &[DataType]) -> TupleLayout {
    let mut offsets = Vec::with_capacity(types.len());
    let mut current = 0u32;
    for ty in types {
        let (size, align) = type_size_and_align(ty);
        let align = align.max(1);
        if align > 1 {
            let remainder = current % align;
            if remainder != 0 {
                current += align - remainder;
            }
        }
        offsets.push(current);
        current += size;
    }
    TupleLayout {
        offsets,
        total_size: current,
    }
}

fn emit_store_for_type(
    generator: &mut WasmGenerator,
    data_type: &DataType,
) -> Result<(), LangError> {
    match data_type {
        DataType::F64 => {
            generator.wat_buffer.push_str("    f64.store\n");
        }
        DataType::Unit => {
            generator
                .wat_buffer
                .push_str("    drop ;; discard address for unit value\n");
        }
        _ => {
            generator.wat_buffer.push_str("    i32.store\n");
        }
    }
    Ok(())
}

fn emit_load_tuple_field(
    generator: &mut WasmGenerator,
    tuple_local: &str,
    offset: u32,
    element_type: &DataType,
    indent: &str,
    span: Span,
) -> Result<(), LangError> {
    generator
        .wat_buffer
        .push_str(&format!("{}local.get ${}\n", indent, tuple_local));
    if offset != 0 {
        generator
            .wat_buffer
            .push_str(&format!("{}i32.const {}\n", indent, offset));
        generator
            .wat_buffer
            .push_str(&format!("{}i32.add\n", indent));
    }
    match element_type {
        DataType::F64 => {
            generator
                .wat_buffer
                .push_str(&format!("{}f64.load\n", indent));
        }
        DataType::Unit => {
            return Err(LangError::Compile(CompileError::new(
                "Tuple elements of type '()' are not supported in code generation",
                span,
            )));
        }
        _ => {
            generator
                .wat_buffer
                .push_str(&format!("{}i32.load\n", indent));
        }
    }
    Ok(())
}

fn generate_tuple_element_condition(
    generator: &mut WasmGenerator,
    pattern: &TypedPattern,
    tuple_local: &str,
    element_type: &DataType,
    offset: u32,
    span: Span,
) -> Result<(), LangError> {
    match pattern {
        TypedPattern::Wildcard | TypedPattern::Binding { .. } => {
            generator.wat_buffer.push_str("        i32.const 1\n");
        }
        TypedPattern::Literal(literal) => {
            emit_load_tuple_field(
                generator,
                tuple_local,
                offset,
                element_type,
                "        ",
                span,
            )?;
            match (element_type, literal) {
                (DataType::I32, LiteralValue::I32(val)) => {
                    generator
                        .wat_buffer
                        .push_str(&format!("        i32.const {}\n", val));
                    generator.wat_buffer.push_str("        i32.eq\n");
                }
                (DataType::Bool, LiteralValue::Bool(val)) => {
                    generator
                        .wat_buffer
                        .push_str(&format!("        i32.const {}\n", if *val { 1 } else { 0 }));
                    generator.wat_buffer.push_str("        i32.eq\n");
                }
                (DataType::F64, LiteralValue::F64(val)) => {
                    generator
                        .wat_buffer
                        .push_str(&format!("        f64.const {}\n", val));
                    generator.wat_buffer.push_str("        f64.eq\n");
                }
                _ => {
                    return Err(LangError::Compile(CompileError::new(
                        "Tuple literal patterns are only supported for integers, floats, and booleans",
                        span,
                    )));
                }
            }
        }
        TypedPattern::Tuple(subpatterns) => {
            if let DataType::Tuple(inner_types) = element_type {
                if inner_types.len() != subpatterns.len() {
                    return Err(LangError::Compile(CompileError::new(
                        "Tuple pattern length does not match tuple value length",
                        span,
                    )));
                }
                let nested_local =
                    generator.tuple_pattern_local_for(pattern as *const _ as usize, element_type);
                emit_load_tuple_field(
                    generator,
                    tuple_local,
                    offset,
                    element_type,
                    "        ",
                    span,
                )?;
                generator
                    .wat_buffer
                    .push_str(&format!("        local.set ${}\n", nested_local));
                generate_pattern_condition(generator, pattern, &nested_local, element_type, span)?;
            } else {
                return Err(LangError::Compile(CompileError::new(
                    "Tuple pattern used on a non-tuple value",
                    span,
                )));
            }
        }
        _ => {
            return Err(LangError::Compile(CompileError::new(
                "Only wildcard, binding, or literal tuple patterns are supported in code generation",
                span,
            )));
        }
    }
    Ok(())
}

fn generate_pattern_condition(
    generator: &mut WasmGenerator,
    pattern: &TypedPattern,
    scrutinee_local: &str,
    value_type: &DataType,
    span: Span,
) -> Result<(), LangError> {
    match pattern {
        TypedPattern::Wildcard | TypedPattern::Binding { .. } => {
            generator.wat_buffer.push_str("        i32.const 1\n");
        }
        TypedPattern::Literal(literal) => match (value_type, literal) {
            (DataType::I32, LiteralValue::I32(val)) => {
                generator
                    .wat_buffer
                    .push_str(&format!("        local.get ${}\n", scrutinee_local));
                generator
                    .wat_buffer
                    .push_str(&format!("        i32.const {}\n", val));
                generator.wat_buffer.push_str("        i32.eq\n");
            }
            (DataType::Bool, LiteralValue::Bool(val)) => {
                generator
                    .wat_buffer
                    .push_str(&format!("        local.get ${}\n", scrutinee_local));
                generator
                    .wat_buffer
                    .push_str(&format!("        i32.const {}\n", if *val { 1 } else { 0 }));
                generator.wat_buffer.push_str("        i32.eq\n");
            }
            (DataType::F64, LiteralValue::F64(val)) => {
                generator
                    .wat_buffer
                    .push_str(&format!("        local.get ${}\n", scrutinee_local));
                generator
                    .wat_buffer
                    .push_str(&format!("        f64.const {}\n", val));
                generator.wat_buffer.push_str("        f64.eq\n");
            }
            _ => {
                return Err(LangError::Compile(CompileError::new(
                    "Literal patterns are only supported for i32, f64, and bool values in code generation",
                    span,
                )));
            }
        },
        TypedPattern::Tuple(elements) => {
            if let DataType::Tuple(field_types) = value_type {
                if elements.len() != field_types.len() {
                    return Err(LangError::Compile(CompileError::new(
                        "Tuple pattern length does not match tuple value length",
                        span,
                    )));
                }
                let layout = compute_tuple_layout(field_types);
                generator.wat_buffer.push_str("        i32.const 1\n");
                for (index, element_pattern) in elements.iter().enumerate() {
                    generate_tuple_element_condition(
                        generator,
                        element_pattern,
                        scrutinee_local,
                        &field_types[index],
                        layout.offsets[index],
                        span,
                    )?;
                    generator.wat_buffer.push_str("        i32.and\n");
                }
            } else {
                return Err(LangError::Compile(CompileError::new(
                    "Tuple pattern used on a non-tuple value",
                    span,
                )));
            }
        }
        _ => {
            return Err(LangError::Compile(CompileError::new(
                "Only tuple, literal, wildcard, and binding patterns are supported in code generation",
                span,
            )));
        }
    }
    Ok(())
}

fn bind_pattern_values(
    generator: &mut WasmGenerator,
    pattern: &TypedPattern,
    scrutinee_local: &str,
    value_type: &DataType,
    span: Span,
) -> Result<(), LangError> {
    match pattern {
        TypedPattern::Binding { name, .. } => {
            generator
                .wat_buffer
                .push_str(&format!("        local.get ${}\n", scrutinee_local));
            generator
                .wat_buffer
                .push_str(&format!("        local.set ${}\n", name.1));
        }
        TypedPattern::Tuple(elements) => {
            if let DataType::Tuple(field_types) = value_type {
                if elements.len() != field_types.len() {
                    return Err(LangError::Compile(CompileError::new(
                        "Tuple pattern length does not match tuple value length",
                        span,
                    )));
                }
                let layout = compute_tuple_layout(field_types);
                for (index, element_pattern) in elements.iter().enumerate() {
                    let field_type = &field_types[index];
                    let offset = layout.offsets[index];
                    match element_pattern {
                        TypedPattern::Binding { name, .. } => {
                            emit_load_tuple_field(
                                generator,
                                scrutinee_local,
                                offset,
                                field_type,
                                "        ",
                                span,
                            )?;
                            generator
                                .wat_buffer
                                .push_str(&format!("        local.set ${}\n", name.1));
                        }
                        TypedPattern::Tuple(_) => {
                            let nested_local = generator.tuple_pattern_local_for(
                                element_pattern as *const _ as usize,
                                field_type,
                            );
                            emit_load_tuple_field(
                                generator,
                                scrutinee_local,
                                offset,
                                field_type,
                                "        ",
                                span,
                            )?;
                            generator
                                .wat_buffer
                                .push_str(&format!("        local.set ${}\n", nested_local));
                            bind_pattern_values(
                                generator,
                                element_pattern,
                                &nested_local,
                                field_type,
                                span,
                            )?;
                        }
                        TypedPattern::Wildcard | TypedPattern::Literal(_) => {}
                        _ => {
                            return Err(LangError::Compile(CompileError::new(
                                "Only wildcard, binding, or literal tuple subpatterns are supported in code generation",
                                span,
                            )));
                        }
                    }
                }
            } else {
                return Err(LangError::Compile(CompileError::new(
                    "Tuple pattern used on a non-tuple value",
                    span,
                )));
            }
        }
        TypedPattern::EnumVariant { .. } => {
            return Err(LangError::Compile(CompileError::new(
                "Enum pattern matching is not yet supported in Wasm code generation",
                span,
            )));
        }
        TypedPattern::Literal(_) | TypedPattern::Wildcard => {}
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
            TypedExprKind::Assignment { value, .. } => find_prints(value, uses),
            TypedExprKind::LetHoistBinding { value, .. } => find_prints(value, uses),
            TypedExprKind::Lambda { body, .. } => find_prints(body, uses),
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
            TypedExprKind::While { condition, body } => {
                find_prints(condition, uses);
                find_prints(body, uses);
            }
            TypedExprKind::TupleLiteral { elements } => {
                for element in elements {
                    find_prints(element, uses);
                }
            }
            TypedExprKind::Match { value, arms } => {
                find_prints(value, uses);
                for arm in arms {
                    find_prints(&arm.body, uses);
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

    fn collect_pattern_locals(
        generator: &mut WasmGenerator,
        pattern: &TypedPattern,
        value_type: Option<&DataType>,
        locals: &mut BTreeMap<String, DataType>,
    ) {
        match pattern {
            TypedPattern::Binding { name, data_type } => {
                locals.insert(name.1.clone(), data_type.clone());
            }
            TypedPattern::Tuple(elements) => {
                if let Some(DataType::Tuple(field_types)) = value_type {
                    for (element, field_type) in elements.iter().zip(field_types.iter()) {
                        if matches!(element, TypedPattern::Tuple(_)) {
                            let local_name = generator
                                .tuple_pattern_local_for(element as *const _ as usize, field_type);
                            locals
                                .entry(local_name.clone())
                                .or_insert_with(|| field_type.clone());
                        }
                        collect_pattern_locals(generator, element, Some(field_type), locals);
                    }
                } else {
                    for element in elements {
                        collect_pattern_locals(generator, element, None, locals);
                    }
                }
            }
            TypedPattern::EnumVariant { fields, .. } => {
                for field in fields {
                    collect_pattern_locals(generator, field, None, locals);
                }
            }
            _ => {}
        }
    }

    fn find_lets(
        generator: &mut WasmGenerator,
        expr: &TypedExpr,
        locals: &mut BTreeMap<String, DataType>,
    ) {
        match &expr.kind {
            TypedExprKind::LetBinding { name, value, .. } => {
                locals.insert(name.1.clone(), value.data_type.clone());
                find_lets(generator, value, locals);
            }
            TypedExprKind::LetHoistBinding { name, value, .. } => {
                locals.insert(name.1.clone(), value.data_type.clone());
                find_lets(generator, value, locals);
            }
            TypedExprKind::Lambda { .. } => {
                // Do not traverse into lambda body for collecting function-level locals;
                // lambda-local lets should be handled when generating the lambda body/function.
            }
            TypedExprKind::Assignment { value, .. } => {
                find_lets(generator, value, locals);
            }
            TypedExprKind::IfExpr {
                condition,
                then_branch,
                else_branch,
            } => {
                find_lets(generator, condition, locals);
                find_lets(generator, then_branch, locals);
                find_lets(generator, else_branch, locals);
            }
            TypedExprKind::Block { statements } => {
                for stmt in statements {
                    find_lets(generator, stmt, locals);
                }
            }
            TypedExprKind::While { condition, body } => {
                find_lets(generator, condition, locals);
                find_lets(generator, body, locals);
            }
            TypedExprKind::TupleLiteral { elements } => {
                let temp_local =
                    generator.tuple_temp_local_for(expr as *const _ as usize, &expr.data_type);
                locals.entry(temp_local).or_insert(expr.data_type.clone());
                for element in elements {
                    find_lets(generator, element, locals);
                }
            }
            TypedExprKind::Match { value, arms } => {
                let (scrutinee_local, scrutinee_type) = generator
                    .scrutinee_local_for(value.as_ref() as *const _ as usize, &value.data_type);
                locals
                    .entry(scrutinee_local)
                    .or_insert(scrutinee_type.clone());
                find_lets(generator, value, locals);
                for arm in arms {
                    collect_pattern_locals(generator, &arm.pattern, Some(&scrutinee_type), locals);
                    find_lets(generator, &arm.body, locals);
                }
            }
            TypedExprKind::FunctionCall { args, .. } => {
                for arg in args {
                    find_lets(generator, arg, locals);
                }
            }
            _ => {}
        }
    }

    find_lets(generator, typed_body, &mut locals);

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
