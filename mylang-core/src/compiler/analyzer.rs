//! RawASTを意味解析・型チェックし、TypedASTに変換するアナライザー。

extern crate alloc;
use super::{Analyzer, FunctionSignature, VariableEntry};
use crate::alloc::string::ToString;
use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::span::Span;
use crate::token::Token;
use alloc::boxed::Box;
use alloc::collections::BTreeSet;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;

/// トップレベルのRawAstNodeを型付きASTに変換するエントリーポイント
pub fn analyze_toplevel(
    analyzer: &mut Analyzer,
    node: &RawAstNode,
) -> Result<TypedAstNode, LangError> {
    match node {
        RawAstNode::StructDef { name, span, .. } => {
            let info = analyzer.struct_table.get(&name.0).cloned().ok_or_else(|| {
                LangError::Compile(CompileError::new(
                    format!("Unknown struct '{}'", name.0),
                    name.1,
                ))
            })?;
            let fields = info
                .fields
                .into_iter()
                .map(|(field_name, field_type)| (field_name, field_type))
                .collect();
            Ok(TypedAstNode::StructDef {
                name: name.0.clone(),
                fields,
                span: *span,
            })
        }
        RawAstNode::EnumDef { name, span, .. } => {
            let info = analyzer.enum_table.get(&name.0).cloned().ok_or_else(|| {
                LangError::Compile(CompileError::new(
                    format!("Unknown enum '{}'", name.0),
                    name.1,
                ))
            })?;
            let variants = info
                .variants
                .into_iter()
                .map(|(variant_name, variant_info)| TypedEnumVariant {
                    name: variant_name,
                    field_types: variant_info.field_types,
                    span: variant_info.span,
                })
                .collect();
            Ok(TypedAstNode::EnumDef {
                name: name.0.clone(),
                variants,
                span: *span,
            })
        }
        RawAstNode::LetHoist { name, value, span } => {
            analyzer.var_counters.clear();

            let func_name = &name.0;
            let signature = analyzer
                .function_table
                .get(func_name)
                .and_then(|candidates| {
                    candidates
                        .iter()
                        .find(|sig| sig.definition_span == name.1)
                        .cloned()
                })
                .ok_or_else(|| {
                    LangError::Compile(CompileError::new(
                        format!("Undefined function '{}'", func_name),
                        name.1,
                    ))
                })?;

            // ラムダ式の本体を解析するために、パラメータをスコープに追加
            analyzer.enter_scope();
            let mut typed_params = Vec::new();

            if let RawAstNode::Lambda { params, .. } = &**value {
                if params.len() != signature.param_types.len() {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Mismatched number of parameters for function '{}': expected {}, but found {}",
                            func_name,
                            signature.param_types.len(),
                            params.len()
                        ),
                        name.1,
                    )));
                }

                for (i, (param_info, _)) in params.iter().enumerate() {
                    let param_name = &param_info.0;
                    let param_type = signature.param_types[i].clone();
                    analyzer.variable_table.push(VariableEntry {
                        original_name: param_name.clone(),
                        unique_name: param_name.clone(), // トップレベル関数ではunique化不要
                        data_type: param_type.clone(),
                        scope_depth: analyzer.scope_depth,
                        is_mutable: false,
                    });
                    typed_params.push((param_name.clone(), param_type));
                }
            }

            // ラムダの本体を直接解析
            let typed_body = if let RawAstNode::Lambda { body, .. } = &**value {
                analyze_expr(analyzer, body)?
            } else {
                return Err(LangError::Compile(CompileError::new(
                    "Invalid let hoist syntax: expected a lambda expression",
                    *span,
                )));
            };

            let mut return_type_resolved = signature.return_type.clone();

            // main関数の型チェック
            if func_name == "main" && return_type_resolved != DataType::Unit {
                return Err(LangError::Compile(CompileError::new(
                    "The 'main' function must have a return type of '()'",
                    name.1,
                )));
            }

            if typed_body.data_type != return_type_resolved {
                // Unit型への暗黙の変換を許容
                if return_type_resolved == DataType::Unit {
                    // 何もしない。コード生成器がdropを追加する
                } else {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Mismatched return type: expected '{}', but function body returns '{}'",
                            return_type_resolved, typed_body.data_type
                        ),
                        typed_body.span,
                    )));
                }
            }

            analyzer.leave_scope();

            if func_name == "main" {
                return_type_resolved = DataType::Unit;
            }

            Ok(TypedAstNode::FnDef {
                name: func_name.clone(),
                params: typed_params,
                body: typed_body,
                return_type: return_type_resolved,
                span: *span,
            })
        }
        // トップレベルの他の式は暗黙のmain関数の一部として扱われる (lib.rsでラップ済み)
        _ => Err(LangError::Compile(CompileError::new(
            "Only function, struct, or enum definitions are allowed at the top level in library mode",
            node.span(),
        ))),
    }
}

/// RawASTノード（式）を受け取り、意味解析と型チェックを行ってTypedExprを返す
pub fn analyze_expr(analyzer: &mut Analyzer, node: &RawAstNode) -> Result<TypedExpr, LangError> {
    match node {
        RawAstNode::Expr(parts) => {
            // 式の解析では、現在のスコープで束縛される変数の知識は不要
            let hoisted_vars: BTreeSet<String> = BTreeSet::new();
            let mut parts_slice = &parts[..];
            let result = analyze_sexp_from_slice(analyzer, &mut parts_slice, &hoisted_vars)?;

            if let Some(RawExprPart::TypeAnnotation(type_name, type_span)) = parts_slice.first() {
                let expected_type = analyzer.string_to_type(type_name, *type_span)?;
                if result.data_type != expected_type {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Type annotation mismatch: expression has type '{}' but is annotated as '{}'",
                            result.data_type, expected_type
                        ),
                        result.span,
                    )));
                }
                parts_slice = &parts_slice[1..];
            }

            if !parts_slice.is_empty() {
                if let TypedExprKind::VariableRef { name, .. } = &result.kind {
                    return Err(LangError::Compile(CompileError::new(
                        format!("Variable '{}' cannot be called as a function", name),
                        result.span,
                    )));
                }
                return Err(LangError::Compile(CompileError::new(
                    "Unexpected tokens after expression",
                    parts_slice[0].span(),
                )));
            }
            Ok(result)
        }
        RawAstNode::Let { name, value, span } => {
            let typed_value = analyze_expr(analyzer, value)?;
            let count = analyzer.var_counters.entry(name.0.clone()).or_insert(0);
            let unique_name = format!("{}_{}", name.0, count);
            *count += 1;
            analyzer.variable_table.push(VariableEntry {
                original_name: name.0.clone(),
                unique_name: unique_name.clone(),
                data_type: typed_value.data_type.clone(),
                scope_depth: analyzer.scope_depth,
                is_mutable: false,
            });
            Ok(TypedExpr {
                kind: TypedExprKind::LetBinding {
                    name: (name.0.clone(), unique_name),
                    value: Box::new(typed_value),
                    is_mutable: false,
                    name_span: name.1,
                },
                data_type: DataType::Unit,
                span: *span,
            })
        }
        RawAstNode::LetMut { name, value, span } => {
            let typed_value = analyze_expr(analyzer, value)?;
            let count = analyzer.var_counters.entry(name.0.clone()).or_insert(0);
            let unique_name = format!("{}_{}", name.0, count);
            *count += 1;
            analyzer.variable_table.push(VariableEntry {
                original_name: name.0.clone(),
                unique_name: unique_name.clone(),
                data_type: typed_value.data_type.clone(),
                scope_depth: analyzer.scope_depth,
                is_mutable: true,
            });
            Ok(TypedExpr {
                kind: TypedExprKind::LetBinding {
                    name: (name.0.clone(), unique_name),
                    value: Box::new(typed_value),
                    is_mutable: true,
                    name_span: name.1,
                },
                data_type: DataType::Unit,
                span: *span,
            })
        }
        RawAstNode::LetHoist { name, value, span } => {
            let typed_value = analyze_expr(analyzer, value)?;
            let count = analyzer.var_counters.entry(name.0.clone()).or_insert(0);
            let unique_name = format!("{}_{}", name.0, count);
            *count += 1;
            analyzer.variable_table.push(VariableEntry {
                original_name: name.0.clone(),
                unique_name: unique_name.clone(),
                data_type: typed_value.data_type.clone(),
                scope_depth: analyzer.scope_depth,
                is_mutable: false, // let hoist is for functions, which are immutable
            });
            Ok(TypedExpr {
                kind: TypedExprKind::LetHoistBinding {
                    name: (name.0.clone(), unique_name),
                    value: Box::new(typed_value),
                    name_span: name.1,
                },
                data_type: DataType::Unit,
                span: *span,
            })
        }
        RawAstNode::Set { name, value, span } => {
            let typed_value = analyze_expr(analyzer, value)?;
            if let Some(entry) = analyzer.find_variable(&name.0) {
                if !entry.is_mutable {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Cannot assign to immutable variable '{}'",
                            entry.original_name
                        ),
                        name.1,
                    )));
                }
                if entry.data_type != typed_value.data_type {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Type mismatch in assignment to '{}': expected '{}' but found '{}'",
                            entry.original_name, entry.data_type, typed_value.data_type
                        ),
                        typed_value.span,
                    )));
                }
                Ok(TypedExpr {
                    kind: TypedExprKind::Assignment {
                        name: (entry.original_name.clone(), entry.unique_name.clone()),
                        value: Box::new(typed_value),
                    },
                    data_type: DataType::Unit,
                    span: *span,
                })
            } else {
                Err(LangError::Compile(CompileError::new(
                    format!("Undefined variable '{}'", name.0),
                    name.1,
                )))
            }
        }
        RawAstNode::Lambda {
            params: lambda_params,
            body: lambda_body,
            return_type: raw_return_type,
            span,
        } => {
            analyzer.enter_scope();
            let mut typed_params = Vec::new();
            let mut param_types = Vec::new();

            for ((param_name, _param_span), (type_str, type_span)) in lambda_params {
                let param_type = analyzer.string_to_type(type_str, *type_span)?;
                analyzer.variable_table.push(VariableEntry {
                    original_name: param_name.clone(),
                    unique_name: param_name.clone(), // ローカルなのでユニーク化は不要
                    data_type: param_type.clone(),
                    scope_depth: analyzer.scope_depth,
                    is_mutable: false,
                });
                typed_params.push((param_name.clone(), param_type.clone()));
                param_types.push(param_type);
            }

            let typed_body = analyze_expr(analyzer, lambda_body)?;
            analyzer.leave_scope();

            let explicit_return_type =
                analyzer.string_to_type(&raw_return_type.0, raw_return_type.1)?;

            if typed_body.data_type != explicit_return_type {
                // Unitへの暗黙のドロップを許容
                if explicit_return_type != DataType::Unit {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Mismatched return type in lambda: expected '{}', but body returns '{}'",
                            explicit_return_type, typed_body.data_type
                        ),
                        typed_body.span,
                    )));
                }
            }

            let function_type = DataType::Function {
                params: param_types,
                return_type: Box::new(explicit_return_type),
            };

            Ok(TypedExpr {
                kind: TypedExprKind::Lambda {
                    params: typed_params,
                    body: Box::new(typed_body),
                },
                data_type: function_type,
                span: *span,
            })
        }
        RawAstNode::Block { statements, span } => {
            analyzer.enter_scope();
            let hoisted_vars: BTreeSet<String> = BTreeSet::new(); // Block-level hoisting might be added later

            let mut typed_statements = Vec::new();
            for stmt in statements {
                let typed_stmt = analyze_statement_with_hoisting(analyzer, stmt, &hoisted_vars)?;
                typed_statements.push(typed_stmt);
            }
            analyzer.leave_scope();

            let last_type = typed_statements
                .last()
                .map_or(DataType::Unit, |n| n.data_type.clone());
            let block_span = typed_statements.last().map_or(*span, |n| n.span);
            Ok(TypedExpr {
                kind: TypedExprKind::Block {
                    statements: typed_statements,
                },
                data_type: last_type,
                span: block_span,
            })
        }
        RawAstNode::While {
            condition,
            body,
            span,
        } => {
            let typed_condition = analyze_expr(analyzer, condition)?;
            if typed_condition.data_type != DataType::Bool {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "While condition must be a boolean expression, but found type '{}'",
                        typed_condition.data_type
                    ),
                    typed_condition.span,
                )));
            }

            analyzer.enter_scope();
            let typed_body_result = analyze_expr(analyzer, body);
            analyzer.leave_scope();
            let typed_body = typed_body_result?;

            if typed_body.data_type != DataType::Unit {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "While body must evaluate to '()', but found type '{}'",
                        typed_body.data_type
                    ),
                    typed_body.span,
                )));
            }

            Ok(TypedExpr {
                kind: TypedExprKind::While {
                    condition: Box::new(typed_condition),
                    body: Box::new(typed_body),
                },
                data_type: DataType::Unit,
                span: *span,
            })
        }
        RawAstNode::Match { value, arms, span } => analyze_match_expr(analyzer, value, arms, *span),
        RawAstNode::StructDef { span, .. } | RawAstNode::EnumDef { span, .. } => {
            Err(LangError::Compile(CompileError::new(
                "Type definitions cannot appear inside expressions",
                *span,
            )))
        }
    }
}

/// Blockスコープ内の文を、そのスコープのhoisted変数情報と共に解析するヘルパー
fn analyze_statement_with_hoisting(
    analyzer: &mut Analyzer,
    node: &RawAstNode,
    hoisted_vars: &BTreeSet<String>,
) -> Result<TypedExpr, LangError> {
    match node {
        RawAstNode::Expr(parts) => {
            let mut parts_slice = &parts[..];
            let result = analyze_sexp_from_slice(analyzer, &mut parts_slice, hoisted_vars)?;
            // ... (analyze_exprからコピー)
            if let Some(RawExprPart::TypeAnnotation(type_name, type_span)) = parts_slice.first() {
                let expected_type = analyzer.string_to_type(type_name, *type_span)?;
                if result.data_type != expected_type {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Type annotation mismatch: expression has type '{}' but is annotated as '{}'",
                            result.data_type, expected_type
                        ),
                        result.span,
                    )));
                }
                parts_slice = &parts_slice[1..];
            }
            if !parts_slice.is_empty() {
                if let TypedExprKind::VariableRef { name, .. } = &result.kind {
                    return Err(LangError::Compile(CompileError::new(
                        format!("Variable '{}' cannot be called as a function", name),
                        result.span,
                    )));
                }
                return Err(LangError::Compile(CompileError::new(
                    "Unexpected tokens after expression",
                    parts_slice[0].span(),
                )));
            }
            Ok(result)
        }
        // 他のケースは `analyze_expr` をそのまま呼ぶ
        _ => analyze_expr(analyzer, node),
    }
}

fn describe_signature(signature: &FunctionSignature) -> String {
    let params = signature
        .param_types
        .iter()
        .map(|ty| ty.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "fn {}({}) -> {}",
        signature.name, params, signature.return_type
    )
}

fn format_argument_types(arg_types: &[DataType]) -> String {
    let joined = arg_types
        .iter()
        .map(|ty| ty.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("({})", joined)
}

fn select_overload(
    name: &str,
    candidates: &[FunctionSignature],
    arg_types: &[DataType],
    span: Span,
) -> Result<FunctionSignature, LangError> {
    let mut matches: Vec<&FunctionSignature> = Vec::new();
    let mut rejection_notes: Vec<(String, Span)> = Vec::new();

    for candidate in candidates {
        if candidate.param_types.len() != arg_types.len() {
            rejection_notes.push((
                format!(
                    "{} rejected: expected {} arguments, but {} provided",
                    describe_signature(candidate),
                    candidate.param_types.len(),
                    arg_types.len()
                ),
                candidate.definition_span,
            ));
            continue;
        }

        let mut mismatch_reason: Option<String> = None;
        for (idx, (expected, actual)) in candidate
            .param_types
            .iter()
            .zip(arg_types.iter())
            .enumerate()
        {
            if expected != actual {
                mismatch_reason = Some(format!(
                    "{} rejected: argument {} expected '{}', but found '{}'",
                    describe_signature(candidate),
                    idx + 1,
                    expected,
                    actual
                ));
                break;
            }
        }

        if let Some(reason) = mismatch_reason {
            rejection_notes.push((reason, candidate.definition_span));
            continue;
        }

        matches.push(candidate);
    }

    if matches.is_empty() {
        let mut err = CompileError::new(
            format!(
                "No overload of '{}' matches argument types {}",
                name,
                format_argument_types(arg_types)
            ),
            span,
        );
        if rejection_notes.is_empty() {
            for candidate in candidates {
                err = err.with_note(
                    format!("Candidate available: {}", describe_signature(candidate)),
                    candidate.definition_span,
                );
            }
        } else {
            for (note, note_span) in rejection_notes {
                err = err.with_note(note, note_span);
            }
        }
        return Err(LangError::Compile(err));
    }

    if matches.len() > 1 {
        let mut err = CompileError::new(
            format!(
                "Ambiguous call to '{}' with argument types {}",
                name,
                format_argument_types(arg_types)
            ),
            span,
        );
        for candidate in matches {
            err = err.with_note(
                format!("Possible candidate: {}", describe_signature(candidate)),
                candidate.definition_span,
            );
        }
        return Err(LangError::Compile(err));
    }

    Ok(matches[0].clone())
}

fn collect_sexp_call_arguments<'a>(
    analyzer: &mut Analyzer,
    parts: &mut &'a [RawExprPart],
    hoisted_vars: &BTreeSet<String>,
) -> Result<(Vec<TypedExpr>, &'a [RawExprPart]), LangError> {
    let mut cursor = *parts;
    let mut typed_args = Vec::new();
    while let Some(part) = cursor.first() {
        if matches!(part, RawExprPart::TypeAnnotation(_, _)) {
            break;
        }
        let typed_arg = analyze_sexp_from_slice(analyzer, &mut cursor, hoisted_vars)?;
        typed_args.push(typed_arg);
    }
    Ok((typed_args, cursor))
}

/// `f(...)` 形式のC-style関数呼び出しを解決する。
fn resolve_c_style_call<'a>(
    analyzer: &mut Analyzer,
    name: &str,
    span: crate::span::Span,
    arg_nodes: &[RawAstNode],
    parts: &mut &'a [RawExprPart],
) -> Result<TypedExpr, LangError> {
    let candidates = analyzer.function_table.get(name).cloned().ok_or_else(|| {
        LangError::Compile(CompileError::new(
            format!("Undefined function '{}'", name),
            span,
        ))
    })?;

    let mut typed_args = Vec::new();
    for arg_node in arg_nodes {
        let typed_arg = analyze_expr(analyzer, arg_node)?;
        typed_args.push(typed_arg);
    }
    let arg_types: Vec<DataType> = typed_args.iter().map(|arg| arg.data_type.clone()).collect();
    let signature = select_overload(name, &candidates, &arg_types, span)?;

    // 呼び出し元のスライスから、消費した識別子とCStyleArgsの2つ分を進める
    *parts = &parts[2..];

    Ok(TypedExpr {
        kind: TypedExprKind::FunctionCall {
            name: name.to_string(),
            args: typed_args,
        },
        data_type: signature.return_type.clone(),
        span,
    })
}

/// S式 `f ...` または変数参照 `f` を解決する。
fn resolve_sexp_call_or_variable<'a>(
    analyzer: &mut Analyzer,
    name: &str,
    span: crate::span::Span,
    parts: &mut &'a [RawExprPart],
    hoisted_vars: &BTreeSet<String>,
) -> Result<TypedExpr, LangError> {
    // 呼び出し元のスライスから、消費した識別子1つ分を進める
    *parts = &parts[1..];

    // 1. 変数として解決できるか試す
    let variable_entry = analyzer.find_variable(name);
    if hoisted_vars.contains(name) || variable_entry.is_some() {
        if let Some(entry) = variable_entry {
            return Ok(TypedExpr {
                kind: TypedExprKind::VariableRef {
                    name: entry.original_name.clone(),
                    unique_name: entry.unique_name.clone(),
                },
                data_type: entry.data_type.clone(),
                span,
            });
        } else {
            return Err(LangError::Compile(CompileError::new(
                format!(
                    "Cannot read local variable '{}' in its own initializer",
                    name
                ),
                span,
            )));
        }
    }
    // 2. 関数として解決できるか試す
    if let Some(candidates) = analyzer.function_table.get(name).cloned() {
        let (typed_args, remaining_parts) =
            collect_sexp_call_arguments(analyzer, parts, hoisted_vars)?;
        let arg_types: Vec<DataType> = typed_args.iter().map(|arg| arg.data_type.clone()).collect();
        let signature = select_overload(name, &candidates, &arg_types, span)?;
        *parts = remaining_parts;
        return Ok(TypedExpr {
            kind: TypedExprKind::FunctionCall {
                name: name.to_string(),
                args: typed_args,
            },
            data_type: signature.return_type.clone(),
            span,
        });
    }
    // 3. どちらでもない場合は未定義エラー
    Err(LangError::Compile(CompileError::new(
        format!("Undefined function or variable '{}'", name),
        span,
    )))
}

/// RawExprPartのスライスから一つの意味のある式(TypedExpr)を解析する。
/// この関数はアナライザーの心臓部であり、パーサーが作った未解決の構造を解釈する。
/// 解析が成功すると、消費した分だけ入力スライスを進め、結果のTypedExprを返す。
fn analyze_sexp_from_slice<'a>(
    analyzer: &mut Analyzer,
    parts: &mut &'a [RawExprPart],
    hoisted_vars: &BTreeSet<String>,
) -> Result<TypedExpr, LangError> {
    if parts.is_empty() {
        return Err(LangError::Compile(CompileError::new(
            "Unexpected end of expression",
            Default::default(),
        )));
    }

    let first_part = &parts[0];

    match first_part {
        RawExprPart::Token(Token::Identifier(name), span) => {
            // C-style `f(...)`呼び出しかどうかをチェック
            if let Some(RawExprPart::CStyleArgs(arg_nodes, _)) = parts.get(1) {
                resolve_c_style_call(analyzer, name, *span, arg_nodes, parts)
            } else {
                resolve_sexp_call_or_variable(analyzer, name, *span, parts, hoisted_vars)
            }
        }
        RawExprPart::Token(token, span) => {
            *parts = &parts[1..]; // トークンを消費
            match token {
                Token::IntLiteral(val) => Ok(TypedExpr {
                    kind: TypedExprKind::Literal(LiteralValue::I32(*val)),
                    data_type: DataType::I32,
                    span: *span,
                }),
                Token::FloatLiteral(val) => Ok(TypedExpr {
                    kind: TypedExprKind::Literal(LiteralValue::F64(*val)),
                    data_type: DataType::F64,
                    span: *span,
                }),
                Token::True => Ok(TypedExpr {
                    kind: TypedExprKind::Literal(LiteralValue::Bool(true)),
                    data_type: DataType::Bool,
                    span: *span,
                }),
                Token::False => Ok(TypedExpr {
                    kind: TypedExprKind::Literal(LiteralValue::Bool(false)),
                    data_type: DataType::Bool,
                    span: *span,
                }),
                Token::StringLiteral(s) => {
                    let header_offset = analyzer.ensure_string_is_statically_allocated(s);
                    Ok(TypedExpr {
                        kind: TypedExprKind::StringLiteral { header_offset },
                        data_type: DataType::String,
                        span: *span,
                    })
                }
                _ => Err(LangError::Compile(CompileError::new(
                    format!(
                        "This token cannot be the start of an expression: {:?}",
                        token
                    ),
                    *span,
                ))),
            }
        }
        RawExprPart::TupleLiteral(elements, span) => {
            *parts = &parts[1..];
            let mut typed_elements = Vec::new();
            let mut element_types = Vec::new();
            for element in elements {
                let typed_element = analyze_expr(analyzer, element)?;
                element_types.push(typed_element.data_type.clone());
                typed_elements.push(typed_element);
            }
            Ok(TypedExpr {
                kind: TypedExprKind::TupleLiteral {
                    elements: typed_elements,
                },
                data_type: DataType::Tuple(element_types),
                span: *span,
            })
        }
        RawExprPart::MathBlock(math_node, _) => {
            *parts = &parts[1..];
            analyze_math_node(analyzer, math_node)
        }
        RawExprPart::Group(inner_parts, _) => {
            *parts = &parts[1..];
            let mut inner_slice = &inner_parts[..];
            let result = analyze_sexp_from_slice(analyzer, &mut inner_slice, &BTreeSet::new())?; // Group内は別スコープ
            if !inner_slice.is_empty() {
                return Err(LangError::Compile(CompileError::new(
                    "Unexpected tokens after expression in group",
                    inner_slice[0].span(),
                )));
            }
            Ok(result)
        }
        RawExprPart::IfExpr {
            condition,
            then_branch,
            else_branch,
            span,
        } => {
            *parts = &parts[1..];
            let typed_cond = analyze_expr(analyzer, condition)?;
            if typed_cond.data_type != DataType::Bool {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "If condition must be a boolean expression, but found type '{}'",
                        typed_cond.data_type
                    ),
                    typed_cond.span,
                )));
            }
            let typed_then = analyze_expr(analyzer, then_branch)?;
            let typed_else = analyze_expr(analyzer, else_branch)?;
            if typed_then.data_type != typed_else.data_type {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "If branches must have the same type, but found '{}' and '{}'",
                        typed_then.data_type, typed_else.data_type
                    ),
                    *span,
                )));
            }
            let expr_type = typed_then.data_type.clone();
            Ok(TypedExpr {
                kind: TypedExprKind::IfExpr {
                    condition: Box::new(typed_cond),
                    then_branch: Box::new(typed_then),
                    else_branch: Box::new(typed_else),
                },
                data_type: expr_type,
                span: *span,
            })
        }
        RawExprPart::MatchExpr { value, arms, span } => {
            *parts = &parts[1..];
            analyze_match_expr(analyzer, value, arms, *span)
        }
        RawExprPart::Lambda {
            params: lambda_params,
            body: lambda_body,
            return_type: raw_return_type,
            span,
        } => {
            *parts = &parts[1..]; // ラムダ式を消費
            // RawAstNode::Lambda を構築し、analyze_expr に渡す
            let raw_lambda_node = RawAstNode::Lambda {
                params: lambda_params.clone(),
                body: lambda_body.clone(),
                return_type: raw_return_type.clone(),
                span: *span,
            };
            analyze_expr(analyzer, &raw_lambda_node)
        }
        RawExprPart::TypeAnnotation(_, span) => Err(LangError::Compile(CompileError::new(
            "Type annotation must follow an expression",
            *span,
        ))),
        RawExprPart::CStyleArgs(_, span) => Err(LangError::Compile(CompileError::new(
            "Argument list cannot be at the start of an expression",
            *span,
        ))),
    }
}

/// 数式ノードの解析ロジック
fn analyze_math_node(analyzer: &mut Analyzer, node: &MathAstNode) -> Result<TypedExpr, LangError> {
    match node {
        MathAstNode::Literal(literal, span) => match literal {
            MathLiteral::Int(val) => Ok(TypedExpr {
                kind: TypedExprKind::Literal(LiteralValue::I32(*val)),
                data_type: DataType::I32,
                span: *span,
            }),
            MathLiteral::Float(val) => Ok(TypedExpr {
                kind: TypedExprKind::Literal(LiteralValue::F64(*val)),
                data_type: DataType::F64,
                span: *span,
            }),
            MathLiteral::Bool(val) => Ok(TypedExpr {
                kind: TypedExprKind::Literal(LiteralValue::Bool(*val)),
                data_type: DataType::Bool,
                span: *span,
            }),
        },
        MathAstNode::Variable(name, span) => {
            if let Some(entry) = analyzer.find_variable(name) {
                Ok(TypedExpr {
                    kind: TypedExprKind::VariableRef {
                        name: entry.original_name.clone(),
                        unique_name: entry.unique_name.clone(),
                    },
                    data_type: entry.data_type.clone(),
                    span: *span,
                })
            } else {
                Err(LangError::Compile(CompileError::new(
                    format!("Undefined variable '{}' in math expression", name),
                    *span,
                )))
            }
        }
        MathAstNode::InfixOp {
            op,
            left,
            right,
            span,
        } => {
            let typed_left = analyze_math_node(analyzer, left)?;
            let typed_right = analyze_math_node(analyzer, right)?;
            if typed_left.data_type != typed_right.data_type {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Mismatched types for binary operator: `{}` and `{}`",
                        typed_left.data_type, typed_right.data_type
                    ),
                    *span,
                )));
            }
            let op_type = &typed_left.data_type;
            let (func_name, result_type) = match (op, op_type) {
                (Token::Plus, DataType::I32) => ("i32.add", DataType::I32),
                (Token::Minus, DataType::I32) => ("i32.sub", DataType::I32),
                (Token::Star, DataType::I32) => ("i32.mul", DataType::I32),
                (Token::Slash, DataType::I32) => ("i32.div_s", DataType::I32),
                (Token::Percent, DataType::I32) => ("i32.rem_s", DataType::I32),
                (Token::Plus, DataType::F64) => ("f64.add", DataType::F64),
                (Token::Minus, DataType::F64) => ("f64.sub", DataType::F64),
                (Token::Star, DataType::F64) => ("f64.mul", DataType::F64),
                (Token::Slash, DataType::F64) => ("f64.div", DataType::F64),
                (Token::Percent, DataType::F64) => ("f64.rem", DataType::F64),
                (Token::EqualsEquals, DataType::I32) => ("i32.eq", DataType::Bool),
                (Token::EqualsEquals, DataType::F64) => ("f64.eq", DataType::Bool),
                (Token::EqualsEquals, DataType::Bool) => ("i32.eq", DataType::Bool),
                (Token::BangEquals, DataType::I32) => ("i32.ne", DataType::Bool),
                (Token::BangEquals, DataType::F64) => ("f64.ne", DataType::Bool),
                (Token::BangEquals, DataType::Bool) => ("i32.ne", DataType::Bool),
                (Token::LessThan, DataType::I32) => ("i32.lt_s", DataType::Bool),
                (Token::LessThanEquals, DataType::I32) => ("i32.le_s", DataType::Bool),
                (Token::GreaterThan, DataType::I32) => ("i32.gt_s", DataType::Bool),
                (Token::GreaterThanEquals, DataType::I32) => ("i32.ge_s", DataType::Bool),
                (Token::LessThan, DataType::F64) => ("f64.lt", DataType::Bool),
                (Token::LessThanEquals, DataType::F64) => ("f64.le", DataType::Bool),
                (Token::GreaterThan, DataType::F64) => ("f64.gt", DataType::Bool),
                (Token::GreaterThanEquals, DataType::F64) => ("f64.ge", DataType::Bool),
                (Token::AndAnd, DataType::Bool) => ("i32.and", DataType::Bool),
                (Token::OrOr, DataType::Bool) => ("i32.or", DataType::Bool),
                _ => {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Operator `{:?}` is not supported for type `{}`",
                            op, op_type
                        ),
                        *span,
                    )));
                }
            };
            Ok(TypedExpr {
                kind: TypedExprKind::FunctionCall {
                    name: func_name.to_string(),
                    args: alloc::vec![typed_left, typed_right],
                },
                data_type: result_type,
                span: *span,
            })
        }
        MathAstNode::PrefixOp { op, expr, span } => {
            let typed_expr = analyze_math_node(analyzer, expr)?;
            match (op, &typed_expr.data_type) {
                (Token::Bang, DataType::Bool) => Ok(TypedExpr {
                    kind: TypedExprKind::FunctionCall {
                        name: "i32.eqz".to_string(),
                        args: alloc::vec![typed_expr],
                    },
                    data_type: DataType::Bool,
                    span: *span,
                }),
                (Token::Minus, DataType::I32) => {
                    let zero = TypedExpr {
                        kind: TypedExprKind::Literal(LiteralValue::I32(0)),
                        data_type: DataType::I32,
                        span: *span,
                    };
                    Ok(TypedExpr {
                        kind: TypedExprKind::FunctionCall {
                            name: "i32.sub".to_string(),
                            args: alloc::vec![zero, typed_expr],
                        },
                        data_type: DataType::I32,
                        span: *span,
                    })
                }
                (Token::Minus, DataType::F64) => Ok(TypedExpr {
                    kind: TypedExprKind::FunctionCall {
                        name: "f64.neg".to_string(),
                        args: alloc::vec![typed_expr],
                    },
                    data_type: DataType::F64,
                    span: *span,
                }),
                _ => Err(LangError::Compile(CompileError::new(
                    format!(
                        "Operator `{:?}` is not supported for type '{}'",
                        op, typed_expr.data_type
                    ),
                    *span,
                ))),
            }
        }
        MathAstNode::Call { name, args, span } => {
            let candidates = analyzer
                .function_table
                .get(&name.0)
                .cloned()
                .ok_or_else(|| {
                    LangError::Compile(CompileError::new(
                        format!("Undefined function '{}' in math expression", name.0),
                        name.1,
                    ))
                })?;
            let mut typed_args = Vec::new();
            for arg_node in args {
                let typed_arg = analyze_expr(analyzer, arg_node)?;
                typed_args.push(typed_arg);
            }
            let arg_types: Vec<DataType> =
                typed_args.iter().map(|arg| arg.data_type.clone()).collect();
            let signature = select_overload(&name.0, &candidates, &arg_types, *span)?;
            Ok(TypedExpr {
                kind: TypedExprKind::FunctionCall {
                    name: name.0.clone(),
                    args: typed_args,
                },
                data_type: signature.return_type.clone(),
                span: *span,
            })
        }
    }
}

fn analyze_match_expr(
    analyzer: &mut Analyzer,
    value: &RawAstNode,
    arms: &[RawMatchArm],
    span: Span,
) -> Result<TypedExpr, LangError> {
    if arms.is_empty() {
        return Err(LangError::Compile(CompileError::new(
            "Match expression must have at least one arm",
            span,
        )));
    }

    let typed_value = analyze_expr(analyzer, value)?;
    let mut typed_arms = Vec::new();
    let mut result_type: Option<DataType> = None;

    for arm in arms {
        analyzer.enter_scope();
        let mut bound_names = BTreeSet::new();
        let pattern_result = analyze_pattern(
            analyzer,
            &arm.pattern,
            &typed_value.data_type,
            &mut bound_names,
        );
        let (typed_pattern, bindings) = match pattern_result {
            Ok(res) => res,
            Err(err) => {
                analyzer.leave_scope();
                return Err(err);
            }
        };

        for entry in bindings {
            analyzer.variable_table.push(entry);
        }

        let body_result = analyze_expr(analyzer, &arm.body);
        analyzer.leave_scope();
        let typed_body = body_result?;

        if let Some(expected_type) = &result_type {
            if *expected_type != typed_body.data_type {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Match arms must return the same type, but found '{}' and '{}'",
                        expected_type, typed_body.data_type
                    ),
                    typed_body.span,
                )));
            }
        } else {
            result_type = Some(typed_body.data_type.clone());
        }

        typed_arms.push(TypedMatchArm {
            pattern: typed_pattern,
            body: typed_body,
        });
    }

    ensure_match_exhaustive(analyzer, &typed_value.data_type, &typed_arms, span)?;

    Ok(TypedExpr {
        kind: TypedExprKind::Match {
            value: Box::new(typed_value),
            arms: typed_arms,
        },
        data_type: result_type.unwrap_or(DataType::Unit),
        span,
    })
}

fn ensure_match_exhaustive(
    analyzer: &Analyzer,
    matched_type: &DataType,
    arms: &[TypedMatchArm],
    match_span: Span,
) -> Result<(), LangError> {
    if arms.iter().any(|arm| pattern_is_catch_all(&arm.pattern)) {
        return Ok(());
    }

    match matched_type {
        DataType::Enum(enum_name) => {
            let enum_info = analyzer.enum_table.get(enum_name).ok_or_else(|| {
                LangError::Compile(CompileError::new(
                    format!("Unknown enum '{}'", enum_name),
                    match_span,
                ))
            })?;

            let mut covered = BTreeSet::new();
            for arm in arms {
                if let TypedPattern::EnumVariant { variant_name, .. } = &arm.pattern {
                    covered.insert(variant_name.clone());
                }
            }

            if covered.len() == enum_info.variants.len() {
                Ok(())
            } else {
                let missing: Vec<_> = enum_info
                    .variants
                    .keys()
                    .filter(|name| !covered.contains(*name))
                    .cloned()
                    .collect();
                Err(LangError::Compile(CompileError::new(
                    format!(
                        "Non-exhaustive match on enum '{}': missing variants {}",
                        enum_name,
                        missing.join(", ")
                    ),
                    match_span,
                )))
            }
        }
        DataType::Bool => {
            let mut seen_true = false;
            let mut seen_false = false;
            for arm in arms {
                if let TypedPattern::Literal(LiteralValue::Bool(value)) = &arm.pattern {
                    if *value {
                        seen_true = true;
                    } else {
                        seen_false = true;
                    }
                }
            }

            if seen_true && seen_false {
                Ok(())
            } else {
                let mut missing = Vec::new();
                if !seen_true {
                    missing.push("true");
                }
                if !seen_false {
                    missing.push("false");
                }
                Err(LangError::Compile(CompileError::new(
                    format!(
                        "Non-exhaustive match on 'bool': add an arm for {} or use '_'",
                        missing.join(" and ")
                    ),
                    match_span,
                )))
            }
        }
        other_type => Err(LangError::Compile(CompileError::new(
            format!(
                "Match expression on type '{}' is not exhaustive; add a '_' arm to cover remaining cases",
                other_type
            ),
            match_span,
        ))),
    }
}

fn pattern_is_catch_all(pattern: &TypedPattern) -> bool {
    match pattern {
        TypedPattern::Wildcard => true,
        TypedPattern::Binding { .. } => true,
        TypedPattern::Tuple(elements) => elements.iter().all(pattern_is_catch_all),
        _ => false,
    }
}

fn analyze_pattern(
    analyzer: &mut Analyzer,
    pattern: &RawPattern,
    expected_type: &DataType,
    bound_names: &mut BTreeSet<String>,
) -> Result<(TypedPattern, Vec<VariableEntry>), LangError> {
    match pattern {
        RawPattern::Wildcard(_) => Ok((TypedPattern::Wildcard, Vec::new())),
        RawPattern::Identifier((name, span)) => {
            if bound_names.contains(name) {
                return Err(LangError::Compile(CompileError::new(
                    format!("Pattern variable '{}' is bound multiple times", name),
                    *span,
                )));
            }
            bound_names.insert(name.clone());
            let count = analyzer.var_counters.entry(name.clone()).or_insert(0);
            let unique_name = format!("{}_{}", name, count);
            *count += 1;
            let entry = VariableEntry {
                original_name: name.clone(),
                unique_name: unique_name.clone(),
                data_type: expected_type.clone(),
                scope_depth: analyzer.scope_depth,
                is_mutable: false,
            };
            Ok((
                TypedPattern::Binding {
                    name: (name.clone(), unique_name),
                    data_type: expected_type.clone(),
                },
                alloc::vec![entry],
            ))
        }
        RawPattern::Tuple(elements, span) => {
            if let DataType::Tuple(expected_elements) = expected_type {
                if expected_elements.len() != elements.len() {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Tuple pattern of length {} does not match tuple of length {}",
                            elements.len(),
                            expected_elements.len()
                        ),
                        *span,
                    )));
                }
                let mut typed_elements = Vec::new();
                let mut bindings = Vec::new();
                for (subpattern, sub_type) in elements.iter().zip(expected_elements.iter()) {
                    let (typed_sub, sub_bindings) =
                        analyze_pattern(analyzer, subpattern, sub_type, bound_names)?;
                    typed_elements.push(typed_sub);
                    bindings.extend(sub_bindings);
                }
                Ok((TypedPattern::Tuple(typed_elements), bindings))
            } else {
                Err(LangError::Compile(CompileError::new(
                    format!(
                        "Tuple pattern cannot match value of type '{}'",
                        expected_type
                    ),
                    *span,
                )))
            }
        }
        RawPattern::Path {
            segments,
            subpatterns,
            span,
        } => {
            if let DataType::Enum(enum_name) = expected_type {
                let variant_name =
                    segments
                        .last()
                        .map(|(name, _)| name.clone())
                        .ok_or_else(|| {
                            LangError::Compile(CompileError::new("Invalid enum pattern", *span))
                        })?;
                if segments.len() > 1 {
                    let first_segment = &segments[0].0;
                    if first_segment != enum_name {
                        return Err(LangError::Compile(CompileError::new(
                            format!(
                                "Expected enum '{}' but pattern refers to '{}'",
                                enum_name, first_segment
                            ),
                            segments[0].1,
                        )));
                    }
                }
                let variant_info = {
                    let enum_info = analyzer.enum_table.get(enum_name).ok_or_else(|| {
                        LangError::Compile(CompileError::new(
                            format!("Unknown enum '{}'", enum_name),
                            *span,
                        ))
                    })?;
                    enum_info
                        .variants
                        .get(&variant_name)
                        .cloned()
                        .ok_or_else(|| {
                            LangError::Compile(CompileError::new(
                                format!(
                                    "Enum '{}' has no variant named '{}'",
                                    enum_name, variant_name
                                ),
                                *span,
                            ))
                        })?
                };
                if variant_info.field_types.len() != subpatterns.len() {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Variant '{}' expects {} fields, but {} patterns were provided",
                            variant_name,
                            variant_info.field_types.len(),
                            subpatterns.len()
                        ),
                        *span,
                    )));
                }
                let mut typed_fields = Vec::new();
                let mut bindings = Vec::new();
                for (subpattern, field_type) in
                    subpatterns.iter().zip(variant_info.field_types.iter())
                {
                    let (typed_sub, sub_bindings) =
                        analyze_pattern(analyzer, subpattern, field_type, bound_names)?;
                    typed_fields.push(typed_sub);
                    bindings.extend(sub_bindings);
                }
                Ok((
                    TypedPattern::EnumVariant {
                        enum_name: enum_name.clone(),
                        variant_name,
                        fields: typed_fields,
                    },
                    bindings,
                ))
            } else {
                Err(LangError::Compile(CompileError::new(
                    format!(
                        "Only enum values can be matched with path patterns, found type '{}'",
                        expected_type
                    ),
                    *span,
                )))
            }
        }
        RawPattern::Literal(token, span) => {
            let literal = match token {
                Token::IntLiteral(val) => {
                    if *expected_type != DataType::I32 {
                        return Err(LangError::Compile(CompileError::new(
                            format!(
                                "Integer literal cannot match value of type '{}'",
                                expected_type
                            ),
                            *span,
                        )));
                    }
                    LiteralValue::I32(*val)
                }
                Token::FloatLiteral(val) => {
                    if *expected_type != DataType::F64 {
                        return Err(LangError::Compile(CompileError::new(
                            format!(
                                "Float literal cannot match value of type '{}'",
                                expected_type
                            ),
                            *span,
                        )));
                    }
                    LiteralValue::F64(*val)
                }
                Token::True => {
                    if *expected_type != DataType::Bool {
                        return Err(LangError::Compile(CompileError::new(
                            format!(
                                "Boolean literal cannot match value of type '{}'",
                                expected_type
                            ),
                            *span,
                        )));
                    }
                    LiteralValue::Bool(true)
                }
                Token::False => {
                    if *expected_type != DataType::Bool {
                        return Err(LangError::Compile(CompileError::new(
                            format!(
                                "Boolean literal cannot match value of type '{}'",
                                expected_type
                            ),
                            *span,
                        )));
                    }
                    LiteralValue::Bool(false)
                }
                Token::StringLiteral(_) => {
                    if *expected_type != DataType::String {
                        return Err(LangError::Compile(CompileError::new(
                            format!(
                                "String literal cannot match value of type '{}'",
                                expected_type
                            ),
                            *span,
                        )));
                    }
                    // String pattern matching is not supported due to runtime constraints
                    return Err(LangError::Compile(CompileError::new(
                        "Matching on string literals is not supported",
                        *span,
                    )));
                }
                _ => {
                    return Err(LangError::Compile(CompileError::new(
                        "Unsupported literal pattern",
                        *span,
                    )));
                }
            };
            Ok((TypedPattern::Literal(literal), Vec::new()))
        }
    }
}
