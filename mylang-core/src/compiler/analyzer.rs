//! RawASTを意味解析・型チェックし、TypedASTに変換するアナライザー。

extern crate alloc;
use super::{Analyzer, VariableEntry};
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
        RawAstNode::FnDef {
            name,
            params,
            body,
            return_type,
            span,
        } => {
            analyzer.var_counters.clear();
            analyzer.enter_scope();
            let mut typed_params = Vec::new();
            for ((param_name, _), (type_name, type_span)) in params {
                let param_type = analyzer.string_to_type(type_name, *type_span)?;
                // パラメータも変数テーブルに追加
                analyzer.variable_table.push(VariableEntry {
                    original_name: param_name.clone(),
                    unique_name: param_name.clone(),
                    data_type: param_type.clone(),
                    scope_depth: analyzer.scope_depth,
                    is_mutable: false,
                });
                typed_params.push((param_name.clone(), param_type));
            }

            let typed_body = analyze_expr(analyzer, body)?;

            let mut original_return_type =
                return_type.as_ref().map_or(Ok(DataType::Unit), |rt| {
                    analyzer.string_to_type(&rt.0, rt.1)
                })?;

            if name.0 == "main" && original_return_type != DataType::Unit {
                return Err(LangError::Compile(CompileError::new(
                    "The 'main' function must have a return type of '()' or no return type",
                    return_type.as_ref().map_or(*span, |rt| rt.1),
                )));
            }

            if typed_body.data_type != original_return_type {
                if original_return_type == DataType::Unit {
                    // 何もしない。コード生成器がdropを追加する
                } else {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Mismatched return type: expected '{}', but function body returns '{}'",
                            original_return_type, typed_body.data_type
                        ),
                        typed_body.span,
                    )));
                }
            }

            analyzer.leave_scope();

            if name.0 == "main" {
                original_return_type = DataType::Unit;
            }

            Ok(TypedAstNode::FnDef {
                name: name.0.clone(),
                params: typed_params,
                body: typed_body,
                return_type: original_return_type,
                span: *span,
            })
        }
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
        _ => Err(LangError::Compile(CompileError::new(
            "Only function definitions are allowed at the top level",
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
        RawAstNode::LetDef {
            name,
            value,
            span,
            is_mutable,
        } => {
            let typed_value = analyze_expr(analyzer, value)?;

            let count = analyzer.var_counters.entry(name.0.clone()).or_insert(0);
            let unique_name = format!("{}_{}", name.0, count);
            *count += 1;

            analyzer.variable_table.push(VariableEntry {
                original_name: name.0.clone(),
                unique_name: unique_name.clone(),
                data_type: typed_value.data_type.clone(),
                scope_depth: analyzer.scope_depth,
                is_mutable: *is_mutable,
            });
            Ok(TypedExpr {
                kind: TypedExprKind::LetBinding {
                    name: (name.0.clone(), unique_name),
                    value: Box::new(typed_value),
                    is_mutable: *is_mutable,
                },
                data_type: DataType::Unit,
                span: *span,
            })
        }
        RawAstNode::Assignment { name, value, span } => {
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
        RawAstNode::Block { statements, span } => {
            analyzer.enter_scope();
            // --- Hoisting Pass ---
            let hoisted_vars: BTreeSet<String> = statements
                .iter()
                .filter_map(|stmt| {
                    if let RawAstNode::LetDef { name, .. } = stmt {
                        Some(name.0.clone())
                    } else {
                        None
                    }
                })
                .collect();

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
        RawAstNode::FnDef { .. } => {
            unreachable!("Function definitions cannot be nested inside expressions.")
        }
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

/// `f(...)` 形式のC-style関数呼び出しを解決する。
fn resolve_c_style_call<'a>(
    analyzer: &mut Analyzer,
    name: &str,
    span: crate::span::Span,
    arg_nodes: &[RawAstNode],
    parts: &mut &'a [RawExprPart],
) -> Result<TypedExpr, LangError> {
    let signature = analyzer.function_table.get(name).cloned().ok_or_else(|| {
        LangError::Compile(CompileError::new(
            format!("Undefined function '{}'", name),
            span,
        ))
    })?;
    if arg_nodes.len() != signature.param_types.len() {
        return Err(LangError::Compile(
            CompileError::new(
                format!(
                    "Function '{}' expects {} arguments, but {} were provided",
                    name,
                    signature.param_types.len(),
                    arg_nodes.len()
                ),
                span,
            )
            .with_note(
                format!("'{}' is defined here", name),
                signature.definition_span,
            ),
        ));
    }

    let mut typed_args = Vec::new();
    for (i, arg_node) in arg_nodes.iter().enumerate() {
        let typed_arg = analyze_expr(analyzer, arg_node)?;
        if typed_arg.data_type != signature.param_types[i] {
            return Err(LangError::Compile(CompileError::new(
                format!(
                    "Mismatched type for argument {} of function '{}': expected '{}', but found '{}'",
                    i + 1,
                    name,
                    signature.param_types[i],
                    typed_arg.data_type
                ),
                typed_arg.span,
            )));
        }
        typed_args.push(typed_arg);
    }

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
    if let Some(signature) = analyzer.function_table.get(name).cloned() {
        let mut typed_args = Vec::new();
        for i in 0..signature.param_types.len() {
            if parts.is_empty() {
                return Err(LangError::Compile(
                    CompileError::new(
                        format!(
                            "Function '{}' expects {} arguments, but only {} were provided",
                            name,
                            signature.param_types.len(),
                            i
                        ),
                        span,
                    )
                    .with_note(
                        format!("'{}' is defined here", name),
                        signature.definition_span,
                    ),
                ));
            }
            let typed_arg = analyze_sexp_from_slice(analyzer, parts, hoisted_vars)?;
            if typed_arg.data_type != signature.param_types[i] {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Mismatched type for argument {} of function '{}': expected '{}', but found '{}'",
                        i + 1,
                        name,
                        signature.param_types[i],
                        typed_arg.data_type
                    ),
                    typed_arg.span,
                )));
            }
            typed_args.push(typed_arg);
        }
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
                (Token::Plus, DataType::F64) => ("f64.add", DataType::F64),
                (Token::Minus, DataType::F64) => ("f64.sub", DataType::F64),
                (Token::Star, DataType::F64) => ("f64.mul", DataType::F64),
                (Token::Slash, DataType::F64) => ("f64.div", DataType::F64),
                (Token::EqualsEquals, DataType::I32) => ("i32.eq", DataType::Bool),
                (Token::BangEquals, DataType::I32) => ("i32.ne", DataType::Bool),
                (Token::LessThan, DataType::I32) => ("i32.lt_s", DataType::Bool),
                (Token::LessThanEquals, DataType::I32) => ("i32.le_s", DataType::Bool),
                (Token::GreaterThan, DataType::I32) => ("i32.gt_s", DataType::Bool),
                (Token::GreaterThanEquals, DataType::I32) => ("i32.ge_s", DataType::Bool),
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
        MathAstNode::Call { name, args, span } => {
            let signature = analyzer
                .function_table
                .get(&name.0)
                .cloned()
                .ok_or_else(|| {
                    LangError::Compile(CompileError::new(
                        format!("Undefined function '{}' in math expression", name.0),
                        name.1,
                    ))
                })?;
            if args.len() != signature.param_types.len() {
                let err = CompileError::new(
                    format!(
                        "Function '{}' expects {} arguments, but {} were provided",
                        name.0,
                        signature.param_types.len(),
                        args.len()
                    ),
                    *span,
                )
                .with_note(
                    format!("'{}' is defined here", name.0),
                    signature.definition_span,
                );
                return Err(err.into());
            }
            let mut typed_args = Vec::new();
            for (i, arg_node) in args.iter().enumerate() {
                // 引数がRawAstNodeになったので、メインの`analyze_expr`で解析する
                let typed_arg = analyze_expr(analyzer, arg_node)?;
                let expected_type = &signature.param_types[i];
                if typed_arg.data_type != *expected_type {
                    return Err(LangError::Compile(CompileError::new(
                        format!(
                            "Mismatched type for argument {} of function '{}': expected '{}', but found '{}'",
                            i + 1,
                            name.0,
                            expected_type,
                            typed_arg.data_type
                        ),
                        typed_arg.span,
                    )));
                }
                typed_args.push(typed_arg);
            }
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
