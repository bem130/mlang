//! RawASTを意味解析・型チェックし、TypedASTに変換するアナライザー。

use super::Compiler;
use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::token::Token;
use std::collections::HashSet;

/// トップレベルのRawAstNodeを型付きASTに変換するエントリーポイント
pub fn analyze_toplevel(compiler: &mut Compiler, node: &RawAstNode) -> Result<TypedAstNode, LangError> {
    match node {
        RawAstNode::FnDef { name, params, body, return_type, span } => {
            compiler.var_counters.clear();
            compiler.enter_scope();
            let mut typed_params = Vec::new();
            for ((param_name, _), (type_name, type_span)) in params {
                let param_type = compiler.string_to_type(type_name, *type_span)?;
                // パラメータも変数テーブルに追加
                compiler.variable_table.push((param_name.clone(), param_name.clone(), param_type.clone(), compiler.scope_depth));
                typed_params.push((param_name.clone(), param_type));
            }
            
            let typed_body = analyze_expr(compiler, body)?;
            
            let mut original_return_type = return_type.as_ref().map_or(Ok(DataType::Unit), |rt| compiler.string_to_type(&rt.0, rt.1))?;

            if name.0 == "main" && original_return_type != DataType::Unit {
                return Err(LangError::Compile(CompileError::new(
                    "The 'main' function must have a return type of '()' or no return type",
                    return_type.as_ref().map_or(*span, |rt| rt.1)
                )));
            }

            if typed_body.data_type != original_return_type {
                if original_return_type == DataType::Unit {
                    // 何もしない。コード生成器がdropを追加する
                } else {
                    return Err(LangError::Compile(CompileError::new(format!("Mismatched return type: expected '{}', but function body returns '{}'", original_return_type, typed_body.data_type), typed_body.span)));
                }
            }
            
            compiler.leave_scope();

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
        _ => Err(LangError::Compile(CompileError::new("Only function definitions are allowed at the top level", node.span()))),
    }
}

/// RawASTノード（式）を受け取り、意味解析と型チェックを行ってTypedExprを返す
pub fn analyze_expr(compiler: &mut Compiler, node: &RawAstNode) -> Result<TypedExpr, LangError> {
    match node {
        RawAstNode::Expr(parts) => {
            // 式の解析では、現在のスコープで束縛される変数の知識は不要
            let hoisted_vars: HashSet<String> = HashSet::new();
            let mut parts_slice = &parts[..];
            let result = analyze_sexp_from_slice(compiler, &mut parts_slice, &hoisted_vars)?;

            if let Some(RawExprPart::TypeAnnotation(type_name, type_span)) = parts_slice.first() {
                let expected_type = compiler.string_to_type(type_name, *type_span)?;
                if result.data_type != expected_type {
                    return Err(LangError::Compile(CompileError::new(
                        format!("Type annotation mismatch: expression has type '{}' but is annotated as '{}'", result.data_type, expected_type),
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
                return Err(LangError::Compile(CompileError::new("Unexpected tokens after expression", parts_slice[0].span())));
            }
            Ok(result)
        }
        RawAstNode::LetDef { name, value, span } => {
            let typed_value = analyze_expr(compiler, value)?;

            let count = compiler.var_counters.entry(name.0.clone()).or_insert(0);
            let unique_name = format!("{}_{}", name.0, count);
            *count += 1;
            
            compiler.variable_table.push((name.0.clone(), unique_name.clone(), typed_value.data_type.clone(), compiler.scope_depth));
            Ok(TypedExpr {
                kind: TypedExprKind::LetBinding { name: (name.0.clone(), unique_name), value: Box::new(typed_value) },
                data_type: DataType::Unit, span: *span,
            })
        }
        RawAstNode::Block { statements, span } => {
            compiler.enter_scope();
            // --- Hoisting Pass ---
            let hoisted_vars: HashSet<String> = statements.iter().filter_map(|stmt| {
                if let RawAstNode::LetDef { name, .. } = stmt { Some(name.0.clone()) } else { None }
            }).collect();

            let mut typed_statements = Vec::new();
            for stmt in statements {
                let typed_stmt = analyze_statement_with_hoisting(compiler, stmt, &hoisted_vars)?;
                typed_statements.push(typed_stmt);
            }
            compiler.leave_scope();

            let last_type = typed_statements.last().map_or(DataType::Unit, |n| n.data_type.clone());
            let block_span = typed_statements.last().map_or(*span, |n| n.span);
            Ok(TypedExpr {
                kind: TypedExprKind::Block { statements: typed_statements },
                data_type: last_type, span: block_span,
            })
        }
        RawAstNode::PrintStmt { value, span } => {
            let typed_value = analyze_expr(compiler, value)?;
            if typed_value.data_type != DataType::String {
                return Err(LangError::Compile(CompileError::new(format!("'print' expects a string, but found type '{}'", typed_value.data_type), typed_value.span)));
            }
            Ok(TypedExpr {
                kind: TypedExprKind::PrintStmt { value: Box::new(typed_value) },
                data_type: DataType::Unit, span: *span,
            })
        }
        RawAstNode::PrintlnStmt { value, span } => {
            let typed_value = analyze_expr(compiler, value)?;
            if typed_value.data_type != DataType::String {
                return Err(LangError::Compile(CompileError::new(format!("'println' expects a string, but found type '{}'", typed_value.data_type), typed_value.span)));
            }
            Ok(TypedExpr {
                kind: TypedExprKind::PrintlnStmt { value: Box::new(typed_value) },
                data_type: DataType::Unit, span: *span,
            })
        }
        RawAstNode::FnDef { .. } => unreachable!("Function definitions cannot be nested inside expressions."),
    }
}

/// Blockスコープ内の文を、そのスコープのhoisted変数情報と共に解析するヘルパー
fn analyze_statement_with_hoisting(compiler: &mut Compiler, node: &RawAstNode, hoisted_vars: &HashSet<String>) -> Result<TypedExpr, LangError> {
     match node {
        RawAstNode::Expr(parts) => {
            let mut parts_slice = &parts[..];
            let result = analyze_sexp_from_slice(compiler, &mut parts_slice, hoisted_vars)?;
            // ... (analyze_exprからコピー)
            if let Some(RawExprPart::TypeAnnotation(type_name, type_span)) = parts_slice.first() {
                let expected_type = compiler.string_to_type(type_name, *type_span)?;
                if result.data_type != expected_type {
                    return Err(LangError::Compile(CompileError::new(format!("Type annotation mismatch: expression has type '{}' but is annotated as '{}'", result.data_type, expected_type), result.span)));
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
                return Err(LangError::Compile(CompileError::new("Unexpected tokens after expression", parts_slice[0].span())));
            }
            Ok(result)
        }
        // 他のケースは `analyze_expr` をそのまま呼ぶ
        _ => analyze_expr(compiler, node),
     }
}

/// RawExprPartのスライスから一つのS式を解析し、消費した分スライスを進める
fn analyze_sexp_from_slice<'a>(compiler: &mut Compiler, parts: &mut &'a [RawExprPart], hoisted_vars: &HashSet<String>) -> Result<TypedExpr, LangError> {
    if parts.is_empty() { return Err(LangError::Compile(CompileError::new("Unexpected end of expression", Default::default()))); }
    let first_part = &parts[0];
    // S式の場合はこの時点でスライスを進めない。C-style呼び出しかどうかで分岐してから進める。

    match first_part {
        RawExprPart::Token(Token::Identifier(name), span) => {
            // --- 名前解決ロジック ---
            // C-style `add(...)`呼び出しかどうかをチェック
            if let Some(RawExprPart::CStyleArgs(arg_nodes, _)) = parts.get(1) {
                *parts = &parts[2..]; // `add`と`CStyleArgs`を消費
                
                let signature = compiler.function_table.get(name).cloned().ok_or_else(|| LangError::Compile(CompileError::new(format!("Undefined function '{}'", name), *span)))?;
                if arg_nodes.len() != signature.param_types.len() {
                    return Err(LangError::Compile(
                        CompileError::new(
                            format!("Function '{}' expects {} arguments, but {} were provided", name, signature.param_types.len(), arg_nodes.len()),
                            *span
                        ).with_note(
                            format!("'{}' is defined here", name),
                            signature.definition_span
                        )
                    ));
                }
                
                let mut typed_args = Vec::new();
                for (i, arg_node) in arg_nodes.iter().enumerate() {
                    let typed_arg = analyze_expr(compiler, arg_node)?;
                    if typed_arg.data_type != signature.param_types[i] { 
                        return Err(LangError::Compile(CompileError::new(format!("Mismatched type for argument {} of function '{}': expected '{}', but found '{}'", i + 1, name, signature.param_types[i], typed_arg.data_type), typed_arg.span))); 
                    }
                    typed_args.push(typed_arg);
                }
                return Ok(TypedExpr { kind: TypedExprKind::FunctionCall { name: name.clone(), args: typed_args }, data_type: signature.return_type.clone(), span: *span });
            }

            // --- S式スタイルの解析 ---
            *parts = &parts[1..]; // `add`などの関数名を消費
            // 1. 変数として解決できるか試す
            if hoisted_vars.contains(name) || compiler.find_variable(name).is_some() {
                if let Some((original_name, unique_name, var_type, _)) = compiler.find_variable(name) {
                    return Ok(TypedExpr { kind: TypedExprKind::VariableRef{ name: original_name.clone(), unique_name: unique_name.clone() }, data_type: var_type.clone(), span: *span });
                } else {
                    return Err(LangError::Compile(CompileError::new(format!("Cannot read local variable '{}' in its own initializer", name), *span)));
                }
            }
            // 2. 関数として解決できるか試す
            if let Some(signature) = compiler.function_table.get(name).cloned() {
                let mut typed_args = Vec::new();
                for i in 0..signature.param_types.len() {
                     if parts.is_empty() {
                         return Err(LangError::Compile(
                            CompileError::new(
                                format!("Function '{}' expects {} arguments, but only {} were provided", name, signature.param_types.len(), i),
                                *span
                            ).with_note(
                                format!("'{}' is defined here", name),
                                signature.definition_span
                            )
                         ));
                     }
                    let typed_arg = analyze_sexp_from_slice(compiler, parts, hoisted_vars)?;
                    if typed_arg.data_type != signature.param_types[i] { return Err(LangError::Compile(CompileError::new(format!("Mismatched type for argument {} of function '{}': expected '{}', but found '{}'", i + 1, name, signature.param_types[i], typed_arg.data_type), typed_arg.span))); }
                    typed_args.push(typed_arg);
                }
                return Ok(TypedExpr { kind: TypedExprKind::FunctionCall { name: name.clone(), args: typed_args }, data_type: signature.return_type.clone(), span: *span });
            }
            // 3. どちらでもない場合は未定義エラー
            Err(LangError::Compile(CompileError::new(format!("Undefined function or variable '{}'", name), *span)))
        }
        RawExprPart::Token(token, span) => {
            *parts = &parts[1..]; // トークンを消費
            match token {
                Token::IntLiteral(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::I32(*val)), data_type: DataType::I32, span: *span }),
                Token::FloatLiteral(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::F64(*val)), data_type: DataType::F64, span: *span }),
                Token::True => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::Bool(true)), data_type: DataType::Bool, span: *span }),
                Token::False => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::Bool(false)), data_type: DataType::Bool, span: *span }),
                Token::StringLiteral(s) => {
                    let header_offset = compiler.ensure_string_is_statically_allocated(s);
                    Ok(TypedExpr { kind: TypedExprKind::StringLiteral { header_offset }, data_type: DataType::String, span: *span })
                }
                _ => Err(LangError::Compile(CompileError::new(format!("This token cannot be the start of an expression: {:?}", token), *span))),
            }
        },
        RawExprPart::MathBlock(math_node, _) => {
            *parts = &parts[1..];
            analyze_math_node(compiler, math_node)
        },
        RawExprPart::Group(inner_parts, _) => {
            *parts = &parts[1..];
            let mut inner_slice = &inner_parts[..];
            let result = analyze_sexp_from_slice(compiler, &mut inner_slice, &HashSet::new())?; // Group内は別スコープ
            if !inner_slice.is_empty() {
                return Err(LangError::Compile(CompileError::new("Unexpected tokens after expression in group", inner_slice[0].span())));
            }
            Ok(result)
        }
        RawExprPart::IfExpr { condition, then_branch, else_branch, span } => {
            *parts = &parts[1..];
            let typed_cond = analyze_expr(compiler, condition)?;
            if typed_cond.data_type != DataType::Bool {
                return Err(LangError::Compile(CompileError::new(format!("If condition must be a boolean expression, but found type '{}'", typed_cond.data_type), typed_cond.span)));
            }
            let typed_then = analyze_expr(compiler, then_branch)?;
            let typed_else = analyze_expr(compiler, else_branch)?;
            if typed_then.data_type != typed_else.data_type {
                return Err(LangError::Compile(CompileError::new(format!("If branches must have the same type, but found '{}' and '{}'", typed_then.data_type, typed_else.data_type), *span)));
            }
            let expr_type = typed_then.data_type.clone();
            Ok(TypedExpr {
                kind: TypedExprKind::IfExpr { condition: Box::new(typed_cond), then_branch: Box::new(typed_then), else_branch: Box::new(typed_else) },
                data_type: expr_type, span: *span,
            })
        }
        RawExprPart::TypeAnnotation(_, span) => Err(LangError::Compile(CompileError::new("Type annotation must follow an expression", *span))),
        RawExprPart::CStyleArgs(_, span) => Err(LangError::Compile(CompileError::new("Argument list cannot be at the start of an expression", *span))),
    }
}

/// 数式ノードの解析ロジック
fn analyze_math_node(compiler: &mut Compiler, node: &MathAstNode) -> Result<TypedExpr, LangError> {
    match node {
        MathAstNode::Literal(literal, span) => match literal {
            MathLiteral::Int(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::I32(*val)), data_type: DataType::I32, span: *span }),
            MathLiteral::Float(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::F64(*val)), data_type: DataType::F64, span: *span }),
        },
        MathAstNode::Variable(name, span) => {
             if let Some((original_name, unique_name, var_type, _)) = compiler.find_variable(name) {
                Ok(TypedExpr { kind: TypedExprKind::VariableRef { name: original_name.clone(), unique_name: unique_name.clone() }, data_type: var_type.clone(), span: *span })
            } else {
                Err(LangError::Compile(CompileError::new(format!("Undefined variable '{}' in math expression", name), *span)))
            }
        }
        MathAstNode::InfixOp { op, left, right, span } => {
            let typed_left = analyze_math_node(compiler, left)?;
            let typed_right = analyze_math_node(compiler, right)?;
            if typed_left.data_type != typed_right.data_type { return Err(LangError::Compile(CompileError::new(format!("Mismatched types for binary operator: `{}` and `{}`", typed_left.data_type, typed_right.data_type), *span))); }
            let op_type = &typed_left.data_type;
            let (func_name, result_type) = match (op, op_type) {
                (Token::Plus, DataType::I32) => ("i32.add", DataType::I32), (Token::Minus, DataType::I32) => ("i32.sub", DataType::I32),
                (Token::Star, DataType::I32) => ("i32.mul", DataType::I32), (Token::Slash, DataType::I32) => ("i32.div_s", DataType::I32),
                (Token::Plus, DataType::F64) => ("f64.add", DataType::F64), (Token::Minus, DataType::F64) => ("f64.sub", DataType::F64),
                (Token::Star, DataType::F64) => ("f64.mul", DataType::F64), (Token::Slash, DataType::F64) => ("f64.div", DataType::F64),
                (Token::EqualsEquals, DataType::I32) => ("i32.eq", DataType::Bool), (Token::BangEquals, DataType::I32) => ("i32.ne", DataType::Bool),
                (Token::LessThan, DataType::I32) => ("i32.lt_s", DataType::Bool), (Token::LessThanEquals, DataType::I32) => ("i32.le_s", DataType::Bool),
                (Token::GreaterThan, DataType::I32) => ("i32.gt_s", DataType::Bool), (Token::GreaterThanEquals, DataType::I32) => ("i32.ge_s", DataType::Bool),
                _ => return Err(LangError::Compile(CompileError::new(format!("Operator `{:?}` is not supported for type `{}`", op, op_type), *span))),
            };
            Ok(TypedExpr { kind: TypedExprKind::FunctionCall { name: func_name.to_string(), args: vec![typed_left, typed_right] }, data_type: result_type, span: *span })
        }
        MathAstNode::Call { name, args, span } => {
            let signature = compiler.function_table.get(&name.0).cloned().ok_or_else(|| LangError::Compile(CompileError::new(format!("Undefined function '{}' in math expression", name.0),name.1)))?;
            if args.len() != signature.param_types.len() {
                let err = CompileError::new(
                    format!("Function '{}' expects {} arguments, but {} were provided", name.0, signature.param_types.len(), args.len()),
                    *span
                ).with_note(
                    format!("'{}' is defined here", name.0),
                    signature.definition_span
                );
                return Err(err.into());
            }
            let mut typed_args = Vec::new();
            for (i, arg_node) in args.iter().enumerate() {
                // 引数がRawAstNodeになったので、メインの`analyze_expr`で解析する
                let typed_arg = analyze_expr(compiler, arg_node)?;
                let expected_type = &signature.param_types[i];
                if typed_arg.data_type != *expected_type { return Err(LangError::Compile(CompileError::new(format!("Mismatched type for argument {} of function '{}': expected '{}', but found '{}'", i + 1, name.0, expected_type, typed_arg.data_type), typed_arg.span))); }
                typed_args.push(typed_arg);
            }
            Ok(TypedExpr { kind: TypedExprKind::FunctionCall { name: name.0.clone(), args: typed_args }, data_type: signature.return_type.clone(), span: *span })
        }
    }
}