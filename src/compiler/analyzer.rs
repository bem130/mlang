//! RawASTを意味解析・型チェックし、TypedASTに変換するアナライザー。

use super::Compiler;
use crate::ast::*;
use crate::error::{CompileError, LangError};

/// トップレベルのRawAstNodeを型付きASTに変換するエントリーポイント
pub fn analyze_toplevel(compiler: &mut Compiler, node: &RawAstNode) -> Result<TypedAstNode, LangError> {
    match node {
        RawAstNode::FnDef { name, params, body, return_type, span } => {
            compiler.enter_scope();
            let mut typed_params = Vec::new();
            for ((param_name, _), (type_name, type_span)) in params {
                let param_type = compiler.string_to_type(type_name, *type_span)?;
                compiler.variable_table.push((param_name.clone(), param_type.clone(), compiler.scope_depth));
                typed_params.push((param_name.clone(), param_type));
            }
            
            let typed_body = analyze_expr(compiler, body)?;
            
            let mut original_return_type = return_type.as_ref().map_or(Ok(DataType::Unit), |rt| compiler.string_to_type(&rt.0, rt.1))?;

            // main関数の戻り値は()でなければならないという制約を追加
            if name.0 == "main" && original_return_type != DataType::Unit {
                return Err(LangError::Compile(CompileError::new(
                    "The 'main' function must have a return type of '()' or no return type",
                    return_type.as_ref().map_or(*span, |rt| rt.1)
                )));
            }

            if typed_body.data_type != original_return_type {
                // Unitを返す関数本体が値を返す式で終わっている場合、暗黙の戻り値を許可する
                if original_return_type == DataType::Unit {
                    // 何もしない。コード生成器がdropを追加する
                } else {
                    return Err(LangError::Compile(CompileError::new(format!("Mismatched return type: expected '{}', but function body returns '{}'", original_return_type, typed_body.data_type), typed_body.span)));
                }
            }
            
            compiler.leave_scope();

            // main関数のシグネチャの戻り値型を強制的にUnitにする
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
fn analyze_expr(compiler: &mut Compiler, node: &RawAstNode) -> Result<TypedExpr, LangError> {
    match node {
        RawAstNode::Expr(parts) => {
            let mut parts_slice = &parts[..];
            let result = analyze_sexp_from_slice(compiler, &mut parts_slice)?;

            // 式の後に続くオプションの型注釈をチェックする
            if !parts_slice.is_empty() {
                if let RawExprPart::TypeAnnotation(type_name, type_span) = &parts_slice[0] {
                    let expected_type = compiler.string_to_type(type_name, *type_span)?;
                    if result.data_type != expected_type {
                        return Err(LangError::Compile(CompileError::new(
                            format!("Type annotation mismatch: expression has type '{}' but is annotated as '{}'", result.data_type, expected_type),
                            result.span,
                        )));
                    }
                    parts_slice = &parts_slice[1..]; // 型注釈を消費
                }
            }

            // 式の後に余計なトークンがないか確認する
            if !parts_slice.is_empty() {
                return Err(LangError::Compile(CompileError::new("Unexpected tokens after expression", parts_slice[0].span())));
            }
            Ok(result)
        }
        RawAstNode::LetDef { name, value, span } => {
            let typed_value = analyze_expr(compiler, value)?;
            compiler.variable_table.push((name.0.clone(), typed_value.data_type.clone(), compiler.scope_depth));
            Ok(TypedExpr {
                kind: TypedExprKind::LetBinding { name: name.0.clone(), value: Box::new(typed_value) },
                data_type: DataType::Unit, span: *span,
            })
        }
        RawAstNode::Block { statements, span } => {
            compiler.enter_scope();
            let mut typed_statements = Vec::new();
            for stmt in statements {
                typed_statements.push(analyze_expr(compiler, stmt)?);
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
        RawAstNode::FnDef { .. } => unreachable!("Function definitions cannot be nested inside expressions."),
    }
}

/// RawExprPartのスライスから一つのS式を解析し、消費した分スライスを進める
fn analyze_sexp_from_slice<'a>(compiler: &mut Compiler, parts: &mut &'a [RawExprPart]) -> Result<TypedExpr, LangError> {
    if parts.is_empty() { return Err(LangError::Compile(CompileError::new("Unexpected end of expression", Default::default()))); }
    let first_part = &parts[0];
    *parts = &parts[1..];

    match first_part {
        RawExprPart::Token(token, span) => match token {
            crate::token::Token::Identifier(name) => {
                if let Some(signature) = compiler.function_table.get(name).cloned() {
                    let mut typed_args = Vec::new();
                    for i in 0..signature.param_types.len() {
                        let typed_arg = analyze_sexp_from_slice(compiler, parts)?;
                        let expected_type = &signature.param_types[i];
                        if typed_arg.data_type != *expected_type {
                            return Err(LangError::Compile(CompileError::new(format!("Mismatched type for argument {} of function '{}': expected '{}', but found '{}'", i + 1, name, expected_type, typed_arg.data_type), typed_arg.span)));
                        }
                        typed_args.push(typed_arg);
                    }
                    return Ok(TypedExpr { kind: TypedExprKind::FunctionCall { name: name.clone(), args: typed_args }, data_type: signature.return_type.clone(), span: *span });
                }
                if let Some((_, var_type, _)) = compiler.find_variable(name) {
                    return Ok(TypedExpr { kind: TypedExprKind::VariableRef(name.clone()), data_type: var_type.clone(), span: *span });
                }
                Err(LangError::Compile(CompileError::new(format!("Undefined function or variable '{}'", name), *span)))
            }
            crate::token::Token::IntLiteral(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::I32(*val)), data_type: DataType::I32, span: *span }),
            crate::token::Token::FloatLiteral(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::F64(*val)), data_type: DataType::F64, span: *span }),
            crate::token::Token::True => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::Bool(true)), data_type: DataType::Bool, span: *span }),
            crate::token::Token::False => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::Bool(false)), data_type: DataType::Bool, span: *span }),
            crate::token::Token::StringLiteral(s) => {
                let s_len = s.len() as u32;
                let header_offset = *compiler.string_headers.entry(s.clone()).or_insert_with(|| {
                    let _data_offset = *compiler.string_data.entry(s.clone()).or_insert_with(|| {
                        let offset = compiler.static_offset;
                        compiler.static_offset += s_len;
                        offset
                    });
                    let header_offset = compiler.static_offset;
                    compiler.static_offset += 12; // ptr, len, cap
                    header_offset
                });
                Ok(TypedExpr { kind: TypedExprKind::StringLiteral { header_offset }, data_type: DataType::String, span: *span })
            }
            _ => Err(LangError::Compile(CompileError::new(format!("This token cannot be the start of an expression: {:?}", token), *span))),
        },
        RawExprPart::MathBlock(math_node, _) => analyze_math_node(compiler, math_node),
        RawExprPart::Group(inner_parts, _) => {
            let mut inner_slice = &inner_parts[..];
            let result = analyze_sexp_from_slice(compiler, &mut inner_slice)?;
            if !inner_slice.is_empty() {
                return Err(LangError::Compile(CompileError::new("Unexpected tokens after expression in group", inner_slice[0].span())));
            }
            Ok(result)
        }
        RawExprPart::IfExpr { condition, then_branch, else_branch, span } => {
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
    }
}

/// 数式のASTノード(MathAstNode)を意味解析・型チェックする
fn analyze_math_node(compiler: &mut Compiler, node: &MathAstNode) -> Result<TypedExpr, LangError> {
    match node {
        MathAstNode::Literal(literal, span) => match literal {
            MathLiteral::Int(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::I32(*val)), data_type: DataType::I32, span: *span }),
            MathLiteral::Float(val) => Ok(TypedExpr { kind: TypedExprKind::Literal(LiteralValue::F64(*val)), data_type: DataType::F64, span: *span }),
        },
        MathAstNode::Variable(name, span) => {
            if let Some((_, var_type, _)) = compiler.find_variable(name) {
                Ok(TypedExpr { kind: TypedExprKind::VariableRef(name.clone()), data_type: var_type.clone(), span: *span })
            } else {
                Err(LangError::Compile(CompileError::new(format!("Undefined variable '{}' in math expression", name), *span)))
            }
        }
        MathAstNode::InfixOp { op, left, right, span } => {
            let typed_left = analyze_math_node(compiler, left)?;
            let typed_right = analyze_math_node(compiler, right)?;
            if typed_left.data_type != typed_right.data_type {
                return Err(LangError::Compile(CompileError::new(format!("Mismatched types for binary operator: `{}` and `{}`", typed_left.data_type, typed_right.data_type), *span)));
            }
            let op_type = &typed_left.data_type;
            let (func_name, result_type) = match (op, op_type) {
                (crate::token::Token::Plus, DataType::I32) => ("i32.add", DataType::I32), (crate::token::Token::Minus, DataType::I32) => ("i32.sub", DataType::I32),
                (crate::token::Token::Star, DataType::I32) => ("i32.mul", DataType::I32), (crate::token::Token::Slash, DataType::I32) => ("i32.div_s", DataType::I32),
                (crate::token::Token::Plus, DataType::F64) => ("f64.add", DataType::F64), (crate::token::Token::Minus, DataType::F64) => ("f64.sub", DataType::F64),
                (crate::token::Token::Star, DataType::F64) => ("f64.mul", DataType::F64), (crate::token::Token::Slash, DataType::F64) => ("f64.div", DataType::F64),
                (crate::token::Token::EqualsEquals, DataType::I32) => ("i32.eq", DataType::Bool), (crate::token::Token::BangEquals, DataType::I32) => ("i32.ne", DataType::Bool),
                (crate::token::Token::LessThan, DataType::I32) => ("i32.lt_s", DataType::Bool), (crate::token::Token::LessThanEquals, DataType::I32) => ("i32.le_s", DataType::Bool),
                (crate::token::Token::GreaterThan, DataType::I32) => ("i32.gt_s", DataType::Bool), (crate::token::Token::GreaterThanEquals, DataType::I32) => ("i32.ge_s", DataType::Bool),
                _ => return Err(LangError::Compile(CompileError::new(format!("Operator `{:?}` is not supported for type `{}`", op, op_type), *span))),
            };
            Ok(TypedExpr { kind: TypedExprKind::FunctionCall { name: func_name.to_string(), args: vec![typed_left, typed_right] }, data_type: result_type, span: *span })
        }
        MathAstNode::Call { name, args, span } => {
            let signature = compiler.function_table.get(&name.0).cloned().ok_or_else(|| LangError::Compile(CompileError::new(format!("Undefined function '{}' in math expression", name.0),name.1)))?;
            if args.len() != signature.param_types.len() {
                return Err(LangError::Compile(CompileError::new(format!("Function '{}' expects {} arguments, but {} were provided", name.0, signature.param_types.len(), args.len()), *span)));
            }
            let mut typed_args = Vec::new();
            for (i, arg_node) in args.iter().enumerate() {
                let typed_arg = analyze_math_node(compiler, arg_node)?;
                let expected_type = &signature.param_types[i];
                if typed_arg.data_type != *expected_type {
                    return Err(LangError::Compile(CompileError::new(format!("Mismatched type for argument {} of function '{}': expected '{}', but found '{}'", i + 1, name.0, expected_type, typed_arg.data_type), typed_arg.span)));
                }
                typed_args.push(typed_arg);
            }
            Ok(TypedExpr { kind: TypedExprKind::FunctionCall { name: name.0.clone(), args: typed_args }, data_type: signature.return_type.clone(), span: *span })
        }
    }
}