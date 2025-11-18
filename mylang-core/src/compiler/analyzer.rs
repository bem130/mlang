//! RawASTを意味解析・型チェックし、TypedASTに変換するアナライザー。

extern crate alloc;
use super::{Analyzer, FunctionSignature, VariableEntry};
use crate::alloc::string::ToString;
use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::span::Span;
use crate::token::Token;
use alloc::boxed::Box;
use alloc::collections::{BTreeMap, BTreeSet};
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
        RawAstNode::TraitDef {
            name,
            type_params,
            span,
            ..
        } => Ok(TypedAstNode::TraitDef {
            name: name.0.clone(),
            type_params: type_params
                .iter()
                .map(|param| param.name.0.clone())
                .collect(),
            span: *span,
        }),
        RawAstNode::ImplDef { span, .. } => Err(LangError::Compile(CompileError::new(
            "Impl blocks are processed separately during analysis",
            *span,
        ))),
        RawAstNode::LetHoist {
            name, value, span, ..
        } => {
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

            if func_name == "main" && !signature.type_params.is_empty() {
                return Err(LangError::Compile(CompileError::new(
                    "The 'main' function cannot have generic parameters",
                    name.1,
                )));
            }

            let type_scope_len = analyzer.push_type_params(&signature.type_params);
            let result = (|| {
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
                            unique_name: param_name.clone(),
                            data_type: param_type.clone(),
                            scope_depth: analyzer.scope_depth,
                            is_mutable: false,
                        });
                        typed_params.push((param_name.clone(), param_type));
                    }
                }

                let typed_body = if let RawAstNode::Lambda { body, .. } = &**value {
                    analyze_expr(analyzer, body)?
                } else {
                    return Err(LangError::Compile(CompileError::new(
                        "Invalid let hoist syntax: expected a lambda expression",
                        *span,
                    )));
                };

                let mut return_type_resolved = signature.return_type.clone();

                if func_name == "main" && return_type_resolved != DataType::Unit {
                    return Err(LangError::Compile(CompileError::new(
                        "The 'main' function must have a return type of '()'",
                        name.1,
                    )));
                }

                if !core_equal(&typed_body.data_type, &return_type_resolved) {
                    if return_type_resolved != DataType::Unit {
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
            })();
            analyzer.pop_type_params(type_scope_len);
            result
        }
        // トップレベルの他の式は暗黙のmain関数の一部として扱われる (lib.rsでラップ済み)
        _ => Err(LangError::Compile(CompileError::new(
            "Only function, struct, or enum definitions are allowed at the top level in library mode",
            node.span(),
        ))),
    }
}

pub fn analyze_impl_block(
    analyzer: &mut Analyzer,
    node: &RawAstNode,
) -> Result<Vec<TypedAstNode>, LangError> {
    let RawAstNode::ImplDef {
        trait_name,
        trait_args,
        self_type,
        methods,
        span,
    } = node
    else {
        return Ok(Vec::new());
    };

    let trait_arg_types = trait_args
        .iter()
        .map(|(ty, ty_span)| analyzer.string_to_type(ty, *ty_span))
        .collect::<Result<Vec<_>, _>>()?;
    let self_type_resolved = analyzer.string_to_type(&self_type.0, self_type.1)?;
    let mut typed_nodes = Vec::new();
    typed_nodes.push(TypedAstNode::ImplDef {
        trait_name: trait_name.0.clone(),
        trait_args: trait_arg_types.clone(),
        self_type: self_type_resolved.clone(),
        span: *span,
    });

    for method in methods {
        if !method.type_params.is_empty() {
            return Err(LangError::Compile(CompileError::new(
                "Trait implementation methods cannot declare generics yet",
                method.name.1,
            )));
        }
        let body = method.body.as_ref().ok_or_else(|| {
            LangError::Compile(CompileError::new(
                format!(
                    "Method '{}' in impl for '{}' must provide a body",
                    method.name.0, self_type_resolved
                ),
                method.name.1,
            ))
        })?;

        let synthetic_lambda = RawAstNode::Lambda {
            params: method.params.clone(),
            body: body.clone(),
            return_type: method.return_type.clone(),
            span: method.span,
        };
        let synthetic_fn = RawAstNode::LetHoist {
            name: (
                format!("{}::{}", trait_name.0, method.name.0),
                method.name.1,
            ),
            type_params: Vec::new(),
            value: Box::new(synthetic_lambda),
            span: method.span,
        };

        let alias_len = analyzer.push_type_alias("Self", self_type_resolved.clone());
        let typed = analyze_toplevel(analyzer, &synthetic_fn);
        analyzer.pop_type_alias(alias_len);
        typed_nodes.push(typed?);
    }

    Ok(typed_nodes)
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
                if !core_equal(&result.data_type, &expected_type) {
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
        RawAstNode::LetHoist {
            name, value, span, ..
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
                if !core_equal(&entry.data_type, &typed_value.data_type) {
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

            if !core_equal(&typed_body.data_type, &explicit_return_type) {
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
            if !core_equal(&typed_condition.data_type, &DataType::Bool) {
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

            if !core_equal(&typed_body.data_type, &DataType::Unit) {
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
        RawAstNode::TraitDef { span, .. } => Err(LangError::Compile(CompileError::new(
            "Trait definitions cannot appear inside expressions",
            *span,
        ))),
        RawAstNode::ImplDef { span, .. } => Err(LangError::Compile(CompileError::new(
            "Impl blocks cannot appear inside expressions",
            *span,
        ))),
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
                if !core_equal(&result.data_type, &expected_type) {
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
    let generics = if signature.type_params.is_empty() {
        String::new()
    } else {
        let mut formatted = Vec::new();
        for param in &signature.type_params {
            let bound_strings = signature
                .trait_bounds
                .iter()
                .filter(|bound| &bound.type_param == param)
                .map(|bound| {
                    if bound.trait_args.is_empty() {
                        bound.trait_name.clone()
                    } else {
                        format!(
                            "{}<{}>",
                            bound.trait_name,
                            bound
                                .trait_args
                                .iter()
                                .map(|arg| arg.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                })
                .collect::<Vec<_>>();
            if bound_strings.is_empty() {
                formatted.push(param.clone());
            } else {
                formatted.push(format!("{}: {}", param, bound_strings.join(" + ")));
            }
        }
        format!("<{}>", formatted.join(", "))
    };
    format!(
        "fn {}{}({}) -> {}",
        signature.name, generics, params, signature.return_type
    )
}

/// Helper to compare two DataTypes by their base/core type (unwrap `Refined`).
fn core_equal(a: &DataType, b: &DataType) -> bool {
    a.core_type() == b.core_type()
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
    analyzer: &Analyzer,
    name: &str,
    candidates: &[FunctionSignature],
    arg_types: &[DataType],
    typed_args: Option<&[TypedExpr]>,
    span: Span,
) -> Result<FunctionSignature, LangError> {
    let mut matches: Vec<FunctionSignature> = Vec::new();
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

        let mut substitution: BTreeMap<String, DataType> = BTreeMap::new();
        let mut rejected = None;
        for (idx, (expected, actual)) in candidate
            .param_types
            .iter()
            .zip(arg_types.iter())
            .enumerate()
        {
            // If expected is a refinement type and actual is a base type, try to prove
            // the refinement when we have the concrete argument expression available.
            if let DataType::Refined {
                base: exp_base,
                binder,
                predicate_id,
            } = expected
            {
                if exp_base.core_type() == actual.core_type() {
                    if let Some(typed_args_slice) = typed_args {
                        if let Some(arg_expr) = typed_args_slice.get(idx) {
                            // Support proving for integer literals only for now
                            if let TypedExprKind::Literal(LiteralValue::I32(val)) = &arg_expr.kind {
                                // construct a MathAstNode representing `binder == val` and translate it
                                let equality_node = crate::ast::MathAstNode::InfixOp {
                                    op: crate::token::Token::EqualsEquals,
                                    left: Box::new(crate::ast::MathAstNode::Variable(
                                        binder.clone(),
                                        Default::default(),
                                    )),
                                    right: Box::new(crate::ast::MathAstNode::Literal(
                                        crate::ast::MathLiteral::Int(*val),
                                        Default::default(),
                                    )),
                                    span: Default::default(),
                                };
                                let phi_a = crate::smt::constraints_from_math_ast(&equality_node);
                                let phi_e = crate::smt::constraints_from_math_ast(
                                    &analyzer.refinements[*predicate_id],
                                );
                                // negate phi_e
                                let neg_phi_e = crate::smt::ConstraintSet {
                                    literals: phi_e
                                        .literals
                                        .iter()
                                        .map(|lit| match lit {
                                            crate::smt::SmtLiteral::Pos(b) => {
                                                crate::smt::SmtLiteral::Neg(b.clone())
                                            }
                                            crate::smt::SmtLiteral::Neg(b) => {
                                                crate::smt::SmtLiteral::Pos(b.clone())
                                            }
                                        })
                                        .collect(),
                                };
                                if crate::smt::implies(&phi_a, &neg_phi_e) {
                                    // proven, continue without calling unify_types
                                    continue;
                                } else {
                                    rejected = Some(format!(
                                        "{} rejected: argument {} expected '{}' but found '{}'",
                                        describe_signature(candidate),
                                        idx + 1,
                                        expected,
                                        actual
                                    ));
                                    break;
                                }
                            }
                        }
                    }
                    // If we couldn't prove, fallthrough to normal unify which will reject conservatively
                }
            }

            if let Err(reason) = unify_types(analyzer, expected, actual, &mut substitution) {
                rejected = Some(format!(
                    "{} rejected: argument {} {}",
                    describe_signature(candidate),
                    idx + 1,
                    reason
                ));
                break;
            }
        }

        if let Some(reason) = rejected {
            rejection_notes.push((reason, candidate.definition_span));
            continue;
        }

        if let Some(unbound) = candidate
            .type_params
            .iter()
            .find(|name| !substitution.contains_key(*name))
        {
            rejection_notes.push((
                format!(
                    "{} rejected: type parameter '{}' could not be inferred from arguments {}",
                    describe_signature(candidate),
                    unbound,
                    format_argument_types(arg_types)
                ),
                candidate.definition_span,
            ));
            continue;
        }

        if let Err(reason) = check_trait_bounds(analyzer, candidate, &substitution) {
            rejection_notes.push((
                format!("{} rejected: {}", describe_signature(candidate), reason),
                candidate.definition_span,
            ));
            continue;
        }

        matches.push(instantiate_signature(candidate, &substitution));
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
        for candidate in &matches {
            err = err.with_note(
                format!("Possible candidate: {}", describe_signature(candidate)),
                candidate.definition_span,
            );
        }
        return Err(LangError::Compile(err));
    }

    Ok(matches[0].clone())
}

fn check_trait_bounds(
    analyzer: &Analyzer,
    signature: &FunctionSignature,
    substitution: &BTreeMap<String, DataType>,
) -> Result<(), String> {
    for bound in &signature.trait_bounds {
        let actual_type = substitution.get(&bound.type_param).ok_or_else(|| {
            format!(
                "type parameter '{}' was not inferred for trait bound",
                bound.type_param
            )
        })?;
        let trait_args = bound
            .trait_args
            .iter()
            .map(|arg| apply_type_substitution(arg, substitution))
            .collect::<Vec<_>>();
        if !analyzer.trait_impl_exists(&bound.trait_name, &trait_args, actual_type) {
            let args_display = if trait_args.is_empty() {
                String::new()
            } else {
                format!(
                    "<{}>",
                    trait_args
                        .iter()
                        .map(|ty| ty.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            return Err(format!(
                "type '{}' does not implement {}{}",
                actual_type, bound.trait_name, args_display
            ));
        }
    }
    Ok(())
}

fn instantiate_signature(
    signature: &FunctionSignature,
    substitution: &BTreeMap<String, DataType>,
) -> FunctionSignature {
    let mut instantiated = signature.clone();
    instantiated.param_types = instantiated
        .param_types
        .iter()
        .map(|ty| apply_type_substitution(ty, substitution))
        .collect();
    instantiated.return_type = apply_type_substitution(&signature.return_type, substitution);
    instantiated
}

pub(crate) fn apply_type_substitution(
    ty: &DataType,
    substitution: &BTreeMap<String, DataType>,
) -> DataType {
    match ty {
        DataType::TypeVar(name) => substitution
            .get(name)
            .cloned()
            .unwrap_or_else(|| DataType::TypeVar(name.clone())),
        DataType::Vector(inner) => {
            DataType::Vector(Box::new(apply_type_substitution(inner, substitution)))
        }
        DataType::Tuple(elements) => DataType::Tuple(
            elements
                .iter()
                .map(|element| apply_type_substitution(element, substitution))
                .collect(),
        ),
        DataType::Function {
            params,
            return_type,
        } => DataType::Function {
            params: params
                .iter()
                .map(|param| apply_type_substitution(param, substitution))
                .collect(),
            return_type: Box::new(apply_type_substitution(return_type, substitution)),
        },
        _ => ty.clone(),
    }
}

fn unify_types(
    analyzer: &Analyzer,
    expected: &DataType,
    actual: &DataType,
    substitution: &mut BTreeMap<String, DataType>,
) -> Result<(), String> {
    match expected {
        DataType::Refined {
            base: exp_base,
            binder: _binder,
            predicate_id: exp_pid,
        } => {
            // expected is a refined type <p: Base | phi_exp>
            // actual must be a refined type with a compatible base and a predicate that implies phi_exp
            match actual {
                DataType::Refined {
                    base: act_base,
                    predicate_id: act_pid,
                    ..
                } => {
                    // First, unify the bases
                    unify_types(analyzer, exp_base, act_base, substitution)?;
                    // Now check that actual's predicate implies expected's predicate using SMT
                    // If either predicate id is out of range, reject conservatively
                    let id_a = *act_pid;
                    let id_e = *exp_pid;
                    if id_a >= analyzer.refinements.len() || id_e >= analyzer.refinements.len() {
                        return Err(format!(
                            "refinement predicate not found ({} -> {})",
                            id_a, id_e
                        ));
                    }
                    // translate to constraints and ask SMT
                    let phi_a = crate::smt::constraints_from_math_ast(&analyzer.refinements[id_a]);
                    let phi_e = crate::smt::constraints_from_math_ast(&analyzer.refinements[id_e]);
                    // negate phi_e
                    let neg_phi_e = crate::smt::ConstraintSet {
                        literals: phi_e
                            .literals
                            .iter()
                            .map(|lit| match lit {
                                crate::smt::SmtLiteral::Pos(b) => {
                                    crate::smt::SmtLiteral::Neg(b.clone())
                                }
                                crate::smt::SmtLiteral::Neg(b) => {
                                    crate::smt::SmtLiteral::Pos(b.clone())
                                }
                            })
                            .collect(),
                    };
                    if crate::smt::implies(&phi_a, &neg_phi_e) {
                        Ok(())
                    } else {
                        Err(format!(
                            "refinement predicate '{}' does not imply '{}'",
                            id_a, id_e
                        ))
                    }
                }
                // allow passing a refined actual to a base expected (handled below), but if actual is base only,
                // we cannot prove it satisfies the refinement predicate, so reject conservatively.
                _ => Err(format!("expected '{}' but found '{}'", expected, actual)),
            }
        }
        DataType::TypeVar(name) => unify_type_variable(name, actual, substitution),
        DataType::Vector(inner) => match actual {
            DataType::Vector(actual_inner) => {
                unify_types(analyzer, inner, actual_inner, substitution)
            }
            _ => Err(format!("expected '{}' but found '{}'", expected, actual)),
        },
        DataType::Tuple(elements) => match actual {
            DataType::Tuple(actual_elements) => {
                if elements.len() != actual_elements.len() {
                    return Err(format!(
                        "expected tuple of length {} but found {}",
                        elements.len(),
                        actual_elements.len()
                    ));
                }
                for (expected_elem, actual_elem) in elements.iter().zip(actual_elements.iter()) {
                    unify_types(analyzer, expected_elem, actual_elem, substitution)?;
                }
                Ok(())
            }
            _ => Err(format!("expected '{}' but found '{}'", expected, actual)),
        },
        DataType::Function {
            params,
            return_type,
        } => match actual {
            DataType::Function {
                params: actual_params,
                return_type: actual_return,
            } => {
                if params.len() != actual_params.len() {
                    return Err(format!(
                        "expected function with {} parameters but found {}",
                        params.len(),
                        actual_params.len()
                    ));
                }
                for (expected_param, actual_param) in params.iter().zip(actual_params.iter()) {
                    unify_types(analyzer, expected_param, actual_param, substitution)?;
                }
                unify_types(analyzer, return_type, actual_return, substitution)
            }
            _ => Err(format!("expected '{}' but found '{}'", expected, actual)),
        },
        _ => {
            if core_equal(expected, actual) {
                Ok(())
            } else {
                Err(format!("expected '{}' but found '{}'", expected, actual))
            }
        }
    }
}

fn unify_type_variable(
    name: &str,
    actual: &DataType,
    substitution: &mut BTreeMap<String, DataType>,
) -> Result<(), String> {
    if let Some(bound) = substitution.get(name) {
        if core_equal(bound, actual) {
            Ok(())
        } else {
            Err(format!(
                "type variable '{}' was inferred as '{}' but must also match '{}'",
                name, bound, actual
            ))
        }
    } else {
        substitution.insert(name.to_string(), actual.clone());
        Ok(())
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
    let signature = select_overload(
        analyzer,
        name,
        &candidates,
        &arg_types,
        Some(&typed_args),
        span,
    )?;

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

/// S式の解析中にスタックに積まれる値。評価済みの式か、未解決の関数名を表す。
enum SexpStackVal {
    Expr(TypedExpr),
    Fn(String, Span),
}

/// スタックベースの縮約アルゴリズムを用いて、ポーランド記法の式を解決する。
fn reduce_stack_once(
    analyzer: &mut Analyzer,
    stack: &mut Vec<SexpStackVal>,
) -> Result<bool, LangError> {
    if stack.len() <= 1 {
        return Ok(false);
    }

    let mut reducible_idx = None;
    let mut cached_candidates: Option<(usize, Vec<FunctionSignature>)> = None;
    // スタックを右から左にスキャンし、最初に縮約可能な「関数 + 引数」の組を探す
    for i in (0..stack.len()).rev() {
        if let SexpStackVal::Fn(name, _) = &stack[i] {
            if let Some(candidates) = analyzer.function_table.get(name).cloned() {
                let max_arity = candidates
                    .iter()
                    .map(|c| c.param_types.len())
                    .max()
                    .unwrap_or(0);
                if i + max_arity < stack.len() {
                    // 後続の要素がすべて評価済みの式(Expr)であることを確認
                    let all_args_are_exprs = (i + 1..=i + max_arity)
                        .all(|j| matches!(stack.get(j), Some(SexpStackVal::Expr(_))));
                    if all_args_are_exprs {
                        reducible_idx = Some(i);
                        cached_candidates = Some((i, candidates));
                        break;
                    }
                }
            }
        }
    }

    let Some(idx) = reducible_idx else {
        return Ok(false);
    };

    let (name, span) = match &stack[idx] {
        SexpStackVal::Fn(name, span) => (name.clone(), *span),
        _ => unreachable!(),
    };

    let candidates = if let Some((cached_idx, candidates)) = cached_candidates {
        if cached_idx == idx {
            candidates
        } else {
            analyzer
                .function_table
                .get(&name)
                .cloned()
                .unwrap_or_default()
        }
    } else {
        analyzer
            .function_table
            .get(&name)
            .cloned()
            .unwrap_or_default()
    };

    let args_slice = &stack[idx + 1..];
    let mut best_match: Option<(FunctionSignature, usize)> = None;

    // アリティが最も大きい（＝最も多くの引数を消費する）オーバーロードから試す
    'outer: for arity in (1..=args_slice.len()).rev() {
        if candidates.iter().any(|c| c.param_types.len() == arity) {
            let mut typed_args = Vec::new();
            let mut arg_types = Vec::new();
            for val in &args_slice[..arity] {
                if let SexpStackVal::Expr(expr) = val {
                    typed_args.push(expr.clone());
                    arg_types.push(expr.data_type.clone());
                } else {
                    // 引数部分に関数名が来てしまった場合は、このアリティではマッチしない
                    continue 'outer;
                }
            }
            if let Ok(signature) = select_overload(
                analyzer,
                &name,
                &candidates,
                &arg_types,
                Some(&typed_args),
                span,
            ) {
                best_match = Some((signature, arity));
                break;
            }
        }
    }

    if let Some((signature, arity)) = best_match {
        let args: Vec<TypedExpr> = (idx + 1..=idx + arity)
            .map(|j| match &stack[j] {
                SexpStackVal::Expr(expr) => expr.clone(),
                _ => unreachable!(),
            })
            .collect();

        let call_expr = TypedExpr {
            kind: TypedExprKind::FunctionCall { name, args },
            data_type: signature.return_type,
            span,
        };

        // スタックの関数と引数を、評価後の式で置き換える
        stack.splice(idx..=idx + arity, [SexpStackVal::Expr(call_expr)]);
        Ok(true)
    } else {
        Err(LangError::Compile(CompileError::new(
            format!("Not enough arguments for function '{}'", name),
            span,
        )))
    }
}

fn reduce_stack_greedily(
    analyzer: &mut Analyzer,
    stack: &mut Vec<SexpStackVal>,
) -> Result<(), LangError> {
    while reduce_stack_once(analyzer, stack)? {}
    Ok(())
}

fn reduce_polish_notation_stack(
    analyzer: &mut Analyzer,
    mut stack: Vec<SexpStackVal>,
) -> Result<TypedExpr, LangError> {
    reduce_stack_greedily(analyzer, &mut stack)?;

    if stack.len() > 1 {
        // これ以上縮約できないのに、スタックに複数の要素が残っている場合はエラー
        let first_fn = stack.iter().find_map(|v| match v {
            SexpStackVal::Fn(name, span) => Some((name.clone(), *span)),
            _ => None,
        });
        if let Some((name, span)) = first_fn {
            return Err(LangError::Compile(CompileError::new(
                format!(
                    "Could not resolve call to function '{}' with available arguments",
                    name
                ),
                span,
            )));
        } else {
            return Err(LangError::Compile(CompileError::new(
                "Invalid expression sequence: expression does not resolve to a single value",
                span_of_stack(&stack),
            )));
        }
    }

    // 最終的にスタックに残った単一の式が、S式全体の結果
    if let Some(SexpStackVal::Expr(final_expr)) = stack.pop() {
        Ok(final_expr)
    } else {
        Err(LangError::Compile(CompileError::new(
            "Expression did not resolve to a single value",
            span_of_stack(&stack),
        )))
    }
}

fn span_of_stack(stack: &[SexpStackVal]) -> Span {
    let first = stack.first().map(|v| match v {
        SexpStackVal::Expr(e) => e.span,
        SexpStackVal::Fn(_, s) => *s,
    });
    let last = stack.last().map(|v| match v {
        SexpStackVal::Expr(e) => e.span,
        SexpStackVal::Fn(_, s) => *s,
    });
    if let (Some(start), Some(end)) = (first, last) {
        crate::span::combine_spans(start, end)
    } else {
        Span::default()
    }
}

/// RawExprPartのスライスから一つの意味のある式(TypedExpr)を解析する。
/// この関数はアナライザーの心臓部であり、パーサーが作った未解決の構造を解釈する。
/// ポーランド記法（S式）の解析にはスタックベースの縮約アルゴリズムを使用する。
fn analyze_sexp_from_slice<'a>(
    analyzer: &mut Analyzer,
    parts: &mut &'a [RawExprPart],
    _hoisted_vars: &BTreeSet<String>,
) -> Result<TypedExpr, LangError> {
    if parts.is_empty() {
        return Err(LangError::Compile(CompileError::new(
            "Unexpected end of expression",
            Default::default(),
        )));
    }

    // C-style `f(...)` は不可分な1単位として扱い、即座に評価する
    if let (
        Some(RawExprPart::Token(Token::Identifier(name), span)),
        Some(RawExprPart::CStyleArgs(arg_nodes, _)),
    ) = (parts.get(0), parts.get(1))
    {
        let mut temp_slice = &parts[..];
        let result = resolve_c_style_call(analyzer, name, *span, arg_nodes, &mut temp_slice)?;
        *parts = temp_slice;
        return Ok(result);
    }

    // S式全体を評価するためのスタックを準備
    let mut stack: Vec<SexpStackVal> = Vec::new();
    let mut current_parts = *parts;
    let mut pending_pipe: Option<(TypedExpr, Span)> = None;

    // S式を構成する全ての部分式を評価し、スタックに積む
    while !current_parts.is_empty() {
        match &current_parts[0] {
            RawExprPart::PipeOperator(span) => {
                if pending_pipe.is_some() {
                    return Err(LangError::Compile(CompileError::new(
                        "Pipe operator requires a function call on the right-hand side",
                        *span,
                    )));
                }
                reduce_stack_greedily(analyzer, &mut stack)?;
                let Some(lhs_idx) = stack
                    .iter()
                    .rposition(|value| matches!(value, SexpStackVal::Expr(_)))
                else {
                    return Err(LangError::Compile(CompileError::new(
                        "Pipe operator requires an expression on the left-hand side",
                        *span,
                    )));
                };
                let lhs_expr = match stack.remove(lhs_idx) {
                    SexpStackVal::Expr(expr) => expr,
                    _ => unreachable!(),
                };
                pending_pipe = Some((lhs_expr, *span));
                current_parts = &current_parts[1..];
            }
            RawExprPart::Token(Token::Identifier(name), span) => {
                // 変数か関数かをこの時点では決定せず、種類に応じてスタックに積む
                if analyzer.function_table.contains_key(name) {
                    stack.push(SexpStackVal::Fn(name.clone(), *span));
                    current_parts = &current_parts[1..];
                    if let Some((pipe_expr, _)) = pending_pipe.take() {
                        stack.push(SexpStackVal::Expr(pipe_expr));
                    }
                    reduce_stack_greedily(analyzer, &mut stack)?;
                } else if let Some(entry) = analyzer.find_variable(name) {
                    if let Some((_, pipe_span)) = pending_pipe.as_ref() {
                        return Err(LangError::Compile(CompileError::new(
                            "Pipe operator requires a function call on the right-hand side",
                            *pipe_span,
                        )));
                    }
                    stack.push(SexpStackVal::Expr(TypedExpr {
                        kind: TypedExprKind::VariableRef {
                            name: entry.original_name.clone(),
                            unique_name: entry.unique_name.clone(),
                        },
                        data_type: entry.data_type.clone(),
                        span: *span,
                    }));
                    current_parts = &current_parts[1..];
                    reduce_stack_greedily(analyzer, &mut stack)?;
                } else {
                    return Err(LangError::Compile(CompileError::new(
                        format!("Undefined function or variable '{}'", name),
                        *span,
                    )));
                }
            }
            RawExprPart::Token(token, span) => {
                if let Some((_, pipe_span)) = pending_pipe.as_ref() {
                    return Err(LangError::Compile(CompileError::new(
                        "Pipe operator requires a function call on the right-hand side",
                        *pipe_span,
                    )));
                }
                // リテラルは評価済みの式としてスタックに積む
                let expr = match token {
                    Token::IntLiteral(val) => TypedExpr {
                        kind: TypedExprKind::Literal(LiteralValue::I32(*val)),
                        data_type: DataType::I32,
                        span: *span,
                    },
                    Token::FloatLiteral(val) => TypedExpr {
                        kind: TypedExprKind::Literal(LiteralValue::F64(*val)),
                        data_type: DataType::F64,
                        span: *span,
                    },
                    Token::StringLiteral(s) => {
                        let header_offset = analyzer.ensure_string_is_statically_allocated(s);
                        TypedExpr {
                            kind: TypedExprKind::StringLiteral { header_offset },
                            data_type: DataType::String,
                            span: *span,
                        }
                    }
                    Token::True => TypedExpr {
                        kind: TypedExprKind::Literal(LiteralValue::Bool(true)),
                        data_type: DataType::Bool,
                        span: *span,
                    },
                    Token::False => TypedExpr {
                        kind: TypedExprKind::Literal(LiteralValue::Bool(false)),
                        data_type: DataType::Bool,
                        span: *span,
                    },
                    _ => {
                        return Err(LangError::Compile(CompileError::new(
                            "Unexpected token",
                            *span,
                        )));
                    }
                };
                stack.push(SexpStackVal::Expr(expr));
                current_parts = &current_parts[1..];
                reduce_stack_greedily(analyzer, &mut stack)?;
            }
            RawExprPart::Group(inner_parts, _) => {
                if let Some((_, pipe_span)) = pending_pipe.as_ref() {
                    return Err(LangError::Compile(CompileError::new(
                        "Pipe operator requires a function call on the right-hand side",
                        *pipe_span,
                    )));
                }
                let mut inner_slice = &inner_parts[..];
                // グループ内を再帰的に解決し、結果の式をスタックに積む
                let group_expr =
                    analyze_sexp_from_slice(analyzer, &mut inner_slice, &BTreeSet::new())?;
                stack.push(SexpStackVal::Expr(group_expr));
                current_parts = &current_parts[1..];
                reduce_stack_greedily(analyzer, &mut stack)?;
            }
            RawExprPart::CStyleArgs(..) => {
                // C-style呼び出しは先頭でのみ処理されるため、ここに来ることはない
                return Err(LangError::Compile(CompileError::new(
                    "Unexpected C-style argument list",
                    current_parts[0].span(),
                )));
            }
            RawExprPart::MathBlock(math, _) => {
                if let Some((_, pipe_span)) = pending_pipe.as_ref() {
                    return Err(LangError::Compile(CompileError::new(
                        "Pipe operator requires a function call on the right-hand side",
                        *pipe_span,
                    )));
                }
                let math_expr = analyze_math_node(analyzer, math)?;
                stack.push(SexpStackVal::Expr(math_expr));
                current_parts = &current_parts[1..];
                reduce_stack_greedily(analyzer, &mut stack)?;
            }
            RawExprPart::IfExpr { .. }
            | RawExprPart::MatchExpr { .. }
            | RawExprPart::Lambda { .. } => {
                if let Some((_, pipe_span)) = pending_pipe.as_ref() {
                    return Err(LangError::Compile(CompileError::new(
                        "Pipe operator requires a function call on the right-hand side",
                        *pipe_span,
                    )));
                }
                let mut temp_slice = &current_parts[..];
                let complex_expr = analyze_complex_part(analyzer, &mut temp_slice)?;
                stack.push(SexpStackVal::Expr(complex_expr));
                current_parts = temp_slice;
                reduce_stack_greedily(analyzer, &mut stack)?;
            }
            RawExprPart::TypeAnnotation(_, _) => {
                // 型注釈は式の終わりを示すため、ループを抜ける
                break;
            }
            RawExprPart::TupleLiteral(elements, span) => {
                if let Some((_, pipe_span)) = pending_pipe.as_ref() {
                    return Err(LangError::Compile(CompileError::new(
                        "Pipe operator requires a function call on the right-hand side",
                        *pipe_span,
                    )));
                }
                let mut typed_elements = Vec::new();
                let mut element_types = Vec::new();
                for element in elements {
                    let typed_element = analyze_expr(analyzer, element)?;
                    element_types.push(typed_element.data_type.clone());
                    typed_elements.push(typed_element);
                }
                let tuple_expr = TypedExpr {
                    kind: TypedExprKind::TupleLiteral {
                        elements: typed_elements,
                    },
                    data_type: DataType::Tuple(element_types),
                    span: *span,
                };
                stack.push(SexpStackVal::Expr(tuple_expr));
                current_parts = &current_parts[1..];
                reduce_stack_greedily(analyzer, &mut stack)?;
            }
        }
    }

    if let Some((_, pipe_span)) = pending_pipe {
        return Err(LangError::Compile(CompileError::new(
            "Pipe operator is missing a function call on the right-hand side",
            pipe_span,
        )));
    }

    *parts = current_parts;
    reduce_polish_notation_stack(analyzer, stack)
}

/// if, match, lambdaなどの不可分な複雑な部分式を評価するヘルパー
fn analyze_complex_part<'a>(
    analyzer: &mut Analyzer,
    parts: &mut &'a [RawExprPart],
) -> Result<TypedExpr, LangError> {
    let part = &parts[0];
    *parts = &parts[1..]; // この部分式を消費する
    match part {
        RawExprPart::IfExpr {
            condition,
            then_branch,
            else_branch,
            span,
        } => {
            let typed_cond = analyze_expr(analyzer, condition)?;
            if !core_equal(&typed_cond.data_type, &DataType::Bool) {
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
            if !core_equal(&typed_then.data_type, &typed_else.data_type) {
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
            analyze_match_expr(analyzer, value, arms, *span)
        }
        RawExprPart::Lambda {
            params,
            body,
            return_type,
            span,
        } => {
            let raw_lambda_node = RawAstNode::Lambda {
                params: params.clone(),
                body: body.clone(),
                return_type: return_type.clone(),
                span: *span,
            };
            analyze_expr(analyzer, &raw_lambda_node)
        }
        _ => unreachable!(),
    }
}

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
            if !core_equal(&typed_left.data_type, &typed_right.data_type) {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Mismatched types for binary operator: `{}` and `{}`",
                        typed_left.data_type, typed_right.data_type
                    ),
                    *span,
                )));
            }
            // Use the core/base type (unwrap `Refined`) when selecting operator implementations.
            let op_core = typed_left.data_type.core_type();
            let (func_name, result_type) = match (op, &op_core) {
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
                            op, op_core
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
            let signature = select_overload(
                analyzer,
                &name.0,
                &candidates,
                &arg_types,
                Some(&typed_args),
                *span,
            )?;
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
            if !core_equal(expected_type, &typed_body.data_type) {
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
