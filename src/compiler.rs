//! RawASTを受け取り、意味解析、型チェック、コード生成を行うコンパイラ。

use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::span::Span;
use crate::token::Token;
use std::collections::HashMap;

/// コンパイル処理全体を管理する構造体
pub struct Compiler {
    // WATコードの生成結果を保持する
    wat_buffer: String,
    // 現在のスコープの深さ
    scope_depth: usize,
    // 関数名とそのシグネチャを保持するテーブル
    function_table: HashMap<String, FunctionSignature>,
    // 変数名とその型、スコープ深度を保持するテーブル
    variable_table: Vec<(String, DataType, usize)>,
    // 文字列リテラルとそのデータオフセットのマッピング
    string_data: HashMap<String, u32>,
    // 文字列リテラルに対応するヘッダのオフセット
    string_headers: HashMap<String, u32>,
    // 次に静的データを配置する線形メモリのオフセット
    static_offset: u32,
}

/// 関数のシグネチャ情報
#[derive(Clone)]
struct FunctionSignature {
    param_types: Vec<DataType>,
    return_type: DataType,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            wat_buffer: String::new(),
            scope_depth: 0,
            function_table: HashMap::new(),
            variable_table: Vec::new(),
            string_data: HashMap::new(),
            string_headers: HashMap::new(),
            // iovecとnwritten用にメモリの先頭を予約しておく
            static_offset: 1024,
        }
    }

    /// コンパイルのメインエントリーポイント
    pub fn compile(&mut self, ast: &[RawAstNode]) -> Result<String, LangError> {
        // 1. 宣言収集パス: 全ての関数定義を先にテーブルに登録
        self.prepass_declarations(ast)?;

        // 2. コード生成パス
        self.wat_buffer.push_str("(module\n");
        // WASIのfd_writeをインポート
        self.wat_buffer.push_str("  (import \"wasi_snapshot_preview1\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))\n");
        // メモリを定義し、エクスポート
        self.wat_buffer.push_str("  (memory (export \"memory\") 1)\n");

        // 全ての関数定義をコンパイル
        for node in ast {
            if let RawAstNode::FnDef { .. } = node {
                self.compile_function_def(node)?;
            }
        }

        // main関数が存在すれば、モジュールの末尾でエクスポートする
        if self.function_table.contains_key("main") {
            self.wat_buffer
                .push_str("  (export \"_start\" (func $main))\n");
        }
        
        // 文字列リテラルのデータセクションとヘッダセクションを生成
        let mut sorted_data: Vec<_> = self.string_data.iter().collect();
        sorted_data.sort_by_key(|&(_, offset)| offset);
        for (s, offset) in sorted_data {
            let escaped_s = s.replace('\\', "\\\\").replace('"', "\\\"");
            self.wat_buffer.push_str(&format!("  (data (i32.const {}) \"{}\")\n", offset, escaped_s));
        }

        let mut sorted_headers: Vec<_> = self.string_headers.iter().collect();
        sorted_headers.sort_by_key(|&(_, offset)| offset);
        for (s, header_offset) in sorted_headers {
            let data_offset = self.string_data.get(s).unwrap();
            let len = s.len() as u32;
            let cap = len * 2; // 容量は2倍確保

            let mut header_bytes = Vec::new();
            header_bytes.extend_from_slice(&data_offset.to_le_bytes());
            header_bytes.extend_from_slice(&len.to_le_bytes());
            header_bytes.extend_from_slice(&cap.to_le_bytes());
            
            let header_data_str = header_bytes.iter().map(|b| format!("\\{:02x}", b)).collect::<String>();
            let escaped_comment = s.escape_default().to_string();

            self.wat_buffer.push_str(&format!("  (data (i32.const {}) \"{}\") ;; Ptr, Len, Cap for \"{}\"\n", header_offset, header_data_str, escaped_comment));
        }


        self.wat_buffer.push_str(")\n");
        Ok(self.wat_buffer.clone())
    }

    /// 1パス目: 関数宣言を収集し、シグネチャをテーブルに登録する
    fn prepass_declarations(&mut self, ast: &[RawAstNode]) -> Result<(), LangError> {
        for node in ast {
            if let RawAstNode::FnDef {
                name,
                params,
                return_type,
                ..
            } = node
            {
                let func_name = &name.0;
                let param_types = params
                    .iter()
                    .map(|(_, type_info)| self.string_to_type(&type_info.0, type_info.1))
                    .collect::<Result<Vec<_>, _>>()?;
                
                let ret_type = match return_type {
                    Some(rt) => self.string_to_type(&rt.0, rt.1)?,
                    None => DataType::Unit,
                };

                self.function_table.insert(
                    func_name.clone(),
                    FunctionSignature {
                        param_types,
                        return_type: ret_type,
                    },
                );
            }
        }
        Ok(())
    }

    /// Typed ASTからトップレベルの`let`束縛を収集してローカル変数として宣言する
    fn collect_and_declare_locals(&mut self, typed_body: &TypedAstNode) {
        if let TypedNodeKind::Block { statements } = &typed_body.kind {
            for stmt in statements {
                if let TypedNodeKind::LetBinding { name, value } = &stmt.kind {
                    self.wat_buffer.push_str(&format!(
                        "    (local ${} {})\n",
                        name,
                        self.type_to_wat(&value.data_type)
                    ));
                }
            }
        }
    }

    /// 関数定義のノードをコンパイルする
    fn compile_function_def(&mut self, node: &RawAstNode) -> Result<(), LangError> {
        if let RawAstNode::FnDef {
            name,
            params,
            body,
            return_type,
            ..
        } = node
        {
            let func_name = &name.0;
            let signature = self.function_table.get(func_name).unwrap().clone();
            
            // ソースコード上での本来の戻り値の型を解決
            let original_return_type = match return_type {
                Some(rt) => self.string_to_type(&rt.0, rt.1)?,
                None => DataType::Unit,
            };

            self.enter_scope();

            self.wat_buffer.push_str(&format!("  (func ${}", func_name));

            for (i, (param_node, _)) in params.iter().enumerate() {
                let param_name = &param_node.0;
                let param_type = &signature.param_types[i];
                self.wat_buffer.push_str(&format!(
                    " (param ${} {})",
                    param_name,
                    self.type_to_wat(param_type)
                ));
                self.variable_table.push((
                    param_name.clone(),
                    param_type.clone(),
                    self.scope_depth,
                ));
            }

            // signature.return_typeはmainの場合常にi32になっている
            if signature.return_type != DataType::Unit {
                self.wat_buffer.push_str(&format!(
                    " (result {})",
                    self.type_to_wat(&signature.return_type)
                ));
            }
            self.wat_buffer.push_str("\n");

            let typed_body = self.analyze_and_type_check(body)?;

            // 型チェック済みのASTからローカル変数を探し、WATで宣言する
            self.collect_and_declare_locals(&typed_body);
            
            // 型チェックはソースコード上の本来の型で行う
            if typed_body.data_type != original_return_type {
                 let error_message = format!(
                    "Mismatched return type: expected '{}', but function body returns '{}'",
                    original_return_type,
                    typed_body.data_type
                );
                return Err(LangError::Compile(CompileError::new(error_message, typed_body.span)));
            }

            self.generate_wat_for_node(&typed_body)?;
            
            // main関数は値を返さないので、もし最後の式が値を返すならスタックからdropする
            if func_name == "main" && original_return_type != DataType::Unit {
                self.wat_buffer.push_str("    drop\n");
            }

            self.wat_buffer.push_str("  )\n");

            self.leave_scope();
            Ok(())
        } else {
            unreachable!()
        }
    }

    /// RawASTノードを受け取り、意味解析と型チェックを行ってTypedASTを返す
    fn analyze_and_type_check(&mut self, node: &RawAstNode) -> Result<TypedAstNode, LangError> {
        match node {
            RawAstNode::Expr(parts) => {
                let mut parts_slice = &parts[..];
                let result = self.analyze_sexp_from_slice(&mut parts_slice)?;
                // 式全体を解析した後、未使用の要素が残っていないかチェック
                if !parts_slice.is_empty() {
                    return Err(LangError::Compile(CompileError::new(
                        "Unexpected tokens after expression",
                        parts_slice[0].span(),
                    )));
                }
                Ok(result)
            }
            RawAstNode::LetDef { name, value, span } => {
                let typed_value = self.analyze_and_type_check(value)?;
                
                // 変数テーブルに登録
                self.variable_table.push((
                    name.0.clone(),
                    typed_value.data_type.clone(),
                    self.scope_depth,
                ));

                Ok(TypedAstNode {
                    kind: TypedNodeKind::LetBinding {
                        name: name.0.clone(),
                        value: Box::new(typed_value),
                    },
                    data_type: DataType::Unit,
                    span: *span,
                })
            }
            RawAstNode::IfExpr { condition, then_branch, else_branch, span } => {
                let typed_cond = self.analyze_and_type_check(condition)?;
                if typed_cond.data_type != DataType::Bool {
                    return Err(LangError::Compile(CompileError::new(
                        format!("If condition must be a boolean expression, but found type '{}'", typed_cond.data_type),
                        typed_cond.span
                    )));
                }

                let typed_then = self.analyze_and_type_check(then_branch)?;
                let typed_else = self.analyze_and_type_check(else_branch)?;
                
                if typed_then.data_type != typed_else.data_type {
                    return Err(LangError::Compile(CompileError::new(
                        format!("If branches must have the same type, but found '{}' and '{}'", typed_then.data_type, typed_else.data_type),
                        *span // TODO: better span
                    )));
                }
                
                let expr_type = typed_then.data_type.clone();

                Ok(TypedAstNode {
                    kind: TypedNodeKind::IfExpr { 
                        condition: Box::new(typed_cond), 
                        then_branch: Box::new(typed_then),
                        else_branch: Box::new(typed_else)
                    },
                    data_type: expr_type,
                    span: *span
                })
            }
            RawAstNode::Block { statements, span } => {
                self.enter_scope();
                let mut typed_statements = Vec::new();
                for stmt in statements {
                    typed_statements.push(self.analyze_and_type_check(stmt)?);
                }
                self.leave_scope();

                let last_type = typed_statements
                    .last()
                    .map_or(DataType::Unit, |n| n.data_type.clone());

                let block_span = typed_statements.last().map_or(*span, |n| n.span);

                Ok(TypedAstNode {
                    kind: TypedNodeKind::Block {
                        statements: typed_statements,
                    },
                    data_type: last_type,
                    span: block_span,
                })
            }
            RawAstNode::PrintStmt { value, span } => {
                let typed_value = self.analyze_and_type_check(value)?;
                if typed_value.data_type != DataType::String {
                     return Err(LangError::Compile(CompileError::new(
                        format!("'print' expects a string, but found type '{}'", typed_value.data_type),
                        typed_value.span
                    )));
                }
                Ok(TypedAstNode {
                    kind: TypedNodeKind::PrintStmt { value: Box::new(typed_value) },
                    data_type: DataType::Unit,
                    span: *span,
                })
            }
            RawAstNode::FnDef { .. } => unreachable!("Function definitions are handled at the top level and should not be analyzed as expressions."),
        }
    }
    
    /// RawExprPartのスライスから一つのS式を解析し、消費した分スライスを進める
    fn analyze_sexp_from_slice<'a>(
        &mut self,
        parts: &mut &'a [RawExprPart],
    ) -> Result<TypedAstNode, LangError> {
        if parts.is_empty() {
            return Err(LangError::Compile(CompileError::new(
                "Unexpected end of expression, expected a value or function call.",
                // TODO: A better span would be the end of the last token
                Span::default(), 
            )));
        }

        // スライスの先頭要素を解析の起点とする
        let first_part = &parts[0];
        *parts = &parts[1..]; // 最初の要素を消費

        match first_part {
            // 識別子の場合: 関数呼び出し or 変数参照
            RawExprPart::Token(Token::Identifier(name), span) => {
                // まず関数として解決を試みる
                if let Some(signature) = self.function_table.get(name).cloned() {
                    let mut typed_args = Vec::new();
                    // 必要な数の引数をスライスから再帰的に解析・消費する
                    for i in 0..signature.param_types.len() {
                        let typed_arg = self.analyze_sexp_from_slice(parts)?;
                        let expected_type = &signature.param_types[i];

                        if typed_arg.data_type != *expected_type {
                            let err_msg = format!(
                                "Mismatched type for argument {} of function '{}': expected '{}', but found '{}'",
                                i + 1, name, expected_type, typed_arg.data_type
                            );
                            return Err(LangError::Compile(CompileError::new(err_msg, typed_arg.span)));
                        }
                        typed_args.push(typed_arg);
                    }
                    
                    return Ok(TypedAstNode {
                        kind: TypedNodeKind::FunctionCall { name: name.clone(), args: typed_args },
                        data_type: signature.return_type.clone(),
                        span: *span,
                    });
                }
                // 関数でなければ変数参照として解決
                if let Some((_, var_type, _)) = self.find_variable(name) {
                    return Ok(TypedAstNode {
                        kind: TypedNodeKind::VariableRef(name.clone()),
                        data_type: var_type.clone(),
                        span: *span,
                    });
                }
                // どちらでもなければ未定義エラー
                Err(LangError::Compile(CompileError::new(
                    format!("Undefined function or variable '{}'", name), *span,
                )))
            }
            // その他の単独で完結する値
            RawExprPart::Token(Token::IntLiteral(val), span) => Ok(TypedAstNode {
                kind: TypedNodeKind::Literal(LiteralValue::I32(*val)),
                data_type: DataType::I32,
                span: *span,
            }),
            RawExprPart::Token(Token::FloatLiteral(val), span) => Ok(TypedAstNode {
                kind: TypedNodeKind::Literal(LiteralValue::F64(*val)),
                data_type: DataType::F64,
                span: *span,
            }),
             RawExprPart::Token(Token::True, span) => Ok(TypedAstNode {
                kind: TypedNodeKind::Literal(LiteralValue::Bool(true)),
                data_type: DataType::Bool,
                span: *span,
            }),
            RawExprPart::Token(Token::False, span) => Ok(TypedAstNode {
                kind: TypedNodeKind::Literal(LiteralValue::Bool(false)),
                data_type: DataType::Bool,
                span: *span,
            }),
            RawExprPart::Token(Token::StringLiteral(s), span) => {
                let s_len = s.len() as u32;
                // 容量は2倍確保する
                let s_cap = s_len * 2;

                let header_offset = *self.string_headers.entry(s.clone()).or_insert_with(|| {
                    // データ領域を確保
                    let data_offset = self.static_offset;
                    self.static_offset += s_cap;

                    // ヘッダ領域を確保
                    let header_offset = self.static_offset;
                    self.static_offset += 12; // ptr, len, cap

                    self.string_data.insert(s.clone(), data_offset);
                    header_offset
                });

                Ok(TypedAstNode {
                    kind: TypedNodeKind::StringLiteral { header_offset },
                    data_type: DataType::String,
                    span: *span,
                })
            }
            RawExprPart::MathBlock(math_node, _) => self.analyze_math_node(math_node),
            RawExprPart::Group(inner_parts, _span) => {
                // グループはそれ全体で一つの式として解析する
                let mut inner_slice = &inner_parts[..];
                let result = self.analyze_sexp_from_slice(&mut inner_slice)?;
                if !inner_slice.is_empty() {
                     return Err(LangError::Compile(CompileError::new(
                        "Unexpected tokens after expression in group",
                        inner_slice[0].span(),
                    )));
                }
                Ok(result)
            }
            _ => Err(LangError::Compile(CompileError::new(
                "This token cannot be the start of an expression",
                first_part.span(),
            ))),
        }
    }


    /// 数式のASTノード(MathAstNode)を意味解析・型チェックする
    fn analyze_math_node(&mut self, node: &MathAstNode) -> Result<TypedAstNode, LangError> {
        match node {
            MathAstNode::Literal(literal, span) => match literal {
                MathLiteral::Int(val) => Ok(TypedAstNode {
                    kind: TypedNodeKind::Literal(LiteralValue::I32(*val)),
                    data_type: DataType::I32,
                    span: *span,
                }),
                MathLiteral::Float(val) => Ok(TypedAstNode {
                    kind: TypedNodeKind::Literal(LiteralValue::F64(*val)),
                    data_type: DataType::F64,
                    span: *span,
                }),
            },
            MathAstNode::Variable(name, span) => {
                if let Some((_, var_type, _)) = self.find_variable(name) {
                    Ok(TypedAstNode {
                        kind: TypedNodeKind::VariableRef(name.clone()),
                        data_type: var_type.clone(),
                        span: *span,
                    })
                } else {
                    Err(LangError::Compile(CompileError::new(
                        &format!("Undefined variable '{}' in math expression", name),
                        *span,
                    )))
                }
            }
            MathAstNode::InfixOp { op, left, right, span } => {
                let typed_left = self.analyze_math_node(left)?;
                let typed_right = self.analyze_math_node(right)?;

                if typed_left.data_type != typed_right.data_type {
                    let err_msg = format!(
                        "Mismatched types for binary operator: `{}` and `{}`",
                        typed_left.data_type, typed_right.data_type
                    );
                    return Err(LangError::Compile(CompileError::new(err_msg, *span)));
                }

                let op_type = &typed_left.data_type;
                let (func_name, result_type) = match (op, op_type) {
                    // Arithmetic
                    (Token::Plus, DataType::I32) => ("i32.add", DataType::I32),
                    (Token::Minus, DataType::I32) => ("i32.sub", DataType::I32),
                    (Token::Star, DataType::I32) => ("i32.mul", DataType::I32),
                    (Token::Slash, DataType::I32) => ("i32.div_s", DataType::I32),
                    (Token::Plus, DataType::F64) => ("f64.add", DataType::F64),
                    (Token::Minus, DataType::F64) => ("f64.sub", DataType::F64),
                    (Token::Star, DataType::F64) => ("f64.mul", DataType::F64),
                    (Token::Slash, DataType::F64) => ("f64.div", DataType::F64),
                    // Comparison
                    (Token::EqualsEquals, DataType::I32) => ("i32.eq", DataType::Bool),
                    (Token::BangEquals, DataType::I32) => ("i32.ne", DataType::Bool),
                    (Token::LessThan, DataType::I32) => ("i32.lt_s", DataType::Bool),
                    (Token::LessThanEquals, DataType::I32) => ("i32.le_s", DataType::Bool),
                    (Token::GreaterThan, DataType::I32) => ("i32.gt_s", DataType::Bool),
                    (Token::GreaterThanEquals, DataType::I32) => ("i32.ge_s", DataType::Bool),

                    _ => {
                        return Err(LangError::Compile(CompileError::new(
                            format!("Operator `{:?}` is not supported for type `{}`", op, op_type),
                            *span,
                        )));
                    }
                };

                Ok(TypedAstNode {
                    kind: TypedNodeKind::FunctionCall {
                        name: func_name.to_string(),
                        args: vec![typed_left, typed_right],
                    },
                    data_type: result_type,
                    span: *span,
                })
            }
            MathAstNode::Call { .. } => todo!("Function calls inside math blocks are not yet implemented"),
        }
    }


    /// TypedASTノードからWATコードを生成する
    fn generate_wat_for_node(&mut self, node: &TypedAstNode) -> Result<(), LangError> {
        match &node.kind {
            TypedNodeKind::Literal(val) => {
                let (wat_type, const_val) = match val {
                    LiteralValue::I32(i) => ("i32", i.to_string()),
                    LiteralValue::F64(f) => ("f64", f.to_string()),
                    LiteralValue::Bool(b) => ("i32", if *b { "1" } else { "0" }.to_string()),
                };
                self.wat_buffer.push_str(&format!(
                    "    {}.const {}\n",
                    wat_type,
                    const_val
                ));
            }
            TypedNodeKind::StringLiteral { header_offset } => {
                self.wat_buffer.push_str(&format!("    i32.const {}\n", header_offset));
            }
            TypedNodeKind::VariableRef(name) => {
                self.wat_buffer
                    .push_str(&format!("    local.get ${}\n", name));
            }
            TypedNodeKind::FunctionCall { name, args } => {
                for arg in args {
                    self.generate_wat_for_node(arg)?;
                }
                // `i32.add`のような組み込み命令か、ユーザー定義関数(`call`)かを判定
                if name.contains('.') {
                    self.wat_buffer.push_str(&format!("    {}\n", name));
                } else {
                    self.wat_buffer
                        .push_str(&format!("    call ${}\n", name));
                }
            }
            TypedNodeKind::LetBinding { name, value } => {
                self.generate_wat_for_node(value)?;
                self.wat_buffer
                    .push_str(&format!("    local.set ${}\n", name));
            }
            TypedNodeKind::IfExpr { condition, then_branch, else_branch } => {
                self.generate_wat_for_node(condition)?;
                self.wat_buffer.push_str(&format!("    (if (result {})\n", self.type_to_wat(&node.data_type)));
                
                self.wat_buffer.push_str("      (then\n");
                self.generate_wat_for_node(then_branch)?;
                self.wat_buffer.push_str("      )\n");

                self.wat_buffer.push_str("      (else\n");
                self.generate_wat_for_node(else_branch)?;
                self.wat_buffer.push_str("      )\n");

                self.wat_buffer.push_str("    )\n");
            }
            TypedNodeKind::Block { statements } => {
                if statements.is_empty() {
                    // Unitを返す空ブロックの場合、何も生成しない
                    if node.data_type != DataType::Unit {
                        return Err(LangError::Compile(CompileError::new(
                            format!("Empty block must have type '()' but found '{}'", node.data_type),
                             node.span,
                        )));
                    }
                    return Ok(());
                }
                for (i, stmt) in statements.iter().enumerate() {
                    self.generate_wat_for_node(stmt)?;
                    // ブロックの最後の式でなければ、スタックから値をポップする
                    if i < statements.len() - 1 && stmt.data_type != DataType::Unit {
                        self.wat_buffer.push_str("    drop\n");
                    }
                }
            }
            TypedNodeKind::PrintStmt { value } => {
                // --- iovec.bufの書き込み ---
                self.wat_buffer.push_str("    i32.const 0 ;; iovec addr\n");
                self.generate_wat_for_node(value)?; // -> [iovec_addr, header_ptr]
                self.wat_buffer.push_str("    i32.load offset=0 ;; load data_ptr\n"); // -> [iovec_addr, data_ptr]
                self.wat_buffer.push_str("    i32.store\n");

                // --- iovec.buf_lenの書き込み ---
                self.wat_buffer.push_str("    i32.const 4 ;; iovec addr + 4\n");
                self.generate_wat_for_node(value)?; // -> [iovec_addr+4, header_ptr]
                self.wat_buffer.push_str("    i32.load offset=4 ;; load data_len\n"); // -> [iovec_addr+4, data_len]
                self.wat_buffer.push_str("    i32.store\n");
                
                // --- fd_writeの呼び出し ---
                self.wat_buffer.push_str("    i32.const 1  ;; fd (stdout)\n");
                self.wat_buffer.push_str("    i32.const 0  ;; iovec_ptr\n");
                self.wat_buffer.push_str("    i32.const 1  ;; iovec_len\n");
                self.wat_buffer.push_str("    i32.const 16 ;; nwritten_ptr (iovecの後ろ)\n");
                self.wat_buffer.push_str("    call $fd_write\n");
                self.wat_buffer.push_str("    drop\n");
            }
        }
        Ok(())
    }

    // --- ヘルパー関数 ---

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }
    fn leave_scope(&mut self) {
        self.variable_table
            .retain(|(_, _, depth)| *depth < self.scope_depth);
        self.scope_depth -= 1;
    }

    fn find_variable(&self, name: &str) -> Option<&(String, DataType, usize)> {
        self.variable_table
            .iter()
            .rev()
            .find(|(var_name, _, _)| var_name == name)
    }

    fn string_to_type(&self, s: &str, span: Span) -> Result<DataType, LangError> {
        match s {
            "i32" => Ok(DataType::I32),
            "f64" => Ok(DataType::F64),
            "bool" => Ok(DataType::Bool),
            "string" => Ok(DataType::String),
            "()" => Ok(DataType::Unit),
            _ => Err(LangError::Compile(CompileError::new(
                &format!("Unknown type '{}'", s),
                span,
            ))),
        }
    }

    fn type_to_wat(&self, t: &DataType) -> &str {
        match t {
            DataType::I32 => "i32",
            DataType::F64 => "f64",
            DataType::Bool => "i32", // Wasm represents bools as i32 (0 or 1)
            DataType::String => "i32", // Strings are pointers to headers
            DataType::Unit => "",
        }
    }
}