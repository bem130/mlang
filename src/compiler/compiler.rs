//! RawASTを受け取り、意味解析、型チェック、コード生成を行うコンパイラ。

use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::span::Span;
use crate::token::Token;
use std::collections::HashMap;

/// コンパイル処理全体で共有される状態を管理する構造体。
pub struct Compiler {
    // WATコードの生成結果を保持する
    pub wat_buffer: String,
    // 現在のスコープの深さ
    pub scope_depth: usize,
    // 関数名とそのシグネチャを保持するテーブル
    pub function_table: HashMap<String, FunctionSignature>,
    // 変数名とその型、スコープ深度を保持するテーブル
    pub variable_table: Vec<(String, DataType, usize)>,
    // 文字列リテラルとそのデータオフセットのマッピング
    pub string_data: HashMap<String, u32>,
    // 文字列リテラルに対応するヘッダのオフセット
    pub string_headers: HashMap<String, u32>,
    // 次に静的データを配置する線形メモリのオフセット
    pub static_offset: u32,
}

/// 関数のシグネチャ情報
#[derive(Clone)]
pub struct FunctionSignature {
    pub param_types: Vec<DataType>,
    pub return_type: DataType,
}

impl Compiler {
    pub fn new() -> Self {
        let mut function_table = HashMap::new();
        // 組み込み関数を登録
        function_table.insert(
            "string_concat".to_string(),
            FunctionSignature {
                param_types: vec![DataType::String, DataType::String],
                return_type: DataType::String,
            },
        );

        Self {
            wat_buffer: String::new(),
            scope_depth: 0,
            function_table,
            variable_table: Vec::new(),
            string_data: HashMap::new(),
            string_headers: HashMap::new(),
            // メモリの先頭はiovec等で使うため、少し余裕をもたせる
            static_offset: 32,
        }
    }

    /// コンパイルのメインエントリーポイント
    pub fn compile(&mut self, ast: &[RawAstNode]) -> Result<String, LangError> {
        self.prepass_declarations(ast)?;

        // --- フェーズ1: 意味解析のみを行い、静的データを収集 ---
        let typed_ast: Vec<TypedAstNode> = ast
            .iter()
            .filter(|node| matches!(node, RawAstNode::FnDef { .. }))
            .map(|node| self.analyze_and_type_check(node))
            .collect::<Result<_, _>>()?;

        // --- フェーズ2: コード生成 ---
        self.wat_buffer.push_str("(module\n");
        self.wat_buffer.push_str("  (import \"wasi_snapshot_preview1\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))\n");
        self.wat_buffer.push_str("  (memory (export \"memory\") 1)\n");
        // ヒープポインタを静的領域の末尾で初期化
        self.wat_buffer.push_str(&format!("  (global $heap_ptr (mut i32) (i32.const {}))\n", self.static_offset));
        
        self.generate_builtin_helpers();

        // 型チェック済みのASTを使ってコード生成
        for node in &typed_ast {
            self.compile_function_def(node)?;
        }

        if self.function_table.contains_key("main") {
            self.wat_buffer.push_str("  (export \"_start\" (func $main))\n");
        }
        
        self.generate_data_sections();

        self.wat_buffer.push_str(")\n");
        Ok(self.wat_buffer.clone())
    }
    
    /// 文字列リテラルのデータセクションとヘッダセクションを生成
    fn generate_data_sections(&mut self) {
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
            let cap = len * 2;

            let mut header_bytes = Vec::new();
            header_bytes.extend_from_slice(&data_offset.to_le_bytes());
            header_bytes.extend_from_slice(&len.to_le_bytes());
            header_bytes.extend_from_slice(&cap.to_le_bytes());
            
            let header_data_str = header_bytes.iter().map(|b| format!("\\{:02x}", b)).collect::<String>();
            let escaped_comment = s.escape_default().to_string();

            self.wat_buffer.push_str(&format!("  (data (i32.const {}) \"{}\") ;; Ptr, Len, Cap for \"{}\"\n", header_offset, header_data_str, escaped_comment));
        }
    }

    /// 1パス目: 関数宣言を収集し、シグネチャをテーブルに登録する
    fn prepass_declarations(&mut self, ast: &[RawAstNode]) -> Result<(), LangError> {
        for node in ast {
            if let RawAstNode::FnDef { name, params, return_type, .. } = node {
                let func_name = &name.0;
                let param_types = params.iter().map(|(_, type_info)| self.string_to_type(&type_info.0, type_info.1)).collect::<Result<Vec<_>, _>>()?;
                let ret_type = match return_type {
                    Some(rt) => self.string_to_type(&rt.0, rt.1)?,
                    None => DataType::Unit,
                };
                self.function_table.insert(func_name.clone(), FunctionSignature { param_types, return_type: ret_type });
            }
        }
        Ok(())
    }

    /// `TypedAstNode`から関数定義をコンパイルする
    fn compile_function_def(&mut self, node: &TypedAstNode) -> Result<(), LangError> {
        if let TypedNodeKind::Block { .. } = &node.kind { // body is always a block
            let func_name = if let Some((name, _)) = self.variable_table.first() { name } else { "" }; // Hacky way to get func name
            
            self.enter_scope(); // Re-enter scope for params
            // This is complex because params are part of the outer scope in analysis, but local scope in codegen
            // For now, we assume `compile` calls this in order and `variable_table` is not yet cleared.
            
            self.wat_buffer.push_str(&format!("  (func ${}\n", "main")); // FIXME: Needs proper name
            
            self.generate_wat_for_node(node)?;

            self.wat_buffer.push_str("  )\n");
            self.leave_scope();
            Ok(())
        } else {
             Ok(()) // Not a FnDef
        }
    }
}

// ... Rest of the file needs to be in analyzer/code_generator ...
// This file is becoming a placeholder for the top-level orchestration.
// The actual implementation of `analyze` and `generate` should be in their respective modules.
// The provided snippet is incomplete and needs the full modularized code.