//! RawASTを受け取り、意味解析、型チェック、コード生成を行うコンパイラ。

// サブモジュールを宣言
pub mod analyzer;
pub mod code_generator;

use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::span::Span;
use std::collections::HashMap;

/// コンパイル処理全体で共有される状態を管理する構造体。
pub struct Compiler {
    pub wat_buffer: String,
    pub scope_depth: usize,
    pub function_table: HashMap<String, FunctionSignature>,
    // (ソース名, ユニーク名, 型, スコープ深度)
    pub variable_table: Vec<(String, String, DataType, usize)>,
    // 各変数名のカウンター
    pub var_counters: HashMap<String, u32>,
    pub string_data: HashMap<String, u32>,
    pub string_headers: HashMap<String, u32>,
    pub static_offset: u32,
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub param_types: Vec<DataType>,
    pub return_type: DataType,
}

impl Compiler {
    pub fn new() -> Self {
        let mut function_table = HashMap::new();
        function_table.insert(
            "string_concat".to_string(),
            FunctionSignature {
                param_types: vec![DataType::String, DataType::String],
                return_type: DataType::String,
            },
        );
        function_table.insert(
            "i32_to_string".to_string(),
            FunctionSignature {
                param_types: vec![DataType::I32],
                return_type: DataType::String,
            },
        );
        function_table.insert(
            "f64_to_string".to_string(),
            FunctionSignature {
                param_types: vec![DataType::F64],
                return_type: DataType::String,
            },
        );

        let mut compiler = Self {
            wat_buffer: String::new(),
            scope_depth: 0,
            function_table,
            variable_table: Vec::new(),
            var_counters: HashMap::new(),
            string_data: HashMap::new(),
            string_headers: HashMap::new(),
            static_offset: 32,
        };
        
        // printlnが内部的に使用する改行文字を静的領域に事前登録しておく
        // これにより、ヒープ開始アドレスが正しく計算される
        compiler.ensure_string_is_statically_allocated(&"\n".to_string());

        compiler
    }

    /// コンパイルのメインエントリーポイント
    pub fn compile(&mut self, ast: &[RawAstNode]) -> Result<String, LangError> {
        self.prepass_declarations(ast)?;
        
        // --- フェーズ1: 意味解析 ---
        let typed_ast: Vec<TypedAstNode> = ast
            .iter()
            .map(|node| analyzer::analyze_toplevel(self, node))
            .collect::<Result<_, _>>()?;

        // --- フェーズ2: コード生成 ---
        self.wat_buffer.push_str("(module\n");
        self.wat_buffer.push_str("  (import \"wasi_snapshot_preview1\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))\n");
        self.wat_buffer.push_str("  (memory (export \"memory\") 1)\n");
        self.wat_buffer.push_str(&format!("  (global $heap_ptr (mut i32) (i32.const {})) ;; Heap starts after static data\n", self.static_offset));
        
        code_generator::generate_builtin_helpers(self);

        for node in &typed_ast {
            code_generator::generate(self, node)?;
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
        self.wat_buffer.push_str("\n  ;; --- Static Data Sections ---\n");
        let mut sorted_data: Vec<_> = self.string_data.iter().collect();
        sorted_data.sort_by_key(|&(_, offset)| offset);
        for (s, offset) in sorted_data {
            let escaped_s = s.escape_default().to_string();
            self.wat_buffer.push_str(&format!("  (data (i32.const {}) \"{}\")\n", offset, escaped_s));
        }

        let mut sorted_headers: Vec<_> = self.string_headers.iter().collect();
        sorted_headers.sort_by_key(|&(_, offset)| offset);
        for (s, header_offset) in sorted_headers {
            let data_offset = self.string_data.get(s).unwrap();
            let len = s.len() as u32;
            let cap = len; // 静的文字列なので容量と長さは同じ

            let mut header_bytes = Vec::new();
            header_bytes.extend_from_slice(&data_offset.to_le_bytes());
            header_bytes.extend_from_slice(&len.to_le_bytes());
            header_bytes.extend_from_slice(&cap.to_le_bytes());
            
            let header_data_str = header_bytes.iter().map(|b| format!("\\{:02x}", b)).collect::<String>();
            let escaped_comment = s.escape_default().to_string();

            self.wat_buffer.push_str(&format!("  (data (i32.const {}) \"{}\") ;; String Header for \"{}\"\n", header_offset, header_data_str, escaped_comment));
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

    pub fn enter_scope(&mut self) { self.scope_depth += 1; }
    pub fn leave_scope(&mut self) {
        self.variable_table.retain(|(_, _, _, depth)| *depth < self.scope_depth);
        self.scope_depth -= 1;
    }
    pub fn find_variable(&self, name: &str) -> Option<&(String, String, DataType, usize)> {
        self.variable_table.iter().rev().find(|(var_name, _, _, _)| var_name == name)
    }
    pub fn string_to_type(&self, s: &str, span: Span) -> Result<DataType, LangError> {
        match s {
            "i32" => Ok(DataType::I32), "f64" => Ok(DataType::F64), "bool" => Ok(DataType::Bool),
            "string" => Ok(DataType::String), "()" => Ok(DataType::Unit),
            _ => Err(LangError::Compile(CompileError::new(format!("Unknown type '{}'", s), span))),
        }
    }
    pub fn type_to_wat(&self, t: &DataType) -> &str {
        match t {
            DataType::I32 => "i32", DataType::F64 => "f64",
            DataType::Bool => "i32", DataType::String => "i32",
            DataType::Unit => "",
        }
    }

    /// 文字列リテラルを静的領域に確保し、そのヘッダオフセットを返す
    pub fn ensure_string_is_statically_allocated(&mut self, s: &String) -> u32 {
        // 既に登録されていれば、そのオフセットを返す
        if let Some(header_offset) = self.string_headers.get(s) {
            return *header_offset;
        }

        // --- 新しい文字列なので、データとヘッダの両方をアロケートする ---

        let s_len = s.len() as u32;

        // 1. データ領域を確保
        let data_offset = self.static_offset;
        self.static_offset += s_len;
        // 次の確保が4バイト境界から始まるように調整
        if self.static_offset % 4 != 0 {
            self.static_offset += 4 - (self.static_offset % 4);
        }

        // 2. ヘッダ領域を確保
        let header_offset = self.static_offset;
        self.static_offset += 12; // ptr(4) + len(4) + cap(4)

        // 3. マップにオフセットを記録
        self.string_data.insert(s.clone(), data_offset);
        self.string_headers.insert(s.clone(), header_offset);

        header_offset
    }
}