//! RawASTを受け取り、意味解析、型チェックを行うアナライザー。

extern crate alloc;
// サブモジュールを宣言
pub mod analyzer;

use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::span::Span;
use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

/// 意味解析処理全体を管理し、状態を保持する構造体。
/// 1. **宣言収集 (Pre-pass)**: `prepass_declarations`で全関数シグネチャを収集。
/// 2. **意味解析 (Analyze)**: `analyzer`モジュールがRawASTをTypedASTに変換。
///    - 名前解決、型チェック、ASTの構造変換を行う。
pub struct Analyzer {
    pub scope_depth: usize,
    pub function_table: BTreeMap<String, FunctionSignature>,
    // (ソース名, ユニーク名, 型, スコープ深度)
    pub variable_table: Vec<(String, String, DataType, usize)>,
    // 各変数名のカウンター
    pub var_counters: BTreeMap<String, u32>,
    // アナライザーは静的文字列のオフセットを計算する必要があるため、
    // これらのフィールドは残すが、コード生成は行わない。
    pub string_headers: BTreeMap<String, (u32, u32)>, // (data_offset, header_offset)
    pub static_offset: u32,
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub param_types: Vec<DataType>,
    pub return_type: DataType,
    pub definition_span: Span,
}

impl Analyzer {
    pub fn new() -> Self {
        let mut function_table = BTreeMap::new();
        // --- 標準組み込み関数を登録 ---
        function_table.insert(
            "string_concat".to_string(),
            FunctionSignature {
                param_types: alloc::vec![DataType::String, DataType::String],
                return_type: DataType::String,
                definition_span: Span::default(),
            },
        );
        function_table.insert(
            "i32_to_string".to_string(),
            FunctionSignature {
                param_types: alloc::vec![DataType::I32],
                return_type: DataType::String,
                definition_span: Span::default(),
            },
        );
        function_table.insert(
            "f64_to_string".to_string(),
            FunctionSignature {
                param_types: alloc::vec![DataType::F64],
                return_type: DataType::String,
                definition_span: Span::default(),
            },
        );
        function_table.insert(
            "print".to_string(),
            FunctionSignature {
                param_types: alloc::vec![DataType::String],
                return_type: DataType::Unit,
                definition_span: Span::default(),
            },
        );
        function_table.insert(
            "println".to_string(),
            FunctionSignature {
                param_types: alloc::vec![DataType::String],
                return_type: DataType::Unit,
                definition_span: Span::default(),
            },
        );

        let mut analyzer = Self {
            scope_depth: 0,
            function_table,
            variable_table: Vec::new(),
            var_counters: BTreeMap::new(),
            string_headers: BTreeMap::new(),
            // メモリの先頭32バイトはIO用に予約
            static_offset: 32,
        };

        // printlnが内部的に使用する改行文字を静的領域に事前登録しておく
        analyzer.ensure_string_is_statically_allocated(&"\n".to_string());

        analyzer
    }

    /// 解析のメインエントリーポイント
    pub fn analyze(&mut self, ast: &[RawAstNode]) -> Result<Vec<TypedAstNode>, LangError> {
        self.prepass_declarations(ast)?;

        // --- 意味解析 ---
        let typed_ast: Vec<TypedAstNode> = ast
            .iter()
            .map(|node| analyzer::analyze_toplevel(self, node))
            .collect::<Result<_, _>>()?;

        Ok(typed_ast)
    }

    /// 1パス目: 関数宣言を収集し、シグネチャをテーブルに登録する
    fn prepass_declarations(&mut self, ast: &[RawAstNode]) -> Result<(), LangError> {
        for node in ast {
            if let RawAstNode::FnDef {
                name,
                params,
                return_type,
                span,
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
                if self.function_table.contains_key(func_name) {
                    // 組み込み関数は上書きできない
                    return Err(LangError::Compile(CompileError::new(
                        format!("Function '{}' is a built-in function and cannot be redefined", func_name),
                        name.1,
                    )));
                }
                self.function_table.insert(
                    func_name.clone(),
                    FunctionSignature {
                        param_types,
                        return_type: ret_type,
                        definition_span: *span,
                    },
                );
            }
        }
        Ok(())
    }

    pub fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }
    pub fn leave_scope(&mut self) {
        self.variable_table
            .retain(|(_, _, _, depth)| *depth < self.scope_depth);
        self.scope_depth -= 1;
    }
    pub fn find_variable(&self, name: &str) -> Option<&(String, String, DataType, usize)> {
        self.variable_table
            .iter()
            .rev()
            .find(|(var_name, _, _, _)| var_name == name)
    }
    pub fn string_to_type(&self, s: &str, span: Span) -> Result<DataType, LangError> {
        match s {
            "i32" => Ok(DataType::I32),
            "f64" => Ok(DataType::F64),
            "bool" => Ok(DataType::Bool),
            "string" => Ok(DataType::String),
            "()" => Ok(DataType::Unit),
            _ => Err(LangError::Compile(CompileError::new(
                format!("Unknown type '{}'", s),
                span,
            ))),
        }
    }

    /// 文字列リテラルを静的領域に確保し、そのヘッダオフセットを返す
    pub fn ensure_string_is_statically_allocated(&mut self, s: &String) -> u32 {
        if let Some((_data_offset, header_offset)) = self.string_headers.get(s) {
            return *header_offset;
        }
        let s_len = s.len() as u32;
        let data_offset = self.static_offset;
        self.static_offset += s_len;
        if self.static_offset % 4 != 0 {
            self.static_offset += 4 - (self.static_offset % 4);
        }
        let header_offset = self.static_offset;
        self.static_offset += 12; // ptr(4) + len(4) + cap(4)
        self.string_headers
            .insert(s.clone(), (data_offset, header_offset));
        header_offset
    }
}