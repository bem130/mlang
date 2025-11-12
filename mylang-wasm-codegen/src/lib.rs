//! mylangのTypedASTを受け取り、WebAssembly Text Format (WAT) を生成するコードジェネレーター。

#![no_std]
extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use mylang_core::ast::{DataType, TypedAstNode};
use mylang_core::error::LangError;

mod code_generator;

/// WATコード生成の状態を管理する構造体。
pub struct WasmGenerator {
    wat_buffer: String,
    // アナライザーから受け取った静的文字列の情報 (data_offset, header_offset)
    string_headers: BTreeMap<String, (u32, u32)>,
    // 静的データの終端を示すオフセット
    static_offset: u32,
    temp_local_counter: usize,
    match_value_locals: BTreeMap<usize, (String, DataType)>,
    tuple_temp_locals: BTreeMap<usize, String>,
}

impl WasmGenerator {
    /// 新しいWasmGeneratorを生成する。
    pub fn new(
        _function_table: BTreeMap<String, mylang_core::compiler::FunctionSignature>,
        string_headers: BTreeMap<String, (u32, u32)>,
        static_offset: u32,
    ) -> Self {
        Self {
            wat_buffer: String::new(),
            string_headers,
            static_offset,
            temp_local_counter: 0,
            match_value_locals: BTreeMap::new(),
            tuple_temp_locals: BTreeMap::new(),
        }
    }

    /// TypedASTを受け取り、完全なWAT文字列を生成するメイン関数。
    pub fn generate(&mut self, ast: &[TypedAstNode]) -> Result<String, LangError> {
        self.wat_buffer.clear();
        self.wat_buffer.push_str("(module\n");
        self.wat_buffer.push_str("  (import \"wasi_snapshot_preview1\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))\n");
        self.wat_buffer
            .push_str("  (memory (export \"memory\") 1)\n");
        self.wat_buffer.push_str(&format!(
            "  (global $heap_ptr (mut i32) (i32.const {})) ;; Static data ends here\n",
            self.static_offset
        ));

        self.generate_static_data();

        code_generator::generate_builtin_helpers(self);

        for node in ast {
            code_generator::generate(self, node)?;
        }

        self.wat_buffer
            .push_str("\n  (export \"_start\" (func $main))\n");
        self.wat_buffer.push_str(")\n");

        Ok(self.wat_buffer.clone())
    }

    /// 静的データセクション（主に文字列リテラル）を生成する。
    fn generate_static_data(&mut self) {
        if self.string_headers.is_empty() {
            return;
        }
        self.wat_buffer
            .push_str("\n  ;; --- Static Data (String Literals) ---\n");

        // データオフセットでソートするためにVecに変換
        let mut sorted_strings: Vec<_> = self.string_headers.iter().collect();
        sorted_strings.sort_by_key(|&(_, (data_offset, _))| data_offset);

        for (s, (data_offset, header_offset)) in sorted_strings {
            let s_escaped = s
                .chars()
                .map(|c| {
                    if c.is_ascii() && !c.is_control() && c != '\\' && c != '\"' {
                        c.to_string()
                    } else {
                        // WATの文字列リテラル形式に従ってエスケープ
                        format!("\\{:02x}", c as u32)
                    }
                })
                .collect::<String>();

            let s_len = s.len() as u32;

            self.wat_buffer.push_str(&format!(
                "  (data (i32.const {}) \"{}\") ;; len={}\n",
                data_offset, s_escaped, s_len
            ));

            // 整数をリトルエンディアンのバイト列に変換し、エスケープ文字列としてフォーマットする
            let ptr_bytes = data_offset
                .to_le_bytes()
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>();
            let len_bytes = s_len
                .to_le_bytes()
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>();
            let cap_bytes = s_len
                .to_le_bytes()
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(); // cap=len

            self.wat_buffer.push_str(&format!(
                "  (data (i32.const {}) \"{}\") ;; header: ptr\n",
                header_offset, ptr_bytes
            ));
            self.wat_buffer.push_str(&format!(
                "  (data (i32.const {}) \"{}\") ;; header: len\n",
                header_offset + 4,
                len_bytes
            ));
            self.wat_buffer.push_str(&format!(
                "  (data (i32.const {}) \"{}\") ;; header: cap\n",
                header_offset + 8,
                cap_bytes
            ));
        }
        self.wat_buffer
            .push_str("  ;; --- End of Static Data ---\n");
    }

    /// DataTypeをWATでの型名に変換する。
    pub fn type_to_wat(&self, data_type: &DataType) -> &'static str {
        match data_type {
            DataType::I32 => "i32",
            DataType::F64 => "f64",
            DataType::Bool => "i32",   // boolはi32として扱う
            DataType::String => "i32", // stringはヘッダポインタ(i32)として扱う
            DataType::Unit => "",      // Unitは値を返さない
            DataType::Tuple(_) | DataType::Struct(_) | DataType::Enum(_) => "i32",
        }
    }

    pub(crate) fn next_temp_index(&mut self) -> usize {
        let idx = self.temp_local_counter;
        self.temp_local_counter += 1;
        idx
    }

    pub(crate) fn scrutinee_local_for(
        &mut self,
        expr_ptr: usize,
        data_type: &DataType,
    ) -> (String, DataType) {
        if let Some(existing) = self.match_value_locals.get(&expr_ptr) {
            return existing.clone();
        }
        let name = format!("__match_scrutinee_{}", self.next_temp_index());
        let entry = (name.clone(), data_type.clone());
        self.match_value_locals.insert(expr_ptr, entry.clone());
        entry
    }

    pub(crate) fn tuple_temp_local_for(
        &mut self,
        expr_ptr: usize,
        _data_type: &DataType,
    ) -> String {
        if let Some(existing) = self.tuple_temp_locals.get(&expr_ptr) {
            return existing.clone();
        }
        let name = format!("__tuple_tmp_{}", self.next_temp_index());
        self.tuple_temp_locals.insert(expr_ptr, name.clone());
        name
    }

    pub(crate) fn clear_temp_state(&mut self) {
        self.temp_local_counter = 0;
        self.match_value_locals.clear();
        self.tuple_temp_locals.clear();
    }
}
