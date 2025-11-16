//! RawASTを受け取り、意味解析、型チェックを行うアナライザー。

extern crate alloc;
// サブモジュールを宣言
pub mod analyzer;

use crate::ast::*;
use crate::error::{CompileError, LangError};
use crate::span::Span;
use alloc::boxed::Box;
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
    pub function_table: BTreeMap<String, Vec<FunctionSignature>>,
    pub variable_table: Vec<VariableEntry>,
    // 各変数名のカウンター
    pub var_counters: BTreeMap<String, u32>,
    // アナライザーは静的文字列のオフセットを計算する必要があるため、
    // これらのフィールドは残すが、コード生成は行わない。
    pub string_headers: BTreeMap<String, (u32, u32)>, // (data_offset, header_offset)
    pub static_offset: u32,
    pub struct_table: BTreeMap<String, StructInfo>,
    pub enum_table: BTreeMap<String, EnumInfo>,
}

#[derive(Clone)]
pub struct VariableEntry {
    pub original_name: String,
    pub unique_name: String,
    pub data_type: DataType,
    pub scope_depth: usize,
    pub is_mutable: bool,
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub param_types: Vec<DataType>,
    pub return_type: DataType,
    pub definition_span: Span,
}

#[derive(Clone)]
pub struct StructInfo {
    pub fields: BTreeMap<String, DataType>,
    pub span: Span,
}

#[derive(Clone)]
pub struct EnumInfo {
    pub variants: BTreeMap<String, EnumVariantInfo>,
    pub span: Span,
}

#[derive(Clone)]
pub struct EnumVariantInfo {
    pub field_types: Vec<DataType>,
    pub span: Span,
}

impl Analyzer {
    pub fn new() -> Self {
        let mut function_table: BTreeMap<String, Vec<FunctionSignature>> = BTreeMap::new();
        {
            let mut register_builtin =
                |name: &str, param_types: Vec<DataType>, return_type: DataType| {
                    function_table
                        .entry(name.to_string())
                        .or_default()
                        .push(FunctionSignature {
                            name: name.to_string(),
                            param_types,
                            return_type,
                            definition_span: Span::default(),
                        });
                };

            // --- 標準組み込み関数を登録 ---
            register_builtin(
                "string_concat",
                alloc::vec![DataType::String, DataType::String],
                DataType::String,
            );
            register_builtin(
                "i32_to_string",
                alloc::vec![DataType::I32],
                DataType::String,
            );
            register_builtin(
                "f64_to_string",
                alloc::vec![DataType::F64],
                DataType::String,
            );
            register_builtin("print", alloc::vec![DataType::String], DataType::Unit);
            register_builtin("println", alloc::vec![DataType::String], DataType::Unit);
            register_builtin("read_line", alloc::vec![], DataType::String);
            register_builtin(
                "string_char_at",
                alloc::vec![DataType::String, DataType::I32],
                DataType::String,
            );
        }

        let mut analyzer = Self {
            scope_depth: 0,
            function_table,
            variable_table: Vec::new(),
            var_counters: BTreeMap::new(),
            string_headers: BTreeMap::new(),
            // メモリの先頭32バイトはIO用に予約
            static_offset: 32,
            struct_table: BTreeMap::new(),
            enum_table: BTreeMap::new(),
        };

        // printlnが内部的に使用する改行文字を静的領域に事前登録しておく
        analyzer.ensure_string_is_statically_allocated(&"\n".to_string());

        analyzer.register_vec_builtins("i32", DataType::I32);

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
        // まず構造体と列挙体の宣言を登録
        for node in ast {
            match node {
                RawAstNode::StructDef { name, fields, span } => {
                    self.register_struct(name, fields, *span)?;
                }
                RawAstNode::EnumDef {
                    name,
                    variants,
                    span,
                } => {
                    self.register_enum(name, variants, *span)?;
                }
                _ => {}
            }
        }

        // 次に let hoist 束縛（関数定義）を収集する
        for node in ast {
            if let RawAstNode::LetHoist {
                name,
                value,
                span: _,
            } = node
            {
                if let RawAstNode::Lambda {
                    params,
                    return_type,
                    ..
                } = &**value
                {
                    let func_name = &name.0;
                    let param_types = params
                        .iter()
                        .map(|(_, type_info)| self.string_to_type(&type_info.0, type_info.1))
                        .collect::<Result<Vec<_>, _>>()?;

                    let ret_type = self.string_to_type(&return_type.0, return_type.1)?;

                    let entry = self.function_table.entry(func_name.clone()).or_default();
                    if let Some(existing) = entry.iter().find(|signature| {
                        signature.param_types == param_types && signature.return_type == ret_type
                    }) {
                        return Err(LangError::Compile(
                            CompileError::new(
                                format!(
                                    "Function overload '{}' with the same signature is already defined",
                                    func_name
                                ),
                                name.1,
                            )
                            .with_note(
                                "Previous definition is located here",
                                existing.definition_span,
                            ),
                        ));
                    }
                    entry.push(FunctionSignature {
                        name: func_name.clone(),
                        param_types,
                        return_type: ret_type,
                        definition_span: name.1,
                    });
                }
            }
        }
        Ok(())
    }

    fn register_struct(
        &mut self,
        name: &(String, Span),
        fields: &[crate::ast::RawStructField],
        span: Span,
    ) -> Result<(), LangError> {
        if self.struct_table.contains_key(&name.0)
            || self.enum_table.contains_key(&name.0)
            || self.function_table.contains_key(&name.0)
        {
            return Err(LangError::Compile(CompileError::new(
                format!("Type or function '{}' is already defined", name.0),
                name.1,
            )));
        }

        self.struct_table.insert(
            name.0.clone(),
            StructInfo {
                fields: BTreeMap::new(),
                span,
            },
        );

        let mut field_map = BTreeMap::new();
        for field in fields {
            if field_map.contains_key(&field.name.0) {
                return Err(LangError::Compile(CompileError::new(
                    format!("Duplicate field '{}' in struct '{}'", field.name.0, name.0),
                    field.name.1,
                )));
            }
            let field_type = self.string_to_type(&field.type_name.0, field.type_name.1)?;
            field_map.insert(field.name.0.clone(), field_type);
        }

        if let Some(info) = self.struct_table.get_mut(&name.0) {
            info.fields = field_map;
            info.span = span;
        }

        Ok(())
    }

    fn register_enum(
        &mut self,
        name: &(String, Span),
        variants: &[crate::ast::RawEnumVariant],
        span: Span,
    ) -> Result<(), LangError> {
        if self.enum_table.contains_key(&name.0)
            || self.struct_table.contains_key(&name.0)
            || self.function_table.contains_key(&name.0)
        {
            return Err(LangError::Compile(CompileError::new(
                format!("Type or function '{}' is already defined", name.0),
                name.1,
            )));
        }

        self.enum_table.insert(
            name.0.clone(),
            EnumInfo {
                variants: BTreeMap::new(),
                span,
            },
        );

        let mut variant_map = BTreeMap::new();
        for variant in variants {
            if variant_map.contains_key(&variant.name.0) {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Duplicate variant '{}' in enum '{}'",
                        variant.name.0, name.0
                    ),
                    variant.name.1,
                )));
            }
            let mut field_types = Vec::new();
            if let crate::ast::RawEnumVariantKind::Tuple(raw_fields) = &variant.kind {
                for field in raw_fields {
                    let ty = self.string_to_type(&field.0, field.1)?;
                    field_types.push(ty);
                }
            }
            variant_map.insert(
                variant.name.0.clone(),
                EnumVariantInfo {
                    field_types,
                    span: variant.span,
                },
            );
        }

        if let Some(info) = self.enum_table.get_mut(&name.0) {
            info.variants = variant_map;
            info.span = span;
        }

        Ok(())
    }

    pub fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }
    pub fn leave_scope(&mut self) {
        self.variable_table
            .retain(|entry| entry.scope_depth < self.scope_depth);
        self.scope_depth -= 1;
    }
    pub fn find_variable(&self, name: &str) -> Option<&VariableEntry> {
        self.variable_table
            .iter()
            .rev()
            .find(|entry| entry.original_name == name)
    }

    /// 型パラメータ文字列を、トップレベルのカンマで分割するヘルパー関数。
    /// `(i32, (i32) -> i32)` のようなネストした型を正しく扱える。
    fn split_type_params(&self, s: &str, span: Span) -> Result<Vec<String>, LangError> {
        if s.trim().is_empty() {
            return Ok(Vec::new());
        }
        let mut parts = Vec::new();
        let mut balance = 0;
        let mut start = 0;
        for (i, c) in s.char_indices() {
            match c {
                '(' | '<' => balance += 1,
                ')' | '>' => {
                    if balance == 0 {
                        return Err(LangError::Compile(CompileError::new(
                            "Mismatched parentheses/brackets in type signature",
                            span,
                        )));
                    }
                    balance -= 1;
                }
                ',' if balance == 0 => {
                    parts.push(s[start..i].trim().to_string());
                    start = i + 1;
                }
                _ => {}
            }
        }
        if balance != 0 {
            return Err(LangError::Compile(CompileError::new(
                "Mismatched parentheses/brackets in type signature",
                span,
            )));
        }
        parts.push(s[start..].trim().to_string());
        Ok(parts)
    }

    pub fn string_to_type(&self, s: &str, span: Span) -> Result<DataType, LangError> {
        let s = s.trim();

        if let Some(func) = self.try_parse_function_type(s, span)? {
            return Ok(func);
        }

        if s == "()" {
            return Ok(DataType::Unit);
        }

        if let Some(tuple) = self.try_parse_tuple_type(s, span)? {
            return Ok(tuple);
        }

        if let Some(vector) = self.try_parse_vector_type(s, span)? {
            return Ok(vector);
        }

        match s {
            "i32" => Ok(DataType::I32),
            "f64" => Ok(DataType::F64),
            "bool" => Ok(DataType::Bool),
            "string" => Ok(DataType::String),
            _ => self.resolve_named_type(s, span),
        }
    }

    fn try_parse_function_type(&self, s: &str, span: Span) -> Result<Option<DataType>, LangError> {
        if !s.starts_with('(') {
            return Ok(None);
        }

        let mut depth = 0;
        let mut closing_index = None;
        for (idx, ch) in s.char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    if depth == 0 {
                        return Err(LangError::Compile(CompileError::new(
                            "Mismatched parentheses in type signature",
                            span,
                        )));
                    }
                    depth -= 1;
                    if depth == 0 {
                        closing_index = Some(idx);
                        break;
                    }
                }
                _ => {}
            }
        }

        let closing_index = match closing_index {
            Some(idx) => idx,
            None => {
                return Err(LangError::Compile(CompileError::new(
                    "Unclosed parentheses in type signature",
                    span,
                )));
            }
        };

        let remainder = &s[closing_index + 1..];
        let remainder_trimmed = remainder.trim_start();
        if !remainder_trimmed.starts_with("->") {
            return Ok(None);
        }
        let return_part = remainder_trimmed[2..].trim_start();
        if return_part.is_empty() {
            return Err(LangError::Compile(CompileError::new(
                "Missing return type in function signature",
                span,
            )));
        }

        let params_inner = &s[1..closing_index];
        let param_strings = self.split_type_params(params_inner, span)?;
        let param_types = param_strings
            .iter()
            .map(|p| self.string_to_type(p, span))
            .collect::<Result<Vec<_>, _>>()?;
        let return_type = self.string_to_type(return_part, span)?;

        Ok(Some(DataType::Function {
            params: param_types,
            return_type: Box::new(return_type),
        }))
    }

    fn try_parse_tuple_type(&self, s: &str, span: Span) -> Result<Option<DataType>, LangError> {
        if !(s.starts_with('(') && s.ends_with(')')) {
            return Ok(None);
        }

        let inner = &s[1..s.len() - 1];
        let element_strings = self.split_type_params(inner, span)?;
        let element_types = element_strings
            .iter()
            .map(|p| self.string_to_type(p, span))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Some(DataType::Tuple(element_types)))
    }

    fn try_parse_vector_type(&self, s: &str, span: Span) -> Result<Option<DataType>, LangError> {
        if let Some(inner) = s.strip_prefix("Vec<") {
            if !inner.ends_with('>') {
                return Err(LangError::Compile(CompileError::new(
                    format!("Malformed Vec type '{}': missing closing '>'", s),
                    span,
                )));
            }
            let inner_type_str = &inner[..inner.len() - 1];
            let inner_type = self.string_to_type(inner_type_str, span)?;
            return Ok(Some(DataType::Vector(Box::new(inner_type))));
        }
        Ok(None)
    }

    fn resolve_named_type(&self, s: &str, span: Span) -> Result<DataType, LangError> {
        if self.struct_table.contains_key(s) {
            Ok(DataType::Struct(s.to_string()))
        } else if self.enum_table.contains_key(s) {
            Ok(DataType::Enum(s.to_string()))
        } else {
            Err(LangError::Compile(CompileError::new(
                format!("Unknown type '{}'", s),
                span,
            )))
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

    fn register_vec_builtins(&mut self, suffix: &str, element_type: DataType) {
        let vec_type = DataType::Vector(Box::new(element_type.clone()));
        let mut register = |name: String, param_types: Vec<DataType>, return_type: DataType| {
            self.function_table
                .entry(name.clone())
                .or_default()
                .push(FunctionSignature {
                    name,
                    param_types,
                    return_type,
                    definition_span: Span::default(),
                });
        };
        register(
            format!("vec_new_{}", suffix),
            alloc::vec![],
            vec_type.clone(),
        );
        register(
            format!("vec_push_{}", suffix),
            alloc::vec![vec_type.clone(), element_type.clone()],
            DataType::Unit,
        );
        register(
            format!("vec_get_{}", suffix),
            alloc::vec![vec_type.clone(), DataType::I32],
            element_type.clone(),
        );
        register(
            format!("vec_len_{}", suffix),
            alloc::vec![vec_type],
            DataType::I32,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn analyzer() -> Analyzer {
        Analyzer::new()
    }

    #[test]
    fn parses_function_types() {
        let analyzer = analyzer();
        let ty = analyzer
            .string_to_type("(i32, string) -> bool", Span::default())
            .unwrap();
        match ty {
            DataType::Function {
                params,
                return_type,
            } => {
                assert_eq!(params, alloc::vec![DataType::I32, DataType::String]);
                assert_eq!(*return_type, DataType::Bool);
            }
            other => panic!("unexpected type: {:?}", other),
        }
    }

    #[test]
    fn parses_nested_vectors() {
        let analyzer = analyzer();
        let ty = analyzer
            .string_to_type("Vec<Vec<i32>>", Span::default())
            .unwrap();
        assert_eq!(
            ty,
            DataType::Vector(Box::new(DataType::Vector(Box::new(DataType::I32))))
        );
    }

    #[test]
    fn parses_tuple_types() {
        let analyzer = analyzer();
        let ty = analyzer
            .string_to_type("(i32, (string, bool))", Span::default())
            .unwrap();
        assert_eq!(
            ty,
            DataType::Tuple(alloc::vec![
                DataType::I32,
                DataType::Tuple(alloc::vec![DataType::String, DataType::Bool]),
            ])
        );
    }

    #[test]
    fn reports_mismatched_parentheses() {
        let analyzer = analyzer();
        let err = analyzer
            .string_to_type("(i32, string", Span::default())
            .unwrap_err();
        assert!(matches!(err, LangError::Compile(_)));
    }
}
