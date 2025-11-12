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
    pub function_table: BTreeMap<String, FunctionSignature>,
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
        function_table.insert(
            "string_char_at".to_string(),
            FunctionSignature {
                param_types: alloc::vec![DataType::String, DataType::I32],
                return_type: DataType::String,
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
                        format!(
                            "Function '{}' is a built-in function and cannot be redefined",
                            func_name
                        ),
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
    pub fn string_to_type(&self, s: &str, span: Span) -> Result<DataType, LangError> {
        match s {
            "i32" => Ok(DataType::I32),
            "f64" => Ok(DataType::F64),
            "bool" => Ok(DataType::Bool),
            "string" => Ok(DataType::String),
            "()" => Ok(DataType::Unit),
            _ => {
                if let Some(inner) = s.strip_prefix("Vec<") {
                    if !inner.ends_with('>') {
                        return Err(LangError::Compile(CompileError::new(
                            format!("Malformed Vec type '{}': missing closing '>'", s),
                            span,
                        )));
                    }
                    let inner_type_str = &inner[..inner.len() - 1];
                    let inner_type = self.string_to_type(inner_type_str, span)?;
                    return Ok(DataType::Vector(Box::new(inner_type)));
                }
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
        self.function_table.insert(
            format!("vec_new_{}", suffix),
            FunctionSignature {
                param_types: alloc::vec![],
                return_type: vec_type.clone(),
                definition_span: Span::default(),
            },
        );
        self.function_table.insert(
            format!("vec_push_{}", suffix),
            FunctionSignature {
                param_types: alloc::vec![vec_type.clone(), element_type.clone()],
                return_type: DataType::Unit,
                definition_span: Span::default(),
            },
        );
        self.function_table.insert(
            format!("vec_get_{}", suffix),
            FunctionSignature {
                param_types: alloc::vec![vec_type.clone(), DataType::I32],
                return_type: element_type.clone(),
                definition_span: Span::default(),
            },
        );
        self.function_table.insert(
            format!("vec_len_{}", suffix),
            FunctionSignature {
                param_types: alloc::vec![vec_type],
                return_type: DataType::I32,
                definition_span: Span::default(),
            },
        );
    }
}
