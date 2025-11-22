//! RawASTを受け取り、意味解析、型チェックを行うアナライザー。

extern crate alloc;
// サブモジュールを宣言
pub mod analyzer;

use crate::ast::*;
use crate::builtins::ProvidedFunction;
use crate::compiler::analyzer::apply_type_substitution;
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
    pub trait_table: BTreeMap<String, TraitInfo>,
    pub impl_table: BTreeMap<String, Vec<TraitImplInfo>>,
    pub refinements: Vec<MathAstNode>,
    type_param_stack: Vec<String>,
    type_alias_stack: Vec<(String, DataType)>,
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
    pub type_params: Vec<String>,
    pub param_types: Vec<DataType>,
    pub return_type: DataType,
    pub trait_bounds: Vec<TraitBound>,
    pub definition_span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TraitBound {
    pub type_param: String,
    pub trait_name: String,
    pub trait_args: Vec<DataType>,
    pub span: Span,
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

#[derive(Clone)]
pub struct TraitInfo {
    pub type_params: Vec<String>,
    pub methods: BTreeMap<String, TraitMethodInfo>,
    pub span: Span,
}

#[derive(Clone)]
pub struct TraitMethodInfo {
    pub signature: FunctionSignature,
}

#[derive(Clone)]
pub struct TraitImplInfo {
    pub trait_name: String,
    pub trait_args: Vec<DataType>,
    pub self_type: DataType,
    pub span: Span,
}

impl Analyzer {
    pub fn new() -> Self {
        let mut analyzer = Self {
            scope_depth: 0,
            function_table: BTreeMap::new(),
            variable_table: Vec::new(),
            var_counters: BTreeMap::new(),
            string_headers: BTreeMap::new(),
            // メモリの先頭32バイトはIO用に予約
            static_offset: 32,
            struct_table: BTreeMap::new(),
            enum_table: BTreeMap::new(),
            trait_table: BTreeMap::new(),
            impl_table: BTreeMap::new(),
            type_param_stack: Vec::new(),
            type_alias_stack: Vec::new(),
            refinements: Vec::new(),
        };

        // --- 標準組み込み関数を登録 ---
        analyzer.register_builtin(
            "string_concat",
            alloc::vec![DataType::String, DataType::String],
            DataType::String,
        );
        analyzer.register_builtin(
            "i32_to_string",
            alloc::vec![DataType::I32],
            DataType::String,
        );
        analyzer.register_builtin(
            "f64_to_string",
            alloc::vec![DataType::F64],
            DataType::String,
        );
        analyzer.register_builtin("print", alloc::vec![DataType::String], DataType::Unit);
        analyzer.register_builtin("println", alloc::vec![DataType::String], DataType::Unit);
        analyzer.register_builtin("read_line", alloc::vec![], DataType::String);
        analyzer.register_builtin(
            "string_char_at",
            alloc::vec![DataType::String, DataType::I32],
            DataType::String,
        );

        // printlnが内部的に使用する改行文字を静的領域に事前登録しておく
        analyzer.ensure_string_is_statically_allocated(&"\n".to_string());

        analyzer.register_vec_builtins("i32", DataType::I32);

        analyzer
    }

    pub fn register_builtin(
        &mut self,
        name: &str,
        param_types: Vec<DataType>,
        return_type: DataType,
    ) {
        let signature = FunctionSignature {
            name: name.to_string(),
            type_params: Vec::new(),
            param_types,
            return_type,
            trait_bounds: Vec::new(),
            definition_span: Span::default(),
        };

        let candidates = self.function_table.entry(name.to_string()).or_default();
        let is_duplicate = candidates.iter().any(|existing| {
            existing.param_types == signature.param_types
                && existing.return_type == signature.return_type
                && existing.type_params == signature.type_params
                && existing.trait_bounds == signature.trait_bounds
        });

        if !is_duplicate {
            candidates.push(signature);
        }
    }

    pub fn register_provided_builtins(&mut self, provided: &[ProvidedFunction]) {
        for func in provided {
            self.register_builtin(
                &func.name,
                func.param_types.clone(),
                func.return_type.clone(),
            );
        }
    }

    /// 解析のメインエントリーポイント
    pub fn analyze(&mut self, ast: &[RawAstNode]) -> Result<Vec<TypedAstNode>, LangError> {
        self.prepass_declarations(ast)?;

        let mut typed_ast = Vec::new();
        for node in ast {
            if let RawAstNode::ImplDef { .. } = node {
                let mut impl_nodes = analyzer::analyze_impl_block(self, node)?;
                typed_ast.append(&mut impl_nodes);
            } else {
                typed_ast.push(analyzer::analyze_toplevel(self, node)?);
            }
        }

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
            if let RawAstNode::TraitDef {
                name,
                type_params,
                methods,
                span,
            } = node
            {
                self.register_trait(name, type_params, methods, *span)?;
            }
        }

        for node in ast {
            if let RawAstNode::ImplDef {
                trait_name,
                trait_args,
                self_type,
                methods,
                span,
            } = node
            {
                self.register_impl(trait_name, trait_args, self_type, methods, *span)?;
            }
        }

        // 次に let hoist 束縛（関数定義）を収集する
        for node in ast {
            if let RawAstNode::LetHoist {
                name,
                type_params,
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
                    let type_param_names: Vec<String> = type_params
                        .iter()
                        .map(|param| param.name.0.clone())
                        .collect();
                    let previous_len = self.push_type_params(&type_param_names);
                    let trait_bounds = self.collect_trait_bounds(type_params)?;
                    let param_types = params
                        .iter()
                        .map(|(_, type_info)| self.string_to_type(&type_info.0, type_info.1))
                        .collect::<Result<Vec<_>, _>>()?;

                    let ret_type = self.string_to_type(&return_type.0, return_type.1)?;
                    self.pop_type_params(previous_len);

                    let candidate_signature = FunctionSignature {
                        name: func_name.clone(),
                        type_params: type_param_names,
                        param_types,
                        return_type: ret_type,
                        trait_bounds,
                        definition_span: name.1,
                    };
                    self.insert_function_signature(candidate_signature)?;
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

    fn insert_function_signature(&mut self, signature: FunctionSignature) -> Result<(), LangError> {
        let func_name = signature.name.clone();
        let entry = self.function_table.entry(func_name.clone()).or_default();
        if let Some(existing) = entry
            .iter()
            .find(|candidate| signatures_equivalent(candidate, &signature))
        {
            return Err(LangError::Compile(
                CompileError::new(
                    format!(
                        "Function overload '{}' with the same signature is already defined",
                        func_name
                    ),
                    signature.definition_span,
                )
                .with_note(
                    "Previous definition is located here",
                    existing.definition_span,
                ),
            ));
        }
        entry.push(signature);
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

    fn register_trait(
        &mut self,
        name: &(String, Span),
        type_params: &[RawTypeParam],
        methods: &[RawTraitMethod],
        span: Span,
    ) -> Result<(), LangError> {
        if self.trait_table.contains_key(&name.0)
            || self.struct_table.contains_key(&name.0)
            || self.enum_table.contains_key(&name.0)
        {
            return Err(LangError::Compile(CompileError::new(
                format!("Trait or type '{}' is already defined", name.0),
                name.1,
            )));
        }

        let param_names: Vec<String> = type_params
            .iter()
            .map(|param| param.name.0.clone())
            .collect();
        let mut scope_params = param_names.clone();
        scope_params.push("Self".to_string());
        let previous_len = self.push_type_params(&scope_params);

        let mut method_map = BTreeMap::new();
        for method in methods {
            if !method.type_params.is_empty() {
                return Err(LangError::Compile(CompileError::new(
                    "Trait methods cannot declare their own generic parameters yet",
                    method.name.1,
                )));
            }
            if method.body.is_some() {
                return Err(LangError::Compile(CompileError::new(
                    "Trait methods must omit their bodies",
                    method.name.1,
                )));
            }
            if method_map.contains_key(&method.name.0) {
                return Err(LangError::Compile(CompileError::new(
                    format!("Duplicate method '{}' in trait '{}'", method.name.0, name.0),
                    method.name.1,
                )));
            }

            let param_types = method
                .params
                .iter()
                .map(|(_, ty)| self.string_to_type(&ty.0, ty.1))
                .collect::<Result<Vec<_>, _>>()?;
            let return_type = self.string_to_type(&method.return_type.0, method.return_type.1)?;
            method_map.insert(
                method.name.0.clone(),
                TraitMethodInfo {
                    signature: FunctionSignature {
                        name: method.name.0.clone(),
                        type_params: Vec::new(),
                        param_types,
                        return_type,
                        trait_bounds: Vec::new(),
                        definition_span: method.name.1,
                    },
                },
            );
        }

        self.pop_type_params(previous_len);

        self.trait_table.insert(
            name.0.clone(),
            TraitInfo {
                type_params: param_names,
                methods: method_map,
                span,
            },
        );
        Ok(())
    }

    fn register_impl(
        &mut self,
        trait_name: &(String, Span),
        trait_args: &[(String, Span)],
        self_type: &(String, Span),
        methods: &[RawTraitMethod],
        span: Span,
    ) -> Result<(), LangError> {
        let trait_info = self
            .trait_table
            .get(&trait_name.0)
            .cloned()
            .ok_or_else(|| {
                LangError::Compile(CompileError::new(
                    format!("Unknown trait '{}'", trait_name.0),
                    trait_name.1,
                ))
            })?;

        if trait_info.type_params.len() != trait_args.len() {
            return Err(LangError::Compile(CompileError::new(
                format!(
                    "Trait '{}' expects {} type argument(s) but {} were provided",
                    trait_name.0,
                    trait_info.type_params.len(),
                    trait_args.len()
                ),
                trait_name.1,
            )));
        }

        let trait_arg_types = trait_args
            .iter()
            .map(|(arg, span)| self.string_to_type(arg, *span))
            .collect::<Result<Vec<_>, _>>()?;
        let self_type_resolved = self.string_to_type(&self_type.0, self_type.1)?;

        if let Some(existing_impls) = self.impl_table.get(&trait_name.0) {
            if existing_impls.iter().any(|info| {
                info.self_type == self_type_resolved && info.trait_args == trait_arg_types
            }) {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "An implementation of '{}' for '{}' with the same type arguments already exists",
                        trait_name.0, self_type_resolved
                    ),
                    span,
                )));
            }
        }

        let mut impl_methods: BTreeMap<String, &RawTraitMethod> = BTreeMap::new();
        for method in methods {
            if !method.type_params.is_empty() {
                return Err(LangError::Compile(CompileError::new(
                    "Trait implementation methods cannot declare generics yet",
                    method.name.1,
                )));
            }
            if method.body.is_none() {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Method '{}' in impl for '{}' is missing a body",
                        method.name.0, self_type_resolved
                    ),
                    method.name.1,
                )));
            }
            if impl_methods.insert(method.name.0.clone(), method).is_some() {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Duplicate method '{}' in impl of '{}' for '{}'",
                        method.name.0, trait_name.0, self_type_resolved
                    ),
                    method.name.1,
                )));
            }
        }

        for method_name in impl_methods.keys() {
            if !trait_info.methods.contains_key(method_name) {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Method '{}' is not part of trait '{}' and cannot appear in its impl",
                        method_name, trait_name.0
                    ),
                    span,
                )));
            }
        }

        let mut substitution: BTreeMap<String, DataType> = trait_info
            .type_params
            .iter()
            .cloned()
            .zip(trait_arg_types.iter().cloned())
            .collect();
        substitution.insert("Self".to_string(), self_type_resolved.clone());

        for (method_name, trait_method) in &trait_info.methods {
            let impl_method = impl_methods.get(method_name).ok_or_else(|| {
                LangError::Compile(CompileError::new(
                    format!(
                        "Trait '{}' implementation for '{}' is missing method '{}'",
                        trait_name.0, self_type_resolved, method_name
                    ),
                    span,
                ))
            })?;

            let alias_len = self.push_type_alias("Self", self_type_resolved.clone());
            let actual_params = impl_method
                .params
                .iter()
                .map(|(_, ty)| self.string_to_type(&ty.0, ty.1))
                .collect::<Result<Vec<_>, _>>()?;
            let actual_return =
                self.string_to_type(&impl_method.return_type.0, impl_method.return_type.1)?;
            self.pop_type_alias(alias_len);

            let expected_params = trait_method
                .signature
                .param_types
                .iter()
                .map(|ty| apply_type_substitution(ty, &substitution))
                .collect::<Vec<_>>();
            let expected_return =
                apply_type_substitution(&trait_method.signature.return_type, &substitution);

            if expected_params != actual_params || expected_return != actual_return {
                return Err(LangError::Compile(CompileError::new(
                    format!(
                        "Method '{}' implementation does not match the trait signature",
                        method_name
                    ),
                    impl_method.name.1,
                )));
            }

            let signature = FunctionSignature {
                name: format!("{}::{}", trait_name.0, method_name),
                type_params: Vec::new(),
                param_types: expected_params,
                return_type: expected_return,
                trait_bounds: Vec::new(),
                definition_span: impl_method.name.1,
            };
            self.insert_function_signature(signature)?;
        }

        self.impl_table
            .entry(trait_name.0.clone())
            .or_default()
            .push(TraitImplInfo {
                trait_name: trait_name.0.clone(),
                trait_args: trait_arg_types.clone(),
                self_type: self_type_resolved.clone(),
                span,
            });
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

    fn push_type_params(&mut self, params: &[String]) -> usize {
        let previous_len = self.type_param_stack.len();
        if !params.is_empty() {
            self.type_param_stack.extend(params.iter().cloned());
        }
        previous_len
    }

    fn pop_type_params(&mut self, previous_len: usize) {
        self.type_param_stack.truncate(previous_len);
    }

    fn is_type_param(&self, name: &str) -> bool {
        self.type_param_stack
            .iter()
            .rev()
            .any(|param| param == name)
    }

    fn push_type_alias(&mut self, name: &str, data_type: DataType) -> usize {
        let previous_len = self.type_alias_stack.len();
        self.type_alias_stack.push((name.to_string(), data_type));
        previous_len
    }

    fn pop_type_alias(&mut self, previous_len: usize) {
        self.type_alias_stack.truncate(previous_len);
    }

    fn lookup_type_alias(&self, name: &str) -> Option<DataType> {
        self.type_alias_stack
            .iter()
            .rev()
            .find(|(alias, _)| alias == name)
            .map(|(_, ty)| ty.clone())
    }

    fn collect_trait_bounds(
        &mut self,
        params: &[RawTypeParam],
    ) -> Result<Vec<TraitBound>, LangError> {
        let mut bounds = Vec::new();
        for param in params {
            for bound in &param.bounds {
                let trait_args = bound
                    .trait_args
                    .iter()
                    .map(|(ty, span)| self.string_to_type(ty, *span))
                    .collect::<Result<Vec<_>, _>>()?;
                bounds.push(TraitBound {
                    type_param: param.name.0.clone(),
                    trait_name: bound.trait_name.0.clone(),
                    trait_args,
                    span: bound.trait_name.1,
                });
            }
        }
        Ok(bounds)
    }

    fn trait_impl_exists(
        &self,
        trait_name: &str,
        trait_args: &[DataType],
        self_type: &DataType,
    ) -> bool {
        self.impl_table.get(trait_name).map_or(false, |impls| {
            impls
                .iter()
                .any(|info| &info.self_type == self_type && info.trait_args == trait_args)
        })
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

        if let Some(alias) = self.lookup_type_alias(s) {
            return Ok(alias);
        }

        if self.is_type_param(s) {
            return Ok(DataType::TypeVar(s.to_string()));
        }

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

        // Special case: placeholder produced by parser for refined types: "__ref_{id}#binder#base"
        if let Some(rest) = s.strip_prefix("__ref_") {
            // format: id#binder#base
            let mut parts = rest.splitn(3, '#');
            if let (Some(id_str), Some(binder), Some(base)) =
                (parts.next(), parts.next(), parts.next())
            {
                if let Ok(id) = id_str.parse::<usize>() {
                    // base may itself be a type string; parse it recursively
                    let base_ty = self.string_to_type(base, span)?;
                    return Ok(DataType::Refined {
                        base: Box::new(base_ty),
                        binder: binder.to_string(),
                        predicate_id: id,
                    });
                }
            }
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
                    type_params: Vec::new(),
                    param_types,
                    return_type,
                    trait_bounds: Vec::new(),
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

fn signatures_equivalent(a: &FunctionSignature, b: &FunctionSignature) -> bool {
    if a.param_types.len() != b.param_types.len() {
        return false;
    }
    let (a_params, a_ret) = canonicalize_signature(a);
    let (b_params, b_ret) = canonicalize_signature(b);
    a_params == b_params && a_ret == b_ret
}

fn canonicalize_signature(signature: &FunctionSignature) -> (Vec<DataType>, DataType) {
    let mapping: BTreeMap<String, String> = signature
        .type_params
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), format!("__T{}", idx)))
        .collect();

    let params = signature
        .param_types
        .iter()
        .map(|ty| canonicalize_type(ty, &mapping))
        .collect();
    let ret = canonicalize_type(&signature.return_type, &mapping);
    (params, ret)
}

fn canonicalize_type(ty: &DataType, mapping: &BTreeMap<String, String>) -> DataType {
    match ty {
        DataType::TypeVar(name) => mapping
            .get(name)
            .map(|mapped| DataType::TypeVar(mapped.clone()))
            .unwrap_or_else(|| DataType::TypeVar(name.clone())),
        DataType::Vector(inner) => DataType::Vector(Box::new(canonicalize_type(inner, mapping))),
        DataType::Tuple(elements) => DataType::Tuple(
            elements
                .iter()
                .map(|element| canonicalize_type(element, mapping))
                .collect(),
        ),
        DataType::Function {
            params,
            return_type,
        } => DataType::Function {
            params: params
                .iter()
                .map(|param| canonicalize_type(param, mapping))
                .collect(),
            return_type: Box::new(canonicalize_type(return_type, mapping)),
        },
        _ => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

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

    #[test]
    fn resolves_type_variables_in_scope() {
        let mut analyzer = analyzer();
        let previous_len = analyzer.push_type_params(&vec!["T".to_string()]);
        let ty = analyzer
            .string_to_type("T", Span::default())
            .expect("type variable should resolve");
        analyzer.pop_type_params(previous_len);
        assert_eq!(ty, DataType::TypeVar("T".to_string()));
    }
}
