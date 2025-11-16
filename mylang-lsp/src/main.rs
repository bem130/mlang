use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use anyhow::Result;
use parking_lot::RwLock;
use tower_lsp::jsonrpc::Result as LspResult;
use tower_lsp::lsp_types::{SemanticTokenModifier, SemanticTokenType, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};

use mylang_core::ast::{
    DataType, RawAstNode, TypedAstNode, TypedExpr, TypedExprKind, TypedMatchArm,
};
use mylang_core::compiler::{Analyzer, FunctionSignature};
use mylang_core::error::{CompileError, LangError, ParseError};
use mylang_core::span::Span;
use mylang_core::token::Token;

const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::TYPE,
    SemanticTokenType::OPERATOR,
];

const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[];

#[derive(Clone)]
struct HoverEntry {
    markdown: String,
}

#[derive(Clone, Copy)]
enum SemanticKind {
    Keyword = 0,
    Function = 1,
    Variable = 2,
    String = 3,
    Number = 4,
    Type = 5,
    Operator = 6,
}

#[derive(Clone)]
struct VariableDefinition {
    name: String,
    span: Span,
    data_type: DataType,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum SymbolKind {
    Function,
    Variable,
    Type,
}

struct TypedAnalysis {
    function_table: BTreeMap<String, Vec<FunctionSignature>>,
    variable_defs: HashMap<String, VariableDefinition>,
    variable_refs: HashMap<(usize, usize), String>,
    hover_entries: HashMap<(usize, usize), HoverEntry>,
    symbol_kinds: HashMap<(usize, usize), SymbolKind>,
}

struct TokenInfo {
    token: Token,
    span: Span,
    length: usize,
}

struct Issue {
    severity: DiagnosticSeverity,
    message: String,
    span: Span,
    related: Vec<(String, Span)>,
    highlight: Option<String>,
}

struct DocumentSnapshot {
    text: String,
    line_index: LineIndex,
    tokens: Vec<TokenInfo>,
    type_definitions: HashMap<String, Span>,
    typed: Option<TypedAnalysis>,
    issues: Vec<Issue>,
}

struct LineIndex {
    offsets: Vec<usize>,
    text_len: usize,
}
impl LineIndex {
    fn new(text: &str) -> Self {
        let mut offsets = vec![0];
        for (idx, ch) in text.char_indices() {
            if ch == '\n' {
                offsets.push(idx + 1);
            }
        }
        Self {
            offsets,
            text_len: text.len(),
        }
    }

    fn line_col_to_offset(&self, line: usize, column: usize) -> usize {
        if line == 0 || column == 0 {
            return 0;
        }
        let line_idx = line.saturating_sub(1);
        let start = *self
            .offsets
            .get(line_idx)
            .unwrap_or_else(|| self.offsets.last().unwrap());
        start + column.saturating_sub(1)
    }

    fn offset_to_position(&self, offset: usize) -> Position {
        if self.offsets.is_empty() {
            return Position::new(0, 0);
        }
        let mut idx = match self.offsets.binary_search(&offset) {
            Ok(index) => index,
            Err(index) => index.saturating_sub(1),
        };
        if idx >= self.offsets.len() {
            idx = self.offsets.len() - 1;
        }
        let line_start = self.offsets[idx];
        Position::new(idx as u32, offset.saturating_sub(line_start) as u32)
    }

    fn range_with_length(&self, span: Span, length: usize) -> Range {
        if span.line == 0 {
            return Range::new(Position::new(0, 0), Position::new(0, 0));
        }
        let start_line = span.line.saturating_sub(1) as u32;
        let start_col = span.column.saturating_sub(1) as u32;
        let start_offset = self.line_col_to_offset(span.line, span.column);
        let end_offset = (start_offset + length).min(self.text_len);
        let end = self.offset_to_position(end_offset);
        Range::new(Position::new(start_line, start_col), end)
    }

    fn range_for_symbol(&self, span: Span, name: &str) -> Range {
        let length = name.chars().count();
        self.range_with_length(span, length.max(1))
    }

    fn range_to_line_end(&self, span: Span, text: &str) -> Range {
        if span.line == 0 {
            return Range::new(Position::new(0, 0), Position::new(0, 0));
        }
        let line_idx = span.line.saturating_sub(1);
        let end_offset = if let Some(next) = self.offsets.get(line_idx + 1) {
            next.saturating_sub(1)
        } else {
            text.len()
        };
        Range::new(
            Position::new(
                (span.line.saturating_sub(1)) as u32,
                span.column.saturating_sub(1) as u32,
            ),
            self.offset_to_position(end_offset.min(text.len())),
        )
    }
}
impl Issue {
    fn to_diagnostic(&self, uri: &Url, line_index: &LineIndex, text: &str) -> Diagnostic {
        let range = if let Some(symbol) = &self.highlight {
            line_index.range_for_symbol(self.span, symbol)
        } else {
            line_index.range_to_line_end(self.span, text)
        };
        let related_information = if self.related.is_empty() {
            None
        } else {
            Some(
                self.related
                    .iter()
                    .map(|(message, span)| DiagnosticRelatedInformation {
                        location: Location::new(
                            uri.clone(),
                            if span.line == 0 {
                                Range::new(Position::new(0, 0), Position::new(0, 0))
                            } else {
                                line_index.range_to_line_end(*span, text)
                            },
                        ),
                        message: message.clone(),
                    })
                    .collect(),
            )
        };
        Diagnostic {
            range,
            severity: Some(self.severity),
            code: None,
            code_description: None,
            source: Some("mylang".into()),
            message: self.message.clone(),
            related_information,
            tags: None,
            data: None,
        }
    }
}
fn compute_token_length(token: &Token, span: Span, text: &str, line_index: &LineIndex) -> usize {
    match token {
        Token::Identifier(name) => name.chars().count(),
        Token::IntLiteral(value) => value.to_string().chars().count(),
        Token::FloatLiteral(value) => {
            let formatted = if value.fract() == 0.0 {
                format!("{:.1}", value)
            } else {
                value.to_string()
            };
            formatted.chars().count()
        }
        Token::StringLiteral(_) => {
            let start = line_index.line_col_to_offset(span.line, span.column);
            string_literal_length(text, start)
        }
        Token::Fn => 2,
        Token::Let => 3,
        Token::Mut => 3,
        Token::Hoist => 5,
        Token::Set => 3,
        Token::If => 2,
        Token::Else => 4,
        Token::While => 5,
        Token::Struct => 6,
        Token::Enum => 4,
        Token::Match => 5,
        Token::True => 4,
        Token::False => 5,
        Token::LParen
        | Token::CallLParen
        | Token::RParen
        | Token::LBrace
        | Token::RBrace
        | Token::Dollar
        | Token::Comma
        | Token::Semicolon
        | Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::Pipe
        | Token::Colon
        | Token::Bang
        | Token::LessThan
        | Token::GreaterThan => 1,
        Token::DoubleColon
        | Token::Arrow
        | Token::FatArrow
        | Token::EqualsEquals
        | Token::BangEquals
        | Token::LessThanEquals
        | Token::GreaterThanEquals
        | Token::AndAnd
        | Token::OrOr => 2,
        Token::Equals => 1,
    }
}

fn string_literal_length(text: &str, start: usize) -> usize {
    let mut chars = text[start..].char_indices();
    if let Some((_, '"')) = chars.next() {
        let mut escaped = false;
        for (idx, ch) in chars {
            if escaped {
                escaped = false;
                continue;
            }
            match ch {
                '\\' => escaped = true,
                '"' => return idx + 1,
                _ => {}
            }
        }
    }
    1
}
fn convert_lang_error(error: LangError) -> Vec<Issue> {
    match error {
        LangError::Parse(ParseError { message, span }) => vec![Issue {
            severity: DiagnosticSeverity::ERROR,
            message,
            span,
            related: Vec::new(),
            highlight: None,
        }],
        LangError::Compile(CompileError {
            message,
            span,
            notes,
        }) => vec![Issue {
            severity: DiagnosticSeverity::ERROR,
            message,
            span,
            related: notes,
            highlight: None,
        }],
    }
}
struct CallContext {
    function_name: String,
    active_parameter: usize,
}

fn build_typed_analysis(
    typed_ast: &[TypedAstNode],
    function_table: BTreeMap<String, Vec<FunctionSignature>>,
) -> (TypedAnalysis, Vec<Issue>) {
    let mut variable_defs = HashMap::new();
    let mut variable_refs = HashMap::new();
    let mut hover_entries = HashMap::new();
    let mut symbol_kinds = HashMap::new();
    let mut usage_counts: HashMap<String, usize> = HashMap::new();
    let mut bindings: Vec<(String, String, Span, DataType)> = Vec::new();

    for (name, signatures) in &function_table {
        if signatures.is_empty() {
            continue;
        }
        let hover_text = format_signature_markdown(name, signatures);
        for signature in signatures {
            if signature.definition_span.line > 0 {
                hover_entries.insert(
                    (
                        signature.definition_span.line,
                        signature.definition_span.column,
                    ),
                    HoverEntry {
                        markdown: hover_text.clone(),
                    },
                );
                symbol_kinds.insert(
                    (
                        signature.definition_span.line,
                        signature.definition_span.column,
                    ),
                    SymbolKind::Function,
                );
            }
        }
    }

    for node in typed_ast {
        if let TypedAstNode::FnDef {
            name, body, span, ..
        } = node
        {
            if let Some(signatures) = function_table.get(name) {
                let hover_text = format_signature_markdown(name, signatures);
                if let Some(signature) = signatures.iter().find(|sig| sig.definition_span == *span)
                {
                    if signature.definition_span.line > 0 {
                        hover_entries.insert(
                            (
                                signature.definition_span.line,
                                signature.definition_span.column,
                            ),
                            HoverEntry {
                                markdown: hover_text.clone(),
                            },
                        );
                        symbol_kinds.insert(
                            (
                                signature.definition_span.line,
                                signature.definition_span.column,
                            ),
                            SymbolKind::Function,
                        );
                    }
                }
            }
            walk_expr(
                body,
                &mut variable_defs,
                &mut variable_refs,
                &mut hover_entries,
                &mut symbol_kinds,
                &mut usage_counts,
                &mut bindings,
                &function_table,
            );
        }
    }

    let mut issues = Vec::new();
    for (unique, name, span, data_type) in bindings {
        let used = usage_counts.get(&unique).copied().unwrap_or(0);
        if used == 0 && name != "_" {
            issues.push(Issue {
                severity: DiagnosticSeverity::WARNING,
                message: format!("Variable '{}' is never used", name),
                span,
                related: Vec::new(),
                highlight: Some(name.clone()),
            });
        }
        if let Some(def) = variable_defs.get_mut(&unique) {
            def.data_type = data_type;
        }
    }

    (
        TypedAnalysis {
            function_table,
            variable_defs,
            variable_refs,
            hover_entries,
            symbol_kinds,
        },
        issues,
    )
}
fn walk_expr(
    expr: &TypedExpr,
    variable_defs: &mut HashMap<String, VariableDefinition>,
    variable_refs: &mut HashMap<(usize, usize), String>,
    hover_entries: &mut HashMap<(usize, usize), HoverEntry>,
    symbol_kinds: &mut HashMap<(usize, usize), SymbolKind>,
    usage_counts: &mut HashMap<String, usize>,
    bindings: &mut Vec<(String, String, Span, DataType)>,
    function_table: &BTreeMap<String, Vec<FunctionSignature>>,
) {
    let key = (expr.span.line, expr.span.column);
    match &expr.kind {
        TypedExprKind::Literal(_) | TypedExprKind::StringLiteral { .. } => {}
        TypedExprKind::VariableRef { name, unique_name } => {
            variable_refs.insert(key, unique_name.clone());
            *usage_counts.entry(unique_name.clone()).or_insert(0) += 1;
            hover_entries.insert(
                key,
                HoverEntry {
                    markdown: format!("```mlang\n{}: {}\n```", name, expr.data_type),
                },
            );
            symbol_kinds.insert(key, SymbolKind::Variable);
        }
        TypedExprKind::LetBinding {
            name,
            value,
            is_mutable: _,
            name_span,
        } => {
            let definition = VariableDefinition {
                name: name.0.clone(),
                span: *name_span,
                data_type: value.data_type.clone(),
            };
            variable_defs.insert(name.1.clone(), definition);
            bindings.push((
                name.1.clone(),
                name.0.clone(),
                *name_span,
                value.data_type.clone(),
            ));
            hover_entries.insert(
                (name_span.line, name_span.column),
                HoverEntry {
                    markdown: format!("```mlang\nlet {}: {}\n```", name.0, value.data_type),
                },
            );
            symbol_kinds.insert((name_span.line, name_span.column), SymbolKind::Variable);
            walk_expr(
                value,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
        }
        TypedExprKind::LetHoistBinding {
            name,
            value,
            name_span,
        } => {
            hover_entries.insert(
                (name_span.line, name_span.column),
                HoverEntry {
                    markdown: format!("```mlang\nlet hoist {}: {}\n```", name.0, value.data_type),
                },
            );
            symbol_kinds.insert((name_span.line, name_span.column), SymbolKind::Function);
            walk_expr(
                value,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
        }
        TypedExprKind::Assignment { name, value } => {
            if let Some(def) = variable_defs.get(&name.1) {
                hover_entries.insert(
                    key,
                    HoverEntry {
                        markdown: format!("```mlang\n{}: {}\n```", def.name, def.data_type),
                    },
                );
            }
            symbol_kinds.insert(key, SymbolKind::Variable);
            walk_expr(
                value,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
        }
        TypedExprKind::Lambda { body, .. } => {
            walk_expr(
                body,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
        }
        TypedExprKind::FunctionCall { name, args } => {
            if let Some(signatures) = function_table.get(name) {
                if !signatures.is_empty() {
                    hover_entries.insert(
                        key,
                        HoverEntry {
                            markdown: format_signature_markdown(name, signatures),
                        },
                    );
                    symbol_kinds.insert(key, SymbolKind::Function);
                }
            }
            for arg in args {
                walk_expr(
                    arg,
                    variable_defs,
                    variable_refs,
                    hover_entries,
                    symbol_kinds,
                    usage_counts,
                    bindings,
                    function_table,
                );
            }
        }
        TypedExprKind::IfExpr {
            condition,
            then_branch,
            else_branch,
        } => {
            walk_expr(
                condition,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
            walk_expr(
                then_branch,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
            walk_expr(
                else_branch,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
        }
        TypedExprKind::Block { statements } => {
            for stmt in statements {
                walk_expr(
                    stmt,
                    variable_defs,
                    variable_refs,
                    hover_entries,
                    symbol_kinds,
                    usage_counts,
                    bindings,
                    function_table,
                );
            }
        }
        TypedExprKind::While { condition, body } => {
            walk_expr(
                condition,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
            walk_expr(
                body,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
        }
        TypedExprKind::TupleLiteral { elements } => {
            for element in elements {
                walk_expr(
                    element,
                    variable_defs,
                    variable_refs,
                    hover_entries,
                    symbol_kinds,
                    usage_counts,
                    bindings,
                    function_table,
                );
            }
        }
        TypedExprKind::Match { value, arms } => {
            walk_expr(
                value,
                variable_defs,
                variable_refs,
                hover_entries,
                symbol_kinds,
                usage_counts,
                bindings,
                function_table,
            );
            for TypedMatchArm { body, .. } in arms {
                walk_expr(
                    body,
                    variable_defs,
                    variable_refs,
                    hover_entries,
                    symbol_kinds,
                    usage_counts,
                    bindings,
                    function_table,
                );
            }
        }
    }
}
impl DocumentSnapshot {
    fn new(text: String) -> Self {
        let line_index = LineIndex::new(&text);
        let mut issues = Vec::new();
        let mut tokens = Vec::new();
        let mut type_definitions = HashMap::new();
        let mut typed = None;

        match mylang_core::lex_source(&text) {
            Ok(raw_tokens) => {
                tokens = raw_tokens
                    .iter()
                    .map(|(token, span)| TokenInfo {
                        token: token.clone(),
                        span: *span,
                        length: compute_token_length(token, *span, &text, &line_index).max(1),
                    })
                    .collect();

                match mylang_core::parse_tokens(&raw_tokens) {
                    Ok(raw_ast) => {
                        for node in &raw_ast {
                            match node {
                                RawAstNode::StructDef { name, .. } => {
                                    type_definitions.insert(name.0.clone(), name.1);
                                }
                                RawAstNode::EnumDef { name, .. } => {
                                    type_definitions.insert(name.0.clone(), name.1);
                                }
                                _ => {}
                            }
                        }

                        match mylang_core::prepare_ast(&raw_ast, true) {
                            Ok(prepared) => {
                                let mut analyzer = Analyzer::new();
                                match analyzer.analyze(&prepared) {
                                    Ok(typed_ast) => {
                                        let function_table = analyzer.function_table.clone();
                                        let (mut analysis, warnings) =
                                            build_typed_analysis(&typed_ast, function_table);
                                        for span in type_definitions.values() {
                                            analysis
                                                .symbol_kinds
                                                .insert((span.line, span.column), SymbolKind::Type);
                                        }
                                        issues.extend(warnings);
                                        typed = Some(analysis);
                                    }
                                    Err(err) => {
                                        issues.extend(convert_lang_error(err));
                                    }
                                }
                            }
                            Err(err) => {
                                issues.extend(convert_lang_error(err));
                            }
                        }
                    }
                    Err(err) => {
                        issues.extend(convert_lang_error(err));
                    }
                }
            }
            Err(err) => issues.extend(convert_lang_error(err)),
        }

        Self {
            text,
            line_index,
            tokens,
            type_definitions,
            typed,
            issues,
        }
    }

    fn diagnostics(&self, uri: &Url) -> Vec<Diagnostic> {
        self.issues
            .iter()
            .map(|issue| issue.to_diagnostic(uri, &self.line_index, &self.text))
            .collect()
    }

    fn hover(&self, position: Position) -> Option<Hover> {
        let key = (
            (position.line as usize) + 1,
            (position.character as usize) + 1,
        );
        self.typed
            .as_ref()
            .and_then(|typed| typed.hover_entries.get(&key))
            .map(|entry| Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: entry.markdown.clone(),
                }),
                range: None,
            })
    }

    fn goto_definition(&self, uri: &Url, position: Position) -> Option<Location> {
        let token = self.token_at_position(position)?;
        if let Token::Identifier(name) = &token.token {
            let key = (
                (position.line as usize) + 1,
                (position.character as usize) + 1,
            );
            if let Some(typed) = &self.typed {
                if let Some(unique) = typed.variable_refs.get(&key) {
                    if let Some(def) = typed.variable_defs.get(unique) {
                        let range = self.line_index.range_for_symbol(def.span, &def.name);
                        return Some(Location::new(uri.clone(), range));
                    }
                }
                if let Some(signatures) = typed.function_table.get(name) {
                    if let Some(signature) =
                        signatures.iter().find(|sig| sig.definition_span.line > 0)
                    {
                        let range = self
                            .line_index
                            .range_for_symbol(signature.definition_span, name);
                        return Some(Location::new(uri.clone(), range));
                    }
                }
            }
            if let Some(span) = self.type_definitions.get(name) {
                let range = self.line_index.range_for_symbol(*span, name);
                return Some(Location::new(uri.clone(), range));
            }
        }
        None
    }

    fn signature_help(&self, position: Position) -> Option<SignatureHelp> {
        let typed = self.typed.as_ref()?;
        let context = find_call_context(&self.tokens, position)?;
        let signatures = typed.function_table.get(&context.function_name)?;
        if signatures.is_empty() {
            return None;
        }
        let provided = context.active_parameter + 1;
        let mut active_signature_index = 0usize;
        let mut best_score = usize::MAX;
        for (idx, signature) in signatures.iter().enumerate() {
            let expected = signature.param_types.len();
            let score = if expected > provided {
                expected - provided
            } else {
                provided - expected
            };
            if score < best_score {
                best_score = score;
                active_signature_index = idx;
            }
        }
        let signature_infos: Vec<SignatureInformation> = signatures
            .iter()
            .map(|signature| {
                let label = format_signature_label(&context.function_name, signature);
                let parameters: Vec<ParameterInformation> = signature
                    .param_types
                    .iter()
                    .map(|ty| ParameterInformation {
                        label: ParameterLabel::Simple(ty.to_string()),
                        documentation: None,
                    })
                    .collect();
                let active_parameter = if signature.param_types.is_empty() {
                    0
                } else {
                    context
                        .active_parameter
                        .min(signature.param_types.len().saturating_sub(1))
                };
                SignatureInformation {
                    label,
                    documentation: None,
                    parameters: Some(parameters),
                    active_parameter: Some(active_parameter as u32),
                }
            })
            .collect();
        Some(SignatureHelp {
            signatures: signature_infos,
            active_signature: Some(active_signature_index as u32),
            active_parameter: Some(context.active_parameter as u32),
        })
    }

    fn semantic_tokens(&self) -> Vec<SemanticToken> {
        let mut result = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;
        for token in &self.tokens {
            if token.span.line == 0 {
                continue;
            }
            if let Some(kind) = self.classify_token(token) {
                let line = token.span.line.saturating_sub(1) as u32;
                let start = token.span.column.saturating_sub(1) as u32;
                let delta_line = line.saturating_sub(prev_line);
                let delta_start = if delta_line == 0 {
                    start.saturating_sub(prev_start)
                } else {
                    start
                };
                result.push(SemanticToken {
                    delta_line,
                    delta_start,
                    length: token.length as u32,
                    token_type: kind as u32,
                    token_modifiers_bitset: 0,
                });
                prev_line = line;
                prev_start = start;
            }
        }
        result
    }

    fn classify_token(&self, token: &TokenInfo) -> Option<SemanticKind> {
        if let Some(typed) = &self.typed {
            if let Some(kind) = typed
                .symbol_kinds
                .get(&(token.span.line, token.span.column))
            {
                return Some(match kind {
                    SymbolKind::Function => SemanticKind::Function,
                    SymbolKind::Variable => SemanticKind::Variable,
                    SymbolKind::Type => SemanticKind::Type,
                });
            }
        }
        match token.token {
            Token::Fn
            | Token::Let
            | Token::Mut
            | Token::Hoist
            | Token::Set
            | Token::If
            | Token::Else
            | Token::While
            | Token::Struct
            | Token::Enum
            | Token::Match
            | Token::True
            | Token::False => Some(SemanticKind::Keyword),
            Token::StringLiteral(_) => Some(SemanticKind::String),
            Token::IntLiteral(_) | Token::FloatLiteral(_) => Some(SemanticKind::Number),
            Token::Identifier(_) => None,
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Percent
            | Token::Equals
            | Token::EqualsEquals
            | Token::Bang
            | Token::BangEquals
            | Token::LessThan
            | Token::LessThanEquals
            | Token::GreaterThan
            | Token::GreaterThanEquals
            | Token::AndAnd
            | Token::OrOr
            | Token::Arrow
            | Token::FatArrow
            | Token::DoubleColon
            | Token::Pipe
            | Token::Colon => Some(SemanticKind::Operator),
            _ => None,
        }
    }

    fn token_at_position(&self, position: Position) -> Option<&TokenInfo> {
        let line = position.line as usize + 1;
        let column = position.character as usize + 1;
        self.tokens.iter().find(|token| {
            if token.span.line != line {
                return false;
            }
            let start = token.span.column;
            let end = start + token.length.max(1) - 1;
            column >= start && column <= end
        })
    }
}
fn format_signature_markdown(name: &str, signatures: &[FunctionSignature]) -> String {
    if signatures.is_empty() {
        return format!("```mlang\nfn {}()\n```", name);
    }
    let lines = signatures
        .iter()
        .map(|signature| format_signature_label(name, signature))
        .collect::<Vec<_>>()
        .join("\n");
    format!("```mlang\n{}\n```", lines)
}

fn format_signature_label(name: &str, signature: &FunctionSignature) -> String {
    let params = signature
        .param_types
        .iter()
        .map(|ty| ty.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("fn {}({}) -> {}", name, params, signature.return_type)
}

enum Frame {
    Call { name: String, arg_index: usize },
    Group,
}

fn find_call_context(tokens: &[TokenInfo], position: Position) -> Option<CallContext> {
    let target_line = position.line as usize + 1;
    let target_col = position.character as usize + 1;
    let mut stack: Vec<Frame> = Vec::new();
    let mut last_identifier: Option<String> = None;

    for token in tokens {
        let token_line = token.span.line;
        let token_col = token.span.column;
        if token_line > target_line || (token_line == target_line && token_col > target_col) {
            break;
        }
        match &token.token {
            Token::Identifier(name) => {
                last_identifier = Some(name.clone());
            }
            Token::CallLParen => {
                if let Some(name) = last_identifier.take() {
                    stack.push(Frame::Call { name, arg_index: 0 });
                } else {
                    stack.push(Frame::Group);
                }
            }
            Token::LParen => {
                stack.push(Frame::Group);
                last_identifier = None;
            }
            Token::RParen => {
                stack.pop();
                last_identifier = None;
            }
            Token::Comma => {
                if let Some(Frame::Call { arg_index, .. }) = stack.last_mut() {
                    *arg_index += 1;
                }
            }
            _ => {
                last_identifier = None;
            }
        }
    }

    while let Some(frame) = stack.pop() {
        if let Frame::Call { name, arg_index } = frame {
            return Some(CallContext {
                function_name: name,
                active_parameter: arg_index,
            });
        }
    }
    None
}
struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, DocumentSnapshot>>>,
    legend: SemanticTokensLegend,
}

impl Backend {
    fn new(client: Client) -> Self {
        let legend = SemanticTokensLegend {
            token_types: TOKEN_TYPES.iter().cloned().collect(),
            token_modifiers: TOKEN_MODIFIERS.iter().cloned().collect(),
        };
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            legend,
        }
    }

    fn update_document(&self, uri: &Url, text: String) -> Vec<Diagnostic> {
        let snapshot = DocumentSnapshot::new(text);
        let diagnostics = snapshot.diagnostics(uri);
        self.documents.write().insert(uri.clone(), snapshot);
        diagnostics
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> LspResult<InitializeResult> {
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".into(), ",".into()]),
                retrigger_characters: Some(vec![")".into(), ",".into()]),
                work_done_progress_options: Default::default(),
            }),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: Default::default(),
                    legend: self.legend.clone(),
                    range: Some(false),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                }),
            ),
            ..Default::default()
        };
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "mylang-lsp".into(),
                version: None,
            }),
            capabilities,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let _ = self
            .client
            .log_message(MessageType::INFO, "mylang language server initialized")
            .await;
    }

    async fn shutdown(&self) -> LspResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let diagnostics = self.update_document(&uri, text);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            let diagnostics = self.update_document(&uri, change.text);
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.write().remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn hover(&self, params: HoverParams) -> LspResult<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let documents = self.documents.read();
        Ok(documents.get(&uri).and_then(|doc| doc.hover(position)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LspResult<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let documents = self.documents.read();
        let location = documents
            .get(&uri)
            .and_then(|doc| doc.goto_definition(&uri, position));
        Ok(location.map(GotoDefinitionResponse::Scalar))
    }

    async fn signature_help(
        &self,
        params: SignatureHelpParams,
    ) -> LspResult<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let documents = self.documents.read();
        Ok(documents
            .get(&uri)
            .and_then(|doc| doc.signature_help(position)))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> LspResult<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let documents = self.documents.read();
        if let Some(snapshot) = documents.get(&uri) {
            let data = snapshot.semantic_tokens();
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data,
            })))
        } else {
            Ok(None)
        }
    }
}
#[tokio::main]
async fn main() -> Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}
