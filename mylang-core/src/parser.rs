//! トークン列を未解決のAST(RawAST)に変換する構文解析器。

extern crate alloc;
use crate::ast::*;
use crate::error::{LangError, ParseError};
use crate::span::{Span, combine_spans};
use crate::token::Token;
use alloc::boxed::Box;
use alloc::collections::BTreeSet;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;

/// 構文解析器
pub struct Parser {
    tokens: Vec<(Token, Span)>,
    position: usize,
    /// Collected refinement predicates parsed from `<binder: Base | $predicate$>`
    pub refinements: Vec<MathAstNode>,
}

impl Parser {
    pub fn new(tokens: Vec<(Token, Span)>) -> Self {
        Self {
            tokens,
            position: 0,
            refinements: Vec::new(),
        }
    }

    /// トップレベルの構文(関数定義など)をパースする
    pub fn parse_toplevel(&mut self) -> Result<Vec<RawAstNode>, LangError> {
        let mut nodes = Vec::new();
        while !self.is_at_end() {
            // 空の文 (;) をスキップする
            while self.check_and_consume(Token::Semicolon) {}
            if self.is_at_end() {
                break;
            }
            nodes.push(self.parse_statement()?);
        }
        Ok(nodes)
    }

    /// `fn name |p: t|->r body` を `let hoist name |p: t|->r body` に脱糖する
    fn parse_fn_def(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Fn)?.1;
        let (name, name_span) = self.consume_identifier()?;
        let type_params = self.parse_type_params()?;

        // fn は let hoist のシンタックスシュガーとして扱う
        // |...|->... body というラムダ式を期待する
        let value = self.parse_lambda_as_node()?;
        let end_span = value.span();

        // 式の後のセミコロンを消費
        self.check_and_consume(Token::Semicolon);

        Ok(RawAstNode::LetHoist {
            name: (name, name_span),
            type_params,
            value: Box::new(value),
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_struct_def(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Struct)?.1;
        let (name, name_span) = self.consume_identifier()?;
        self.consume(Token::LBrace)?;

        let mut fields = Vec::new();
        while self.peek() != Some(&Token::RBrace) {
            let (field_name, field_span) = self.consume_identifier()?;
            self.consume(Token::Colon)?;
            let (type_name, type_span) = self.parse_type()?;
            fields.push(RawStructField {
                name: (field_name, field_span),
                type_name: (type_name, type_span),
            });

            if !self.check_and_consume(Token::Comma) {
                break;
            }
        }

        // 末尾の余分なカンマを許容
        while self.peek() == Some(&Token::Comma) {
            self.advance();
        }

        let end_span = self.consume(Token::RBrace)?.1;

        Ok(RawAstNode::StructDef {
            name: (name, name_span),
            fields,
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_enum_def(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Enum)?.1;
        let (name, name_span) = self.consume_identifier()?;
        self.consume(Token::LBrace)?;

        let mut variants = Vec::new();
        while self.peek() != Some(&Token::RBrace) {
            let (variant_name, variant_span) = self.consume_identifier()?;
            let mut end_span = variant_span;
            let kind = if self.peek() == Some(&Token::LParen) {
                self.consume(Token::LParen)?;
                let mut fields = Vec::new();
                if self.peek() != Some(&Token::RParen) {
                    loop {
                        let (type_name, type_span) = self.parse_type()?;
                        fields.push((type_name, type_span));
                        if !self.check_and_consume(Token::Comma) {
                            break;
                        }
                    }
                }
                end_span = self.consume(Token::RParen)?.1;
                RawEnumVariantKind::Tuple(fields)
            } else {
                RawEnumVariantKind::Unit
            };

            variants.push(RawEnumVariant {
                name: (variant_name, variant_span),
                kind,
                span: combine_spans(variant_span, end_span),
            });

            if !self.check_and_consume(Token::Comma) {
                break;
            }
        }

        while self.peek() == Some(&Token::Comma) {
            self.advance();
        }

        let end_span = self.consume(Token::RBrace)?.1;

        Ok(RawAstNode::EnumDef {
            name: (name, name_span),
            variants,
            span: combine_spans(start_span, end_span),
        })
    }

    /// `{ ... }` ブロックをパースする
    fn parse_block(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::LBrace)?.1;
        let mut statements = Vec::new();

        while self.peek() != Some(&Token::RBrace) && !self.is_at_end() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);

            // ブロックの終わり(`}`)が見えている場合、それは最後の式かもしれないのでセミコロンを要求しない
            if self.peek() == Some(&Token::RBrace) {
                break;
            }

            // それ以外の場合は、文の区切りとしてセミコロンを要求する
            if self.check_and_consume(Token::Semicolon) {
                // オプショナルなセミコロンを許容する (例: `let a = 1;;`)
                while self.peek() == Some(&Token::Semicolon) {
                    self.advance();
                }
            } else {
                // ブロックの最後の式（セミコロンなし）の直後が } でない場合はエラー
                if self.peek() != Some(&Token::RBrace) {
                    return Err(ParseError::new(
                        "Expected semicolon or '}' after statement",
                        self.peek_span(),
                    )
                    .into());
                }
            }
        }

        let end_span = self.consume(Token::RBrace)?.1;

        Ok(RawAstNode::Block {
            statements,
            span: combine_spans(start_span, end_span),
        })
    }

    /// 1つの文 (`let`, `{}` または 式) をパースする
    fn parse_statement(&mut self) -> Result<RawAstNode, LangError> {
        match self.peek() {
            Some(Token::Let) => {
                let start_span = self.consume(Token::Let)?.1;
                if self.check_and_consume(Token::Mut) {
                    // let mut
                    self.parse_let_mut_binding(start_span)
                } else if self.check_and_consume(Token::Hoist) {
                    // let hoist
                    self.parse_let_hoist_binding(start_span)
                } else {
                    // let
                    self.parse_let_binding(start_span)
                }
            }
            Some(Token::Set) => self.parse_set_assignment(),
            Some(Token::While) => self.parse_while_loop(),
            Some(Token::Fn) => self.parse_fn_def(),
            Some(Token::Trait) => self.parse_trait_def(),
            Some(Token::Impl) => self.parse_impl_def(),
            Some(Token::Struct) => self.parse_struct_def(),
            Some(Token::Enum) => self.parse_enum_def(),
            _ => self.parse_sexpression(),
        }
    }

    /// let束縛 `let name ...` をパースする
    fn parse_let_binding(&mut self, start_span: Span) -> Result<RawAstNode, LangError> {
        let (name, name_span) = self.consume_identifier()?;

        let value = self.parse_sexpression()?;
        let end_span = value.span();

        Ok(RawAstNode::Let {
            name: (name, name_span),
            value: Box::new(value),
            span: combine_spans(start_span, end_span),
        })
    }

    /// let mut束縛 `let mut name ...` をパースする
    fn parse_let_mut_binding(&mut self, start_span: Span) -> Result<RawAstNode, LangError> {
        let (name, name_span) = self.consume_identifier()?;

        let value = self.parse_sexpression()?;
        let end_span = value.span();

        Ok(RawAstNode::LetMut {
            name: (name, name_span),
            value: Box::new(value),
            span: combine_spans(start_span, end_span),
        })
    }

    /// let hoist束縛 `let hoist name ...` をパースする
    fn parse_let_hoist_binding(&mut self, start_span: Span) -> Result<RawAstNode, LangError> {
        let (name, name_span) = self.consume_identifier()?;
        let type_params = self.parse_type_params()?;

        let value = self.parse_sexpression()?;
        let end_span = value.span();

        Ok(RawAstNode::LetHoist {
            name: (name, name_span),
            type_params,
            value: Box::new(value),
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_trait_def(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Trait)?.1;
        let (name, name_span) = self.consume_identifier()?;
        let type_params = self.parse_type_params()?;
        self.consume(Token::LBrace)?;

        let mut methods = Vec::new();
        while self.peek() != Some(&Token::RBrace) {
            let method = self.parse_trait_method(false)?;
            methods.push(method);
        }

        let end_span = self.consume(Token::RBrace)?.1;
        Ok(RawAstNode::TraitDef {
            name: (name, name_span),
            type_params,
            methods,
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_impl_def(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Impl)?.1;
        let (trait_name, trait_span) = self.consume_identifier()?;
        let trait_args = if self.peek() == Some(&Token::LessThan) {
            self.parse_type_argument_list()?
        } else {
            Vec::new()
        };

        self.consume(Token::For)?;
        let (self_type_name, self_type_span) = self.parse_type()?;
        self.consume(Token::LBrace)?;

        let mut methods = Vec::new();
        while self.peek() != Some(&Token::RBrace) {
            let method = self.parse_trait_method(true)?;
            methods.push(method);
        }

        let end_span = self.consume(Token::RBrace)?.1;
        Ok(RawAstNode::ImplDef {
            trait_name: (trait_name, trait_span),
            trait_args,
            self_type: (self_type_name, self_type_span),
            methods,
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_trait_method(&mut self, require_body: bool) -> Result<RawTraitMethod, LangError> {
        let start_span = self.consume(Token::Fn)?.1;
        let (name, name_span) = self.consume_identifier()?;
        let type_params = self.parse_type_params()?;
        let (params, return_type) = self.parse_function_signature_components()?;

        let (body, end_span) = if require_body {
            let body = self.parse_sexpression()?;
            let span = body.span();
            if self.peek() == Some(&Token::Semicolon) {
                self.consume(Token::Semicolon)?;
            }
            (Some(Box::new(body)), span)
        } else {
            let semi_span = self.consume(Token::Semicolon)?.1;
            (None, semi_span)
        };

        Ok(RawTraitMethod {
            name: (name, name_span),
            type_params,
            params,
            return_type,
            body,
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_type_argument_list(&mut self) -> Result<Vec<(String, Span)>, LangError> {
        self.consume(Token::LessThan)?;
        let mut args = Vec::new();
        if self.peek() == Some(&Token::GreaterThan) {
            return Err(
                ParseError::new("Type argument list cannot be empty", self.peek_span()).into(),
            );
        }
        loop {
            let (ty, span) = self.parse_type()?;
            args.push((ty, span));
            if !self.check_and_consume(Token::Comma) {
                break;
            }
        }
        self.consume(Token::GreaterThan)?;
        Ok(args)
    }

    /// 代入文 `set name expr` をパースする
    fn parse_set_assignment(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Set)?.1;
        let (name, name_span) = self.consume_identifier()?;
        let value = self.parse_sexpression()?;
        let end_span = value.span();

        Ok(RawAstNode::Set {
            name: (name, name_span),
            value: Box::new(value),
            span: combine_spans(start_span, end_span),
        })
    }

    /// `while <condition> { ... }` をパースする
    fn parse_while_loop(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::While)?.1;
        let condition = self.parse_sexpression()?;
        let body = self.parse_block()?;
        let end_span = body.span();

        Ok(RawAstNode::While {
            condition: Box::new(condition),
            body: Box::new(body),
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_pattern(&mut self) -> Result<RawPattern, LangError> {
        let (token, span) = self
            .peek_full()
            .cloned()
            .ok_or_else(|| ParseError::new("Unexpected end of pattern", self.peek_span()))?;

        match token {
            Token::Identifier(name) if name == "_" => {
                self.advance();
                Ok(RawPattern::Wildcard(span))
            }
            Token::Identifier(_) => {
                let mut segments = Vec::new();
                let (first_name, first_span) = self.consume_identifier()?;
                let mut end_span = first_span;
                segments.push((first_name, first_span));
                while self.peek() == Some(&Token::DoubleColon) {
                    self.advance();
                    let (seg_name, seg_span) = self.consume_identifier()?;
                    segments.push((seg_name, seg_span));
                    end_span = seg_span;
                }

                if segments.len() == 1 && self.peek() != Some(&Token::LParen) {
                    return Ok(RawPattern::Identifier(segments.remove(0)));
                }

                let subpatterns = if self.peek() == Some(&Token::LParen) {
                    self.consume(Token::LParen)?;
                    let mut patterns = Vec::new();
                    if self.peek() != Some(&Token::RParen) {
                        loop {
                            let pat = self.parse_pattern()?;
                            patterns.push(pat);
                            if !self.check_and_consume(Token::Comma) {
                                break;
                            }
                        }
                    }
                    end_span = self.consume(Token::RParen)?.1;
                    patterns
                } else {
                    Vec::new()
                };

                Ok(RawPattern::Path {
                    segments,
                    subpatterns,
                    span: combine_spans(span, end_span),
                })
            }
            Token::LParen => {
                let start_span = span;
                self.consume(Token::LParen)?;
                let mut elements = Vec::new();
                if self.peek() != Some(&Token::RParen) {
                    loop {
                        let pat = self.parse_pattern()?;
                        elements.push(pat);
                        if !self.check_and_consume(Token::Comma) {
                            break;
                        }
                    }
                }
                let end_span = self.consume(Token::RParen)?.1;
                Ok(RawPattern::Tuple(
                    elements,
                    combine_spans(start_span, end_span),
                ))
            }
            Token::IntLiteral(_)
            | Token::FloatLiteral(_)
            | Token::True
            | Token::False
            | Token::StringLiteral(_) => {
                let (t, s) = self.advance();
                Ok(RawPattern::Literal(t, s))
            }
            _ => Err(ParseError::new("Invalid pattern", span).into()),
        }
    }

    /// S式（S-expression）をパースする
    fn parse_sexpression(&mut self) -> Result<RawAstNode, LangError> {
        // 式の先頭がブロックなら、ブロック式としてパースする
        if self.peek() == Some(&Token::LBrace) {
            return self.parse_block();
        }

        let mut parts = Vec::new();
        // 式の終わりは、文脈を区切るトークン
        while !self.is_at_end() {
            match self.peek() {
                // 式の区切りならループを終了
                Some(Token::Comma)
                | Some(Token::Semicolon)
                | Some(Token::RBrace)
                | Some(Token::LBrace)
                | Some(Token::RParen)
                | Some(Token::Else) => break,
                _ => {}
            }

            // 式の部品を1つパースする
            let part = self.parse_expr_part()?;
            parts.push(part);

            // 【LL(1)ロジック】今パースしたのがIdentifierで、次にCallLParenが続くなら、それはC-style呼び出しの一部
            if let Some(RawExprPart::Token(Token::Identifier(_), _)) = parts.last() {
                if self.peek() == Some(&Token::CallLParen) {
                    // C-styleの引数リストをパースしてpartsに追加する
                    let args_part = self.parse_c_style_args()?;
                    parts.push(args_part);
                }
            }
        }
        if parts.is_empty() {
            return Err(ParseError::new(
                "Expected an expression, but found nothing.",
                self.peek_span(),
            )
            .into());
        }
        Ok(RawAstNode::Expr(parts))
    }

    /// 式の構成要素（トークン、グループ、数式ブロックなど）を1つパースする。
    /// この関数は常に1つの`RawExprPart`だけを返し、LL(1)の原則に従う。
    fn parse_expr_part(&mut self) -> Result<RawExprPart, LangError> {
        let (token, span) = self
            .peek_full()
            .ok_or_else(|| ParseError::new("Unexpected end of file", self.peek_span()))?;
        match token {
            // Identifierとリテラルは、単にトークンとして消費するだけ
            Token::Identifier(_)
            | Token::IntLiteral(_)
            | Token::FloatLiteral(_)
            | Token::StringLiteral(_)
            | Token::True
            | Token::False => {
                let (t, s) = self.advance();
                if let Token::Identifier(mut name) = t {
                    let mut end_span = s;
                    while self.peek() == Some(&Token::DoubleColon) {
                        self.advance();
                        let (segment, segment_span) = self.consume_identifier()?;
                        name.push_str("::");
                        name.push_str(&segment);
                        end_span = segment_span;
                    }
                    Ok(RawExprPart::Token(
                        Token::Identifier(name),
                        combine_spans(s, end_span),
                    ))
                } else {
                    Ok(RawExprPart::Token(t, s))
                }
            }
            Token::Dollar => self.parse_math_block(),
            // `LParen` は常にS式グループの開始
            Token::LParen => self.parse_s_expr_group(),
            Token::Colon => self.parse_type_annotation(),
            Token::If => self.parse_if_as_part(),
            Token::Pipe | Token::OrOr => self.parse_lambda_expression(),
            Token::Match => self.parse_match_as_part(),
            _ => Err(ParseError::new(
                format!("Unexpected token in expression: {:?}", token),
                *span,
            )
            .into()),
        }
    }

    /// `match`式を`RawExprPart`としてパースする
    fn parse_match_as_part(&mut self) -> Result<RawExprPart, LangError> {
        let start_span = self.consume(Token::Match)?.1;
        let value = self.parse_sexpression()?;
        self.consume(Token::LBrace)?;

        let mut arms = Vec::new();
        while self.peek() != Some(&Token::RBrace) {
            let pattern = self.parse_pattern()?;
            self.consume(Token::FatArrow)?;
            let body = self.parse_sexpression()?;
            let arm_span = combine_spans(pattern.span(), body.span());
            arms.push(RawMatchArm {
                pattern,
                body: Box::new(body),
                span: arm_span,
            });

            if !self.check_and_consume(Token::Comma) {
                break;
            }
        }

        while self.peek() == Some(&Token::Comma) {
            self.advance();
        }

        let end_span = self.consume(Token::RBrace)?.1;

        Ok(RawExprPart::MatchExpr {
            value: Box::new(value),
            arms,
            span: combine_spans(start_span, end_span),
        })
    }

    /// ラムダ式 `|arg1: type, ...|->return_type body` をパースする
    fn parse_lambda_as_node(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.peek_span();

        let (params, return_type) = self.parse_function_signature_components()?;

        // 本体をパース
        let body = self.parse_sexpression()?;
        let end_span = body.span();

        Ok(RawAstNode::Lambda {
            params,
            body: Box::new(body),
            return_type,
            span: combine_spans(start_span, end_span),
        })
    }

    fn parse_function_signature_components(
        &mut self,
    ) -> Result<(Vec<((String, Span), (String, Span))>, (String, Span)), LangError> {
        let mut params = Vec::new();
        if self.check_and_consume(Token::OrOr) {
            // empty parameter list via ||
        } else {
            self.consume(Token::Pipe)?;
            if self.peek() != Some(&Token::Pipe) {
                loop {
                    let (param_name, param_span) = self.consume_identifier()?;
                    self.consume(Token::Colon)?;
                    let (type_name, type_span) = self.parse_type()?;
                    params.push(((param_name, param_span), (type_name, type_span)));
                    if !self.check_and_consume(Token::Comma) {
                        break;
                    }
                }
            }
            self.consume(Token::Pipe)?;
        }

        self.consume(Token::Arrow)?;
        let return_type = self.parse_type()?;
        Ok((params, return_type))
    }

    fn parse_type_params(&mut self) -> Result<Vec<RawTypeParam>, LangError> {
        if self.peek() != Some(&Token::LessThan) {
            return Ok(Vec::new());
        }

        let mut params = Vec::new();
        self.consume(Token::LessThan)?;

        if self.peek() == Some(&Token::GreaterThan) {
            return Err(ParseError::new(
                "Generic parameter list cannot be empty",
                self.peek_span(),
            )
            .into());
        }

        let mut seen: BTreeSet<String> = BTreeSet::new();
        loop {
            let (param_name, param_span) = self.consume_identifier()?;
            if !seen.insert(param_name.clone()) {
                return Err(ParseError::new(
                    format!("Duplicate type parameter '{}'", param_name),
                    param_span,
                )
                .into());
            }
            let mut bounds = Vec::new();
            if self.check_and_consume(Token::Colon) {
                if self.peek() == Some(&Token::Comma) || self.peek() == Some(&Token::GreaterThan) {
                    return Err(ParseError::new(
                        "Expected trait bound after ':'",
                        self.peek_span(),
                    )
                    .into());
                }
                loop {
                    let (trait_name, trait_span) = self.consume_identifier()?;
                    let trait_args = if self.peek() == Some(&Token::LessThan) {
                        self.parse_type_argument_list()?
                    } else {
                        Vec::new()
                    };
                    bounds.push(RawTraitBound {
                        trait_name: (trait_name, trait_span),
                        trait_args,
                    });
                    if !self.check_and_consume(Token::Plus) {
                        break;
                    }
                }
            }
            params.push(RawTypeParam {
                name: (param_name, param_span),
                bounds,
            });
            if !self.check_and_consume(Token::Comma) {
                break;
            }
        }

        self.consume(Token::GreaterThan)?;
        Ok(params)
    }

    /// ラムダ式を`RawExprPart`としてパースする
    fn parse_lambda_expression(&mut self) -> Result<RawExprPart, LangError> {
        // ラムダ式の実体をパース
        let node = self.parse_lambda_as_node()?;
        if let RawAstNode::Lambda {
            params,
            body,
            return_type,
            span,
        } = node
        {
            Ok(RawExprPart::Lambda {
                params,
                body,
                return_type,
                span,
            })
        } else {
            // This should never happen as parse_lambda_as_node always returns a Lambda variant
            unreachable!();
        }
    }

    /// S式の一部として `if <cond> { ... } else { ... }` をパースする
    fn parse_if_as_part(&mut self) -> Result<RawExprPart, LangError> {
        let start_span = self.consume(Token::If)?.1;
        let condition = self.parse_sexpression()?;
        let then_branch = self.parse_block()?;

        let else_branch = if self.check_and_consume(Token::Else) {
            // `else if ...` は `else` の後に続く式として解釈する
            // `else {` の場合はブロックとして解釈する
            if self.peek() == Some(&Token::LBrace) {
                self.parse_block()?
            } else {
                self.parse_sexpression()?
            }
        } else {
            // elseがない場合、Unitを返す空のブロックを生成する
            let dummy_span = self.peek_span();
            RawAstNode::Block {
                statements: vec![],
                span: dummy_span,
            }
        };

        let end_span = else_branch.span();

        Ok(RawExprPart::IfExpr {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
            span: combine_spans(start_span, end_span),
        })
    }

    /// `: type` 型注釈をパースする
    fn parse_type_annotation(&mut self) -> Result<RawExprPart, LangError> {
        self.consume(Token::Colon)?;
        let (type_name, type_span) = self.parse_type()?;
        Ok(RawExprPart::TypeAnnotation(type_name, type_span))
    }

    /// 型シグネチャをパースする。単純な型、ジェネリクス、関数型を扱える。
    fn parse_type(&mut self) -> Result<(String, Span), LangError> {
        // Support refinement syntax: `<binder: Base | $predicate$>`
        if self.peek() == Some(&Token::LessThan) {
            // Tentatively parse `<binder: Base | $predicate$>`
            let start_span = self.consume(Token::LessThan)?.1;
            // binder
            let (binder, binder_span) = self.consume_identifier()?;
            self.consume(Token::Colon)?;
            // base type as string
            let (base_name, _base_span) = self.parse_type()?;
            // expect pipe and math predicate
            if self.check_and_consume(Token::Pipe) {
                // predicate must be a math block starting with `$` ... `$`
                if self.peek() != Some(&Token::Dollar) {
                    return Err(ParseError::new("Expected '$' to start refinement predicate", self.peek_span()).into());
                }
                // parse math expression
                self.consume(Token::Dollar)?;
                let math_node = self.parse_math_expression(0)?;
                self.consume(Token::Dollar)?;
                let end_span = self.consume(Token::GreaterThan)?.1;
                // register predicate and return a placeholder string referencing its index
                let idx = self.refinements.len();
                self.refinements.push(math_node);
                let placeholder = format!("__ref_{}#{}#{}", idx, binder, base_name);
                return Ok((placeholder, combine_spans(start_span, end_span)));
            } else {
                // not a refinement; roll back is difficult but we'll treat as generic '<' parsed earlier
                // For now, fallthrough to normal handling by constructing the generic-style name
                let end_span = self.consume(Token::GreaterThan)?.1;
                let name = format!("<{}:{}>", binder, base_name);
                return Ok((name, combine_spans(binder_span, end_span)));
            }
        }
        let (start_token, start_span) = self
            .peek_full()
            .cloned()
            .ok_or_else(|| ParseError::new("Expected a type", self.peek_span()))?;

        // 関数型: (T1, T2) -> R
        if start_token == Token::LParen {
            self.consume(Token::LParen)?;
            let mut signature = "(".to_string();

            // パラメータ型をパース
            let mut first = true;
            if self.peek() != Some(&Token::RParen) {
                loop {
                    if !first {
                        signature.push_str(", ");
                    }
                    // 再帰呼び出し
                    let (param_type, _) = self.parse_type()?;
                    signature.push_str(&param_type);
                    first = false;
                    if !self.check_and_consume(Token::Comma) {
                        break;
                    }
                }
            }
            self.consume(Token::RParen)?;
            signature.push(')');

            if self.peek() == Some(&Token::Arrow) {
                self.consume(Token::Arrow)?;
                signature.push_str(" -> ");

                // 戻り値の型をパース
                let (return_type, end_span) = self.parse_type()?;
                signature.push_str(&return_type);
                return Ok((signature, combine_spans(start_span, end_span)));
            } else {
                let end_span = self.peek_span();
                return Ok((signature, combine_spans(start_span, end_span)));
            }
        }

        // 単純な型またはジェネリクス型: i32, Vec<i32>
        let (base_name, base_span) = self.consume_identifier()?;
        let mut name = base_name.clone();
        let mut end_span = base_span;

        if self.peek() == Some(&Token::LessThan) {
            self.consume(Token::LessThan)?;
            name.push('<');
            let mut first = true;
            loop {
                // ジェネリクスパラメータをパース
                let (inner_name, _inner_span) = self.parse_type()?;
                if !first {
                    name.push_str(", ");
                }
                name.push_str(&inner_name);
                first = false;
                if !self.check_and_consume(Token::Comma) {
                    break;
                }
            }
            let greater_span = self.consume(Token::GreaterThan)?.1;
            name.push('>');
            end_span = greater_span;
        }

        Ok((name, combine_spans(base_span, end_span)))
    }

    /// `( ... )` で囲まれたS式グループをパースする
    fn parse_s_expr_group(&mut self) -> Result<RawExprPart, LangError> {
        let start_span = self.consume(Token::LParen)?.1;
        if self.peek() == Some(&Token::RParen) {
            let end_span = self.consume(Token::RParen)?.1;
            return Ok(RawExprPart::TupleLiteral(
                vec![],
                combine_spans(start_span, end_span),
            ));
        }

        let first_expr = self.parse_sexpression()?;

        if self.peek() == Some(&Token::Comma) {
            let mut elements = vec![first_expr];
            while self.check_and_consume(Token::Comma) {
                if self.peek() == Some(&Token::RParen) {
                    break;
                }
                let element = self.parse_sexpression()?;
                elements.push(element);
            }
            let end_span = self.consume(Token::RParen)?.1;
            return Ok(RawExprPart::TupleLiteral(
                elements,
                combine_spans(start_span, end_span),
            ));
        }

        let end_span = self.consume(Token::RParen)?.1;

        if let RawAstNode::Expr(parts) = first_expr {
            Ok(RawExprPart::Group(
                parts,
                combine_spans(start_span, end_span),
            ))
        } else {
            unreachable!()
        }
    }

    /// `(...)` C-style呼び出しの引数リストをパースする
    fn parse_c_style_args(&mut self) -> Result<RawExprPart, LangError> {
        // この関数が呼ばれるとき、次のトークンは `CallLParen` であることが保証されている
        let start_span = self.consume(Token::CallLParen)?.1;
        let mut args = Vec::new();

        if self.peek() != Some(&Token::RParen) {
            loop {
                // 各引数はそれ自体が完結したS式
                let arg_node = self.parse_sexpression()?;
                args.push(arg_node);

                if !self.check_and_consume(Token::Comma) {
                    break;
                }
            }
        }
        let end_span = self.consume(Token::RParen)?.1;
        Ok(RawExprPart::CStyleArgs(
            args,
            combine_spans(start_span, end_span),
        ))
    }

    /// `$ ... $` で囲まれた数式ブロックをパースする
    fn parse_math_block(&mut self) -> Result<RawExprPart, LangError> {
        let start_span = self.consume(Token::Dollar)?.1;
        let math_ast = self.parse_math_expression(0)?;
        let end_span = self.consume(Token::Dollar)?.1;
        Ok(RawExprPart::MathBlock(
            math_ast,
            combine_spans(start_span, end_span),
        ))
    }

    /// Pratt-parser を用いて中置記法の数式をパースする
    fn parse_math_expression(&mut self, precedence: u8) -> Result<MathAstNode, LangError> {
        // Prefix (リテラル, 変数, 関数呼び出し, グループ化された式など)
        let mut left = {
            let (token, span) = self
                .peek_full()
                .cloned()
                .ok_or_else(|| {
                    ParseError::new("Unexpected end of math expression", self.peek_span())
                })?
                .clone();
            match token {
                Token::IntLiteral(_) | Token::FloatLiteral(_) | Token::True | Token::False => {
                    let (t, s) = self.advance();
                    match t {
                        Token::IntLiteral(v) => MathAstNode::Literal(MathLiteral::Int(v), s),
                        Token::FloatLiteral(v) => MathAstNode::Literal(MathLiteral::Float(v), s),
                        Token::True => MathAstNode::Literal(MathLiteral::Bool(true), s),
                        Token::False => MathAstNode::Literal(MathLiteral::Bool(false), s),
                        _ => unreachable!(),
                    }
                }
                Token::Bang | Token::Minus => {
                    let (op_token, op_span) = self.advance();
                    let operand = self.parse_math_expression(6)?;
                    let span = combine_spans(op_span, operand.span());
                    MathAstNode::PrefixOp {
                        op: op_token,
                        expr: Box::new(operand),
                        span,
                    }
                }
                Token::Identifier(name) => {
                    self.advance(); // 識別子を消費
                                  // 数式ブロック内では、トップレベルのパーサーとは異なり、識別子の次に'('があれば、
                                  // 間に空白があってもC-style呼び出しとみなす。
                    if self.peek() == Some(&Token::LParen)
                        || self.peek() == Some(&Token::CallLParen)
                    {
                        self.parse_math_call(name, span)?
                    } else {
                        MathAstNode::Variable(name, span)
                    }
                }
                Token::LParen | Token::CallLParen => {
                    self.advance(); // '(' を消費
                    let expr = self.parse_math_expression(0)?; // 内側の式をパース
                    self.consume(Token::RParen)?; // ')' を消費
                    expr
                }
                _ => {
                    return Err(ParseError::new(
                        format!(
                            "Expected literal, variable, function call or '(' in math expression, found {:?}",
                            token
                        ),
                        span,
                    )
                    .into())
                }
            }
        };

        // Infix (中置演算子)
        while precedence < self.get_infix_precedence(&self.peek()) {
            let (op_token, _op_span) = self.advance();

            let op_prec = self.get_token_precedence(&op_token);
            let right = self.parse_math_expression(op_prec)?;
            let combined_span = combine_spans(left.span(), right.span());
            left = MathAstNode::InfixOp {
                op: op_token,
                left: Box::new(left),
                right: Box::new(right),
                span: combined_span,
            };
        }

        Ok(left)
    }

    /// 数式内の関数呼び出し `name(...)` をパースする
    fn parse_math_call(&mut self, name: String, name_span: Span) -> Result<MathAstNode, LangError> {
        // 【修正点】数式ブロック内の `(` も LParen と CallLParen の両方を許容する
        if self.peek() == Some(&Token::LParen) {
            self.consume(Token::LParen)?;
        } else {
            self.consume(Token::CallLParen)?;
        }

        let mut args = Vec::new();

        if self.peek() != Some(&Token::RParen) {
            loop {
                // 各引数はS式として解析する。これにより、`my_func(add 1 2)` や `my_func($a+b$)` のように、
                // 数式ブロックの中から通常のS式パーサーを再帰的に呼び出す、協調的な構造になっている。
                let arg_node = self.parse_sexpression()?;
                args.push(arg_node);

                if !self.check_and_consume(Token::Comma) {
                    break;
                }
            }
        }

        let end_span = self.consume(Token::RParen)?.1;

        Ok(MathAstNode::Call {
            name: (name, name_span),
            args,
            span: combine_spans(name_span, end_span),
        })
    }

    /// 次のトークンから中置演算子の優先順位を返す (lookahead)
    fn get_infix_precedence(&self, token: &Option<&Token>) -> u8 {
        if let Some(token) = token {
            self.get_token_precedence(token)
        } else {
            0
        }
    }

    /// トークン自体から優先順位を返す
    fn get_token_precedence(&self, token: &Token) -> u8 {
        match token {
            Token::OrOr => 1,
            Token::AndAnd => 2,
            Token::EqualsEquals
            | Token::BangEquals
            | Token::LessThan
            | Token::LessThanEquals
            | Token::GreaterThan
            | Token::GreaterThanEquals => 3,
            Token::Plus | Token::Minus => 4,
            Token::Star | Token::Slash | Token::Percent => 5,
            _ => 0,
        }
    }

    // --- パーサーヘルパー ---
    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position).map(|(t, _)| t)
    }
    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.position)
            .map(|(_, s)| *s)
            .unwrap_or_default()
    }
    fn peek_full(&self) -> Option<&(Token, Span)> {
        self.tokens.get(self.position)
    }
    fn advance(&mut self) -> (Token, Span) {
        self.position += 1;
        self.tokens[self.position - 1].clone()
    }

    fn check_and_consume(&mut self, token: Token) -> bool {
        if self.peek() == Some(&token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume(&mut self, expected: Token) -> Result<(Token, Span), LangError> {
        let (found, span) = self.peek_full().ok_or_else(|| {
            ParseError::new(
                format!("Expected '{:?}' but found end of file", expected),
                self.peek_span(),
            )
        })?;
        if *found == expected {
            Ok(self.advance())
        } else {
            Err(ParseError::new(
                format!("Expected '{:?}' but found '{:?}'", expected, found),
                *span,
            )
            .into())
        }
    }

    fn consume_identifier(&mut self) -> Result<(String, Span), LangError> {
        let (token, span) = self.advance();
        if let Token::Identifier(s) = token {
            Ok((s, span))
        } else {
            Err(ParseError::new(
                format!("Expected an identifier but found '{:?}'", token),
                span,
            )
            .into())
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    extern crate alloc;
    use alloc::vec;

    // Helper function to parse a math expression string
    fn parse_math_str(input: &str) -> Result<MathAstNode, LangError> {
        let mut lexer = Lexer::new(input);
        // テストのために`$`をダミーでトークナイズリストの前後に追加
        let mut tokens = vec![(Token::Dollar, Span::default())];
        tokens.extend(lexer.tokenize_all().unwrap());
        tokens.push((Token::Dollar, Span::default()));

        let mut parser = Parser::new(tokens);
        parser.consume(Token::Dollar)?; // 開始の`$`を消費
        parser.parse_math_expression(0)
    }

    #[test]
    fn test_math_call_with_simple_sexpr_arg() {
        // 数式内の関数呼び出しの引数が、単純なS式（リテラル）であるケース
        let result = parse_math_str("my_func(123)");
        assert!(result.is_ok());
        if let Ok(MathAstNode::Call { name, args, .. }) = result {
            assert_eq!(name.0, "my_func");
            assert_eq!(args.len(), 1);
            // 引数が `RawAstNode::Expr([RawExprPart::Token(IntLiteral(123), ...)])` であることを確認
            if let RawAstNode::Expr(parts) = &args[0] {
                assert_eq!(parts.len(), 1);
                assert!(matches!(
                    &parts[0],
                    RawExprPart::Token(Token::IntLiteral(123), _)
                ));
            } else {
                panic!("Argument should be a RawAstNode::Expr");
            }
        } else {
            panic!("Expected MathAstNode::Call, got {:?}", result);
        }
    }

    #[test]
    fn test_math_call_with_sexpr_function_call_arg() {
        // 数式内の関数呼び出しの引数が、S式の関数呼び出しであるケース
        let result = parse_math_str("my_func(add 1 2)");
        assert!(result.is_ok());
        if let Ok(MathAstNode::Call { name, args, .. }) = result {
            assert_eq!(name.0, "my_func");
            assert_eq!(args.len(), 1);
            // 引数が `RawAstNode::Expr([Token(Ident("add")), Token(Num(1)), Token(Num(2))])` であることを確認
            if let RawAstNode::Expr(parts) = &args[0] {
                assert_eq!(parts.len(), 3);
                assert!(
                    matches!(&parts[0], RawExprPart::Token(Token::Identifier(s), _) if s == "add")
                );
            } else {
                panic!("Argument should be a RawAstNode::Expr");
            }
        } else {
            panic!("Expected MathAstNode::Call, got {:?}", result);
        }
    }

    #[test]
    fn test_the_problematic_factorial_case() {
        // 当初問題となった、数式内の関数呼び出しの引数が、さらに数式ブロックであるケース
        let result = parse_math_str("n * factorial($n - 1$)");
        assert!(result.is_ok());

        // $n * f($n-1$)$ の構造が InfixOp { left: Var(n), op: Star, right: Call { args: [Expr([MathBlock(...)])] } }
        // になっていることを確認
        if let Ok(MathAstNode::InfixOp { op, right, .. }) = result {
            assert_eq!(op, Token::Star);
            if let MathAstNode::Call { name, args, .. } = *right {
                assert_eq!(name.0, "factorial");
                assert_eq!(args.len(), 1);
                if let RawAstNode::Expr(parts) = &args[0] {
                    assert_eq!(parts.len(), 1);
                    assert!(matches!(&parts[0], RawExprPart::MathBlock(_, _)));
                } else {
                    panic!("Argument should be a RawAstNode::Expr containing a MathBlock");
                }
            } else {
                panic!("Right side of InfixOp should be a Call");
            }
        } else {
            panic!("Expected MathAstNode::InfixOp, got {:?}", result);
        }
    }
}
