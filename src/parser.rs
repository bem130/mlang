//! トークン列を未解決のAST(RawAST)に変換する構文解析器。

use crate::ast::*;
use crate::error::{LangError, ParseError};
use crate::span::{combine_spans, Span};
use crate::token::Token;

/// 構文解析器
pub struct Parser {
    tokens: Vec<(Token, Span)>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<(Token, Span)>) -> Self {
        Self { tokens, position: 0 }
    }

    /// トップレベルの構文(関数定義など)をパースする
    pub fn parse_toplevel(&mut self) -> Result<Vec<RawAstNode>, LangError> {
        let mut nodes = Vec::new();
        while !self.is_at_end() {
            match self.peek() {
                Some(Token::Fn) => nodes.push(self.parse_fn_def()?),
                _ => {
                    return Err(ParseError::new(
                        "Expected function definition at toplevel",
                        self.peek_span(),
                    )
                    .into())
                }
            }
        }
        Ok(nodes)
    }

    /// 関数定義 `fn name(p1: t1, ...) -> type { ... }` をパースする
    fn parse_fn_def(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Fn)?.1;

        let name_token = self.consume_identifier()?;
        let name = (name_token.0.clone(), name_token.1);

        self.consume(Token::LParen)?;

        // パラメータのパース
        let mut params = Vec::new();
        if self.peek() != Some(&Token::RParen) {
            loop {
                let (param_name, param_span) = self.consume_identifier()?;
                self.consume(Token::Colon)?;
                let (type_name, type_span) = self.consume_identifier()?;
                params.push(((param_name, param_span), (type_name, type_span)));

                if !self.check_and_consume(Token::Comma) {
                    break;
                }
            }
        }

        self.consume(Token::RParen)?;

        // 戻り値の型 (オプショナル)
        let return_type = if self.check_and_consume(Token::Arrow) {
            let type_token = self.consume_identifier()?;
            Some((type_token.0.clone(), type_token.1))
        } else {
            None
        };

        let body = self.parse_block()?;
        let end_span = body.span();

        Ok(RawAstNode::FnDef {
            name,
            params,
            return_type,
            body: Box::new(body),
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
            self.consume(Token::Semicolon)?;

            // オプショナルなセミコロンを許容する (例: `let a = 1;;`)
            while self.peek() == Some(&Token::Semicolon) {
                self.advance();
            }
        }

        let end_span = self.consume(Token::RBrace)?.1;

        Ok(RawAstNode::Block {
            statements,
            span: combine_spans(start_span, end_span),
        })
    }
    
    /// 1つの文 (`let` または 式) をパースする (セミコロンは消費しない)
    fn parse_statement(&mut self) -> Result<RawAstNode, LangError> {
        if self.peek() == Some(&Token::Let) {
            self.parse_let_def()
        } else {
            self.parse_expression()
        }
    }

    /// let束縛 `let name = ...` をパースする
    fn parse_let_def(&mut self) -> Result<RawAstNode, LangError> {
        let start_span = self.consume(Token::Let)?.1;
        let (name, name_span) = self.consume_identifier()?;
        self.consume(Token::Equals)?;

        let value = self.parse_expression()?;
        let end_span = value.span();

        Ok(RawAstNode::LetDef {
            name: (name, name_span),
            value: Box::new(value),
            span: combine_spans(start_span, end_span),
        })
    }

    /// 式 (S式やリテラルなど) をパースする
    fn parse_expression(&mut self) -> Result<RawAstNode, LangError> {
        let mut parts = Vec::new();
        // 式の終わりは、セミコロン、閉じブレース、閉じ括弧など
        while !self.is_at_end() {
            match self.peek() {
                Some(Token::Semicolon) | Some(Token::RBrace) | Some(Token::RParen) => break,
                _ => parts.push(self.parse_expr_part()?),
            }
        }
        if parts.is_empty() {
            return Err(
                ParseError::new("Expected an expression, but found nothing.", self.peek_span())
                    .into(),
            );
        }
        Ok(RawAstNode::Expr(parts))
    }

    /// 式の構成要素（トークン、グループ、数式ブロックなど）を1つパースする
    fn parse_expr_part(&mut self) -> Result<RawExprPart, LangError> {
        let (token, span) = self
            .peek_full()
            .ok_or_else(|| ParseError::new("Unexpected end of file", self.peek_span()))?;
        match token {
            Token::IntLiteral(_) | Token::FloatLiteral(_) | Token::Identifier(_) => {
                let (t, s) = self.advance();
                Ok(RawExprPart::Token(t, s))
            }
            Token::Dollar => self.parse_math_block(),
            Token::LParen => self.parse_group(),
            _ => {
                Err(ParseError::new(format!("Unexpected token in expression: {:?}", token), *span)
                    .into())
            }
        }
    }
    
    /// `( ... )` で囲まれたグループをパースする
    fn parse_group(&mut self) -> Result<RawExprPart, LangError> {
        let start_span = self.consume(Token::LParen)?.1;
        let mut parts = Vec::new();
        while self.peek() != Some(&Token::RParen) && !self.is_at_end() {
            parts.push(self.parse_expr_part()?);
        }
        let end_span = self.consume(Token::RParen)?.1;
        Ok(RawExprPart::Group(parts, combine_spans(start_span, end_span)))
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
        // Prefix (リテラル, 変数, 前置演算子など)
        let (token, span) = self.advance();
        let mut left = match token {
            Token::IntLiteral(v) => MathAstNode::Literal(MathLiteral::Int(v), span),
            Token::FloatLiteral(v) => MathAstNode::Literal(MathLiteral::Float(v), span),
            Token::Identifier(s) => MathAstNode::Variable(s, span),
            _ => {
                return Err(ParseError::new(
                    format!("Expected literal or variable in math expression, found {:?}", token),
                    span,
                )
                .into())
            }
        };

        // Infix (中置演算子)
        while precedence < self.get_infix_precedence() {
            let (op_token, _op_span) = self.advance();
            let right_precedence = self.get_infix_precedence();
            let right = self.parse_math_expression(right_precedence)?;
            let combined_span = combine_spans(left.span(), right.span());
            left = MathAstNode::InfixOp {
                op: op_token,
                left: Box::new(left.clone()),
                right: Box::new(right),
                span: combined_span,
            };
        }

        Ok(left)
    }

    /// 中置演算子の優先順位を返す
    fn get_infix_precedence(&self) -> u8 {
        match self.peek() {
            Some(Token::Plus) | Some(Token::Minus) => 1,
            Some(Token::Star) | Some(Token::Slash) => 2,
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
            Err(
                ParseError::new(format!("Expected an identifier but found '{:?}'", token), span)
                    .into(),
            )
        }
    }
}