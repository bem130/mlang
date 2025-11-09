//! ソースコード文字列をトークンのシーケンスに変換する字句解析器(Lexer)。

use crate::span::Span;
use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

/// 字句解析器
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    /// 新しいLexerを生成する
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            line: 1,
            column: 1,
        }
    }

    /// ソースコード全体をトークンに変換する
    pub fn tokenize_all(&mut self) -> Result<Vec<(Token, Span)>, String> {
        let mut tokens = Vec::new();
        while self.input.peek().is_some() {
            if let Some(token) = self.next_token()? {
                tokens.push(token);
            }
        }
        Ok(tokens)
    }

    /// 次の1トークンを解析して返す
    fn next_token(&mut self) -> Result<Option<(Token, Span)>, String> {
        let span = self.span();
        let Some(char) = self.next_char() else { return Ok(None) };

        match char {
            // 空白や改行はスキップ
            c if c.is_whitespace() => Ok(None),
            // コメントは行末までスキップ
            '/' if self.peek() == Some(&'/') => {
                self.consume_line_comment();
                Ok(None)
            }
            // シンボル
            '(' => Ok(Some((Token::LParen, span))),
            ')' => Ok(Some((Token::RParen, span))),
            '{' => Ok(Some((Token::LBrace, span))),
            '}' => Ok(Some((Token::RBrace, span))),
            '$' => Ok(Some((Token::Dollar, span))),
            ':' => Ok(Some((Token::Colon, span))),
            ',' => Ok(Some((Token::Comma, span))),
            ';' => Ok(Some((Token::Semicolon, span))),
            '+' => Ok(Some((Token::Plus, span))),
            '-' if self.peek() == Some(&'>') => {
                self.next_char(); // '>'を消費
                Ok(Some((Token::Arrow, span)))
            }
            // `-` の後に数字が続く場合は、負の数リテラルとして扱う
            '-' if self.peek().map_or(false, |c| c.is_ascii_digit()) => {
                Ok(Some((self.consume_number(char), span)))
            }
            '-' => Ok(Some((Token::Minus, span))),
            '*' => Ok(Some((Token::Star, span))),
            '/' => Ok(Some((Token::Slash, span))),

            // 1文字 or 2文字の演算子
            '=' => {
                if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::EqualsEquals, span)))
                } else {
                    Ok(Some((Token::Equals, span)))
                }
            }
            '!' => {
                if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::BangEquals, span)))
                } else {
                    Err(format!("Unexpected character: {} at {}", char, span))
                }
            }
            '<' => {
                if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::LessThanEquals, span)))
                } else {
                    Ok(Some((Token::LessThan, span)))
                }
            }
            '>' => {
                if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::GreaterThanEquals, span)))
                } else {
                    Ok(Some((Token::GreaterThan, span)))
                }
            }
            
            // 文字列リテラル
            '"' => Ok(Some((self.consume_string()?, span))),

            // 数値リテラル
            '0'..='9' => Ok(Some((self.consume_number(char), span))),
            // 識別子 or キーワード
            c if is_ident_start(c) => Ok(Some((self.consume_identifier(char), span))),
            _ => Err(format!("Unexpected character: {} at {}", char, span)),
        }
    }

    // --- ヘルパー関数 ---

    fn span(&self) -> Span { Span { line: self.line, column: self.column } }
    fn next_char(&mut self) -> Option<char> {
        let char = self.input.next()?;
        if char == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(char)
    }
    fn peek(&mut self) -> Option<&char> { self.input.peek() }
    
    fn consume_line_comment(&mut self) {
        while let Some(c) = self.peek() {
            if *c == '\n' { break; }
            self.next_char();
        }
    }
    
    fn consume_string(&mut self) -> Result<Token, String> {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if *c == '"' {
                break;
            }
            s.push(self.next_char().unwrap());
        }
        
        if self.peek().is_none() {
            return Err("Unterminated string literal".to_string());
        }
        self.next_char(); // `"` を消費
        Ok(Token::StringLiteral(s))
    }

    fn consume_number(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || *c == '.' {
                s.push(self.next_char().unwrap());
            } else {
                break;
            }
        }
        // `.`が含まれているかどうかでIntかFloatかを判断する
        if s.contains('.') {
            Token::FloatLiteral(s.parse().expect("Failed to parse float literal"))
        } else {
            Token::IntLiteral(s.parse().expect("Failed to parse integer literal"))
        }
    }

    fn consume_identifier(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);
        while let Some(c) = self.peek() {
            if is_ident_continue(*c) {
                s.push(self.next_char().unwrap());
            } else {
                break;
            }
        }
        Token::from_keyword(&s).unwrap_or(Token::Identifier(s))
    }
}

fn is_ident_start(c: char) -> bool { c.is_alphabetic() || c == '_' }
fn is_ident_continue(c: char) -> bool { c.is_alphanumeric() || c == '_' }