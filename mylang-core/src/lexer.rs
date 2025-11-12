//! ソースコード文字列をトークンのシーケンスに変換する字句解析器(Lexer)。

extern crate alloc;
use crate::span::Span;
use crate::token::Token;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::iter::Peekable;
use core::str::Chars;

/// 字句解析器
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    /// 直前に生成したトークンが識別子やリテラルの一部だったかを示すフラグ。
    /// これにより、`ident()`と`ident ()`を区別する。
    last_char_was_ident_part: bool,
}

impl<'a> Lexer<'a> {
    /// 新しいLexerを生成する
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            line: 1,
            column: 1,
            last_char_was_ident_part: false,
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
        let Some(char) = self.next_char() else {
            return Ok(None);
        };

        match char {
            // 空白や改行はスキップし、識別子フラグをリセットする
            c if c.is_whitespace() => {
                self.last_char_was_ident_part = false;
                Ok(None)
            }
            // コメントは行末までスキップ
            '/' if self.peek() == Some(&'/') => {
                self.consume_line_comment();
                self.last_char_was_ident_part = false;
                Ok(None)
            }
            // --- '()' のコンテキスト判断 ---
            '(' => {
                let token = if self.last_char_was_ident_part {
                    Token::CallLParen
                } else {
                    Token::LParen
                };
                self.last_char_was_ident_part = false; // `(`自体は識別子の一部ではない
                Ok(Some((token, span)))
            }
            // シンボルは識別子フラグをリセットする
            ')' | '{' | '}' | '$' | ',' | ';' | '+' | '*' | '/' => {
                self.last_char_was_ident_part = false;
                let token = match char {
                    ')' => Token::RParen,
                    '{' => Token::LBrace,
                    '}' => Token::RBrace,
                    '$' => Token::Dollar,
                    ',' => Token::Comma,
                    ';' => Token::Semicolon,
                    '+' => Token::Plus,
                    '*' => Token::Star,
                    '/' => Token::Slash,
                    _ => unreachable!(),
                };
                Ok(Some((token, span)))
            }
            ':' => {
                self.last_char_was_ident_part = false;
                if self.peek() == Some(&':') {
                    self.next_char();
                    Ok(Some((Token::DoubleColon, span)))
                } else {
                    Ok(Some((Token::Colon, span)))
                }
            }

            // 複数文字の可能性があるシンボル
            '-' if self.peek() == Some(&'>') => {
                self.next_char(); // '>'を消費
                self.last_char_was_ident_part = false;
                Ok(Some((Token::Arrow, span)))
            }
            '-' if self.peek().map_or(false, |c| c.is_ascii_digit()) => {
                let token = self.consume_number(char);
                self.last_char_was_ident_part = true;
                Ok(Some((token, span)))
            }
            '-' => {
                self.last_char_was_ident_part = false;
                Ok(Some((Token::Minus, span)))
            }

            '=' => {
                self.last_char_was_ident_part = false;
                if self.peek() == Some(&'>') {
                    self.next_char();
                    Ok(Some((Token::FatArrow, span)))
                } else if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::EqualsEquals, span)))
                } else {
                    Ok(Some((Token::Equals, span)))
                }
            }
            '!' => {
                self.last_char_was_ident_part = false;
                if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::BangEquals, span)))
                } else {
                    Err(format!("Unexpected character: {} at {}", char, span))
                }
            }
            '<' => {
                self.last_char_was_ident_part = false;
                if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::LessThanEquals, span)))
                } else {
                    Ok(Some((Token::LessThan, span)))
                }
            }
            '>' => {
                self.last_char_was_ident_part = false;
                if self.peek() == Some(&'=') {
                    self.next_char();
                    Ok(Some((Token::GreaterThanEquals, span)))
                } else {
                    Ok(Some((Token::GreaterThan, span)))
                }
            }

            // リテラルや識別子はフラグをセットする
            '"' => {
                let token = self.consume_string()?;
                self.last_char_was_ident_part = true;
                Ok(Some((token, span)))
            }
            '0'..='9' => {
                let token = self.consume_number(char);
                self.last_char_was_ident_part = true;
                Ok(Some((token, span)))
            }
            c if is_ident_start(c) => {
                let token = self.consume_identifier(char);
                self.last_char_was_ident_part = true;
                Ok(Some((token, span)))
            }

            _ => Err(format!("Unexpected character: {} at {}", char, span)),
        }
    }

    // --- ヘルパー関数 ---

    fn span(&self) -> Span {
        Span {
            line: self.line,
            column: self.column,
        }
    }
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
    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn consume_line_comment(&mut self) {
        while let Some(c) = self.peek() {
            if *c == '\n' {
                break;
            }
            self.next_char();
        }
    }

    fn consume_string(&mut self) -> Result<Token, String> {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if *c == '"' {
                break;
            }

            let current_char = self.next_char().unwrap();
            // エスケープシーケンスの処理
            if current_char == '\\' {
                match self.peek() {
                    Some('n') => {
                        self.next_char(); // 'n' を消費
                        s.push('\n');
                    }
                    Some('t') => {
                        self.next_char(); // 't' を消費
                        s.push('\t');
                    }
                    Some('\\') => {
                        self.next_char(); // '\' を消費
                        s.push('\\');
                    }
                    Some('"') => {
                        self.next_char(); // '"' を消費
                        s.push('"');
                    }
                    Some(other) => {
                        return Err(format!("Unknown escape sequence '\\{}'", other));
                    }
                    None => return Err("Unterminated string literal".to_string()),
                }
            } else {
                s.push(current_char);
            }
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

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}
fn is_ident_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}
