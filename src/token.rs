//! 字句解析の結果であるトークンの種類を定義します。

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Fn,
    Let,
    If,
    Else,
    True,
    False,
    Print,

    // Identifier and Literals
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // Symbols
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    Dollar,   // $
    Colon,    // :
    Comma,    // ,
    Equals,   // =
    Arrow,    // ->
    Semicolon,// ;

    // Operators
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /

    // Comparison Operators
    EqualsEquals,   // ==
    BangEquals,     // !=
    LessThan,       // <
    LessThanEquals, // <=
    GreaterThan,    // >
    GreaterThanEquals, // >=
}

impl Token {
    /// 文字列がキーワードに一致する場合、対応するTokenを返す
    pub fn from_keyword(s: &str) -> Option<Self> {
        match s {
            "fn" => Some(Token::Fn),
            "let" => Some(Token::Let),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            "print" => Some(Token::Print),
            _ => None,
        }
    }
}