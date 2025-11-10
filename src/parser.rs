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

        // 【修正点】関数定義の `(` は LParen と CallLParen の両方を許容する
        if self.peek() == Some(&Token::LParen) {
            self.consume(Token::LParen)?;
        } else {
            self.consume(Token::CallLParen)?;
        }

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
            if self.check_and_consume(Token::Semicolon) {
                // オプショナルなセミコロンを許容する (例: `let a = 1;;`)
                while self.peek() == Some(&Token::Semicolon) {
                    self.advance();
                }
            } else {
                // ブロックの最後の式（セミコロンなし）の直後が } でない場合はエラー
                if self.peek() != Some(&Token::RBrace) {
                    return Err(ParseError::new("Expected semicolon or '}' after statement", self.peek_span()).into());
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
            Some(Token::Let) => self.parse_let_def(),
            _ => self.parse_expression(),
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

    /// 式をパースする
    fn parse_expression(&mut self) -> Result<RawAstNode, LangError> {
        if self.peek() == Some(&Token::LBrace) {
            self.parse_block()
        } else {
            self.parse_sexpression()
        }
    }

    /// S式（S-expression）をパースする
    fn parse_sexpression(&mut self) -> Result<RawAstNode, LangError> {
        let mut parts = Vec::new();
        // 式の終わりは、文脈を区切るトークン
        while !self.is_at_end() {
            match self.peek() {
                // 式の区切りならループを終了
                Some(Token::Comma) | Some(Token::Semicolon) | Some(Token::RBrace) | Some(Token::LBrace) | Some(Token::RParen) | Some(Token::Else) => break,
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
            return Err(
                ParseError::new("Expected an expression, but found nothing.", self.peek_span())
                    .into(),
            );
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
            Token::Identifier(_) | Token::IntLiteral(_) | Token::FloatLiteral(_) | Token::StringLiteral(_) | Token::True | Token::False => {
                let (t, s) = self.advance();
                Ok(RawExprPart::Token(t, s))
            }
            Token::Dollar => self.parse_math_block(),
            // `LParen` は常にS式グループの開始
            Token::LParen => self.parse_s_expr_group(),
            Token::Colon => self.parse_type_annotation(),
            Token::If => self.parse_if_as_part(),
            // CallLParenは `parse_sexpression` の中で処理されるため、ここに来たらエラー
            Token::CallLParen => {
                Err(ParseError::new("Unexpected token: C-style parenthesis cannot start an expression part.", *span).into())
            }
            _ => {
                Err(ParseError::new(format!("Unexpected token in expression: {:?}", token), *span)
                    .into())
            }
        }
    }
    
    /// S式の一部として `if <cond> { ... } else { ... }` をパースする
    fn parse_if_as_part(&mut self) -> Result<RawExprPart, LangError> {
        let start_span = self.consume(Token::If)?.1;
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        
        let else_branch = if self.check_and_consume(Token::Else) {
            // `else if ...` は `else` の後に続く式として解釈する
            // `else {` の場合はブロックとして解釈する
            if self.peek() == Some(&Token::LBrace) {
                self.parse_block()?
            } else {
                self.parse_expression()?
            }
        } else {
             // elseがない場合、Unitを返す空のブロックを生成する
            let dummy_span = self.peek_span();
            RawAstNode::Block { statements: vec![], span: dummy_span }
        };

        let end_span = else_branch.span();

        Ok(RawExprPart::IfExpr {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
            span: combine_spans(start_span, end_span)
        })
    }

    /// `: type` 型注釈をパースする
    fn parse_type_annotation(&mut self) -> Result<RawExprPart, LangError> {
        self.consume(Token::Colon)?;
        let (type_name, type_span) = self.consume_identifier()?;
        Ok(RawExprPart::TypeAnnotation(type_name, type_span))
    }
    
    /// `( ... )` で囲まれたS式グループをパースする
    fn parse_s_expr_group(&mut self) -> Result<RawExprPart, LangError> {
        let start_span = self.consume(Token::LParen)?.1;
        // グループの中身は、単一のS式としてパースする
        let inner_expr = self.parse_sexpression()?;
        let end_span = self.consume(Token::RParen)?.1;
        
        if let RawAstNode::Expr(parts) = inner_expr {
            Ok(RawExprPart::Group(parts, combine_spans(start_span, end_span)))
        } else {
            // parse_sexpressionは常にExprを返すはず
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
        Ok(RawExprPart::CStyleArgs(args, combine_spans(start_span, end_span)))
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
            let (token, span) = self.peek_full().ok_or_else(|| ParseError::new("Unexpected end of math expression", self.peek_span()))?.clone();
            match token {
                Token::IntLiteral(_) | Token::FloatLiteral(_) => {
                    let (t, s) = self.advance();
                    match t {
                        Token::IntLiteral(v) => MathAstNode::Literal(MathLiteral::Int(v), s),
                        Token::FloatLiteral(v) => MathAstNode::Literal(MathLiteral::Float(v), s),
                        _ => unreachable!()
                    }
                }
                Token::Identifier(name) => {
                    self.advance(); // 識別子を消費
                    // 数式ブロック内では、トップレベルのパーサーとは異なり、識別子の次に'('があれば、
                    // 間に空白があってもC-style呼び出しとみなす。
                    if self.peek() == Some(&Token::LParen) || self.peek() == Some(&Token::CallLParen) {
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
                        format!("Expected literal, variable, function call or '(' in math expression, found {:?}", token),
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
            Token::EqualsEquals | Token::BangEquals | Token::LessThan
            | Token::LessThanEquals | Token::GreaterThan | Token::GreaterThanEquals => 1,
            Token::Plus | Token::Minus => 2,
            Token::Star | Token::Slash => 3,
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

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
                assert!(matches!(&parts[0], RawExprPart::Token(Token::IntLiteral(123), _)));
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
                assert!(matches!(&parts[0], RawExprPart::Token(Token::Identifier(s), _) if s == "add"));
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