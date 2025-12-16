use std::{collections::HashMap, num::IntErrorKind, str::FromStr};

use crate::{
    error::{ErrorType, MGLError},
    loc::SourceLoc,
    saving::{literal::*, token::Token, token_type::TokenType},
};

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    keywords: HashMap<String, TokenType>,
    col: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            keywords: HashMap::from([
                (String::from("class"), TokenType::Class),
                (String::from("struct"), TokenType::Struct),
                (String::from("fn"), TokenType::Fn),
                (String::from("let"), TokenType::Let),
                (String::from("const"), TokenType::Const),
                (String::from("Self"), TokenType::SelfTy),
                (String::from("self"), TokenType::SelfParam),
                (String::from("while"), TokenType::While),
                (String::from("for"), TokenType::For),
                (String::from("if"), TokenType::If),
                (String::from("else"), TokenType::Else),
                (String::from("true"), TokenType::True),
                (String::from("false"), TokenType::False),
                (String::from("return"), TokenType::Return),
                (String::from("pub"), TokenType::Pub),
                (String::from("and"), TokenType::And),
                (String::from("or"), TokenType::Or),
            ]),
            col: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, MGLError> {
        while !self.is_at_end() {
            self.start = self.current;

            match self.scan_token() {
                Ok(_) => {}
                Err(err) => return Err(err),
            }
        }

        self.tokens
            .push(Token::new(TokenType::EOF, "", None, self.line, self.col));

        Ok(self.tokens.clone())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), MGLError> {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => {
                if self.matches_char('>') {
                    self.add_token(TokenType::Arrow)
                } else {
                    self.add_token(TokenType::Minus)
                }
            }
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::BangEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::EqualEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            }
            '>' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            }
            '/' => {
                if self.matches_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
                self.col = 0;
            }
            '"' => self.string()?,
            ':' => self.add_token(TokenType::Colon),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            _ => {
                if self.is_digit(c) {
                    self.number()?;
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    return Err(MGLError {
                        error_type: ErrorType::SyntaxError,
                        msg: format!("Unexpected char '{}'", c),
                        loc: Some(SourceLoc {
                            line: self.line,
                            col: self.col,
                        }),
                    });
                }
            }
        }

        Ok(())
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];

        let keywords = &self.keywords;

        match keywords.get(text) {
            Some(token_type) => {
                self.add_token(token_type.clone());
            }
            None => {
                self.add_token(TokenType::Ident);
            }
        }
    }

    fn number(&mut self) -> Result<(), MGLError> {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();
            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let lit = match self.parse_number::<usize>() {
            Ok(num) => Literal::UInt(num),
            Err(err) if *err.kind() == IntErrorKind::NegOverflow => {
                match self.parse_number::<isize>() {
                    Ok(num) => Literal::Int(num),
                    Err(err)
                        if *err.kind() == IntErrorKind::NegOverflow
                            || *err.kind() == IntErrorKind::PosOverflow =>
                    {
                        match self.parse_number::<i128>() {
                            Ok(num) => Literal::I128(num),
                            Err(err)
                                if *err.kind() == IntErrorKind::NegOverflow
                                    || *err.kind() == IntErrorKind::PosOverflow =>
                            {
                                return Err(MGLError {
                                    error_type: ErrorType::OverflowException,
                                    msg: String::from("value of type 'i128' is out of range"),
                                    loc: Some(SourceLoc {
                                        line: self.line,
                                        col: self.col,
                                    }),
                                });
                            }
                            _ => panic!("Should be impossible!"),
                        }
                    }
                    _ => panic!("Should be impossible!"),
                }
            }
            Err(err) if *err.kind() == IntErrorKind::PosOverflow => {
                match self.parse_number::<u128>() {
                    Ok(num) => Literal::U128(num),
                    Err(err) if *err.kind() == IntErrorKind::PosOverflow => {
                        return Err(MGLError {
                            error_type: ErrorType::OverflowException,
                            msg: String::from("value of type 'u128' is out of range"),
                            loc: Some(SourceLoc {
                                line: self.line,
                                col: self.col,
                            }),
                        });
                    }
                    _ => panic!("Should be impossible!"),
                }
            }
            Err(err) if *err.kind() == IntErrorKind::InvalidDigit => {
                match self.parse_number::<f64>() {
                    Ok(num) => Literal::Float(num),
                    Err(_) => {
                        return Err(MGLError {
                            error_type: ErrorType::SyntaxError,
                            msg: String::from("invalid digits for a number"),
                            loc: Some(SourceLoc {
                                line: self.line,
                                col: self.col,
                            }),
                        });
                    }
                }
            }
            _ => panic!("Should be impossible!"),
        };

        self.add_token_lit(TokenType::Number, Some(lit));
        Ok(())
    }

    fn parse_number<F>(&self) -> Result<F, F::Err>
    where
        F: FromStr,
    {
        self.source[self.start..self.current].parse::<F>()
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn string(&mut self) -> Result<(), Error> {
        let start = SourceLocation {
            line: self.line,
            col: self.col,
        };
        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(MGLError {
                error_type: ErrorType::SyntaxError,
                msg: String::from("unclosed string"),
                loc: Some(SourceLoc {
                    line: self.line,
                    col: self.col,
                }),
            });
        }

        self.advance();

        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token_lit(
            TokenType::String,
            Some(Value::Literal(Literal::String(value))),
        );

        Ok(())
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.as_bytes()[self.current] as char
    }

    fn peek_next(&self) -> char {
        if (self.current + 1) >= self.source.len() {
            return '\0';
        }

        self.source.as_bytes()[self.current + 1] as char
    }

    fn advance(&mut self) -> char {
        let bytes = self.source.as_bytes();
        self.current += 1;
        self.col += 1;

        bytes[self.current - 1] as char
    }

    fn matches_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.as_bytes()[self.current] as char != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn add_token(&mut self, token: TokenType) {
        self.add_token_lit(token, None);
    }

    fn add_token_lit(&mut self, token: TokenType, literal: Option<Literal>) {
        let text = &self.source[self.start..self.current];

        self.tokens
            .push(Token::new(token, text, literal, self.line, self.col));
    }
}
