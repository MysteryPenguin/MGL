use std::collections::HashMap;
use crate::ARGS;

fn keywords() -> HashMap<String, TokenType> {
    let mut keywords: HashMap<String, TokenType> = HashMap::new();
    keywords.insert(String::from("and"),     TokenType::And);
    keywords.insert(String::from("class"),   TokenType::Class);
    keywords.insert(String::from("else"),    TokenType::Else);
    keywords.insert(String::from("false"),   TokenType::False);
    keywords.insert(String::from("for"),     TokenType::For);
    keywords.insert(String::from("fn"),      TokenType::Fn);
    keywords.insert(String::from("if"),      TokenType::If);
    keywords.insert(String::from("none"),    TokenType::None);
    keywords.insert(String::from("or"),      TokenType::Or);
    keywords.insert(String::from("print"),   TokenType::Print);
    keywords.insert(String::from("return"),  TokenType::Return);
    keywords.insert(String::from("super"),   TokenType::Super);
    keywords.insert(String::from("this"),    TokenType::This);
    keywords.insert(String::from("true"),    TokenType::True);
    keywords.insert(String::from("let"),     TokenType::Let);
    keywords.insert(String::from("while"),   TokenType::While);
    keywords
}

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: String::from(source),
            tokens: Vec::new(),
            start: 0,
            current: 1,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, String> {
        let mut errors: Vec<String> = Vec::new();
        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(_) => (),
                Err(msg) => errors.push(msg),
            }
        }

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            literal: None,
            line: self.line,
        });

        if errors.len() > 0 {
            let mut joined = String::new();
            for msg in &errors {
                joined.push_str(msg);
                joined.push_str("\n");
            }
            return Err(joined);
        }
        Ok(self.tokens.clone())
    }

    fn is_at_end(self: &Self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance();

        match c {
            '(' => Ok(self.add_token(TokenType::LeftParen)),
            ')' => Ok(self.add_token(TokenType::RightParen)),
            '{' => Ok(self.add_token(TokenType::LeftBrace)),
            '}' => Ok(self.add_token(TokenType::RightBrace)),
            ',' => Ok(self.add_token(TokenType::Comma)),
            '.' => Ok(self.add_token(TokenType::Dot)),
            ';' => Ok(self.add_token(TokenType::Semicolon)),
            '+' => Ok(self.add_token(TokenType::Plus)),
            '-' => Ok(self.add_token(TokenType::Minus)),
            '*' => Ok(self.add_token(TokenType::Star)),
            '!' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::BangEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token);
                Ok(())
            },
            '=' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::EqualEqual
                } else {
                    TokenType::LessEqual
                };
                self.add_token(token);
                Ok(())
            }
            '>' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token);
                Ok(())
            }
            '<' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token);
                Ok(())
            }
            '/' => {
                if self.matches_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(())
                } else {
                    self.add_token(TokenType::Slash);
                    Ok(())
                }
            }
            ' ' | '\r' | '\t' => Ok(()),
            '\n' => {
                self.line += 1;
                Ok(())
            }
            '"' => {
                self.string()?; 
                Ok(())
            },
            'o' => {
                if self.matches_char('r') {
                    self.add_token(TokenType::Or);
                    Ok(())
                } else {
                    return Err(format!("Unexpected character at line {}: {}.", self.line, c));
                }
            }
            _ => {
                if self.is_digit(c) {
                    self.number();
                    Ok(())
                } else if self.is_alpha(c) {
                    self.identifier();
                    Ok(())
                } else {
                    return Err(format!("Unexpected character at line {}: {}", self.line, c));
                }
            }
        }
    }

    fn is_alpa_num(&self, c: char) -> bool {
        return self.is_alpha(c) || self.is_digit(c);
    }

    fn is_alpha(&self, c: char) -> bool {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    fn identifier(&mut self) {
        let peek = self.peek();
        let mut text = String::new();
        let keywords = keywords();
        
        while self.is_alpa_num(peek) {
            self.advance();
        }

        for i in self.start..self.current {
            text.push(self.source.as_bytes()[i] as char);
        }

        let mut r#type = keywords.get(&text);

        if r#type.is_none() {
            r#type = Some(&TokenType::Identifier);
        }

        self.add_token(r#type.unwrap().clone());
    }

    fn peek_next(&self) -> char {
        let c = self.source.as_bytes()[self.current + 1];
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        c as char
    }

    fn is_digit(&self, c: char) -> bool {
        return c >= '0' && c <= '9';
    }

    fn number(self: &mut Self) {
        let peek = self.peek();
        let mut float: String = String::new();
        while self.is_digit(peek) {
            self.advance();
        }
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();
            
            while self.is_digit(peek) {
                self.advance();
            }
        }

        for i in self.start..self.current {
            float.push(self.source.as_bytes()[i] as char);
        }

        self.add_token_lit(TokenType::Number, Some(LiteralValue::FloatValue(float.parse().unwrap())));
    }

    fn peek(&mut self) -> char {
        let chars: Vec<char> = self.source.chars().collect();

        if !self.is_at_end() {
            return '\0';
        }

        chars[self.current - 1]
    }

    fn string(&mut self) -> Result<(), String> {
        let mut value = String::new();
        let mut space = String::new();
        let lenght: usize = self.line
            .to_string()
            .len();
        let mut i = 1;

        while i <= lenght {
            space.push(' ');
            i += 1;
        }

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() && self.peek() != '"' {
            unsafe {
                return Err(format!(
                    "\u{001b}[1;31m000: \u{001b}[1;37mUnterminated string.
    \u{001b}[38;5;159m--> \u{001b}[0m{}, line {}, caracter {}
 {} |
 {} | {}
 {} |", ARGS[1], self.line, self.current, space, self.line, self.source, space));
            }
        }

        self.advance();

        for i in self.start..self.current - 2 {
            value.push(self.source.as_bytes()[i] as char);
        }

        self.add_token_lit(TokenType::String, Some(LiteralValue::StringValue(value)));
        Ok(())
    }

    fn matches_char(&mut self, expected: char) -> bool {
        let chars: Vec<char> = self.source.chars().collect();

        if self.is_at_end() {
            return false;
        }

        if chars[self.current + 1] != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn advance(&mut self) -> char {
        let c = self.source.as_bytes()[self.current - 1];
        self.current += 1;

        c as char
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_lit(token_type, None);
    }

    fn add_token_lit(self: &mut Self, token_type: TokenType, literal: Option<LiteralValue>) {
        let mut text = String::new();
        for i in self.start..self.current {
            text.push(self.source.as_bytes()[i - 1] as char);
        }

        self.tokens.push(Token {
            token_type,
            lexeme: text,
            literal,
            line: self.line,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<LiteralValue>,
    line: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<LiteralValue>,
        line: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    pub fn to_string(&self) -> String {
        format!("{} {} {:?}", self.token_type, self.lexeme, self.literal)
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    // Sigle-char tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // 1 or 2 chars
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fn,
    For,
    If,
    None,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    While,

    Eof,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    IntValue(i64),
    StringValue(String),
    FloatValue(f64),
    IdentifierValue(String),
}
