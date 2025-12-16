use crate::error::{ErrorType, MGLError};
use crate::loc::SourceLoc;
use crate::saving::expr::ClosureDecl;
use crate::saving::pattern::Pattern;
use crate::saving::r#type::{FnType, Type};

use crate::saving::{
    expr::Expr,
    item::{FnItem, GlobalVarItem, Item, ObjectTemplItem},
    literal::Literal,
    stmt::Stmt,
    symbol::Symbol,
    token::Token,
    token_type::TokenType,
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Decl>, MGLError> {
        let mut items: Vec<Item> = Vec::new();

        while !self.is_at_end() {
            let item = self.item()?;
            items.push(decl);
        }

        Ok(items)
    }

    fn expression(&mut self) -> Result<Expr, MGLError> {
        self.assignment()
    }

    fn item(&mut self) -> Result<Item, MGLError> {
        let public = self.match_tokens(&[TokenType::Pub]);

        if self.match_tokens(&[TokenType::Class]) {
            return Ok(Item::Class(self.object_templ_item(public)?));
        }
        if self.match_tokens(&[TokenType::Struct]) {
            return Ok(Item::Struct(self.object_templ_item(public)?));
        }
        if self.match_tokens(&[TokenType::Fn]) {
            return Ok(Item::Fn(self.function(public)?));
        }
        if self.match_tokens(&[TokenType::Const]) {
            return Ok(Item::GlobalVar(self.global_var_item(public)?));
        }

        Err(MGLError {
            error_type: ErrorType::StructureError,
            msg: String::from("only items are allowed in the global scope."),
            loc: Some(self.peek().to_source_loc()),
        })
    }

    fn function(&mut self, public: bool) -> Result<FnItem, MGLError> {
        let name = self
            .consume(
                TokenType::Ident,
                String::from("expect identifier after function keyword"),
            )?
            .to_symbol();
    }

    fn pattern(&mut self) -> Result<Pattern, MGLError> {
        if self.match_tokens(&[TokenType::LeftParen]) {}
    }

    fn tuple_pat(&mut self) -> Result<Pattern, MGLError> {}

    fn stmt(&mut self) -> Result<Stmt, MGLError> {
        if self.match_tokens(&[TokenType::For]) {
            return self.for_stmt();
        }
        if self.match_tokens(&[TokenType::If]) {
            return self.if_stmt();
        }
        if self.match_tokens(&[TokenType::Return]) {
            return self.return_stmt();
        }
        if self.match_tokens(&[TokenType::While]) {
            return self.while_stmt();
        }

        let token = self.peek().clone();
        if self.match_tokens(&[TokenType::LeftBrace]) {
            return Ok(Stmt::Block(self.block(token.to_source_location())?));
        }
        if self.match_tokens(&[TokenType::Import]) {
            return Ok(Stmt::Decl(Decl::Import(self.import_stmt()?)));
        }

        self.expression_stmt()
    }

    fn import_stmt(&mut self) -> Result<Import, MGLError> {
        let mut imports: Vec<Symbol> = Vec::new();

        if !self.match_tokens(&[TokenType::From]) {
            loop {
                imports.push(
                    self.consume_expected_kind(
                        TokenType::Identifier,
                        String::from("identifier"),
                        String::from("as import"),
                    )?
                    .to_symbol(),
                );

                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume_expected_kind(
            TokenType::From,
            String::from("'from'"),
            String::from("after imports"),
        )?;

        let file_path_token = self.consume_expected_kind(
            TokenType::String,
            String::from("file path"),
            String::from("after from"),
        )?;
        let file_path = Symbol::new(
            file_path_token.literal.unwrap().to_string().unwrap(),
            file_path_token.line,
            file_path_token.col,
        );

        self.semicolon();

        Ok(Import { imports, file_path })
    }

    fn object_templ_item(&mut self) -> Result<ClassDecl, MGLError> {
        let name = self
            .consume_expected_kind(
                TokenType::Identifier,
                String::from("class name"),
                String::from("in class declaration"),
            )?
            .to_symbol();
        let left_brace =
            self.consume_expected(TokenType::LeftBrace, '{', String::from("after class body"))?;

        let mut methods = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function(String::from("method"))?);
        }

        self.consume_unclosed(TokenType::RightBrace, '}', left_brace.to_source_location())?;

        Ok(ClassDecl { name, methods })
    }

    fn for_stmt(&mut self) -> Result<Stmt, MGLError> {
        let left_paren =
            self.consume_expected(TokenType::LeftParen, '(', String::from("after for"))?;

        let initializer = if self.match_tokens(&[TokenType::Semicolon]) {
            None
        } else if self.match_tokens(&[TokenType::Let]) {
            Some(Stmt::Decl(Decl::Var(self.var_declaration()?)))
        } else {
            Some(self.expression_stmt()?)
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            self.expression()?
        } else {
            let token = self.peek();
            Expr::Literal {
                lit: Value::Literal(Literal::Bool(true)),
                loc: SourceLocation {
                    line: token.line,
                    col: token.col,
                },
            }
        };
        self.consume_expected(
            TokenType::Semicolon,
            ';',
            String::from("after loop condition"),
        )?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume_unclosed(TokenType::RightParen, ')', left_paren.to_source_location())?;
        let mut body = self.stmt()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }
        body = Stmt::While {
            condition,
            body: Box::new(body),
        };

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn while_stmt(&mut self) -> Result<Stmt, MGLError> {
        let condition = self.expression()?;

        let body = self.stmt()?;

        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    fn if_stmt(&mut self) -> Result<Stmt, MGLError> {
        let condition = self.expression()?;

        let then_branch = Box::new(self.stmt()?);
        let else_branch = if self.match_tokens(&[TokenType::Else]) {
            Some(Box::new(self.stmt()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn return_stmt(&mut self) -> Result<Stmt, MGLError> {
        let loc = self.previous().to_source_location();

        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.semicolon();
        Ok(Stmt::Return { loc, value })
    }

    fn expression_stmt(&mut self) -> Result<Stmt, MGLError> {
        let expr = self.expression()?;
        self.semicolon();

        Ok(Stmt::Expr(expr))
    }

    fn block(&mut self, loc: SourceLocation) -> Result<Vec<Stmt>, MGLError> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume_unclosed(TokenType::RightBrace, '}', loc)?;

        Ok(stmts)
    }

    fn match_type(&mut self) -> Result<Type, MGLError> {
        let token = self.peek().clone();
        self.advance();
        match token.token_type {
            TokenType::Identifier => Ok(token.to_symbol().to_type()),
            TokenType::LeftParen => Ok(Type::Literal(Some(LiteralType::Fn(self.function_type()?)))),
            _ => Err(self.MGLError_builder.build(MGLErrorType::ExpectedKind {
                kind: String::from("type"),
                message: String::from("after type annotation"),
                loc: [token.to_source_location()],
            })),
        }
    }

    fn var_declaration(&mut self) -> Result<VarDecl, MGLError> {
        let sym = self
            .consume_expected_kind(
                TokenType::Identifier,
                String::from("variable name"),
                String::from("in var declaration"),
            )?
            .to_symbol();

        let mut init = None;
        let mut r#type = None;

        if self.match_tokens(&[TokenType::Colon]) {
            r#type = Some(self.match_type()?);
        }
        if self.match_tokens(&[TokenType::Equal]) {
            init = Some(self.expression()?);
        }

        self.semicolon();

        Ok(VarDecl { sym, init, r#type })
    }

    fn function_type(&mut self) -> Result<FnType, MGLError> {
        let mut params = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(self.MGLError_builder.build(MGLErrorType::Overflow {
                        allowed: 255,
                        value: TryInto::<u32>::try_into(params.len()).unwrap(),
                        item: String::from("parameters of closures"),
                        count_type: String::from("count"),
                        loc: [self.peek().to_source_location()],
                    }));
                }
                let r#type = self.match_type()?;

                params.push(r#type);

                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume_expected(TokenType::RightParen, ')', String::from("after parameters"))?;

        self.consume_expected_kind(
            TokenType::Arrow,
            String::from("'->'"),
            String::from("after parameter types"),
        )?;

        let return_type = Box::new(self.match_type()?);

        Ok(FnType {
            params,
            return_type,
        })
    }

    fn semicolon(&mut self) {
        if self.check(&TokenType::Semicolon) {
            self.advance();
        }
    }
}
