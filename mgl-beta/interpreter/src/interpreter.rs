use std::collections::HashMap;

use crate::{
    File,
    enviroment::Enviroment,
    lexer::Lexer,
    parser::Parser,
    saving::{
        callable::{Class, Closure, Function, INIT, Instance, NativeFn, as_callable},
        error::{Error, ErrorBuilder, ErrorType},
        expr::Expr,
        project::Project,
        stmt::{ClassDecl, Decl, FnDecl, Import, Stmt, VarDecl},
        symbol::{SourceLocation, Symbol},
        token::Token,
        token_type::TokenType,
        literal::{Identifier, Imported, Literal, Value},
    },
};

#[derive(Clone)]
pub struct Interpreter {
    pub enviroment: Enviroment,
    pub globals: Enviroment,
    pub return_value: Option<Value>,
    pub counter: u64,
    pub classes: HashMap<u64, Class>,
    pub functions: HashMap<u64, Function>,
    pub instances: HashMap<u64, Instance>,
    pub closures: HashMap<u64, Closure>,
    pub error_builder: ErrorBuilder,
    pub project: Project,
    pub modules: HashMap<String, Interpreter>,
}

impl Interpreter {
    pub fn interpret(&mut self, decls: Vec<Decl>) -> Result<(), Error> {
        for decl in decls {
            self.execute_decl(decl, false)?;
        }

        Ok(())
    }

    pub fn interpret_stmts(&mut self, stmts: Vec<Stmt>) -> Result<(), Error> {
        for stmt in stmts {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute_decl(&mut self, decl: Decl, is_pub: bool) -> Result<(), Error> {
        match decl {
            Decl::Var(VarDecl { sym, init, r#type }) => match (init, r#type) {
                (Some(expr), Some(r#type)) => {
                    let value = self.evaluate(expr)?;
                    self.enviroment.define(
                        sym.clone(),
                        Identifier::with_type(self, sym, r#type, value, is_pub)?,
                    );
                }
                (None, Some(r#type)) => self.enviroment.define(
                    sym.clone(),
                    Identifier::with_type(
                        self,
                        sym.clone(),
                        r#type,
                        Value::Literal(Literal::None),
                        is_pub,
                    )?,
                ),
                (Some(expr), None) => {
                    let value = self.evaluate(expr)?;
                    self.enviroment
                        .define(sym.clone(), Identifier::new(self, sym, value, is_pub));
                }
                (None, None) => self.enviroment.define(
                    sym.clone(),
                    Identifier::new(self, sym, Value::Literal(Literal::None), is_pub),
                ),
            },
            Decl::Fn(FnDecl { name, params, body }) => {
                let id = self.alloc_id();
                let function = Function {
                    name: name.clone(),
                    id,
                    params,
                    body,
                    closure: self.enviroment.clone(),
                    instance: None,
                    is_initializer: false,
                };
                self.functions.insert(id, function.clone());
                self.enviroment.define(
                    name.clone(),
                    Identifier::new(
                        self,
                        name.clone(),
                        Value::Literal(Literal::Fn {
                            name: name.clone(),
                            id,
                            instance: None,
                        }),
                        is_pub,
                    ),
                );
            }
            Decl::Class(ClassDecl {
                name: sym,
                methods: stmt_methods,
            }) => {
                let id = self.alloc_id();
                self.enviroment.define(
                    sym.clone(),
                    Identifier::new(
                        self,
                        sym.clone(),
                        Value::Literal(Literal::Class {
                            name: sym.clone(),
                            id,
                        }),
                        is_pub,
                    ),
                );

                let mut methods = HashMap::new();
                for method in stmt_methods.iter() {
                    let func_id = self.alloc_id();

                    methods.insert(method.name.name.clone(), func_id);

                    let is_initializer = method.name.name == INIT;

                    let function = Function {
                        id: func_id,
                        name: method.name.clone(),
                        params: method.params.clone(),
                        body: method.body.clone(),
                        closure: self.enviroment.clone(),
                        instance: None,
                        is_initializer,
                    };

                    self.functions.insert(func_id, function);
                }

                let class = Class {
                    name: sym.clone(),
                    id,
                    methods,
                };

                self.classes.insert(id, class);
            }
            Decl::Pub(decl) => self.execute_decl(*decl, true)?,
            Decl::Import(Import {
                imports,
                file_path: file_path_sym,
            }) => {
                let enviroment = match self.modules.get(&file_path_sym.name) {
                    Some(interpreter) => interpreter.enviroment.clone(),
                    None => {
                        let file_path: Vec<String> =
                            file_path_sym.name.split("/").map(String::from).collect();

                        match self.project.dir.look_for_file(&file_path, 0) {
                            Some(file) => {
                                let tokens = Lexer::new(file.clone()).scan_tokens()?;
                                let decls = Parser::new(tokens, file.clone()).parse()?;
                                let mut interpreter =
                                    Interpreter::new(file.clone(), self.project.clone());
                                interpreter.interpret(decls)?;
                                self.modules
                                    .insert(file_path_sym.name.clone(), interpreter.clone());
                                interpreter.enviroment
                            }
                            None => {
                                return Err(self.error_builder.build(ErrorType::Undefined {
                                    ident: file_path.join("/"),
                                    kind: String::from("module"),
                                    on: String::from("in this project"),
                                    loc: [file_path_sym.to_source_loc()],
                                }));
                            }
                        }
                    }
                };

                for import in imports {
                    let ident = enviroment.get(&import, &self.error_builder)?;

                    if !ident.is_pub {
                        return Err(self.error_builder.build(ErrorType::Undefined {
                            ident: ident.name.name,
                            kind: String::from("identifier"),
                            on: String::new(),
                            loc: [import.to_source_loc()],
                        }));
                    }

                    self.enviroment.define(
                        import.clone(),
                        Identifier::new(
                            self,
                            import.clone(),
                            Value::Imported(Imported {
                                sym: import,
                                url: file_path_sym.name.clone(),
                            }),
                            false,
                        ),
                    );
                }
            }
        }
        Ok(())
    }

    pub fn execute(&mut self, stmt: Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Expr(expr) => {
                self.evaluate(expr)?;
            }
            Stmt::Decl(decl) => self.execute_decl(decl, false)?,
            Stmt::Block(stmts) => {
                self.execute_block(stmts, Enviroment::from(self.enviroment.clone()))?
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_eval = self.evaluate(condition.clone())?;
                let loc = Self::get_loc(condition.clone());

                if self.is_truthy(condition_eval, loc)? == Value::Literal(Literal::Bool(true)) {
                    self.execute(*then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(*else_branch)?;
                }
            }
            Stmt::While { condition, body } => {
                let loc = Self::get_loc(condition.clone());
                let value = self.is_truthy(self.clone().evaluate(condition.clone())?, loc)?;

                while value == Value::Literal(Literal::Bool(true)) {
                    self.execute(*body.clone())?;
                }
            }
            Stmt::Return { loc: _, value } => {
                self.return_value = Some(if let Some(value) = value {
                    self.evaluate(value)?
                } else {
                    Value::Literal(Literal::None)
                });
            }
        }

        Ok(())
    }

    pub fn execute_block(&mut self, stmts: Vec<Stmt>, env: Enviroment) -> Result<(), Error> {
        self.enviroment = Enviroment::from(env);

        for stmt in stmts {
            self.execute(stmt)?;
        }

        if let Some(enclosing) = self.enviroment.enclosing.clone() {
            self.enviroment = *enclosing;
        }

        Ok(())
    }

    fn get_loc(expr: Expr) -> SourceLocation {
        match expr {
            Expr::Binary {
                left: _,
                operator,
                right: _,
            } => operator.to_source_location(),
            Expr::Logical {
                left: _,
                operator,
                right: _,
            } => operator.to_source_location(),
            Expr::Call {
                callee: _,
                loc,
                args: _,
            } => loc,
            Expr::Group(expr) => Self::get_loc(*expr),
            Expr::Literal { lit: _, loc } => loc,
            Expr::Unary { operator, expr: _ } => operator.to_source_location(),
            Expr::Var(symbol) => SourceLocation {
                line: symbol.line,
                col: symbol.col,
            },
            Expr::Assign { sym, value: _ } => SourceLocation {
                line: sym.line,
                col: sym.col,
            },
            Expr::Get { attr, lhs: _ } => SourceLocation {
                line: attr.line,
                col: attr.col,
            },
            Expr::Set {
                lhs: _,
                attr,
                rhs: _,
            } => SourceLocation {
                line: attr.line,
                col: attr.col,
            },
            Expr::This(loc) => loc,
            Expr::Closure { decl: _, loc } => loc,
            Expr::Array { loc, exprs: _ } => loc,
            Expr::Index { expr: _, index } => Self::get_loc(*index),
        }
    }

    fn evaluate(&mut self, expr: Expr) -> Result<Value, Error> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(*left, operator, *right),
            Expr::Group(expr) => self.evaluate(*expr),
            Expr::Literal { lit, loc: _ } => Ok(lit.calc(self)),
            Expr::Unary { operator, expr } => self.eval_unary(operator, *expr),
            Expr::Var(name) => Ok(*self.enviroment.get(&name, &self.error_builder)?.value),
            Expr::Assign { sym, value } => {
                let lit = self.evaluate(*value)?.calc(self);

                if let Value::Imported(imported) =
                    *self.enviroment.get(&sym, &self.error_builder)?.value
                {
                    let error_builder = self.error_builder.clone();
                    let interpreter = self.get_mut_interpreter(
                        &imported.url,
                        SourceLocation {
                            line: sym.line,
                            col: sym.col,
                        },
                    )?;

                    interpreter.enviroment.assign(
                        sym.clone(),
                        Identifier::new(interpreter, sym, lit.clone(), true),
                        &error_builder,
                    )?;
                } else {
                    self.enviroment.assign(
                        sym.clone(),
                        Identifier::new(self, sym, lit.clone(), false),
                        &self.error_builder,
                    )?;
                }

                Ok(lit)
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(*left)?.calc(self);

                if operator.token_type == TokenType::OrOr {
                    if self.is_truthy_operator(left.clone(), operator)?
                        == Value::Literal(Literal::Bool(true))
                    {
                        return Ok(left);
                    }
                } else if self.is_falsy_operator(left.clone(), operator)?
                    == Value::Literal(Literal::Bool(true))
                {
                    return Ok(left);
                }

                self.evaluate(*right)
            }
            Expr::Call {
                callee,
                loc,
                args: arg_exprs,
            } => {
                let callee = self.evaluate(*callee)?;

                let callable = as_callable(self, &callee, loc.clone())?;

                let interpreter = if callable.0 == self.error_builder.0.path.join("/") {
                    self
                } else {
                    self.get_mut_interpreter(&callable.0, loc.clone())?
                };

                let maybe_args: Result<Vec<_>, _> = arg_exprs
                    .iter()
                    .map(|arg| interpreter.evaluate(*arg.clone()))
                    .collect();

                let arity = <u8 as Into<usize>>::into(callable.1.arity(interpreter));
                match maybe_args {
                    Ok(args) => {
                        if args.len() != arity {
                            Err(interpreter.error_builder.build(ErrorType::WrongCalling {
                                expected: arity,
                                got: args.len(),
                                loc: [loc],
                            }))
                        } else {
                            callable.1.call(interpreter, &args)
                        }
                    }
                    Err(err) => Err(err),
                }
            }

            Expr::Get { attr, lhs } => {
                let val = self.evaluate(*lhs.clone())?;

                match val {
                    Value::Literal(Literal::Instance { name, id }) => {
                        if let Value::Imported(Imported { sym: _, url }) =
                            *self.enviroment.get(&name, &self.error_builder)?.value
                        {
                            return self
                                .get_interpreter(
                                    &url,
                                    SourceLocation {
                                        line: name.line,
                                        col: name.col,
                                    },
                                )?
                                .get_instance(id)
                                .get_attr(attr, self);
                        }

                        self.get_instance(id).get_attr(attr, self)
                    }
                    Value::Imported(ref import) => {
                        let interpreter = self.get_mut_interpreter(
                            &import.url,
                            SourceLocation {
                                line: attr.line,
                                col: attr.col,
                            },
                        )?;
                        interpreter.evaluate(Expr::Get {
                            attr: attr.clone(),
                            lhs: Box::new(Expr::Literal {
                                lit: val,
                                loc: SourceLocation {
                                    line: attr.line,
                                    col: attr.col,
                                },
                            }),
                        })
                    }
                    _ => Err(self.error_builder.build(ErrorType::Undefined {
                        ident: attr.name.clone(),
                        kind: String::from("attribute"),
                        on: format!("on value of type {}", val.calc(self).to_type(self)),
                        loc: [attr.to_source_loc()],
                    })),
                }
            }
            Expr::Closure { decl, loc: _ } => {
                let id = self.alloc_id();

                let closure = Closure {
                    id,
                    params: decl.params,
                    body: *decl.body,
                    closure: self.enviroment.clone(),
                };

                self.closures.insert(id, closure);

                Ok(Value::Literal(Literal::Closure(id)))
            }
            Expr::Set { lhs, attr, rhs } => {
                let lhs = self.evaluate(*lhs)?;
                let rhs = self.evaluate(*rhs)?;

                match lhs {
                    Value::Literal(Literal::Instance { name, id }) => {
                        if let Value::Imported(Imported { sym: _, url }) =
                            *self.enviroment.get(&name, &self.error_builder)?.value
                        {
                            match self
                                .get_mut_interpreter(
                                    &url,
                                    SourceLocation {
                                        line: attr.line,
                                        col: attr.col,
                                    },
                                )?
                                .instances
                                .get_mut(&id)
                            {
                                Some(inst) => {
                                    inst.fields.insert(attr.name.clone(), rhs.clone());
                                    return Ok(rhs);
                                }
                                None => panic!("Couldn't find instance with id {id}"),
                            }
                        }
                        match self.instances.get_mut(&id) {
                            Some(inst) => {
                                inst.fields.insert(attr.name.clone(), rhs.clone());
                                Ok(rhs)
                            }
                            None => panic!("Couldn't find instance with id {id}"),
                        }
                    }
                    Value::Imported(ref import) => {
                        let interpreter = self.get_mut_interpreter(
                            &import.url,
                            SourceLocation {
                                line: attr.line,
                                col: attr.col,
                            },
                        )?;
                        interpreter.evaluate(Expr::Set {
                            lhs: Box::new(Expr::Literal {
                                lit: lhs,
                                loc: SourceLocation {
                                    line: attr.line,
                                    col: attr.col,
                                },
                            }),
                            attr: attr.clone(),
                            rhs: Box::new(Expr::Literal {
                                lit: rhs,
                                loc: SourceLocation {
                                    line: attr.line,
                                    col: attr.col,
                                },
                            }),
                        })
                    }
                    _ => Err(self.error_builder.build(ErrorType::Undefined {
                        ident: attr.name.clone(),
                        kind: String::from("attribute"),
                        on: format!("on value of type {}", lhs.calc(self).to_type(self)),
                        loc: [attr.to_source_loc()],
                    })),
                }
            }
            Expr::This(loc) => {
                match self.lookup(Symbol::new(String::from("this"), loc.line, loc.col)) {
                    Ok(value) => Ok(value),
                    Err(err) => Err(err),
                }
            }
            Expr::Array { loc: _, exprs } => {
                let mut values = Vec::new();

                for expr in exprs {
                    values.push(self.evaluate(expr)?);
                }

                Ok(Value::Literal(Literal::Array(values)))
            }
            Expr::Index { expr, index } => {
                let value = self.evaluate(*expr)?;
                let index = self.evaluate(*index)?;
                match (value, index) {
                    (Value::Literal(Literal::Array(values)), Value::Literal(Literal::Number(number))) =>
                }
            }
        }
    }

    pub fn get_mut_interpreter(
        &mut self,
        file_path: &str,
        source_location: SourceLocation,
    ) -> Result<&mut Interpreter, Error> {
        match self.modules.get_mut(file_path) {
            Some(interpreter) => Ok(interpreter),
            None => Err(self.error_builder.build(ErrorType::Undefined {
                ident: String::from(file_path),
                kind: String::from("module"),
                on: String::new(),
                loc: [source_location],
            })),
        }
    }

    pub fn get_interpreter(
        &self,
        file_path: &str,
        source_location: SourceLocation,
    ) -> Result<&Interpreter, Error> {
        match self.modules.get(file_path) {
            Some(interpreter) => Ok(interpreter),
            None => Err(self.error_builder.build(ErrorType::Undefined {
                ident: String::from(file_path),
                kind: String::from("module"),
                on: String::new(),
                loc: [source_location],
            })),
        }
    }

    pub fn lookup(&self, variable: Symbol) -> Result<Value, Error> {
        match self.enviroment.get(&variable, &self.error_builder) {
            Ok(ident) => Ok(*ident.value),
            Err(_) => Ok(*self.globals.get(&variable, &self.error_builder)?.value),
        }
    }

    fn eval_unary(&mut self, operator: Token, expr: Expr) -> Result<Value, Error> {
        let right = self.evaluate(expr)?.calc(self);

        match operator.token_type {
            TokenType::Bang => Ok(self.is_falsy_operator(right, operator)?),
            TokenType::Minus => match right {
                Value::Literal(Literal::Number(n)) => Ok(Value::Literal(Literal::Number(-n))),
                _ => Err(self.error_builder.build(ErrorType::Unsupported {
                    r#type: String::from("unary"),
                    operator: String::from("-"),
                    r#for: format!("{}", right.calc(self).to_type(self)),
                    loc: [operator.to_source_location()],
                })),
            },
            _ => Err(self.error_builder.build(ErrorType::Invalid {
                target: format!("unary operator '{}'", operator.lexeme),
                loc: [operator.to_source_location()],
            })),
        }
    }

    fn eval_binary(&mut self, left: Expr, operator: Token, right: Expr) -> Result<Value, Error> {
        let left = self.evaluate(left)?.calc(self);
        let right = self.evaluate(right)?.calc(self);

        match (left, operator.token_type.clone(), right) {
            // ==, !=
            (literal_left, TokenType::EqualEqual, literal_right) => {
                Ok(Value::Literal(Literal::Bool(literal_left == literal_right)))
            }
            (literal_left, TokenType::BangEqual, literal_right) => {
                Ok(Value::Literal(Literal::Bool(literal_left != literal_right)))
            }

            // <, >, <=, >=
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::Less,
                Value::Literal(Literal::Number(right_num)),
            ) => Ok(Value::Literal(Literal::Bool(left_num < right_num))),
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::Greater,
                Value::Literal(Literal::Number(right_num)),
            ) => Ok(Value::Literal(Literal::Bool(left_num > right_num))),
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::LessEqual,
                Value::Literal(Literal::Number(right_num)),
            ) => Ok(Value::Literal(Literal::Bool(left_num <= right_num))),
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::GreaterEqual,
                Value::Literal(Literal::Number(right_num)),
            ) => Ok(Value::Literal(Literal::Bool(left_num >= right_num))),

            // +, -, *, /
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::Plus,
                Value::Literal(Literal::Number(right_num)),
            ) => Ok(Value::Literal(Literal::Number(left_num + right_num))),
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::Minus,
                Value::Literal(Literal::Number(right_num)),
            ) => Ok(Value::Literal(Literal::Number(left_num - right_num))),
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::Star,
                Value::Literal(Literal::Number(right_num)),
            ) => Ok(Value::Literal(Literal::Number(left_num * right_num))),
            (
                Value::Literal(Literal::Number(left_num)),
                TokenType::Slash,
                Value::Literal(Literal::Number(right_num)),
            ) => {
                if right_num == 0.0 {
                    Err(self
                        .error_builder
                        .build(ErrorType::DivisionByZero([operator.to_source_location()])))
                } else {
                    Ok(Value::Literal(Literal::Number(left_num / right_num)))
                }
            }
            (
                Value::Literal(Literal::String(left_string)),
                TokenType::Plus,
                Value::Literal(Literal::String(right_string)),
            ) => Ok(Value::Literal(Literal::String(left_string + &right_string))),

            (literal_left, _, literal_right) => {
                Err(self.error_builder.build(ErrorType::UnsupportedBinary {
                    operator: operator.lexeme.to_string(),
                    r#for: [
                        format!("{}", literal_left.calc(self).to_type(self)),
                        format!("{}", literal_right.calc(self).to_type(self)),
                    ],
                    loc: [operator.to_source_location()],
                }))
            }
        }
    }

    fn is_truthy_operator(&self, literal: Value, operator: Token) -> Result<Value, Error> {
        match literal {
            Value::Literal(Literal::Bool(bool)) => Ok(Value::Literal(Literal::Bool(bool))),
            literal => Err(self.error_builder.build(ErrorType::Unsupported {
                r#type: literal.to_type(self).to_string(),
                operator: operator.lexeme,
                r#for: String::from("unary"),
                loc: [SourceLocation {
                    line: operator.line,
                    col: operator.col,
                }],
            })),
        }
    }

    fn is_truthy(&self, literal: Value, loc: SourceLocation) -> Result<Value, Error> {
        match literal {
            Value::Literal(Literal::Bool(bool)) => Ok(Value::Literal(Literal::Bool(bool))),
            literal => Err(self.error_builder.build(ErrorType::OnlyAllowed {
                expected: String::from("booleans"),
                of: String::from("result of this expression"),
                item: literal.to_type(self).to_string(),
                loc: [loc],
            })),
        }
    }

    fn is_falsy_operator(&self, literal: Value, operator: Token) -> Result<Value, Error> {
        match literal {
            Value::Literal(Literal::Bool(bool)) => Ok(Value::Literal(Literal::Bool(!bool))),
            literal => Err(self.error_builder.build(ErrorType::Unsupported {
                r#type: literal.to_type(self).to_string(),
                operator: operator.lexeme,
                r#for: String::from("unary"),
                loc: [SourceLocation {
                    line: operator.line,
                    col: operator.col,
                }],
            })),
        }
    }

    fn alloc_id(&mut self) -> u64 {
        let res = self.counter;
        self.counter += 1;
        res
    }

    pub fn create_instance(&mut self, class_name: &Symbol, class_id: u64) -> Value {
        let inst_id = self.alloc_id();
        let inst = Instance {
            class_name: class_name.clone(),
            class_id,
            id: inst_id,
            fields: HashMap::new(),
        };
        self.instances.insert(inst_id, inst);

        Value::Literal(Literal::Instance {
            name: class_name.clone(),
            id: inst_id,
        })
    }

    pub fn get_function(&self, id: u64) -> &Function {
        match self.functions.get(&id) {
            Some(function) => function,
            None => panic!("Couldn't find function with id {}", id),
        }
    }

    pub fn get_class(&self, id: u64) -> &Class {
        match self.classes.get(&id) {
            Some(class) => class,
            None => panic!("Couldn't find class with id {}", id),
        }
    }

    pub fn get_instance(&self, id: u64) -> &Instance {
        match self.instances.get(&id) {
            Some(inst) => inst,
            None => panic!("Couldn't find instance with id {id}."),
        }
    }

    pub fn get_closure(&self, id: u64) -> &Closure {
        match self.closures.get(&id) {
            Some(closure) => closure,
            None => panic!("Couldn't find closure with id {id}."),
        }
    }

    pub fn new(file: File, project: Project) -> Self {
        let mut global_envs = HashMap::new();

        global_envs.insert(
            String::from("log"),
            (
                Identifier::native_fn(NativeFn {
                    name: String::from("log"),
                    arity: 1,
                    callable: |_, lits| {
                        let arg = &lits[0];
                        {
                            println!("{}", arg);
                            Ok(Value::Literal(Literal::Void))
                        }
                    },
                }),
                SourceLocation {
                    line: 1337,
                    col: 1337,
                },
            ),
        );

        let globals = Enviroment {
            enclosing: None,
            values: global_envs,
        };

        Self {
            enviroment: globals.clone(),
            globals,
            return_value: None,
            counter: 0,
            classes: HashMap::new(),
            functions: HashMap::new(),
            instances: HashMap::new(),
            closures: HashMap::new(),
            error_builder: ErrorBuilder(file.clone()),
            project,
            modules: HashMap::new(),
        }
    }
}
