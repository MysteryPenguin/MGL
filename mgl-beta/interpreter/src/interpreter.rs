use std::collections::HashMap;

use crate::{
    enviroment::Enviroment, saving::{
        callable::{as_callable, Class, Closure, Function, Instance, NativeFn, INIT}, error::{Error, ErrorBuilder, ErrorType}, expr::Expr, project::Project, stmt::{ClassDecl, Decl, FnDecl, Stmt, VarDecl}, symbol::{SourceLocation, Symbol}, token::Token, token_type::TokenType, r#type::{LiteralType, Type}, value::{Identifier, Literal, Value}
    }, File
};

#[derive(Clone, Debug)]
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
    pub project: Project
}

impl Interpreter {
    pub fn interpret(&mut self, decls: Vec<Decl>) -> Result<(), Error> {
        for decl in decls {
            self.execute_decl(decl)?;
        }

        Ok(())
    }

    pub fn interpret_stmts(&mut self, stmts: Vec<Stmt>) -> Result<(), Error> {
        for stmt in stmts {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute_decl(&mut self, decl: Decl) -> Result<(), Error> {
        match decl {
            Decl::Var(VarDecl { sym, init, r#type }) => match (init, r#type) {
                (Some(expr), Some(r#type)) => {
                    let value = self.evaluate(expr)?;
                    self.enviroment.define(
                        sym.clone(),
                        Identifier::with_type(self, sym, r#type, value)?,
                    );
                }
                (None, Some(r#type)) => self.enviroment.define(
                    sym.clone(),
                    Identifier::with_type(
                        self,
                        sym.clone(),
                        r#type,
                        Value::Literal(Literal::None)
                    )?,
                ),
                (Some(expr), None) => {
                    let value = self.evaluate(expr)?;
                    self.enviroment
                        .define(sym.clone(), Identifier::new(self, sym, value));
                }
                (None, None) => self.enviroment.define(
                    sym.clone(),
                    Identifier::new(self, sym, Value::Literal(Literal::None)),
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
                        })
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
                        })
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
            Decl::Pub(decl) => match *decl {
                Decl::Pub(_) => {
                    return Err(self.error_builder.build(
                        ErrorType::Runtime,
                        String::from("a public declaration is not allowed in a public declaration"),
                        SourceLocation { line: 1, col: 0 },
                    ));
                }
                _ => return self.execute_decl(*decl),
            },
        }
        Ok(())
    }

    pub fn execute(&mut self, stmt: Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Print(expr) => {
                let literal = self.evaluate(expr)?;
                println!("{literal}")
            }
            Stmt::Expr(expr) => {
                self.evaluate(expr)?;
            }
            Stmt::Var(VarDecl { sym, init, r#type }) => match (init, r#type) {
                (Some(expr), Some(r#type)) => {
                    let value = self.evaluate(expr)?;
                    if value.to_type(self) != r#type {
                        return Err(self.error_builder.build(
                            ErrorType::Type,
                            format!(
                                "expected value of type '{}'. Found '{}'",
                                r#type,
                                value.to_type(self)
                            ),
                            SourceLocation {
                                line: sym.line,
                                col: sym.col,
                            },
                        ));
                    }
                    self.enviroment
                        .define(sym.clone(), Identifier::new(self, sym, value));
                }
                (None, Some(r#type)) => {
                    if r#type != Type::Literal(Some(LiteralType::None)) {
                        return Err(self.error_builder.build(
                            ErrorType::Type,
                            format!("expected value of type '{}'. Found 'none'", r#type,),
                            SourceLocation {
                                line: sym.line,
                                col: sym.col,
                            },
                        ));
                    }
                    self.enviroment.define(
                        sym.clone(),
                        Identifier::new(self, sym, Value::Literal(Literal::None)),
                    );
                }
                (Some(expr), None) => {
                    let value = self.evaluate(expr)?;
                    self.enviroment
                        .define(sym.clone(), Identifier::new(self, sym, value));
                }
                (None, None) => self.enviroment.define(
                    sym.clone(),
                    Identifier::new(self, sym, Value::Literal(Literal::None)),
                ),
            },
            Stmt::Block(stmts) => {
                self.execute_block(stmts, Enviroment::from(self.enviroment.clone()))?
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_eval = self.evaluate(condition.clone())?;
                let loc = self.get_loc(condition.clone());

                if self.is_truthy(condition_eval, loc)? == Value::Literal(Literal::Bool(true)) {
                    self.execute(*then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(*else_branch)?;
                }
            }
            Stmt::While { condition, body } => {
                let loc = self.get_loc(condition.clone());
                let value = self.is_truthy(self.clone().evaluate(condition.clone())?, loc)?;

                while value == Value::Literal(Literal::Bool(true)) {
                    self.execute(*body.clone())?;
                }
            }
            Stmt::FnDecl(FnDecl { name, params, body }) => {
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

                self.functions.insert(id, function);

                self.enviroment.define(
                    name.clone(),
                    Identifier::new(
                        self,
                        name.clone(),
                        Value::Literal(Literal::Fn {
                            name: name.clone(),
                            id,
                            instance: None,
                        })
                    ),
                );
            }
            Stmt::Return { loc: _, value } => {
                self.return_value = Some(if let Some(value) = value {
                    self.evaluate(value)?
                } else {
                    Value::Literal(Literal::None)
                });
            }
            Stmt::Import { imports, file_path } => {
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

    fn get_loc(&self, expr: Expr) -> SourceLocation {
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
            Expr::Group(expr) => self.get_loc(*expr),
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
            Expr::Literal { lit, loc: _ } => Ok(lit),
            Expr::Unary { operator, expr } => self.eval_unary(operator, *expr),
            Expr::Var(name) => Ok(*self.enviroment.get(name, self)?.value),
            Expr::Assign { sym, value } => {
                let lit = self.evaluate(*value)?;

                self.enviroment.assign(sym.clone(), Identifier::new(self, sym, lit.clone()), &self.clone())?;

                Ok(lit)
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(*left)?;

                if operator.token_type == TokenType::Or {
                    if self.is_truthy_operator(left.clone(), operator)?
                        == Value::Literal(Literal::Bool(true))
                    {
                        return Ok(left);
                    }
                } else {
                    if self.is_falsy_operator(left.clone(), operator)?
                        == Value::Literal(Literal::Bool(true))
                    {
                        return Ok(left);
                    }
                }

                return self.evaluate(*right);
            }
            Expr::Call {
                callee,
                loc,
                args: arg_exprs,
            } => {
                let callee = self.evaluate(*callee)?;

                match as_callable(self, &callee) {
                    Some(callable) => {
                        let maybe_args: Result<Vec<_>, _> = arg_exprs
                            .iter()
                            .map(|arg| self.evaluate(*arg.clone()))
                            .collect();

                        match maybe_args {
                            Ok(args) => {
                                if args.len() != <u8 as Into<usize>>::into(callable.arity(self)) {
                                    Err(self.error_builder.build(ErrorType::Runtime, format!("invalid call: callee has {} arguments, but it was called with {} arguments", callable.arity(&mut self.clone()), args.len()), loc))
                                } else {
                                    callable.call(self, &args)
                                }
                            }
                            Err(err) => Err(err),
                        }
                    }
                    None => Err(self.error_builder.build(
                        ErrorType::Runtime,
                        format!("value of type '{}' is not callable", callee.to_type(self)),
                        loc,
                    )),
                }
            }
            Expr::Get { attr, lhs } => {
                let val = self.evaluate(*lhs)?;

                match val {
                    Value::Literal(Literal::Instance { name: _, id }) => {
                        self.get_instance(id).get_attr(attr, self)
                    }
                    _ => Err(self.error_builder.build(
                        ErrorType::Runtime,
                        format!(
                            "only instances have attributes but found value of type '{}'",
                            val.to_type(self)
                        ),
                        SourceLocation {
                            line: attr.line,
                            col: attr.col,
                        },
                    )),
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

                return Ok(Value::Literal(Literal::Closure(id)));
            }
            Expr::Set { lhs, attr, rhs } => {
                let lhs = self.evaluate(*lhs)?;
                let rhs = self.evaluate(*rhs)?;

                match lhs {
                    Value::Literal(Literal::Instance { name: _, id }) => {
                        match self.instances.get_mut(&id) {
                            Some(inst) => {
                                inst.fields.insert(attr.name.clone(), rhs.clone());
                                Ok(rhs)
                            }
                            None => panic!("Couldn't find instance with id {id}"),
                        }
                    }
                    _ => Err(self.error_builder.build(
                        ErrorType::Runtime,
                        format!(
                            "only instances have attributes but found value of type '{}'",
                            lhs.to_type(self)
                        ),
                        SourceLocation {
                            line: attr.line,
                            col: attr.col,
                        },
                    )),
                }
            }
            Expr::This(loc) => {
                match self.lookup(Symbol::new(String::from("this"), loc.line, loc.col)) {
                    Ok(value) => Ok(value.clone()),
                    Err(err) => Err(err),
                }
            }
        }
    }

    pub fn lookup(&self, variable: Symbol) -> Result<Value, Error> {
        match self.enviroment.get(variable.clone(), self) {
            Ok(ident) => Ok(*ident.value),
            Err(_) => Ok(*self.globals.get(variable, self)?.value),
        }
    }

    fn eval_unary(&mut self, operator: Token, expr: Expr) -> Result<Value, Error> {
        let right = self.evaluate(expr)?;

        match operator.token_type {
            TokenType::Bang => Ok(self.is_falsy_operator(right, operator)?),
            TokenType::Minus => match right {
                Value::Literal(Literal::Number(n)) => Ok(Value::Literal(Literal::Number(-n))),
                _ => Err(self.error_builder.build(
                    ErrorType::Runtime,
                    format!(
                        "unary operator '-' cannot be used on values of type '{}'",
                        right.to_type(self)
                    ),
                    SourceLocation {
                        line: operator.line,
                        col: operator.col,
                    },
                )),
            },
            _ => Err(self.error_builder.build(
                ErrorType::Runtime,
                format!("invalid unary operator '{}'", operator.lexeme),
                SourceLocation {
                    line: operator.line,
                    col: operator.col,
                },
            )),
        }
    }

    fn eval_binary(&mut self, left: Expr, operator: Token, right: Expr) -> Result<Value, Error> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match (left, operator.token_type, right) {
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
                    Err(self.error_builder.build(
                        ErrorType::Runtime,
                        format!("division by zero"),
                        SourceLocation {
                            line: operator.line,
                            col: operator.col,
                        },
                    ))
                } else {
                    Ok(Value::Literal(Literal::Number(left_num / right_num)))
                }
            }
            (
                Value::Literal(Literal::String(left_string)),
                TokenType::Plus,
                Value::Literal(Literal::String(right_string)),
            ) => Ok(Value::Literal(Literal::String(left_string + &right_string))),

            (literal_left, _, literal_right) => Err(self.error_builder.build(
                ErrorType::Runtime,
                format!(
                    "operator is compatible with values of type '{}' and '{}'",
                    literal_left.to_type(self),
                    literal_right.to_type(self)
                ),
                SourceLocation {
                    line: operator.line,
                    col: operator.col,
                },
            )),
        }
    }

    fn is_truthy_operator(&self, literal: Value, operator: Token) -> Result<Value, Error> {
        match literal {
            Value::Literal(Literal::Bool(bool)) => Ok(Value::Literal(Literal::Bool(bool))),
            literal => Err(self.error_builder.build(
                ErrorType::Runtime,
                format!(
                    "operator '{}' is not compatible with value of type '{}'",
                    operator.lexeme,
                    literal.to_type(self)
                ),
                SourceLocation {
                    line: operator.line,
                    col: operator.col,
                },
            )),
        }
    }

    fn is_truthy(&self, literal: Value, loc: SourceLocation) -> Result<Value, Error> {
        match literal {
            Value::Literal(Literal::Bool(bool)) => Ok(Value::Literal(Literal::Bool(bool))),
            literal => Err(self.error_builder.build(ErrorType::Runtime, format!("only booleans are allowed as result of an expression but found value of type {}", literal.to_type(self)), SourceLocation { line: loc.line, col: loc.col })),
        }
    }

    fn is_falsy_operator(&self, literal: Value, operator: Token) -> Result<Value, Error> {
        match literal {
            Value::Literal(Literal::Bool(bool)) => Ok(Value::Literal(Literal::Bool(!bool))),
            literal => Err(self.error_builder.build(
                ErrorType::Runtime,
                format!(
                    "operator '{}' is not compatible with value of type '{}'",
                    operator.lexeme,
                    literal.to_type(self)
                ),
                SourceLocation {
                    line: operator.line,
                    col: operator.col,
                },
            )),
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
                    callable: |_, lits| match &lits[0] {
                        arg => {
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
            project
        }
    }
}
