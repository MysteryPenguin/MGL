use std::{collections::HashMap, fmt};

use super::{
    error::*,
    stmt::Stmt,
    symbol::{SourceLocation, Symbol},
    r#type::Type,
    value::{Identifier, Literal, Value},
};
use crate::{enviroment::Enviroment, interpreter::Interpreter};

pub static INIT: &str = "init";

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Error>;
    fn arity(&self, interpreter: &mut Interpreter) -> u8;
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFn {
    pub name: String,
    pub arity: u8,
    pub callable: fn(&mut Interpreter, &[Value]) -> Result<Value, Error>,
}

impl fmt::Display for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}

impl Callable for NativeFn {
    fn arity(&self, _interpreter: &mut Interpreter) -> u8 {
        self.arity
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Error> {
        (self.callable)(interpreter, args)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: u64,
    pub name: Symbol,
    pub params: Vec<(Symbol, Type)>,
    pub body: Vec<Stmt>,
    pub closure: Enviroment,
    pub instance: Option<Box<Value>>,
    pub is_initializer: bool,
}

impl Callable for Function {
    fn arity(&self, _interpreter: &mut Interpreter) -> u8 {
        self.params.len().try_into().unwrap()
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Error> {
        let mut args_env = HashMap::new();
        for (param, arg) in self.params.iter().zip(args.iter()) {
            let ident = Identifier::with_type(interpreter, param.0.clone(), param.1.clone(), arg.clone())?;
            args_env.insert(
                param.0.name.clone(),
                (
                    ident,
                    SourceLocation {
                        line: param.0.line,
                        col: param.0.col,
                    },
                ),
            );
        }

        let previous_env = interpreter.enviroment.clone();
        let previous_return_value = interpreter.return_value.clone();

        let mut env = self.closure.clone();
        env.values.extend(previous_env.values.clone());
        env.values.extend(args_env);

        if let Some(this_val) = &self.instance {
            let this_symbol = Symbol::new("this".to_string(), 0, 0);
            env.values.insert(
                this_symbol.name.clone(),
                (
                    Identifier::new(interpreter, this_symbol.clone(), *this_val.clone()),
                    SourceLocation {
                        line: this_symbol.line,
                        col: this_symbol.col,
                    },
                ),
            );
        } else if let Ok(this_val) = interpreter.lookup(Symbol::new("this".to_string(), 0, 0)) {
            let this_symbol = Symbol::new("this".to_string(), 0, 0);
            env.values.insert(
                this_symbol.name.clone(),
                (
                    Identifier::new(interpreter, this_symbol.clone(), this_val.clone()),
                    SourceLocation {
                        line: this_symbol.line,
                        col: this_symbol.col,
                    },
                ),
            );
        }

        let env = env;
        interpreter.enviroment = env;

        interpreter.interpret_stmts(self.body.clone())?;

        let return_val = interpreter.return_value.clone();

        interpreter.enviroment = previous_env;
        interpreter.return_value = previous_return_value;

        match return_val {
            Some(val) => Ok(val),
            None => {
                if self.is_initializer {
                    match &self.instance {
                        Some(this_val) => Ok(*this_val.clone()),
                        None => Err(interpreter.error_builder.build(
                            ErrorType::Runtime,
                            format!("could not find instance of this"),
                            SourceLocation { line: 1, col: 0 },
                        )),
                    }
                } else {
                    Ok(Value::Literal(Literal::Void))
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub id: u64,
    pub params: Vec<(Symbol, Type)>,
    pub body: Stmt,
    pub closure: Enviroment
}

impl Callable for Closure {
    fn arity(&self, _interpreter: &mut Interpreter) -> u8 {
        self.params.len().try_into().unwrap()
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Error> {
        let mut args_env = HashMap::new();
        for (param, arg) in self.params.iter().zip(args.iter()) {
            let ident = Identifier::with_type(interpreter, param.0.clone(), param.1.clone(), arg.clone())?;
            args_env.insert(
                param.0.name.clone(),
                (
                    ident,
                    SourceLocation {
                        line: param.0.line,
                        col: param.0.col,
                    },
                ),
            );
        }

        let previous_env = interpreter.enviroment.clone();
        let previous_return_value = interpreter.return_value.clone();

        let mut env = self.closure.clone();
        env.values.extend(previous_env.values.clone());
        env.values.extend(args_env);

        let env = env;
        interpreter.enviroment = env;

        interpreter.execute(self.body.clone())?;

        let return_val = interpreter.return_value.clone();

        interpreter.enviroment = previous_env;
        interpreter.return_value = previous_return_value;

        match return_val {
            Some(val) => Ok(val),
            None => Ok(Value::Literal(Literal::Void))
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub id: u64,
    pub name: Symbol,
    pub methods: HashMap<String, u64>,
}

impl Class {
    fn init(&self, interpreter: &Interpreter) -> Option<Function> {
        self.methods
            .get(&String::from(INIT))
            .map(|init_id| interpreter.get_function(*init_id).clone())
    }

    pub fn find_method(&self, name: &str, interpreter: &Interpreter) -> Option<(Symbol, u64)> {
        if let Some(id) = self.methods.get(name) {
            let function = interpreter.get_function(*id);
            return Some((function.name.clone(), *id));
        }
        None
    }
}

impl Callable for Class {
    fn arity(&self, interpreter: &mut Interpreter) -> u8 {
        match self.init(interpreter) {
            Some(initializer) => initializer.params.len().try_into().unwrap(),
            None => 0,
        }
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Error> {
        let instance = interpreter.create_instance(&self.name, self.id);

        if let Some(mut initializer) = self.init(interpreter) {
            initializer.instance = Some(Box::new(instance.clone()));
            initializer.call(interpreter, args)?;
        }

        Ok(instance)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class_name: Symbol,
    pub class_id: u64,
    pub id: u64,
    pub fields: HashMap<String, Value>,
}

impl Instance {
    pub fn get_attr(&self, attr: Symbol, interpreter: &Interpreter) -> Result<Value, Error> {
        match self.fields.get(&attr.name) {
            Some(val) => Ok(val.clone()),
            None => {
                let class = interpreter.get_class(self.class_id);
                if let Some((func_name, method_id)) = class.find_method(&attr.name, interpreter) {
                    return Ok(Value::Literal(Literal::Fn {
                        name: func_name,
                        id: method_id,
                        instance: Some(Box::new(Value::Literal(Literal::Instance {
                            name: self.class_name.clone(),
                            id: self.id,
                        }))),
                    }));
                }
                Err(interpreter.error_builder.build(
                    ErrorType::Runtime,
                    format!(
                        "'{}' instance has no '{}' attribute",
                        self.class_name.name, attr.name
                    ),
                    SourceLocation {
                        line: attr.line,
                        col: attr.col,
                    },
                ))
            }
        }
    }
}

pub fn as_callable(interpreter: &Interpreter, value: &Value) -> Option<Box<dyn Callable>> {
    match value {
        Value::Literal(Literal::NativeFn(f)) => Some(Box::new(f.clone())),
        Value::Literal(Literal::Fn {
            name: _,
            id,
            instance,
        }) => {
            let f = interpreter.get_function(*id);
            let mut f_copy = f.clone();
            f_copy.instance = instance.clone();
            Some(Box::new(f_copy))
        }
        Value::Literal(Literal::Class { name: _, id }) => Some(Box::new(interpreter.get_class(*id).clone())),
        Value::Literal(Literal::Closure(id)) => Some(Box::new(interpreter.get_closure(*id).clone())),
        _ => None,
    }
}
