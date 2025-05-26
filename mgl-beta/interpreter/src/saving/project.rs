use serde::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, process};

use crate::{Dir, interpreter::Interpreter, lexer::Lexer, parser::Parser};

use super::{callable::Callable, error::Error, symbol::{SourceLocation, Symbol}, value::Literal};

#[derive(Debug, Clone)]
pub struct Project {
    pub manifest: ProjectManifest,
    pub dir: Dir,
}

impl Project {
    pub fn new(manifest: ProjectManifest, dir: Dir) -> Self {
        Self {
            manifest,
            dir
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        match self.manifest.r#type {
            ProjectType::Lib => match self.dir.look_for_file(&[String::from("src"), String::from("lib.mgl")], 0) {
                Some(_) => Ok(()),
                None => {
                    println!(
                        "\x1b[1;31merror\x1b[1;39m: 'lib.mgl' does not exist in this library\x1b[0m"
                    );
                    process::exit(64);
                }
            },
            ProjectType::Normal => match self.dir.look_for_file(&[String::from("src"), String::from("main.mgl")], 0) {
                Some(file) => {
                    let tokens = Lexer::new(file.clone()).scan_tokens()?;
                    let decls = Parser::new(tokens, file.clone()).parse()?;
                    let mut interpreter = Interpreter::new(file.clone(), self.clone());
                    interpreter.interpret(decls)?;

                    match interpreter.enviroment.get(Symbol::new(String::from("main"), 0, 0), &interpreter) {
                        Ok(ident) => {
                            match *ident.value {
                                super::value::Value::Literal(Literal::Fn { name: _, id, instance: _ }) => {
                                    let main_fn = interpreter.get_function(id).clone();

                                    if main_fn.arity(&mut interpreter) != 0 {
                                        return Err(interpreter.error_builder.build(super::error::ErrorType::Runtime, String::from("function 'main' must have 0 arguments"), SourceLocation { line: ident.name.line, col: ident.name.col }));
                                    }

                                    main_fn.call(&mut interpreter, &[])?;
                                    return Ok(())
                                }
                                _ => return Err(interpreter.error_builder.build(super::error::ErrorType::Runtime, String::from("item 'main' is not a function"), SourceLocation { line: ident.name.line, col: ident.name.col }))
                            }
                        }
                        Err(_) => return Ok(())
                    }
                },
                None => {
                    println!("\x1b[1;31merror\x1b[1;39m: 'main.mgl' does not exist in this executable project\x1b[0m");
                    process::exit(64);
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct ProjectManifest {
    pub title: String,
    pub description: String,
    pub version: String,
    #[serde(default = "HashMap::new")]
    pub dependencies: HashMap<String, Dependency>,
    #[serde(default = "ProjectType::default")]
    pub r#type: ProjectType,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Default)]
pub enum ProjectType {
    #[default]
    #[serde(rename = "normal")]
    Normal,
    #[serde(rename = "lib")]
    Lib,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct Dependency {
    pub name: Option<String>,
    pub version: String,
    pub options: HashMap<String, Value>,
}
