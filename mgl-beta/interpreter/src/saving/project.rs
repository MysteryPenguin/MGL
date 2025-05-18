use serde::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, process};

use crate::{Dir, PathTree, interpreter::Interpreter, lexer::Lexer, parser::Parser};

use super::{callable::Callable, error::Error, symbol::Symbol, value::Literal};

#[derive(Debug, Clone)]
pub struct Project {
    pub manifest: ProjectManifest,
    pub modules: HashMap<String, Interpreter>,
}

impl Project {
    pub fn new(manifest: ProjectManifest, dir: Dir) -> Project {
        let mut project = Project { manifest, modules: HashMap::new() };
        match project.dir(dir) {
            Ok(modules) => project.modules = modules,
            Err(err) => {
                println!("{err}");
                process::exit(64);
            }
        }

        project
    }

    pub fn dir(&self, dir: Dir) -> Result<HashMap<String, Interpreter>, Error> {
        let mut modules = HashMap::new();
        for path_tree in dir.content {
            match path_tree {
                PathTree::Dir(dir) => {
                    let child_modules = self.dir(dir)?;

                    child_modules.into_iter().for_each(|(path, interpreter)| {
                        modules.insert(path, interpreter);
                    });
                }
                PathTree::File(file) => {
                    let path = file.path.join("/");

                    let tokens = Lexer::new(file.clone()).scan_tokens()?;
                    let decls = Parser::new(tokens, file.clone()).parse()?;
                    let mut interpreter = Interpreter::new(file.clone(), self.clone());
                    interpreter.interpret(decls)?;

                    modules.insert(path, interpreter);
                }
            }
        }

        Ok(modules)
    }

    pub fn run(&mut self) {
        match self.manifest.r#type {
            ProjectType::Lib => match self.modules.get(&String::from("./src/lib.mgl")) {
                Some(_) => (),
                None => {
                    println!(
                        "\x1b[1;31merror\x1b[1;39m: 'lib.mgl' does not exist in this library\x1b[0m"
                    );
                    process::exit(64);
                }
            },
            ProjectType::Normal => match self.modules.get_mut(&String::from("./src/main.mgl")) {
                Some(interpreter) => {
                    match interpreter.enviroment.get(Symbol::new(String::from("main"), 0, 0), interpreter) {
                        Ok(ident) => match *ident.value {
                            super::value::Value::Literal(Literal::Fn { name: _, id, instance: _ }) => {
                                let main_function = interpreter.get_function(id).clone();
                                if main_function.arity(interpreter) != 0 {
                                    println!("\x1b[1;31merror\x1b[1;39m: function main must have 0 arguments\x1b[0m");
                                    process::exit(64);
                                }

                                match main_function.call(interpreter, &[]) {
                                    Ok(_) => (),
                                    Err(err) => {
                                        println!("{err}");
                                        process::exit(64);
                                    }
                                }
                            },
                            _ => ()
                        },
                        Err(_) => ()
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
