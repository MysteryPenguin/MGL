pub mod enviroment;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod saving;

use std::{
    env,
    fs,
    path::Path,
    process,
};

use saving::project::*;

fn main() {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);

    if args[0] == String::from("run") {
        run_project("./");
    }
}

fn run_project(path: &str) {
    let dir_tree = Dir::generate(path);

    let manifest = look_for_file_error(
        dir_tree.look_for_file(&[String::from("."), String::from("manifest.json")], 0),
        "manifest.json",
    );
    let manifest = match serde_json::from_str::<ProjectManifest>(&manifest.source) {
        Ok(val) => val,
        Err(err) => {
            println!("\x1b[1;31merror\x1b[1;39m: {err}\x1b[0m");
            process::exit(64)
        }
    };

    let src = look_for_dir_error(dir_tree.look_for_dir(&[String::from("."), String::from("src")], 0), "src");

    let mut project = Project::new(manifest, src.clone());
    project.run();
}

fn fs_error<T, P: AsRef<Path> + std::fmt::Debug + Clone>(
    function: fn(P) -> std::io::Result<T>,
    arg: P,
) -> T {
    match function(arg.clone()) {
        Ok(value) => value,
        Err(err) => {
            println!("\x1b[1;31merror\x1b[1;39m: {err} at {arg:?}\x1b[0m");
            process::exit(64)
        }
    }
}

fn look_for_file_error<'a>(file: Option<&'a File>, file_name: &'a str) -> &'a File {
    match file {
        Some(file) => file,
        None => {
            println!(
                "\x1b[1;31merror\x1b[1;39m: cannot find file '{file_name}' which is required for the project\x1b[0m"
            );
            process::exit(64)
        }
    }
}

fn look_for_dir_error<'a>(file: Option<&'a Dir>, dir_name: &'a str) -> &'a Dir {
    match file {
        Some(dir) => dir,
        None => {
            println!(
                "\x1b[1;31merror\x1b[1;39m: cannot find file '{dir_name}' which is required for the project\x1b[0m"
            );
            process::exit(64)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Dir {
    pub path: Vec<String>,
    pub content: Vec<PathTree>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct File {
    pub path: Vec<String>,
    pub source: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PathTree {
    Dir(Dir),
    File(File),
}

impl Dir {
    fn generate(path: &str) -> Dir {
        let mut dir_tree = Dir {
            path: path
                .to_string()
                .split(|c| c == '\\' || c == '/')
                .map(String::from)
                .collect(),
            content: Vec::new(),
        };

        let dir = fs_error(fs::read_dir, &path);

        for item in dir {
            let item = item.unwrap();

            if item.file_name() != "interpreter"
                && item.file_name() != "target"
                && item.file_name() != "Cargo.lock"
                && item.file_name() != "Cargo.toml"
                && item.file_name() != ".git"
            {
                if item.file_type().unwrap().is_dir() {
                    let child_dir = Self::generate(item.path().display().to_string().as_str());

                    dir_tree.content.push(PathTree::Dir(child_dir));
                } else {
                    let content = fs_error(fs::read_to_string, item.path().display().to_string());

                    dir_tree.content.push(PathTree::File(File {
                        path: item
                            .path()
                            .display()
                            .to_string()
                            .split(|c| c == '\\' || c == '/')
                            .map(String::from)
                            .collect(),
                        source: content,
                    }));
                }
            }
        }

        dir_tree
    }

    fn look_for_file(&self, path: &[String], iters: usize) -> Option<&File> {
        if self.path[iters] == path[iters] {
            for item in &self.content {
                match item {
                    PathTree::Dir(dir) => match dir.look_for_file(path, iters + 1) {
                        Some(value) => return Some(value),
                        None => continue,
                    },
                    PathTree::File(file) => {
                        if file.path == path {
                            return Some(file);
                        }
                        continue;
                    }
                }
            }
        }

        return None;
    }
    fn look_for_dir(&self, path: &[String], iters: usize) -> Option<&Dir> {
        if self.path[iters] == path[iters] {
            for item in &self.content {
                match item {
                    PathTree::Dir(dir) => {
                        if dir.path == path {
                            return Some(dir);
                        } else if dir.path[iters] == path[iters] {
                            return self.look_for_dir(&dir.path, iters + 1);
                        } else {
                            continue;
                        }
                    }
                    PathTree::File(_) => continue,
                }
            }
        }

        return None;
    }
}
