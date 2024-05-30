pub mod lexer;
pub mod scanner;

pub static mut ARGS: Vec<String> = Vec::new();

use std::{env, process::exit};
use std::fs;
use std::io::{stdin, stdout, BufRead, Write};
use crate::lexer::lexer;

fn main() {
    let args: Vec<String> = env::args().collect();

    unsafe {
        ARGS = args.clone();
    }

    if args.len() > 2 {
        println!("Usage: MGL [script]");
        exit(64);
    } else if args.len() == 2 {
        match read_file(&args[1]) {
            Ok(_) => exit(0),
            Err(msg) => {
                println!("\u{001b}[1;31mError {}", msg);
                exit(0);
            }
        }
    } else {
        match run_prompt() {
            Ok(_) => exit(0),
            Err(msg) => {
                println!("\u{001b}[1;31mError:\n{}", msg);
                exit(1);
            }
        }
    }
}

pub fn read_file(path: &str) -> Result<(), String> {
    match fs::read_to_string(path) {
        Err(msg) => Err(msg.to_string()),
        Ok(contents) => lexer(&contents),
    }
}

pub fn run_prompt() -> Result<(), String> {
    loop {
        print!("> ");
        match stdout().flush() {
            Ok(_) => (),
            Err(_) => return Err(String::from("Couldn't flush!")),
        }
        let input = stdin();
        let mut buffer = String::new();
        let mut handle = input.lock();
        match handle.read_line(&mut buffer) {
            Ok(n) => {
                if n <= 2 {
                    return Ok(());
                }
            },
            Err(_) => return Err(String::from("Failed to read the line")),
        }
        println!("ECHO: {}", buffer);
        match lexer(&buffer) {
            Ok(_) => (),
            Err(msg) => println!("{}", msg),
        }
    }
}