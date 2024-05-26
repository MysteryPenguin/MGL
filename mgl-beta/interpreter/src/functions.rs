use std::fs;
use std::io::{stdin, stdout};
use std::io::{BufRead, Write};

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
        println!("You wrote: {}", buffer);
    }
    Ok(())
}

fn lexer(contents: &str) -> Result<(), String> {
    Err(String::from("Waiting for the next version..."))
}
