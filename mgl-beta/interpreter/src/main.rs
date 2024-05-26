pub mod functions;

use std::{env, process::exit};
use crate::functions::{read_file, run_prompt};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: Neon [script]");
        exit(64);
    } else if args.len() == 2 {
        match read_file(&args[1]) {
            Ok(_) => exit(0),
            Err(msg) => {
                println!("Error:\n{}", msg);
                exit(0);
            }
        }
    } else {
        match run_prompt() {
            Ok(_) => exit(0),
            Err(msg) => {
                println!("Error:\n{}", msg);
                exit(1);
            }
        }
    }
}