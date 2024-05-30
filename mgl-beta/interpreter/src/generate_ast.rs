use std::io::Write;
use std::process;
use std::io;
use std::fs::File;
use std::path::Path;

pub struct GenerateAst;

impl GenerateAst {
    pub fn run(&self, args: Vec<String>) {
        if args.len() != 1 {
            println!("Usage: generate_ast <output directory>");
            process::exit(64);
        }
        let output_dir = &args[0];

        self.define_ast(output_dir, "Expr", vec![
            String::from("Binary   : Expr left, Token operator, Expr right"),
            String::from("Grouping : Expr expression"),
            String::from("Literal  : Object value"),
            String::from("Unary    : Token operator, Expr right")
        ]);
    }

    fn define_ast(output_dir: String, base_name: String, types: Vec<String>) -> io::Result<()> {
        let path: String = format!("{}/{}", output_dir, base_name);

        let path = Path::new(&path);
        let mut file = File::create(&path)?;

        file.write(format!());
        Ok(())
    }
}