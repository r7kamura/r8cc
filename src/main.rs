mod code_generator;
mod parser;
mod tokenizer;

use code_generator::generate;
use parser::parse;
use tokenizer::tokenize;

fn abort(message: &str) -> ! {
    eprintln!("{}", message);
    std::process::exit(1);
}

fn main() {
    let mut args = std::env::args();
    if args.len() != 2 {
        abort("Usage: r8cc <PROGRAM>");
    }

    let input = args.nth(1).unwrap();
    let tokens = tokenize(&input).peekable();
    let node = parse(tokens);
    generate(node);
}
