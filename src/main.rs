mod tokenizer;

use tokenizer::Token;

fn warn_usage() {
    eprintln!("Usage: r8cc <PROGRAM>");
}

fn main() {
    let mut args = std::env::args();
    if args.len() != 2 {
        warn_usage();
        return;
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let input = args.nth(1).unwrap();
    let mut tokens = tokenizer::tokenize(&input);

    if let Some(token) = tokens.next() {
        match token {
            Token::Integer { value } => {
                println!("  mov rax, {}", value);
            },
            _ => {
                warn_usage();
                return;
            }
        }
    } else {
        warn_usage();
        return;
    }

    let mut positive = true;
    for token in tokens {
        match token {
            Token::Plus => {
                positive = true;
            },
            Token::Minus => {
                positive = false;
            },
            Token::Integer { value } => {
                if positive {
                    println!("  add rax, {}", value);
                } else {
                    println!("  sub rax, {}", value);
                }
            },
        }
    }
    println!("  ret");
}
