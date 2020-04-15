fn warn_usage() {
    eprintln!("Usage: r8cc <PROGRAM>");
}

fn consume_number(peekable: &mut std::iter::Peekable<std::str::Chars>) -> u32 {
    let mut number = 0;

    while let Some(&character) = peekable.peek() {
        if character.is_ascii_digit() {
            peekable.next();
            number = number * 10 + character.to_digit(10).unwrap();
        } else if character.is_whitespace() {
            peekable.next();
        } else {
            break;
        }
    }
    number
}

fn main() {
    let mut args = std::env::args();
    if args.len() != 2 {
        warn_usage();
        return;
    }

    let input = args.nth(1).unwrap();
    let mut chars = input.chars().peekable();

    let number = consume_number(&mut chars);
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  mov rax, {}", number);
    while let Some(&character) = chars.peek() {
        match character {
            '+' => {
                chars.next();
                println!("  add rax, {}", consume_number(&mut chars));
            },
            '-' => {
                chars.next();
                println!("  sub rax, {}", consume_number(&mut chars));
            },
            ' ' => {
                chars.next();
            },
            _ => {
                panic!("Unexpected token: '{}'", chars.next().unwrap());
            },
        }
    }
    println!("  ret");
}
