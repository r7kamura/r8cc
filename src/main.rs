fn warn_usage() {
    eprintln!("Usage: r8cc <NUMBER>");
}

fn main() {
    let mut args = std::env::args();
    if args.len() != 2 {
        warn_usage();
        return;
    }

    let number = args.nth(1).unwrap().parse::<u32>();
    if let Ok(number) = number {
        println!(".intel_syntax noprefix");
        println!(".global main");
        println!("main:");
        println!("  mov rax, {}", number);
        println!("  ret");
    } else {
        warn_usage();
    }
}
