use crate::parser::Node;

pub fn generate(node: Node) {
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    process(node);
    println!("  pop rax");
    println!("  ret");
}

fn process(node: Node) {
    match node {
        Node::Integer { value } => {
            println!("  push {}", value);
        },
        Node::Add { left, right } => {
            process(*left);
            process(*right);
            println!("  pop rdi");
            println!("  pop rax");
            println!("  add rax, rdi");
            println!("  push rax");
        },
        Node::Subtract { left, right } => {
            process(*left);
            process(*right);
            println!("  pop rdi");
            println!("  pop rax");
            println!("  sub rax, rdi");
            println!("  push rax");
        },
    }
}
