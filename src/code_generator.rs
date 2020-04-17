use crate::parser::Node;

pub fn generate(node: Node) {
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    process(node);
    return_();
}

fn process(node: Node) {
    match node {
        Node::Program { statements } => {
            println!("  push rbp");
            println!("  mov rbp, rsp");
            println!("  sub rsp, 16"); // TODO: Adjust 16 for local variables count.
            for statement in statements {
                process(statement);
            }
        },
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
        Node::Assign { left, right } => {
            process_address_of(*left);
            process(*right);
            store();
        },
        Node::LocalVariable { .. } => {
            process_address_of(node);
            load();
        },
        Node::Return { value } => {
            process(*value);
            return_();
        },
    }
}

fn process_address_of(node: Node) {
    if let Node::LocalVariable { local_variable } = node {
        println!("  mov rax, rbp");
        println!("  sub rax, {}", local_variable.offset);
        println!("  push rax");
    }
}

fn load() {
    println!("  pop rax");
    println!("  mov rax, [rax]");
    println!("  push rax");
}

fn store() {
    println!("  pop rdi");
    println!("  pop rax");
    println!("  mov [rax], rdi");
    println!("  push rdi");
}

fn return_() {
    println!("  pop rax");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
