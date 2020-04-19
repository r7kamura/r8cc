use crate::parser::{Node, NodeKind};

pub fn generate(node: Node) {
    CodeGenerator::default().generate(node);
}

#[derive(Default)]
struct CodeGenerator {
    labels_count: usize,
}

impl CodeGenerator {
    fn generate(&mut self, node: Node) {
        println!(".intel_syntax noprefix");
        self.process(node);
    }

    fn process(&mut self, node: Node) {
        match node.kind {
            NodeKind::Program {
                function_definitions,
            } => {
                for function_definition in function_definitions {
                    if let Node {
                        kind: NodeKind::FunctionDefinition { name, block },
                    } = function_definition
                    {
                        println!(".global {}", name);
                        println!("{}:", name);
                        println!("  push rbp");
                        println!("  mov rbp, rsp");
                        println!("  sub rsp, 16"); // TODO: Adjust 16 for local variables count.
                        self.process(*block);
                    }
                }
            }
            NodeKind::Integer { value } => {
                println!("  push {}", value);
            }
            NodeKind::Add { left, right } => {
                self.process(*left);
                self.process(*right);
                println!("  pop rdi");
                println!("  pop rax");
                println!("  add rax, rdi");
                println!("  push rax");
            }
            NodeKind::Subtract { left, right } => {
                self.process(*left);
                self.process(*right);
                println!("  pop rdi");
                println!("  pop rax");
                println!("  sub rax, rdi");
                println!("  push rax");
            }
            NodeKind::Assign { left, right } => {
                self.process_address_of(*left);
                self.process(*right);
                self.store();
            }
            NodeKind::LocalVariable { .. } => {
                self.process_address_of(node);
                self.load();
            }
            NodeKind::Return { value } => {
                self.process(*value);
                self.return_();
            }
            NodeKind::If {
                condition,
                statement_true,
                statement_false,
            } => {
                self.labels_count += 1;
                if statement_false.is_some() {
                    self.process(*condition);
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .Lelse{}", self.labels_count);
                    self.process(*statement_true);
                    println!("  jmp .Lend{}", self.labels_count);
                    println!(".Lelse{}:", self.labels_count);
                    self.process(*statement_false.unwrap());
                    println!(".Lend{}:", self.labels_count);
                } else {
                    self.process(*condition);
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .Lend{}", self.labels_count);
                    self.process(*statement_true);
                    println!(".Lend{}:", self.labels_count);
                }
            }
            NodeKind::While {
                condition,
                statement,
            } => {
                self.labels_count += 1;
                println!(".Lbegin{}:", self.labels_count);
                self.process(*condition);
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je .Lend{}", self.labels_count);
                self.process(*statement);
                println!("  jmp .Lbegin{}", self.labels_count);
                println!(".Lend{}:", self.labels_count);
            }
            NodeKind::For {
                initialization,
                condition,
                afterthrough,
                statement,
            } => {
                self.labels_count += 1;
                if initialization.is_some() {
                    self.process(*initialization.unwrap());
                }
                println!(".Lbegin{}:", self.labels_count);
                if condition.is_some() {
                    self.process(*condition.unwrap());
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .Lend{}", self.labels_count);
                }
                self.process(*statement);
                if afterthrough.is_some() {
                    self.process(*afterthrough.unwrap());
                }
                println!("  jmp .Lbegin{}", self.labels_count);
                println!(".Lend{}:", self.labels_count);
            }
            NodeKind::Block { statements } => {
                for statement in statements {
                    self.process(statement);
                }
            }
            _ => {}
        }
    }

    fn process_address_of(&self, node: Node) {
        if let NodeKind::LocalVariable { local_variable } = node.kind {
            println!("  mov rax, rbp");
            println!("  sub rax, {}", local_variable.offset);
            println!("  push rax");
        }
    }

    fn load(&self) {
        println!("  pop rax");
        println!("  mov rax, [rax]");
        println!("  push rax");
    }

    fn store(&self) {
        println!("  pop rdi");
        println!("  pop rax");
        println!("  mov [rax], rdi");
        println!("  push rdi");
    }

    fn return_(&self) {
        println!("  pop rax");
        println!("  mov rsp, rbp");
        println!("  pop rbp");
        println!("  ret");
    }
}
