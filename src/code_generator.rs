use crate::parser::{Node, NodeKind, Type};

pub fn generate(node: Node) {
    CodeGenerator::default().generate(node);
}

#[derive(Default)]
struct CodeGenerator {
    labels_count: usize,
}

impl CodeGenerator {
    fn generate(&mut self, node: Node) {
        match node.kind {
            NodeKind::Program {
                function_definitions,
                global_variable_declarations,
                strings,
            } => {
                println!(".intel_syntax noprefix");
                println!(".data");
                for global_variable_declaration in global_variable_declarations {
                    if let Node {
                        kind: NodeKind::GlobalVariableDeclaration { local_variable },
                        ..
                    } = global_variable_declaration
                    {
                        println!("{}:", local_variable.name);
                        println!("  .zero {}", local_variable.type_.size());
                    }
                }
                for (index, string) in strings.iter().enumerate() {
                    println!(".LC{}:", index);
                    println!("  .string \"{}\"", string);
                }
                for function_definition in function_definitions {
                    if let Node {
                        kind:
                            NodeKind::FunctionDefinition {
                                name,
                                local_variables,
                                block,
                            },
                        ..
                    } = function_definition
                    {
                        println!(".global {}", name);
                        println!("{}:", name);
                        println!("  push rbp");
                        println!("  mov rbp, rsp");
                        let mut sum: u32 = 0;
                        for (_, local_variable) in local_variables {
                            sum += local_variable.type_.size();
                        }
                        println!("  sub rsp, {}", align(sum, 8));
                        self.generate(*block);
                    }
                }
            }
            NodeKind::Integer { value } => {
                println!("  push {}", value);
            }
            NodeKind::String { index } => {
                println!("  lea rax, .LC{}[rip]", index);
                println!("  push rax");
            }
            NodeKind::Reference { value } => match value.type_.clone() {
                Type::Array { .. } => {
                    self.generate(*value);
                }
                _ => {
                    self.generate_address_of(*value);
                }
            },
            NodeKind::Dereference { value } => {
                self.generate(*value);
                self.load(node.type_);
            }
            NodeKind::Add { left, right } => {
                self.generate(*left);
                self.generate(*right);
                println!("  pop rdi");
                println!("  pop rax");
                println!("  add rax, rdi");
                println!("  push rax");
            }
            NodeKind::Subtract { left, right } => {
                self.generate(*left);
                self.generate(*right);
                println!("  pop rdi");
                println!("  pop rax");
                println!("  sub rax, rdi");
                println!("  push rax");
            }
            NodeKind::AddPointer { left, right } => match left.type_.clone() {
                Type::Pointer { type_ } | Type::Array { type_, .. } => {
                    self.generate(*left);
                    self.generate(*right);
                    println!("  pop rdi");
                    println!("  pop rax");
                    println!("  imul rdi, {}", type_.size());
                    println!("  add rax, rdi");
                    println!("  push rax");
                }
                _ => {
                    panic!("Unexpected left operand on add pointer operator.");
                }
            },
            NodeKind::SubtractPointer { left, right } => match left.type_.clone() {
                Type::Pointer { type_ } | Type::Array { type_, .. } => {
                    self.generate(*left);
                    self.generate(*right);
                    println!("  pop rdi");
                    println!("  pop rax");
                    println!("  imul rdi, {}", type_.size());
                    println!("  sub rax, rdi");
                    println!("  push rax");
                }
                _ => {
                    panic!("Unexpected left operand on add pointer operator.");
                }
            },
            NodeKind::Assign { left, right } => {
                self.generate_address_of(*left);
                self.generate(*right);
                self.store();
            }
            NodeKind::LocalVariable { .. } => {
                let type_ = node.type_.clone();
                match type_ {
                    Type::Array { .. } => {
                        self.generate_address_of(node);
                    }
                    _ => {
                        self.generate_address_of(node);
                        self.load(type_);
                    }
                }
            }
            NodeKind::Return { value } => {
                self.generate(*value);
                self.return_();
            }
            NodeKind::If {
                condition,
                statement_true,
                statement_false,
            } => {
                self.labels_count += 1;
                if statement_false.is_some() {
                    self.generate(*condition);
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .Lelse{}", self.labels_count);
                    self.generate(*statement_true);
                    println!("  jmp .Lend{}", self.labels_count);
                    println!(".Lelse{}:", self.labels_count);
                    self.generate(*statement_false.unwrap());
                    println!(".Lend{}:", self.labels_count);
                } else {
                    self.generate(*condition);
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .Lend{}", self.labels_count);
                    self.generate(*statement_true);
                    println!(".Lend{}:", self.labels_count);
                }
            }
            NodeKind::While {
                condition,
                statement,
            } => {
                self.labels_count += 1;
                println!(".Lbegin{}:", self.labels_count);
                self.generate(*condition);
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je .Lend{}", self.labels_count);
                self.generate(*statement);
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
                    self.generate(*initialization.unwrap());
                }
                println!(".Lbegin{}:", self.labels_count);
                if condition.is_some() {
                    self.generate(*condition.unwrap());
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .Lend{}", self.labels_count);
                }
                self.generate(*statement);
                if afterthrough.is_some() {
                    self.generate(*afterthrough.unwrap());
                }
                println!("  jmp .Lbegin{}", self.labels_count);
                println!(".Lend{}:", self.labels_count);
            }
            NodeKind::Block { statements } => {
                for statement in statements {
                    self.generate(statement);
                }
            }
            _ => {}
        }
    }

    fn generate_address_of(&self, node: Node) {
        match node.kind {
            NodeKind::Dereference { value } => {
                generate(*value);
            }
            NodeKind::LocalVariable { local_variable } => {
                if local_variable.global {
                    println!("  lea rax, {}[rip]", local_variable.name);
                } else {
                    println!("  mov rax, rbp");
                    println!("  sub rax, {}", local_variable.offset);
                }
                println!("  push rax");
            }
            _ => {
                panic!("{:?}", node);
            }
        }
    }

    fn load(&self, type_: Type) {
        println!("  pop rax");
        if type_.size() == 1 {
            println!("  movsx rax, BYTE PTR [rax]");
        } else {
            println!("  mov rax, [rax]");
        }
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

fn align(target: u32, unit: u32) -> u32 {
    (target + unit - 1) & !(unit - 1)
}
