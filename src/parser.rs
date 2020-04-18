use std::collections::HashMap;
use std::iter::Peekable;

use crate::tokenizer::{Token, TokenKind};

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    Program {
        statements: Vec<Node>,
    },
    Integer {
        value: u32,
    },
    Add {
        left: Box<Node>,
        right: Box<Node>,
    },
    Subtract {
        left: Box<Node>,
        right: Box<Node>,
    },
    Assign {
        left: Box<Node>,
        right: Box<Node>,
    },
    LocalVariable {
        local_variable: LocalVariable,
    },
    Return {
        value: Box<Node>,
    },
    If {
        condition: Box<Node>,
        statement_true: Box<Node>,
        statement_false: Option<Box<Node>>,
    },
    While {
        condition: Box<Node>,
        statement: Box<Node>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LocalVariable {
    pub name: String,
    pub offset: usize,
}

#[derive(Default)]
struct Environment {
    local_variables: HashMap<String, LocalVariable>,
    offset: usize,
}

impl Environment {
    fn add_local_variable(&mut self, name: String) -> LocalVariable {
        self.offset += 8;
        let local_variable = LocalVariable {
            name: name.clone(),
            offset: self.offset,
        };
        self.local_variables.insert(name.clone(), local_variable.clone());
        local_variable
    }

    fn find_local_variable_by(&self, name: &str) -> Option<LocalVariable> {
        self.local_variables.get(name).map(|v| v.clone())
    }
}

pub fn parse<T: Iterator<Item = Token>>(tokens: T) -> Node {
    let mut parser = Parser {
        tokens: tokens.peekable(),
        environment: Environment::default(),
    };
    parser.parse()
}

struct Parser<T: Iterator<Item = Token>> {
    environment: Environment,
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    // program
    //   = statement*
    //
    // statement
    //   = "return" expression ";"
    //   | "if" "(" expression ")" statement ("else" statement)?
    //   | "while" "(" expression ")" statement
    //   | expression ";"
    //
    // expression
    //   = assign
    //
    // assign
    //   = equality ("=" assign)?
    //
    // equality
    //   = relational ("==" relational | "!=" relational)*
    //
    // relational
    //   = add ("<" add | "<=" add | ">" add | ">=" add)*
    //
    // add
    //   = multiply ("+" multiply | "-" multiply)*
    //
    // multiply
    //   = unary ("*" unary | "/" unary)*
    //
    // unary
    //   = ("+" | "-")? primary
    //
    // primary
    //   = number
    //   | identifier
    //   | "(" expression ")"
    fn parse(&mut self) -> Node {
        let mut statements = Vec::new();
        while self.tokens.peek().is_some() {
            statements.push(self.parse_statement());
        }
        Node::Program {
            statements,
        }
    }

    fn parse_statement(&mut self) -> Node {
        let token = self.tokens.peek().unwrap();
        match token {
            Token { kind: TokenKind::Return, .. } => {
                self.tokens.next();
                let expression = self.parse_expression();
                self.tokens.next(); // Consume ";".
                Node::Return {
                    value: Box::new(expression),
                }
            },
            Token { kind: TokenKind::If, .. } => {
                self.tokens.next(); // Consume "if".
                self.tokens.next(); // Consume "(".
                let condition = self.parse_expression();
                self.tokens.next(); // Consume ")".
                let statement = self.parse_statement();
                match self.tokens.peek() {
                    Some(Token { kind: TokenKind::Else, .. }) => {
                        self.tokens.next();
                        Node::If {
                            condition: Box::new(condition),
                            statement_true: Box::new(statement),
                            statement_false: Some(Box::new(self.parse_statement())),
                        }
                    },
                    _ => {
                        Node::If {
                            condition: Box::new(condition),
                            statement_true: Box::new(statement),
                            statement_false: None,
                        }
                    }
                }
            },
            Token { kind: TokenKind::While, .. } => {
                self.tokens.next(); // Consume "while".
                self.tokens.next(); // Consume "(".
                let condition = self.parse_expression();
                self.tokens.next(); // Consume ")".
                let statement = self.parse_statement();
                Node::While {
                    condition: Box::new(condition),
                    statement: Box::new(statement),
                }
            },
            _ => {
                let expression = self.parse_expression();
                self.tokens.next(); // Consume ";".
                expression
            }
        }
    }

    fn parse_expression(&mut self) -> Node {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> Node {
        let mut node = self.parse_add();
        if let Some(token) = self.tokens.peek() {
            match token {
                Token { kind: TokenKind::Equal, .. } => {
                    self.tokens.next();
                    node = Node::Assign {
                        left: Box::new(node),
                        right: Box::new(self.parse_assign()),
                    };
                },
                _ => {},
            }
        }
        node
    }

    fn parse_add(&mut self) -> Node {
        let mut node = self.parse_multiply();
        while let Some(token) = self.tokens.peek() {
            match token {
                Token { kind: TokenKind::Plus, .. } => {
                    self.tokens.next();
                    let right = self.parse_multiply();
                    node = Node::Add {
                        left: Box::new(node),
                        right: Box::new(right),
                    };
                },
                Token { kind: TokenKind::Minus, .. } => {
                    self.tokens.next();
                    let right = self.parse_multiply();
                    node = Node::Subtract {
                        left: Box::new(node),
                        right: Box::new(right),
                    };
                },
                _ => {
                    break;
                },
            }
        }
        node
    }

    fn parse_multiply(&mut self) -> Node {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Node {
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Node {
        let token = self.tokens.next().expect("Expect token.");
        match token {
            Token { kind: TokenKind::Integer(value), .. } => {
                Node::Integer { value }
            },
            Token { kind: TokenKind::Identifier(name), .. } => {
                Node::LocalVariable {
                    local_variable: self.environment.find_local_variable_by(&name).
                        unwrap_or_else(|| self.environment.add_local_variable(name))
                }
            },
            _ => {
                panic!("Unexpected token: `{:?}`", token);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::tokenize;
    use super::{LocalVariable, Node, parse};

    #[test]
    fn test_parse_number() {
        let tokens = tokenize("1");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::Integer {
                        value: 1,
                    },
                ],
            }
        )
    }

    #[test]
    fn test_parse_add() {
        let tokens = tokenize("1 + 2");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::Add {
                        left: Box::new(
                            Node::Integer {
                                value: 1,
                            }
                        ),
                        right: Box::new(
                            Node::Integer {
                                value: 2,
                            }
                        )
                    },
                ],
            }
        )
    }

    #[test]
    fn test_parse_subtract() {
        let tokens = tokenize("1 - 2");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::Subtract {
                        left: Box::new(
                            Node::Integer {
                                value: 1,
                            }
                        ),
                        right: Box::new(
                            Node::Integer {
                                value: 2,
                            }
                        )
                    },
                ],
            }
        )
    }

    #[test]
    fn test_parse_add_and_subtract() {
        let tokens = tokenize("1 + 2 - 3");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::Subtract {
                        left: Box::new(
                            Node::Add {
                                left: Box::new(
                                    Node::Integer {
                                        value: 1,
                                    }
                                ),
                                right: Box::new(
                                    Node::Integer {
                                        value: 2,
                                    }
                                ),
                            }
                        ),
                        right: Box::new(
                            Node::Integer {
                                value: 3,
                            }
                        )
                    },
                ],
            }
        )
    }

    #[test]
    fn test_parse_local_variable() {
        let tokens = tokenize("a");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::LocalVariable {
                        local_variable: LocalVariable {
                            name: "a".to_string(),
                            offset: 8,
                        },
                    },
                ],
            }
        );
    }

    #[test]
    fn test_parse_assign() {
        let tokens = tokenize("a = 1");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::Assign {
                        left: Box::new(
                            Node::LocalVariable {
                                local_variable: LocalVariable {
                                    name: "a".to_string(),
                                    offset: 8,
                                }
                            }
                        ),
                        right: Box::new(
                            Node::Integer {
                                value: 1,
                            }
                        )
                    },
                ],
            }
        );
    }

    #[test]
    fn test_parse_statements() {
        let tokens = tokenize("a = 1; a");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::Assign {
                        left: Box::new(
                            Node::LocalVariable {
                                local_variable: LocalVariable {
                                    name: "a".to_string(),
                                    offset: 8,
                                },
                            }
                        ),
                        right: Box::new(
                            Node::Integer {
                                value: 1,
                            }
                        )
                    },
                    Node::LocalVariable {
                        local_variable: LocalVariable {
                            name: "a".to_string(),
                            offset: 8,
                        },
                    },
                ],
            }
        );
    }

    #[test]
    fn test_parse_return() {
        let tokens = tokenize("return 1;");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::Return {
                        value: Box::new(
                            Node::Integer {
                                value: 1,
                            }
                        ),
                    },
                ],
            }
        );
    }

    #[test]
    fn test_parse_if() {
        let tokens = tokenize("if (1) 2; else 3;");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::If {
                        condition: Box::new(
                            Node::Integer {
                                value: 1,
                            }
                        ),
                        statement_true: Box::new(
                            Node::Integer {
                                value: 2,
                            }
                        ),
                        statement_false: Some(
                            Box::new(
                                Node::Integer {
                                    value: 3,
                                }
                            )
                        ),
                    },
                ],
            }
        );
    }

    #[test]
    fn test_parse_while() {
        let tokens = tokenize("while (1) 2;");
        assert_eq!(
            parse(tokens),
            Node::Program {
                statements: vec![
                    Node::While {
                        condition: Box::new(
                            Node::Integer {
                                value: 1,
                            }
                        ),
                        statement: Box::new(
                            Node::Integer {
                                value: 2,
                            }
                        ),
                    },
                ],
            }
        );
    }
}
