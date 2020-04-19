use std::collections::HashMap;
use std::iter::Peekable;

use crate::tokenizer::{Keyword, Symbol, Token, TokenKind};

#[derive(Debug, PartialEq, Eq)]
pub enum NodeKind {
    Program {
        function_definitions: Vec<Node>,
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
    For {
        initialization: Option<Box<Node>>,
        condition: Option<Box<Node>>,
        afterthrough: Option<Box<Node>>,
        statement: Box<Node>,
    },
    Block {
        statements: Vec<Node>,
    },
    LocalVariableDeclaration,
    Type {
        type_: Type,
    },
    FunctionDefinition {
        name: String,
        block: Box<Node>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Integer,
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
        self.local_variables
            .insert(name.clone(), local_variable.clone());
        local_variable
    }

    fn find_local_variable_by(&self, name: &str) -> Option<LocalVariable> {
        self.local_variables.get(name).map(|v| v.clone())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    MissingToken(TokenKind),
    UnexpectedToken {
        expected: Option<TokenKind>, // Use this hint field whenever there is one expected TokenKind.
        actual: Token,
    },
    NotIdentifierToken {
        actual: Token,
    },
    UnexpectedEos,
    UndefinedLocalVariable {
        name: String,
    },
}

type ParseResult = Result<Node, ParseError>;

pub fn parse<T: Iterator<Item = Token>>(tokens: T) -> ParseResult {
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
    //   = function_definition*
    //
    // function_definition
    //   = type identifier "(" ")" block
    //
    // block
    //   = "{" statement* "}"
    //
    // statement
    //   = "return" expression ";"
    //   | "if" "(" expression ")" statement ("else" statement)?
    //   | "while" "(" expression ")" statement
    //   | "for" "(" expression? ";" expression? ";" expression? ")" statement
    //   | type identifier ";"
    //   | block
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
    fn parse(&mut self) -> ParseResult {
        let mut function_definitions = Vec::new();
        while self.tokens.peek().is_some() {
            function_definitions.push(self.parse_function_definition()?);
        }
        Ok(Node {
            kind: NodeKind::Program {
                function_definitions,
            },
        })
    }

    fn parse_function_definition(&mut self) -> ParseResult {
        self.parse_type()?;
        let token = self.consume_token()?;
        if let Token {
            kind: TokenKind::Identifier(name),
            ..
        } = token
        {
            self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
            self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
            Ok(Node {
                kind: NodeKind::FunctionDefinition {
                    name,
                    block: Box::new(self.parse_block()?),
                },
            })
        } else {
            Err(ParseError::NotIdentifierToken {
                actual: self.tokens.next().unwrap(),
            })
        }
    }

    fn parse_block(&mut self) -> ParseResult {
        let mut statements = Vec::new();
        self.consume_token_of(TokenKind::Symbol(Symbol::BraceLeft))?;
        while !self.has_next_token(TokenKind::Symbol(Symbol::BraceRight)) {
            statements.push(self.parse_statement()?);
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::BraceRight))?;
        Ok(Node {
            kind: NodeKind::Block { statements },
        })
    }

    fn parse_statement(&mut self) -> ParseResult {
        if let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenKind::Keyword(Keyword::For) => self.parse_statement_for(),
                TokenKind::Keyword(Keyword::If) => self.parse_statement_if(),
                TokenKind::Keyword(Keyword::Return) => self.parse_statement_return(),
                TokenKind::Keyword(Keyword::While) => self.parse_statement_while(),
                TokenKind::Keyword(Keyword::Integer) => {
                    self.parse_statement_local_variable_declaration()
                }
                TokenKind::Symbol(Symbol::BraceLeft) => self.parse_block(),
                _ => self.parse_statement_expression(),
            }
        } else {
            Err(ParseError::UnexpectedEos)
        }
    }

    fn parse_statement_for(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::For))?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let mut initialization = None;
        let mut condition = None;
        let mut afterthrough = None;
        if !self.has_next_token(TokenKind::Symbol(Symbol::Semicolon)) {
            initialization = Some(Box::new(self.parse_expression()?));
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        if !self.has_next_token(TokenKind::Symbol(Symbol::Semicolon)) {
            condition = Some(Box::new(self.parse_expression()?));
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        match self.tokens.peek() {
            Some(Token {
                kind: TokenKind::Symbol(Symbol::ParenthesisRight),
                ..
            }) => {}
            _ => {
                afterthrough = Some(Box::new(self.parse_expression()?));
            }
        }
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        Ok(Node {
            kind: NodeKind::For {
                initialization,
                condition,
                afterthrough,
                statement: Box::new(self.parse_statement()?),
            },
        })
    }

    fn parse_statement_return(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::Return))?;
        let expression = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(Node {
            kind: NodeKind::Return {
                value: Box::new(expression),
            },
        })
    }

    fn parse_statement_if(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::If))?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let condition = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        let statement = self.parse_statement()?;
        match self.tokens.peek() {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Else),
                ..
            }) => {
                self.consume_token_of(TokenKind::Keyword(Keyword::Else))?;
                Ok(Node {
                    kind: NodeKind::If {
                        condition: Box::new(condition),
                        statement_true: Box::new(statement),
                        statement_false: Some(Box::new(self.parse_statement()?)),
                    },
                })
            }
            _ => Ok(Node {
                kind: NodeKind::If {
                    condition: Box::new(condition),
                    statement_true: Box::new(statement),
                    statement_false: None,
                },
            }),
        }
    }

    fn parse_statement_while(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::While))?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let condition = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        let statement = self.parse_statement()?;
        Ok(Node {
            kind: NodeKind::While {
                condition: Box::new(condition),
                statement: Box::new(statement),
            },
        })
    }

    fn parse_statement_local_variable_declaration(&mut self) -> ParseResult {
        self.parse_type()?;
        match self.tokens.next() {
            Some(token) => match token {
                Token {
                    kind: TokenKind::Identifier(name),
                    ..
                } => {
                    self.environment.add_local_variable(name);
                    self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
                    Ok(Node {
                        kind: NodeKind::LocalVariableDeclaration,
                    })
                }
                _ => Err(ParseError::UnexpectedToken {
                    expected: None,
                    actual: token,
                }),
            },
            _ => Err(ParseError::UnexpectedEos),
        }
    }

    fn parse_type(&mut self) -> ParseResult {
        self.consume_token_of(TokenKind::Keyword(Keyword::Integer))?;
        Ok(Node {
            kind: NodeKind::Type {
                type_: Type::Integer,
            },
        })
    }

    fn parse_statement_expression(&mut self) -> ParseResult {
        let expression = self.parse_expression()?;
        self.consume_token_of(TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(expression)
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> ParseResult {
        let mut node = self.parse_add()?;
        if let Some(token) = self.tokens.peek() {
            match token {
                Token {
                    kind: TokenKind::Symbol(Symbol::Equal),
                    ..
                } => {
                    self.tokens.next();
                    node = Node {
                        kind: NodeKind::Assign {
                            left: Box::new(node),
                            right: Box::new(self.parse_assign()?),
                        },
                    };
                }
                _ => {}
            }
        }
        Ok(node)
    }

    fn parse_add(&mut self) -> ParseResult {
        let mut node = self.parse_multiply()?;
        while let Some(token) = self.tokens.peek() {
            match token {
                Token {
                    kind: TokenKind::Symbol(Symbol::Plus),
                    ..
                } => {
                    self.tokens.next();
                    let right = self.parse_multiply()?;
                    node = Node {
                        kind: NodeKind::Add {
                            left: Box::new(node),
                            right: Box::new(right),
                        },
                    };
                }
                Token {
                    kind: TokenKind::Symbol(Symbol::Minus),
                    ..
                } => {
                    self.tokens.next();
                    let right = self.parse_multiply()?;
                    node = Node {
                        kind: NodeKind::Subtract {
                            left: Box::new(node),
                            right: Box::new(right),
                        },
                    };
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn parse_multiply(&mut self) -> ParseResult {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> ParseResult {
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult {
        let token = self.tokens.next().expect("Expect token.");
        match token {
            Token {
                kind: TokenKind::Integer(value),
                ..
            } => Ok(Node {
                kind: NodeKind::Integer { value },
            }),
            Token {
                kind: TokenKind::Identifier(name),
                ..
            } => {
                if let Some(local_variable) = self.environment.find_local_variable_by(&name) {
                    Ok(Node {
                        kind: NodeKind::LocalVariable { local_variable },
                    })
                } else {
                    Err(ParseError::UndefinedLocalVariable { name })
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: None,
                actual: token,
            }),
        }
    }

    fn consume_token(&mut self) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.next() {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedEos)
        }
    }

    fn consume_token_of(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.kind == kind {
                Ok(())
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: Some(kind),
                    actual: token,
                })
            }
        } else {
            Err(ParseError::MissingToken(kind))
        }
    }

    fn has_next_token(&mut self, kind: TokenKind) -> bool {
        if let Some(token) = self.tokens.peek() {
            token.kind == kind
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{parse, Node, NodeKind, ParseError};
    use crate::tokenizer::tokenize;

    #[test]
    fn test_parse_undefined_variable() {
        let tokens = tokenize("int main() { return a; }");
        assert_eq!(
            parse(tokens),
            Err(ParseError::UndefinedLocalVariable {
                name: "a".to_string()
            })
        )
    }

    #[test]
    fn test_parse_if() {
        let tokens = tokenize("int main() { if (1) { return 2; } else { return 3; } }");
        assert_eq!(
            parse(tokens),
            Ok(Node {
                kind: NodeKind::Program {
                    function_definitions: vec![Node {
                        kind: NodeKind::FunctionDefinition {
                            name: "main".to_string(),
                            block: Box::new(Node {
                                kind: NodeKind::Block {
                                    statements: vec![Node {
                                        kind: NodeKind::If {
                                            condition: Box::new(Node {
                                                kind: NodeKind::Integer { value: 1 }
                                            }),
                                            statement_true: Box::new(Node {
                                                kind: NodeKind::Block {
                                                    statements: vec![Node {
                                                        kind: NodeKind::Return {
                                                            value: Box::new(Node {
                                                                kind: NodeKind::Integer {
                                                                    value: 2
                                                                }
                                                            })
                                                        }
                                                    }],
                                                }
                                            }),
                                            statement_false: Some(Box::new(Node {
                                                kind: NodeKind::Block {
                                                    statements: vec![Node {
                                                        kind: NodeKind::Return {
                                                            value: Box::new(Node {
                                                                kind: NodeKind::Integer {
                                                                    value: 3
                                                                }
                                                            })
                                                        }
                                                    }],
                                                }
                                            }))
                                        },
                                    }]
                                }
                            }),
                        },
                    }]
                }
            })
        )
    }

    #[test]
    fn test_parse_while() {
        let tokens = tokenize("int main() { while(1) return 2; }");
        assert_eq!(
            parse(tokens),
            Ok(Node {
                kind: NodeKind::Program {
                    function_definitions: vec![Node {
                        kind: NodeKind::FunctionDefinition {
                            name: "main".to_string(),
                            block: Box::new(Node {
                                kind: NodeKind::Block {
                                    statements: vec![Node {
                                        kind: NodeKind::While {
                                            condition: Box::new(Node {
                                                kind: NodeKind::Integer { value: 1 }
                                            }),
                                            statement: Box::new(Node {
                                                kind: NodeKind::Return {
                                                    value: Box::new(Node {
                                                        kind: NodeKind::Integer { value: 2 }
                                                    }),
                                                }
                                            }),
                                        }
                                    }],
                                }
                            })
                        }
                    }]
                }
            })
        )
    }
}
