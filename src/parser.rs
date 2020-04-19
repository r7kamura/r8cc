use std::collections::HashMap;
use std::iter::Peekable;

use crate::tokenizer::{Keyword, Symbol, Token, TokenKind};

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
    //   = statement*
    //
    // statement
    //   = "return" expression ";"
    //   | "if" "(" expression ")" statement ("else" statement)?
    //   | "while" "(" expression ")" statement
    //   | "for" "(" expression? ";" expression? ";" expression? ")" statement
    //   | type identifier ";"
    //   | "{" statements* "}"
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
        let mut statements = Vec::new();
        while self.tokens.peek().is_some() {
            statements.push(self.parse_statement()?);
        }
        Ok(Node::Program { statements })
    }

    fn parse_statement(&mut self) -> ParseResult {
        match self.tokens.peek().unwrap().kind {
            TokenKind::Keyword(Keyword::For) => self.parse_statement_for(),
            TokenKind::Keyword(Keyword::If) => self.parse_statement_if(),
            TokenKind::Keyword(Keyword::Return) => self.parse_statement_return(),
            TokenKind::Keyword(Keyword::While) => self.parse_statement_while(),
            TokenKind::Keyword(Keyword::Integer) => {
                self.parse_statement_local_variable_declaration()
            }
            TokenKind::Symbol(Symbol::BraceLeft) => self.parse_statement_block(),
            _ => self.parse_statement_expression(),
        }
    }

    fn parse_statement_block(&mut self) -> ParseResult {
        let mut statements = Vec::new();
        self.consume_token(TokenKind::Symbol(Symbol::BraceLeft))?;
        while !self.has_next_token(TokenKind::Symbol(Symbol::BraceRight)) {
            statements.push(self.parse_statement()?);
        }
        self.consume_token(TokenKind::Symbol(Symbol::BraceRight))?;
        Ok(Node::Block { statements })
    }

    fn parse_statement_for(&mut self) -> ParseResult {
        self.consume_token(TokenKind::Keyword(Keyword::For))?;
        self.consume_token(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let mut initialization = None;
        let mut condition = None;
        let mut afterthrough = None;
        if !self.has_next_token(TokenKind::Symbol(Symbol::Semicolon)) {
            initialization = Some(Box::new(self.parse_expression()?));
        }
        self.consume_token(TokenKind::Symbol(Symbol::Semicolon))?;
        if !self.has_next_token(TokenKind::Symbol(Symbol::Semicolon)) {
            condition = Some(Box::new(self.parse_expression()?));
        }
        self.consume_token(TokenKind::Symbol(Symbol::Semicolon))?;
        match self.tokens.peek() {
            Some(Token {
                kind: TokenKind::Symbol(Symbol::ParenthesisRight),
                ..
            }) => {}
            _ => {
                afterthrough = Some(Box::new(self.parse_expression()?));
            }
        }
        self.consume_token(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        Ok(Node::For {
            initialization,
            condition,
            afterthrough,
            statement: Box::new(self.parse_statement()?),
        })
    }

    fn parse_statement_return(&mut self) -> ParseResult {
        self.consume_token(TokenKind::Keyword(Keyword::Return))?;
        let expression = self.parse_expression()?;
        self.consume_token(TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(Node::Return {
            value: Box::new(expression),
        })
    }

    fn parse_statement_if(&mut self) -> ParseResult {
        self.consume_token(TokenKind::Keyword(Keyword::If))?;
        self.consume_token(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let condition = self.parse_expression()?;
        self.consume_token(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        let statement = self.parse_statement()?;
        match self.tokens.peek() {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Else),
                ..
            }) => {
                self.consume_token(TokenKind::Keyword(Keyword::Else))?;
                Ok(Node::If {
                    condition: Box::new(condition),
                    statement_true: Box::new(statement),
                    statement_false: Some(Box::new(self.parse_statement()?)),
                })
            }
            _ => Ok(Node::If {
                condition: Box::new(condition),
                statement_true: Box::new(statement),
                statement_false: None,
            }),
        }
    }

    fn parse_statement_while(&mut self) -> ParseResult {
        self.consume_token(TokenKind::Keyword(Keyword::While))?;
        self.consume_token(TokenKind::Symbol(Symbol::ParenthesisLeft))?;
        let condition = self.parse_expression()?;
        self.consume_token(TokenKind::Symbol(Symbol::ParenthesisRight))?;
        let statement = self.parse_statement()?;
        Ok(Node::While {
            condition: Box::new(condition),
            statement: Box::new(statement),
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
                    self.consume_token(TokenKind::Symbol(Symbol::Semicolon))?;
                    Ok(Node::LocalVariableDeclaration)
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
        self.consume_token(TokenKind::Keyword(Keyword::Integer))?;
        Ok(Node::Type {
            type_: Type::Integer,
        })
    }

    fn parse_statement_expression(&mut self) -> ParseResult {
        let expression = self.parse_expression()?;
        self.consume_token(TokenKind::Symbol(Symbol::Semicolon))?;
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
                    node = Node::Assign {
                        left: Box::new(node),
                        right: Box::new(self.parse_assign()?),
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
                    node = Node::Add {
                        left: Box::new(node),
                        right: Box::new(right),
                    };
                }
                Token {
                    kind: TokenKind::Symbol(Symbol::Minus),
                    ..
                } => {
                    self.tokens.next();
                    let right = self.parse_multiply()?;
                    node = Node::Subtract {
                        left: Box::new(node),
                        right: Box::new(right),
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
            } => Ok(Node::Integer { value }),
            Token {
                kind: TokenKind::Identifier(name),
                ..
            } => {
                if let Some(local_variable) = self.environment.find_local_variable_by(&name) {
                    Ok(Node::LocalVariable { local_variable })
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

    fn consume_token(&mut self, kind: TokenKind) -> Result<(), ParseError> {
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
    use super::{parse, LocalVariable, Node, ParseError};
    use crate::tokenizer::tokenize;

    #[test]
    fn test_parse_number() {
        let tokens = tokenize("1;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::Integer { value: 1 },],
            })
        )
    }

    #[test]
    fn test_parse_add() {
        let tokens = tokenize("1 + 2;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::Add {
                    left: Box::new(Node::Integer { value: 1 }),
                    right: Box::new(Node::Integer { value: 2 })
                },],
            })
        )
    }

    #[test]
    fn test_parse_subtract() {
        let tokens = tokenize("1 - 2;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::Subtract {
                    left: Box::new(Node::Integer { value: 1 }),
                    right: Box::new(Node::Integer { value: 2 })
                },],
            })
        )
    }

    #[test]
    fn test_parse_add_and_subtract() {
        let tokens = tokenize("1 + 2 - 3;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::Subtract {
                    left: Box::new(Node::Add {
                        left: Box::new(Node::Integer { value: 1 }),
                        right: Box::new(Node::Integer { value: 2 }),
                    }),
                    right: Box::new(Node::Integer { value: 3 })
                },],
            })
        )
    }

    #[test]
    fn test_parse_assign() {
        let tokens = tokenize("int a; a = 1;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![
                    Node::LocalVariableDeclaration,
                    Node::Assign {
                        left: Box::new(Node::LocalVariable {
                            local_variable: LocalVariable {
                                name: "a".to_string(),
                                offset: 8,
                            }
                        }),
                        right: Box::new(Node::Integer { value: 1 })
                    }
                ],
            })
        );
    }

    #[test]
    fn test_parse_undefined_variable() {
        let tokens = tokenize("a;");
        assert_eq!(
            parse(tokens),
            Err(ParseError::UndefinedLocalVariable {
                name: "a".to_string()
            })
        );
    }

    #[test]
    fn test_parse_return() {
        let tokens = tokenize("return 1;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::Return {
                    value: Box::new(Node::Integer { value: 1 }),
                },],
            })
        );
    }

    #[test]
    fn test_parse_if() {
        let tokens = tokenize("if (1) 2; else 3;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::If {
                    condition: Box::new(Node::Integer { value: 1 }),
                    statement_true: Box::new(Node::Integer { value: 2 }),
                    statement_false: Some(Box::new(Node::Integer { value: 3 })),
                },],
            })
        );
    }

    #[test]
    fn test_parse_while() {
        let tokens = tokenize("while (1) 2;");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::While {
                    condition: Box::new(Node::Integer { value: 1 }),
                    statement: Box::new(Node::Integer { value: 2 }),
                },],
            })
        );
    }

    #[test]
    fn test_parse_for() {
        let tokens = tokenize("for (;;) { 1; 2; }");
        assert_eq!(
            parse(tokens),
            Ok(Node::Program {
                statements: vec![Node::For {
                    initialization: None,
                    condition: None,
                    afterthrough: None,
                    statement: Box::new(Node::Block {
                        statements: vec![Node::Integer { value: 1 }, Node::Integer { value: 2 }],
                    }),
                }],
            })
        );
    }
}
