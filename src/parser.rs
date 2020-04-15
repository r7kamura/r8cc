use crate::tokenizer::{Token, Tokens};

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
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
}

pub fn parse(tokens: &mut std::iter::Peekable<Tokens>) -> Node {
    parse_expression(tokens)
}

// expression = number ("+" number | "-" number)*
fn parse_expression(tokens: &mut std::iter::Peekable<Tokens>) -> Node {
    let mut node = parse_number(tokens);
    while let Some(token) = tokens.peek() {
        match token {
            Token::Plus => {
                tokens.next();
                let right = parse_number(tokens);
                node = Node::Add {
                    left: Box::new(node),
                    right: Box::new(right),
                };
            },
            Token::Minus => {
                tokens.next();
                let right = parse_number(tokens);
                node = Node::Subtract {
                    left: Box::new(node),
                    right: Box::new(right),
                };
            },
            _ => {},
        }
    }
    node
}

// number = INTEGER
fn parse_number(tokens: &mut std::iter::Peekable<Tokens>) -> Node {
    let token = tokens.next().expect("Expect token.");
    match token {
        Token::Integer { value } => {
            Node::Integer { value }
        },
        _ => panic!("Expect Integer token.")
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::tokenize;
    use super::{Node, parse};

    #[test]
    fn test_parse_number() {
        let mut tokens = tokenize("1").peekable();
        assert_eq!(
            parse(&mut tokens),
            Node::Integer {
                value: 1,
            }
        )
    }

    #[test]
    fn test_parse_add() {
        let mut tokens = tokenize("1 + 2").peekable();
        assert_eq!(
            parse(&mut tokens),
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
            }
        )
    }

    #[test]
    fn test_parse_subtract() {
        let mut tokens = tokenize("1 - 2").peekable();
        assert_eq!(
            parse(&mut tokens),
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
            }
        )
    }

    #[test]
    fn test_parse_add_and_subtract() {
        let mut tokens = tokenize("1 + 2 - 3").peekable();
        assert_eq!(
            parse(&mut tokens),
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
            }
        )
    }
}
