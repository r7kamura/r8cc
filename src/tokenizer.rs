use std::iter::Peekable;
use std::str::Chars;

pub fn tokenize(input: &str) -> Tokens {
    Tokens::new(input.chars().peekable())
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Semicolon,
    Plus,
    Minus,
    Equal,
    BraceLeft,
    BraceRight,
    ParenthesisLeft,
    ParenthesisRight,
    If,
    Else,
    While,
    Return,
    Integer(u32),
    String(String),
    Character(char),
    Identifier(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub column: usize,
    pub line: usize,
}

impl Position {
    fn new(column: usize, line: usize) -> Self {
        Self { column, line }
    }
}

impl Default for Position {
    fn default() -> Position {
        Position::new(1, 1)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

impl Token {
    fn new(kind: TokenKind, position: Position) -> Self {
        Self { kind, position }
    }
}

pub struct Tokens<'a> {
    chars: Peekable<Chars<'a>>,
    position: Position,
}

impl<'a> Tokens<'a> {
    fn new(chars: Peekable<Chars<'a>>) -> Self {
        Self {
            chars,
            position: Position::default(),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.position.column += 1;
        self.chars.next()
    }

    fn consume_number_literal(&mut self) -> u32 {
        let mut number = 0;
        while let Some(&character) = self.chars.peek() {
            if character.is_ascii_digit() {
                self.next_char();
                number = number * 10 + character.to_digit(10).unwrap();
            } else {
                break;
            }
        }
        number
    }

    fn consume_character_literal(&mut self) -> char {
        self.next_char();
        let character = self.next_char();
        self.next_char();
        character.unwrap()
    }

    fn consume_string_literal(&mut self) -> String {
        let mut string = String::new();
        self.next_char();
        while let Some(character) = self.next_char() {
            if character == '"' {
                break;
            } else {
                string.push(character);
            }
        }
        string
    }

    fn consume_identifier(&mut self) -> String {
        let mut string = String::new();
        while let Some(&character) = self.chars.peek() {
            if character.is_ascii_alphanumeric() {
                string.push(character);
                self.next_char();
            } else {
                break;
            }
        }
        string
    }

    fn skip_whitespaces(&mut self) {
        while let Some(&character) = self.chars.peek() {
            if character == '\n' {
                self.position.column = 1;
                self.position.line += 1;
                self.chars.next();
            } else if character.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }
}

impl Iterator for Tokens<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespaces();
        let mut token = None;
        if let Some(&character) = self.chars.peek() {
            let position = self.position.clone();
            match character {
                ';' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Semicolon, position));
                }
                '+' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Plus, position));
                }
                '-' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Minus, position));
                }
                '=' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Equal, position));
                }
                '(' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::ParenthesisLeft, position));
                }
                ')' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::ParenthesisRight, position));
                }
                '{' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::BraceLeft, position));
                }
                '}' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::BraceRight, position));
                }
                '"' => {
                    let value = self.consume_string_literal();
                    token = Some(Token::new(TokenKind::String(value), position));
                }
                '\'' => {
                    let value = self.consume_character_literal();
                    token = Some(Token::new(TokenKind::Character(value), position));
                }
                _ => {
                    if character.is_ascii_digit() {
                        token = Some(Token::new(
                            TokenKind::Integer(self.consume_number_literal()),
                            position,
                        ));
                    } else if character.is_ascii_alphabetic() {
                        let name = self.consume_identifier();
                        match &*name {
                            "if" => {
                                token = Some(Token::new(TokenKind::If, position));
                            }
                            "else" => {
                                token = Some(Token::new(TokenKind::Else, position));
                            }
                            "return" => {
                                token = Some(Token::new(TokenKind::Return, position));
                            }
                            "while" => {
                                token = Some(Token::new(TokenKind::While, position));
                            }
                            _ => {
                                token = Some(Token::new(TokenKind::Identifier(name), position));
                            }
                        }
                    }
                }
            }
        }
        token
    }
}

#[cfg(test)]
mod tests {
    use super::{tokenize, Position, Token, TokenKind};

    #[test]
    fn test_tokenize_empty_string() {
        let tokens: Vec<Token> = tokenize("").collect();
        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn test_tokenize_whitespace() {
        let tokens: Vec<Token> = tokenize(" ").collect();
        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn test_tokenize_integer_and_plus_and_minus() {
        let tokens: Vec<Token> = tokenize("1 + 2 - 3").collect();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Integer(1), Position::new(1, 1)),
                Token::new(TokenKind::Plus, Position::new(3, 1)),
                Token::new(TokenKind::Integer(2), Position::new(5, 1)),
                Token::new(TokenKind::Minus, Position::new(7, 1)),
                Token::new(TokenKind::Integer(3), Position::new(9, 1)),
            ]
        );
    }

    #[test]
    fn test_tokenize_string() {
        let tokens: Vec<Token> = tokenize("\"dummy\"").collect();
        assert_eq!(
            tokens,
            vec![Token::new(
                TokenKind::String("dummy".to_string()),
                Position::new(1, 1)
            ),]
        );
    }

    #[test]
    fn test_tokenize_character() {
        let tokens: Vec<Token> = tokenize("'a'").collect();
        assert_eq!(
            tokens,
            vec![Token::new(TokenKind::Character('a'), Position::new(1, 1)),]
        );
    }

    #[test]
    fn test_tokenize_assign() {
        let tokens: Vec<Token> = tokenize("a = 1").collect();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Identifier("a".to_string()), Position::new(1, 1)),
                Token::new(TokenKind::Equal, Position::new(3, 1)),
                Token::new(TokenKind::Integer(1), Position::new(5, 1)),
            ]
        )
    }

    #[test]
    fn test_tokenize_if() {
        let tokens: Vec<Token> = tokenize("if (1) { 2 } else { 3 }").collect();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::If, Position::new(1, 1)),
                Token::new(TokenKind::ParenthesisLeft, Position::new(4, 1)),
                Token::new(TokenKind::Integer(1), Position::new(5, 1)),
                Token::new(TokenKind::ParenthesisRight, Position::new(6, 1)),
                Token::new(TokenKind::BraceLeft, Position::new(8, 1)),
                Token::new(TokenKind::Integer(2), Position::new(10, 1)),
                Token::new(TokenKind::BraceRight, Position::new(12, 1)),
                Token::new(TokenKind::Else, Position::new(14, 1)),
                Token::new(TokenKind::BraceLeft, Position::new(19, 1)),
                Token::new(TokenKind::Integer(3), Position::new(21, 1)),
                Token::new(TokenKind::BraceRight, Position::new(23, 1)),
            ]
        )
    }

    #[test]
    fn test_tokenize_while() {
        let tokens: Vec<Token> = tokenize("while (1) return 2;").collect();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::While, Position::new(1, 1)),
                Token::new(TokenKind::ParenthesisLeft, Position::new(7, 1)),
                Token::new(TokenKind::Integer(1), Position::new(8, 1)),
                Token::new(TokenKind::ParenthesisRight, Position::new(9, 1)),
                Token::new(TokenKind::Return, Position::new(11, 1)),
                Token::new(TokenKind::Integer(2), Position::new(18, 1)),
                Token::new(TokenKind::Semicolon, Position::new(19, 1)),
            ]
        )
    }

    #[test]
    fn test_tokenize_new_line() {
        let tokens: Vec<Token> = tokenize("1\n;").collect();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Integer(1), Position::new(1, 1)),
                Token::new(TokenKind::Semicolon, Position::new(1, 2)),
            ]
        );
    }
}
