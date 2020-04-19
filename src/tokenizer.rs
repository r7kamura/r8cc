use std::iter::Peekable;
use std::str::Chars;

pub fn tokenize(input: &str) -> Tokens {
    Tokens::new(input.chars().peekable())
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    BraceLeft,
    BraceRight,
    Equal,
    Minus,
    ParenthesisLeft,
    ParenthesisRight,
    Plus,
    Semicolon,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Else,
    For,
    If,
    Return,
    While,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Character(char),
    Identifier(String),
    Integer(u32),
    Keyword(Keyword),
    String(String),
    Symbol(Symbol),
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
                    token = Some(Token::new(TokenKind::Symbol(Symbol::Semicolon), position));
                }
                '+' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Symbol(Symbol::Plus), position));
                }
                '-' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Symbol(Symbol::Minus), position));
                }
                '=' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Symbol(Symbol::Equal), position));
                }
                '(' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Symbol(Symbol::ParenthesisLeft), position));
                }
                ')' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Symbol(Symbol::ParenthesisRight), position));
                }
                '{' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Symbol(Symbol::BraceLeft), position));
                }
                '}' => {
                    self.next_char();
                    token = Some(Token::new(TokenKind::Symbol(Symbol::BraceRight), position));
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
                                token = Some(Token::new(TokenKind::Keyword(Keyword::If), position));
                            }
                            "else" => {
                                token = Some(Token::new(TokenKind::Keyword(Keyword::Else), position));
                            }
                            "return" => {
                                token = Some(Token::new(TokenKind::Keyword(Keyword::Return), position));
                            }
                            "while" => {
                                token = Some(Token::new(TokenKind::Keyword(Keyword::While), position));
                            }
                            "for" => {
                                token = Some(Token::new(TokenKind::Keyword(Keyword::For), position));
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
    use super::{tokenize, Position, Token, TokenKind, Keyword, Symbol};

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
                Token::new(TokenKind::Symbol(Symbol::Plus), Position::new(3, 1)),
                Token::new(TokenKind::Integer(2), Position::new(5, 1)),
                Token::new(TokenKind::Symbol(Symbol::Minus), Position::new(7, 1)),
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
                Token::new(TokenKind::Symbol(Symbol::Equal), Position::new(3, 1)),
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
                Token::new(TokenKind::Keyword(Keyword::If), Position::new(1, 1)),
                Token::new(TokenKind::Symbol(Symbol::ParenthesisLeft), Position::new(4, 1)),
                Token::new(TokenKind::Integer(1), Position::new(5, 1)),
                Token::new(TokenKind::Symbol(Symbol::ParenthesisRight), Position::new(6, 1)),
                Token::new(TokenKind::Symbol(Symbol::BraceLeft), Position::new(8, 1)),
                Token::new(TokenKind::Integer(2), Position::new(10, 1)),
                Token::new(TokenKind::Symbol(Symbol::BraceRight), Position::new(12, 1)),
                Token::new(TokenKind::Keyword(Keyword::Else), Position::new(14, 1)),
                Token::new(TokenKind::Symbol(Symbol::BraceLeft), Position::new(19, 1)),
                Token::new(TokenKind::Integer(3), Position::new(21, 1)),
                Token::new(TokenKind::Symbol(Symbol::BraceRight), Position::new(23, 1)),
            ]
        )
    }

    #[test]
    fn test_tokenize_while() {
        let tokens: Vec<Token> = tokenize("while (1) return 2;").collect();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Keyword(Keyword::While), Position::new(1, 1)),
                Token::new(TokenKind::Symbol(Symbol::ParenthesisLeft), Position::new(7, 1)),
                Token::new(TokenKind::Integer(1), Position::new(8, 1)),
                Token::new(TokenKind::Symbol(Symbol::ParenthesisRight), Position::new(9, 1)),
                Token::new(TokenKind::Keyword(Keyword::Return), Position::new(11, 1)),
                Token::new(TokenKind::Integer(2), Position::new(18, 1)),
                Token::new(TokenKind::Symbol(Symbol::Semicolon), Position::new(19, 1)),
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
                Token::new(TokenKind::Symbol(Symbol::Semicolon), Position::new(1, 2)),
            ]
        );
    }

    #[test]
    fn test_tokenize_for() {
        let tokens: Vec<Token> = tokenize("for (i = 10; i; i = i - 1) 2;").collect();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Keyword(Keyword::For), Position::new(1, 1)),
                Token::new(TokenKind::Symbol(Symbol::ParenthesisLeft), Position::new(5, 1)),
                Token::new(TokenKind::Identifier("i".to_string()), Position::new(6, 1)),
                Token::new(TokenKind::Symbol(Symbol::Equal), Position::new(8, 1)),
                Token::new(TokenKind::Integer(10), Position::new(10, 1)),
                Token::new(TokenKind::Symbol(Symbol::Semicolon), Position::new(12, 1)),
                Token::new(TokenKind::Identifier("i".to_string()), Position::new(14, 1)),
                Token::new(TokenKind::Symbol(Symbol::Semicolon), Position::new(15, 1)),
                Token::new(TokenKind::Identifier("i".to_string()), Position::new(17, 1)),
                Token::new(TokenKind::Symbol(Symbol::Equal), Position::new(19, 1)),
                Token::new(TokenKind::Identifier("i".to_string()), Position::new(21, 1)),
                Token::new(TokenKind::Symbol(Symbol::Minus), Position::new(23, 1)),
                Token::new(TokenKind::Integer(1), Position::new(25, 1)),
                Token::new(TokenKind::Symbol(Symbol::ParenthesisRight), Position::new(26, 1)),
                Token::new(TokenKind::Integer(2), Position::new(28, 1)),
                Token::new(TokenKind::Symbol(Symbol::Semicolon), Position::new(29, 1)),
            ]
        )
    }
}
