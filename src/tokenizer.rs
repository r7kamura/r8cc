#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Unknown,
    Plus,
    Minus,
    Equal,
    BraceLeft,
    BraceRight,
    ParenthesisLeft,
    ParenthesisRight,
    If,
    Else,
    Return,
    Integer {
        value: u32,
    },
    String {
        value: String,
    },
    Character {
        value: char,
    },
    Identifier {
        name: String,
    },
}

pub struct Tokens<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl Tokens<'_> {
    fn consume_number_literal(&mut self) -> u32 {
        let mut number = 0;
        while let Some(&character) = self.chars.peek() {
            if character.is_ascii_digit() {
                self.chars.next();
                number = number * 10 + character.to_digit(10).unwrap();
            } else {
                break;
            }
        }
        number
    }

    fn consume_character_literal(&mut self) -> char {
        self.chars.next();
        let character = self.chars.next();
        self.chars.next();
        character.unwrap()
    }

    fn consume_string_literal(&mut self) -> String {
        let mut string = String::new();
        self.chars.next();
        while let Some(character) = self.chars.next() {
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
                self.chars.next();
            } else {
                break;
            }
        }
        string
    }

    fn skip_whitespaces(&mut self) {
        while let Some(&character) = self.chars.peek() {
            if character.is_whitespace() {
                self.chars.next();
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
        if let Some(&character) = self.chars.peek() {
            match character {
                '+' => {
                    self.chars.next();
                    Some(Token::Plus)
                },
                '-' => {
                    self.chars.next();
                    Some(Token::Minus)
                },
                '=' => {
                    self.chars.next();
                    Some(Token::Equal)
                },
                '"' => {
                    Some(
                        Token::String {
                            value: self.consume_string_literal(),
                        }
                    )
                },
                '\'' => {
                    Some(
                        Token::Character {
                            value: self.consume_character_literal(),
                        }
                    )
                },
                '(' => {
                    self.chars.next();
                    Some(Token::ParenthesisLeft)
                },
                ')' => {
                    self.chars.next();
                    Some(Token::ParenthesisRight)
                },
                '{' => {
                    self.chars.next();
                    Some(Token::BraceLeft)
                },
                '}' => {
                    self.chars.next();
                    Some(Token::BraceRight)
                }
                _ => {
                    if character.is_ascii_digit() {
                        Some(
                            Token::Integer {
                                value: self.consume_number_literal(),
                            }
                        )
                    } else if character.is_ascii_alphabetic() {
                        let name = self.consume_identifier();
                        match &*name {
                            "if" => {
                                Some(Token::If)
                            },
                            "else" => {
                                Some(Token::Else)
                            },
                            "return" => {
                                Some(Token::Return)
                            },
                            _ => {
                                Some(
                                    Token::Identifier {
                                        name,
                                    }
                                )
                            },
                        }
                    } else {
                        self.chars.next();
                        Some(Token::Unknown)
                    }
                },
            }
        } else {
            None
        }
    }
}

pub fn tokenize(input: &str) -> Tokens {
    Tokens {
        chars: input.chars().peekable(),
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, tokenize};

    #[test]
    fn test_tokenize_empty_string() {
        let tokens: Vec<Token> = tokenize("").collect();
        assert_eq!(
            tokens,
            vec![]
        );
    }

    #[test]
    fn test_tokenize_whitespace() {
        let tokens: Vec<Token> = tokenize(" ").collect();
        assert_eq!(
            tokens,
            vec![]
        );
    }

    #[test]
    fn test_tokenize_integer_and_plus_and_minus() {
        let tokens: Vec<Token> = tokenize("1 + 2 - 3").collect();
        assert_eq!(
            tokens,
            vec![
                Token::Integer { value: 1 },
                Token::Plus,
                Token::Integer { value: 2 },
                Token::Minus,
                Token::Integer { value: 3 },
            ]
        );
    }

    #[test]
    fn test_tokenize_unknown_token() {
        let tokens: Vec<Token> = tokenize("1 + あ").collect();
        assert_eq!(
            tokens,
            vec![
                Token::Integer { value: 1 },
                Token::Plus,
                Token::Unknown,
            ]
        );
    }

    #[test]
    fn test_tokenize_string() {
        let tokens: Vec<Token> = tokenize("\"dummy\"").collect();
        assert_eq!(
            tokens,
            vec![
                Token::String { value: "dummy".to_string() },
            ]
        );
    }

    #[test]
    fn test_tokenize_character() {
        let tokens: Vec<Token> = tokenize("'a'").collect();
        assert_eq!(
            tokens,
            vec![
                Token::Character { value: 'a' },
            ]
        );
    }

    #[test]
    fn test_tokenize_assign() {
        let tokens: Vec<Token> = tokenize("a = 1").collect();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier { name: "a".to_string() },
                Token::Equal,
                Token::Integer { value: 1 },
            ]
        )
    }

    #[test]
    fn test_if() {
        let tokens: Vec<Token> = tokenize("if (1) { 2 } else { 3 }").collect();
        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::ParenthesisLeft,
                Token::Integer { value: 1 },
                Token::ParenthesisRight,
                Token::BraceLeft,
                Token::Integer { value: 2 },
                Token::BraceRight,
                Token::Else,
                Token::BraceLeft,
                Token::Integer { value: 3 },
                Token::BraceRight,
            ]
        )
    }
}
