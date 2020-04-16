#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Unknown,
    Plus,
    Minus,
    Integer {
        value: u32,
    },
    String {
        value: String,
    },
    Character {
        value: char,
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
                    return Some(Token::Plus);
                },
                '-' => {
                    self.chars.next();
                    return Some(Token::Minus);
                },
                '"' => {
                    return Some(
                        Token::String {
                            value: self.consume_string_literal(),
                        }
                    );
                },
                '\'' => {
                    return Some(
                        Token::Character {
                            value: self.consume_character_literal(),
                        }
                    );
                },
                _ => {
                    if character.is_ascii_digit() {
                        return Some(
                            Token::Integer {
                                value: self.consume_number_literal(),
                            }
                        );
                    } else {
                        self.chars.next();
                        return Some(Token::Unknown);
                    }
                },
            }
        }
        None
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
    fn test_empty_string() {
        let tokens: Vec<Token> = tokenize("").collect();
        assert_eq!(
            tokens,
            vec![]
        );
    }

    #[test]
    fn test_whitespace() {
        let tokens: Vec<Token> = tokenize(" ").collect();
        assert_eq!(
            tokens,
            vec![]
        );
    }

    #[test]
    fn test_integer_and_plus_and_minus() {
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
    fn test_unknown_token() {
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
    fn test_string() {
        let tokens: Vec<Token> = tokenize("\"dummy\"").collect();
        assert_eq!(
            tokens,
            vec![
                Token::String { value: "dummy".to_string() },
            ]
        );
    }

    #[test]
    fn test_character() {
        let tokens: Vec<Token> = tokenize("'a'").collect();
        assert_eq!(
            tokens,
            vec![
                Token::Character { value: 'a' },
            ]
        );
    }
}
