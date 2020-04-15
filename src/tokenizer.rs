#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Plus,
    Minus,
    Integer {
        value: u32,
    }
}

pub struct Tokens<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl Tokens<'_> {
    fn consume_number(&mut self) -> u32 {
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
        let mut result = None;
        if let Some(&character) = self.chars.peek() {
            if character.is_ascii_digit() {
                result = Some(
                    Token::Integer {
                        value: self.consume_number()
                    }
                );
            } else if character == '+' {
                self.chars.next();
                result = Some(Token::Plus);
            } else if character == '-' {
                self.chars.next();
                result = Some(Token::Minus);
            } else {
                panic!("Unexpected character: `{}`", self.chars.next().unwrap());
            }
        }
        result
    }
}

pub fn tokenize(input: &str) -> Tokens {
    Tokens {
        chars: input.chars().peekable(),
    }
}

#[cfg(test)]
mod tests {
    use super::Token;

    #[test]
    fn test_empty_string() {
        let tokens: Vec<Token> = super::tokenize("").collect();
        assert_eq!(
            tokens,
            vec![]
        );
    }

    #[test]
    fn test_whitespace() {
        let tokens: Vec<Token> = super::tokenize(" ").collect();
        assert_eq!(
            tokens,
            vec![]
        );
    }

    #[test]
    fn test_integer_and_plus_and_minus() {
        let tokens: Vec<Token> = super::tokenize("1 + 2 - 3").collect();
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
}
