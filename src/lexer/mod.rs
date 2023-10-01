pub mod token;

use crate::lexer::token::Token;
use std::str;

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };

        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }

    pub fn next_token(&mut self) -> token::Token {
        let tok: token::Token;

        self.skip_whitespace();

        match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    tok = token::Token::Eq;
                } else {
                    tok = token::Token::Assign;
                }
            }
            b';' => tok = token::Token::Semicolon,
            b'(' => tok = token::Token::LParen,
            b')' => tok = token::Token::RParen,
            b',' => tok = token::Token::Comma,
            b'+' => tok = token::Token::Plus,
            b'-' => tok = token::Token::Minus,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    tok = token::Token::NotEq;
                } else {
                    tok = token::Token::Bang;
                }
            }
            b'/' => tok = token::Token::Slash,
            b'*' => tok = token::Token::Asterisk,
            b'<' => tok = token::Token::Lt,
            b'>' => tok = token::Token::Gt,
            b'{' => tok = token::Token::LBrace,
            b'}' => tok = token::Token::RBrace,
            0 => tok = token::Token::Eof,
            ch if ch.is_ascii_alphabetic() => {
                tok = Token::from_literal(self.read_literal());
                return tok;
            }
            ch if ch.is_ascii_digit() => {
                tok = Token::from_number(self.read_number());
                return tok;
            }
            _ => tok = token::Token::Illegal,
        }

        self.read_char();

        tok
    }

    fn read_literal(&mut self) -> &str {
        let position = self.position;

        while self.ch.is_ascii_alphabetic() {
            self.read_char();
        }

        let literal = str::from_utf8(&self.input[position..self.position]);

        literal.unwrap()
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let number = str::from_utf8(&self.input[position..self.position]);

        number.unwrap()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::token::Token;

    use super::Lexer;

    #[test]
    fn test_next_token_1() {
        let input = String::from("=+(){},;");

        let tests = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token: Token = lexer.next_token();

            assert_eq!(test, token);
        }
    }

    #[test]
    fn test_next_token_2() {
        let input = String::from(
            "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
            x + y;
            };
            let result = add(five, ten);
            ",
        );

        let tests = vec![
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RParen,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token: Token = lexer.next_token();

            assert_eq!(test, token);
        }
    }

    #[test]
    fn test_next_token_3() {
        let input = String::from(
            "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
            x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            ",
        );

        let tests = vec![
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token: Token = lexer.next_token();

            assert_eq!(test, token);
        }
    }

    #[test]
    fn test_next_token_4() {
        let input = String::from(
            "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
            x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            ",
        );

        let tests = vec![
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token: Token = lexer.next_token();

            assert_eq!(test, token);
        }
    }

    #[test]
    fn test_next_token_5() {
        let input = String::from(
            "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
            x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            ",
        );

        let tests = vec![
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token: Token = lexer.next_token();

            assert_eq!(test, token);
        }
    }
}
