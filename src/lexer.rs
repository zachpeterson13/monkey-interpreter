use crate::token::{self, Token};
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

        return lexer;
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
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    pub fn next_token(&mut self) -> token::Token {
        let tok: token::Token;

        self.skip_whitespace();

        match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    tok = token::Token::EQ;
                } else {
                    tok = token::Token::ASSIGN;
                }
            }
            b';' => tok = token::Token::SEMICOLON,
            b'(' => tok = token::Token::LPAREN,
            b')' => tok = token::Token::RPAREN,
            b',' => tok = token::Token::COMMA,
            b'+' => tok = token::Token::PLUS,
            b'-' => tok = token::Token::MINUS,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    tok = token::Token::NOTEQ;
                } else {
                    tok = token::Token::BANG;
                }
            }
            b'/' => tok = token::Token::SLASH,
            b'*' => tok = token::Token::ASTERISK,
            b'<' => tok = token::Token::LT,
            b'>' => tok = token::Token::GT,
            b'{' => tok = token::Token::LBRACE,
            b'}' => tok = token::Token::RBRACE,
            0 => tok = token::Token::EOF,
            ch if ch.is_ascii_alphabetic() => {
                tok = Token::from_literal(self.read_literal());
                return tok;
            }
            ch if ch.is_ascii_digit() => {
                tok = Token::from_number(self.read_number());
                return tok;
            }
            _ => tok = token::Token::ILLEGAL,
        }

        self.read_char();

        return tok;
    }

    fn read_literal(&mut self) -> &str {
        let position = self.position;

        while self.ch.is_ascii_alphabetic() {
            self.read_char();
        }

        let literal = str::from_utf8(&self.input[position..self.position]);

        return literal.unwrap();
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let number = str::from_utf8(&self.input[position..self.position]);

        return number.unwrap();
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn test_next_token_1() {
        let input = String::from("=+(){},;");

        let tests = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::EOF,
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
            Token::LET,
            Token::IDENT("five".into()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".into()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".into()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".into()),
            Token::COMMA,
            Token::IDENT("y".into()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".into()),
            Token::PLUS,
            Token::IDENT("y".into()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".into()),
            Token::ASSIGN,
            Token::IDENT("add".into()),
            Token::LPAREN,
            Token::IDENT("five".into()),
            Token::COMMA,
            Token::IDENT("ten".into()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::EOF,
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
            Token::LET,
            Token::IDENT("five".into()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".into()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".into()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".into()),
            Token::COMMA,
            Token::IDENT("y".into()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".into()),
            Token::PLUS,
            Token::IDENT("y".into()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".into()),
            Token::ASSIGN,
            Token::IDENT("add".into()),
            Token::LPAREN,
            Token::IDENT("five".into()),
            Token::COMMA,
            Token::IDENT("ten".into()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::EOF,
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
            Token::LET,
            Token::IDENT("five".into()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".into()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".into()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".into()),
            Token::COMMA,
            Token::IDENT("y".into()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".into()),
            Token::PLUS,
            Token::IDENT("y".into()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".into()),
            Token::ASSIGN,
            Token::IDENT("add".into()),
            Token::LPAREN,
            Token::IDENT("five".into()),
            Token::COMMA,
            Token::IDENT("ten".into()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::EOF,
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
            Token::LET,
            Token::IDENT("five".into()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".into()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".into()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".into()),
            Token::COMMA,
            Token::IDENT("y".into()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".into()),
            Token::PLUS,
            Token::IDENT("y".into()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".into()),
            Token::ASSIGN,
            Token::IDENT("add".into()),
            Token::LPAREN,
            Token::IDENT("five".into()),
            Token::COMMA,
            Token::IDENT("ten".into()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOTEQ,
            Token::INT(9),
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token: Token = lexer.next_token();

            assert_eq!(test, token);
        }
    }
}
