use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Special
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(isize),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn from_literal(literal: &str) -> Token {
        match literal {
            "let" => Token::Let,
            "fn" => Token::Function,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(literal.into()),
        }
    }

    pub fn from_number(number: &str) -> Token {
        let number = isize::from_str(number).unwrap();
        Token::Int(number)
    }
}
