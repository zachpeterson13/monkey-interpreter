use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum Token {
    // Special
    ILLEGAL,
    EOF,
    
    // Identifiers + literals
    IDENT(String),
    INT(isize),
    
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOTEQ,

    // Delimiters
    COMMA,
    SEMICOLON,

    
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Token {
    pub fn from_literal(literal: &str) -> Token {
        match literal {
           "let" => Token::LET,
           "fn" => Token::FUNCTION,
           "true" => Token::TRUE,
           "false" => Token::FALSE,
           "if" => Token::IF,
           "else" => Token::ELSE,
           "return" => Token::RETURN,
           _ => Token::IDENT(literal.into()),
        }
    }

    pub fn from_number(number: &str) -> Token {
        let number = isize::from_str(number).unwrap();
        Token::INT(number)
    }
}


