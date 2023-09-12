use std::fmt::Display;

use crate::token::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    LetStatement(LetStatement),
}

impl Statement {
    pub fn token_literal(&self) -> Token {
        match self {
            Statement::LetStatement(_) => Token::LET,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Identifier(String),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        return Program { statements: vec![] };
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression
}
