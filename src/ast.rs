use std::fmt::Display;

use crate::token::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Program(p) => p.fmt(f),
            Node::Statement(s) => s.fmt(f),
            Node::Expression(e) => e.fmt(f),
        }
    }
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

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }

        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(Expression),
}

impl Statement {
    pub fn token_literal(&self) -> Token {
        match self {
            Statement::LetStatement(_) => Token::LET,
            Statement::ReturnStatement(_) => Token::RETURN,
            Statement::ExpressionStatement(es) => es.token_literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(ls) => ls.fmt(f),
            Statement::ReturnStatement(rs) => rs.fmt(f),
            Statement::ExpressionStatement(es) => es.fmt(f),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Identifier(String),
}

impl Expression {
    pub fn token_literal(&self) -> Token {
        match self {
            Expression::Identifier(s) => Token::IDENT(s.to_string()),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(s) => s.fmt(f),
        }
    }
}
