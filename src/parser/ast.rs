use std::fmt::Display;

use crate::lexer::token::Token;

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
    IntegerLiteral(isize),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

impl Expression {
    pub fn token_literal(&self) -> Token {
        match self {
            Expression::Identifier(s) => Token::IDENT(s.to_string()),
            Expression::IntegerLiteral(i) => Token::INT(*i),
            Expression::PrefixExpression(pe) => pe.token.clone(),
            Expression::InfixExpression(ie) => ie.token.clone(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(s) => s.fmt(f),
            Expression::IntegerLiteral(i) => i.fmt(f),
            Expression::PrefixExpression(pe) => pe.fmt(f),
            Expression::InfixExpression(ie) => ie.fmt(f),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, right: Expression) -> PrefixExpression {
        let operator = match token {
            Token::BANG => String::from("!"),
            Token::MINUS => String::from("-"),
            _ => panic!(),
        };

        let right = Box::new(right);

        return PrefixExpression {
            token,
            operator,
            right,
        };
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(token: Token, left: Expression, right: Expression) -> InfixExpression {
        let operator = match token {
            Token::PLUS => String::from("+"),
            Token::MINUS => String::from("-"),
            Token::ASTERISK => String::from("*"),
            Token::SLASH => String::from("/"),
            Token::LT => String::from("<"),
            Token::GT => String::from(">"),
            Token::EQ => String::from("=="),
            Token::NOTEQ => String::from("!="),
            _ => panic!(),
        };

        let left = Box::new(left);
        let right = Box::new(right);

        return InfixExpression {
            token,
            left,
            operator,
            right,
        };
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}
