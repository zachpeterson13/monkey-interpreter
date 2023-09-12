use crate::{ast::{self, Program, Statement, LetStatement, Expression}, lexer::Lexer, token::Token};

struct Parser<'a> {
    lexer: &'a mut Lexer,

    curent_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    fn new(lexer: &mut Lexer) -> Parser {
        let mut parser = Parser { 
            lexer,
            curent_token: Token::ILLEGAL,
            peek_token: Token::ILLEGAL,

        };

        parser.next_token();
        parser.next_token();

        return parser;
    }

    fn next_token(&mut self) {
        self.curent_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Option<ast::Program> {
        let mut program = Program::new();

        while self.curent_token != Token::EOF {
            let statement = self.parse_statement()?;
            program.statements.push(statement);
            self.next_token();
        }

        return Some(program);
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curent_token {
            Token::LET => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if let Token::IDENT(_) = self.peek_token {
            self.next_token();
        } else {
            return None;
        }
        
        let name = match &self.curent_token {
            Token::IDENT(name) => name.clone(),
            _ => panic!(),
        };

        if !self.peek_token_is(Token::ASSIGN) {
            return None;
        }

        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        let statement = LetStatement {
            name,
            value: Expression::Identifier("TEST".to_owned()),
        };

        return Some(Statement::LetStatement(statement));
    }

    fn cur_token_is(&self, tok: Token) -> bool {
        return self.curent_token == tok;
    }

    fn peek_token_is(&self, tok: Token) -> bool {
        return self.peek_token == tok;
    }

    fn expect_peek(&mut self, tok: Token) -> bool {
        if self.peek_token_is(tok) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::{self, LetStatement, Statement}, lexer, token::Token};

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = String::from(
        "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ",
        );

        let mut lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().expect("parse_program returned none");

        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statments. got {}", program.statements.len());
        }

        let tests = vec![
            "x",
            "y",
            "foobar",
        ];

        let mut i = 0;
        for test in tests {
            let statement = &program.statements[i];
            test_let_statement(statement, test);
            i += 1;
        }
    }

    fn test_let_statement(statement: &Statement, name: &str) {
        assert_eq!(statement.token_literal(), Token::LET);

        let let_statement = match statement {
            Statement::LetStatement(ls) => ls,
            _ => panic!("statement is not LetStatement"),
        };

        assert_eq!(name, let_statement.name);
    }
}
