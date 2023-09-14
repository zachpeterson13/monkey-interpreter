pub mod ast;

use crate::lexer::{token::Token, Lexer};

struct Parser {
    lexer: Lexer,

    curent_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curent_token: Token::ILLEGAL,
            peek_token: Token::ILLEGAL,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        return parser;
    }

    fn errors(&self) -> &[String] {
        return &self.errors;
    }

    fn peek_error(&mut self, tok: Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            tok, self.peek_token
        );

        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.curent_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();

        while !self.cur_token_is(Token::EOF) {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }

        return program;
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.curent_token {
            Token::LET => Some(ast::Statement::LetStatement(self.parse_let_statement()?)),
            Token::RETURN => Some(ast::Statement::ReturnStatement(self.parse_return_statement()?)),
            _ => Some(ast::Statement::ExpressionStatement(self.parse_expression_statement()?))
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        if !self.expect_peek(Token::IDENT("".to_string())) {
            return None;
        }

        let name = match &self.curent_token {
            Token::IDENT(name) => name.clone(),
            _ => panic!(),
        };

        if !self.expect_peek(Token::ASSIGN) {
            return None;
        }

        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        let let_statement = ast::LetStatement {
            name,
            value: ast::Expression::Identifier("TEST".to_owned()),
        };

        return Some(let_statement);
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        self.next_token();

        let return_statement = ast::ReturnStatement {
            return_value: ast::Expression::Identifier("TEST".to_owned()),
        };

        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        return Some(return_statement);
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Expression> {
        let statement = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        return Some(statement);
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let left_expression = self.prefix_parse()?;

        return Some(left_expression);
    }

    fn cur_token_is(&self, tok: Token) -> bool {
        match tok {
            Token::IDENT(_) => {
                return matches!(self.curent_token, Token::IDENT(_));
            }
            Token::INT(_) => {
                return matches!(self.curent_token, Token::INT(_));
            }
            _ => {
                return self.curent_token == tok;
            }
        }
    }

    fn peek_token_is(&self, tok: Token) -> bool {
        match tok {
            Token::IDENT(_) => {
                return matches!(self.peek_token, Token::IDENT(_));
            }
            Token::INT(_) => {
                return matches!(self.peek_token, Token::INT(_));
            }
            _ => {
                return self.peek_token == tok;
            }
        }
    }

    fn expect_peek(&mut self, tok: Token) -> bool {
        if self.peek_token_is(tok.clone()) {
            self.next_token();
            return true;
        } else {
            self.peek_error(tok.clone());
            return false;
        }
    }

    fn prefix_parse(&mut self) -> Option<ast::Expression> {
        match &self.curent_token {
            Token::IDENT(name) => {
                let identifier = ast::Expression::Identifier(name.into());

                return Some(identifier);
            },
            Token::INT(int) => {
                let int_literal = ast::Expression::IntegerLiteral(*int);

                return Some(int_literal);
            },
            _ => {
                return None;
            },
        }
    }

    fn infix_parse(&mut self, left: &ast::Expression) -> Option<ast::Expression> {
        None
    }
}

enum Precedence {
    LOWEST,
    EQUALS, // ==
    LESSGREATER, // > or <
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunc(X)
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::ast,
        lexer::{token::Token, self},
    };

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

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statments. got {}",
                program.statements.len()
            );
        }

        let tests = vec!["x", "y", "foobar"];

        let mut i = 0;
        for test in tests {
            let statement = &program.statements[i];
            test_let_statement(statement, test);
            i += 1;
        }
    }

    #[test]
    fn test_return_statements() {
        let input = String::from(
            "
        return 5;
        return 10;
        return 993322;
        ",
        );

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statments. got {}",
                program.statements.len()
            );
        }

        for statement in program.statements {
            assert_eq!(statement.token_literal(), Token::RETURN);

            let _return_statement = match statement {
                ast::Statement::ReturnStatement(rs) => rs,
                _ => panic!("statement is not ReturnStatement"),
            };
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("foobar;");

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
            "program does not heave enough statements. got: {}",
            program.statements.len(),
            )
        }

        let statement = match &program.statements[0] {
            ast::Statement::ExpressionStatement(s) => s,
            x => panic!("statement is not ExpressionStatement. got: {}", x),
        };

        let identifier = match statement {
            ast::Expression::Identifier(i) => i,
            _ => panic!("ExpressionStatement is not Identifier.")
        };

        assert_eq!(identifier, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5;");

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
            "program does not have enough statements. got: {}",
            program.statements.len(),
            );
        }

        let statement = match &program.statements[0] {
            ast::Statement::ExpressionStatement(s) => s,
            x => panic!("statement is not ExpressionStatement. got: {:?}", x),
        };

        let int_literal = match statement {
            ast::Expression::IntegerLiteral(il) => *il,
            x => panic!("ExpressionStatement is not IntegerLiteral got: {:?}", x),
        };

        assert_eq!(5, int_literal);
    }

    #[test]
    fn test_display() {
        let s = ast::Statement::LetStatement(ast::LetStatement {
            name: "myVar".to_owned(),
            value: ast::Expression::Identifier("anotherVar".to_owned()),
        });

        let program = ast::Program {
            statements: vec![s.clone()],
        };

        assert_eq!("let myVar = anotherVar;", program.to_string());
    }

    fn test_let_statement(statement: &ast::Statement, name: &str) {
        assert_eq!(statement.token_literal(), Token::LET);

        let let_statement = match statement {
            ast::Statement::LetStatement(ls) => ls,
            _ => panic!("statement is not LetStatement"),
        };

        assert_eq!(name, let_statement.name);
    }

    fn check_parse_errors(parser: &Parser) {
        let errors = parser.errors();

        if errors.is_empty() {
            return;
        }

        println!("parser has {} errors", errors.len());

        for msg in errors {
            println!("parser error: {}", msg);
        }

        panic!();
    }
}
