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
            curent_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn errors(&self) -> &[String] {
        &self.errors
    }

    fn peek_error(&mut self, tok: &Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            tok, self.peek_token
        );

        self.errors.push(msg);
    }

    fn no_prefix_parse_error(&mut self, tok: &Token) {
        let msg = format!("no prefix parse function for {:?} found", tok);

        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.curent_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();

        while !self.cur_token_is(Token::Eof) {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.curent_token {
            Token::Let => Some(ast::Statement::LetStatement(self.parse_let_statement()?)),
            Token::Return => Some(ast::Statement::ReturnStatement(
                self.parse_return_statement()?,
            )),
            _ => Some(ast::Statement::ExpressionStatement(
                self.parse_expression_statement()?,
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        if !self.expect_peek(&Token::Ident("".to_string())) {
            return None;
        }

        let name = match &self.curent_token {
            Token::Ident(name) => name.clone(),
            _ => panic!(),
        };

        if !self.expect_peek(&Token::Assign) {
            return None;
        }

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }

        let let_statement = ast::LetStatement {
            name,
            value: ast::Expression::Identifier("TEST".to_owned()),
        };

        Some(let_statement)
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        self.next_token();

        let return_statement = ast::ReturnStatement {
            return_value: ast::Expression::Identifier("TEST".to_owned()),
        };

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(return_statement)
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Expression> {
        let statement = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let left_expression = self.prefix_parse();

        let mut left_expression = match left_expression {
            Some(le) => le,
            None => {
                self.no_prefix_parse_error(&self.curent_token.clone());

                return None;
            }
        };

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            self.next_token();

            left_expression = self.infix_parse(left_expression)?;
        }

        Some(left_expression)
    }

    fn cur_token_is(&self, tok: Token) -> bool {
        match tok {
            Token::Ident(_) => {
                matches!(self.curent_token, Token::Ident(_))
            }
            Token::Int(_) => {
                matches!(self.curent_token, Token::Int(_))
            }
            _ => self.curent_token == tok,
        }
    }

    fn peek_token_is(&self, tok: &Token) -> bool {
        match tok {
            Token::Ident(_) => {
                matches!(self.peek_token, Token::Ident(_))
            }
            Token::Int(_) => {
                matches!(self.peek_token, Token::Int(_))
            }
            _ => &self.peek_token == tok,
        }
    }

    fn expect_peek(&mut self, tok: &Token) -> bool {
        if self.peek_token_is(tok) {
            self.next_token();
            true
        } else {
            self.peek_error(tok);
            false
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match self.curent_token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match self.peek_token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn prefix_parse(&mut self) -> Option<ast::Expression> {
        match &self.curent_token {
            Token::Ident(name) => {
                let identifier = ast::Expression::Identifier(name.into());

                Some(identifier)
            }
            Token::Int(int) => {
                let int_literal = ast::Expression::IntegerLiteral(*int);

                Some(int_literal)
            }
            Token::Bang | Token::Minus => {
                let prefix_expression =
                    ast::Expression::PrefixExpression(self.parse_prefix_expression()?);

                Some(prefix_expression)
            }
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::PrefixExpression> {
        let token = self.curent_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        let expression = ast::PrefixExpression::new(token, right);

        Some(expression)
    }

    fn infix_parse(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        match &self.curent_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Eq
            | Token::NotEq
            | Token::Lt
            | Token::Gt => {
                let infix_expression =
                    ast::Expression::InfixExpression(self.parse_infix_expression(left)?);

                Some(infix_expression)
            }
            _ => None,
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::InfixExpression> {
        let token = self.curent_token.clone();
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        let expression = ast::InfixExpression::new(token, left, right);

        Some(expression)
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunc(X)
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{self, token::Token},
        parser::ast,
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

        for (i, test) in tests.into_iter().enumerate() {
            let statement = &program.statements[i];
            test_let_statement(statement, test);
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
            assert_eq!(statement.token_literal(), Token::Return);

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
            _ => panic!("ExpressionStatement is not Identifier."),
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
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for test in prefix_tests {
            let lexer = lexer::Lexer::new(test.0.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_errors(&parser);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not have enough statements. got: {}",
                    program.statements.len(),
                );
            }

            let statement = match &program.statements[0] {
                ast::Statement::ExpressionStatement(s) => s,
                x => panic!("statement is not ExpressionStatement. got: {:?}", x),
            };

            let expression = match statement {
                ast::Expression::PrefixExpression(pe) => pe,
                x => panic!("ExpressionStatement is not PrefixExpression got: {:?}", x),
            };

            assert_eq!(test.1, expression.operator);
            test_integer_literal(&expression.right, test.2);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 > 5", 5, ">", 5),
            ("5 < 5", 5, "<", 5),
            ("5 == 5", 5, "==", 5),
            ("5 != 5", 5, "!=", 5),
        ];

        for test in infix_tests {
            let lexer = lexer::Lexer::new(test.0.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_errors(&parser);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not have enough statements. got: {}",
                    program.statements.len(),
                );
            }

            let statement = match &program.statements[0] {
                ast::Statement::ExpressionStatement(s) => s,
                x => panic!("statement is not ExpressionStatement. got: {:?}", x),
            };

            let expression = match statement {
                ast::Expression::InfixExpression(ie) => ie,
                x => panic!("ExpressionStatement is not InfixExpression got: {:?}", x),
            };

            test_integer_literal(&expression.left, test.1);

            assert_eq!(expression.operator, test.2);

            test_integer_literal(&expression.right, test.3);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for test in tests {
            let lexer = lexer::Lexer::new(test.0.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(&parser);

            let actual = program.to_string();

            assert_eq!(test.1, actual);
        }
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
        assert_eq!(statement.token_literal(), Token::Let);

        let let_statement = match statement {
            ast::Statement::LetStatement(ls) => ls,
            _ => panic!("statement is not LetStatement"),
        };

        assert_eq!(name, let_statement.name);
    }

    fn test_integer_literal(expression: &ast::Expression, value: isize) {
        let int = match expression {
            ast::Expression::IntegerLiteral(il) => *il,
            x => panic!("expression is not IntegerLiteral. got: {:?}", x),
        };

        assert_eq!(int, value);
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
