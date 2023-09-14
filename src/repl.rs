use std::io::Write;

use crate::lexer::{token::Token, Lexer};

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let mut scanned = String::new();
        std::io::stdin().read_line(&mut scanned).unwrap();

        let mut lexer = Lexer::new(scanned);

        let mut tok = lexer.next_token();
        while tok != Token::EOF {
            print!("{:?}\n", tok);
            tok = lexer.next_token();
        }
    }
}
