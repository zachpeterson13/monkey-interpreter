pub mod lexer;
pub mod parser;
pub mod repl;

fn main() {
    println!("This is the Monkey programming language!");
    println!("Feel free to type in commands");

    repl::start();
}
