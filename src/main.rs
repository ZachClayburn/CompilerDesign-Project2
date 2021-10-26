mod scanner;
mod parser;

use scanner::Scanner;
use parser::parse;

fn main() {
    println!("Hello, world!");
    let scan = Scanner::from_text("1 + 1");
    parse(scan).unwrap();
}
