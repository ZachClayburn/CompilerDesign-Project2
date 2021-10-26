mod scanner;
mod parser;

use simple_logger::SimpleLogger;

use scanner::Scanner;
use parser::parse;

fn main() {
    SimpleLogger::new().init().unwrap();

    let scan = Scanner::from_text("1 + 1");
    parse(scan).unwrap();
}
