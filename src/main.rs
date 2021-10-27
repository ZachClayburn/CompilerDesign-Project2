mod parser;
mod scanner;

use clap::{App, Arg};
use parser::parse;
use scanner::Scanner;
use simple_logger::SimpleLogger;

fn main() {
    let matches = App::new(env!("CARGO_BIN_NAME"))
        .version("1.0")
        .author("Zach Clayburn <zachclayburn@gmail.com>")
        .arg(
            Arg::new("expression")
                .about("The expression to be evaluated")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .multiple_occurrences(true)
                .about("Turn on debug information, repeat for more verbose output"),
        )
        .get_matches();

    let exp = matches.value_of("expression").unwrap();

    let level = match matches.occurrences_of("debug") {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        2 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };
    SimpleLogger::new().with_level(level).init().unwrap();

    let scan = Scanner::from_text(exp);
    match parse(scan) {
        Ok(result) => println!("{}", result),
        Err(error) => {
            eprintln!("{}", error);
            std::process::exit(1);
        }
    }
}
