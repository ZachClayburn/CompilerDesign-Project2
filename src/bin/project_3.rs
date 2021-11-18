use clap::{App, Arg, ArgGroup};
use colored::*;
use compiler_design::*;
use scanner::Scanner;
use parser::parse;
use parser::ast::CompilationUnit;
use simple_logger::SimpleLogger;

#[cfg(not(tarpaulin_include))]
fn main() {
    let matches = App::new(env!("CARGO_BIN_NAME"))
        .version("1.0")
        .author("Zach Clayburn <zachclayburn@gmail.com>")
        .group(ArgGroup::new("input").required(true))
        .arg(
            Arg::new("expression")
                .short('e')
                .long("--expression")
                .group("input")
                .takes_value(true)
                .about("The expression to be evaluated"),
        )
        .arg(
            Arg::new("validation-file")
                .short('v')
                .long("--validation-file")
                .group("input")
                .takes_value(true)
                .value_names(&["multiline-expression-file"])
                .about("Attempt to parse each line in <multiline-expression-file>. Reports if expression is valid, and prints parsing result.")
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .multiple_occurrences(true)
                .about("Turn on debug information, repeat for more verbose output"),
        )
        .about("Attempts to parse a mathmatical expression. Reports if it is valid, and prints the result if it is.")
        .get_matches();

    let level = match matches.occurrences_of("debug") {
        0 => log::LevelFilter::Error,
        1 => log::LevelFilter::Warn,
        2 => log::LevelFilter::Info,
        3 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };
    SimpleLogger::new().with_level(level).init().unwrap();

    if let Some(expression) = matches.value_of("expression") {
        let scan = Scanner::from_text(expression);
        match parse::<_, CompilationUnit>(scan) {
            Ok(result) => println!("{}: {}", "valid".green(), result),
            Err(error) => {
                eprintln!("{}: {}", "invalid".red(), error);
                std::process::exit(1);
            }
        }
    } else {
        panic!("Not a valid option, need to fix it");
    }
}
