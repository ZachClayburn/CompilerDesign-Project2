use clap::{App, Arg, ArgGroup};
use compiler_design_project_2::*;
use colored::*;
use parser::parse;
use scanner::Scanner;
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
        match parse(scan) {
            Ok(result) => println!("{}: {}", "valid".green(), result),
            Err(error) => {
                eprintln!("{}: {}", "invalid".red(), error);
                std::process::exit(1);
            }
        }
    } else {
        let validation_file_name = matches.value_of("validation-file").unwrap();
        let file_contents = match std::fs::read_to_string(validation_file_name) {
            Ok(s) => s,
            Err(err) => {
                eprintln!("Could not open {}: {}", validation_file_name, err);
                std::process::exit(1);
            }
        };

        let max_width = file_contents.lines().map(|x| x.len()).max().unwrap_or(0);
        for line in file_contents.lines() {
            print!("{:width$}", line, width = max_width);
            match parse(Scanner::from_text(line)) {
                Ok(result) => println!(" {} {}", "valid".green(), result),
                Err(err) => println!(" {} {}", "invalid".red(), err),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn get_test_dir() -> std::path::PathBuf {
        [env!("CARGO_MANIFEST_DIR"), "resources", "test"]
            .iter()
            .collect()
    }

    #[test]
    fn valid_examples_are_all_valid() {
        let mut d = get_test_dir();
        d.push("ll1valid-1");
        d.set_extension("txt");
        let test_file_contents = std::fs::read_to_string(d).unwrap();
        let bad_lines = test_file_contents
            .lines()
            .filter(|x| parse(Scanner::from_text(x)).is_err())
            .collect::<Vec<_>>();
        let expected: Vec<&str> = vec![];
        assert_eq!(bad_lines, expected);
    }

    #[test]
    fn invalid_examples_are_all_invalid() {
        let mut d = get_test_dir();
        d.push("ll1invalid-1");
        d.set_extension("txt");
        let test_file_contents = std::fs::read_to_string(d).unwrap();
        let bad_lines = test_file_contents
            .lines()
            .filter(|x| parse(Scanner::from_text(x)).is_ok())
            .collect::<Vec<_>>();
        let expected: Vec<&str> = vec![];
        assert_eq!(bad_lines, expected);
    }
}
