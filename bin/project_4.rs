use clap::{App, Arg};
use codegen::{generate_assembly, CodeGenError};
use compiler_design::*;
use evaluation::{evaluate, EvaluationError};
use parser::ast::CompilationUnit;
use parser::parse;
use scanner::Scanner;
use simple_logger::SimpleLogger;
use std::{env, fs};

#[cfg(not(tarpaulin_include))]
fn main() {
    use std::fs;

    let matches = App::new(env!("CARGO_BIN_NAME"))
        .version("1.0")
        .author("Zach Clayburn <zachclayburn@gmail.com>")
        .arg(
            Arg::new("source-file")
                .required(true)
                .takes_value(true)
                .about("Attempt to parse each line in <source-file>. Reports if expression is valid, and prints parsing result.")
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

    let validation_file_name = matches.value_of("source-file").unwrap();
    let scan = match Scanner::from_file(validation_file_name) {
        Ok(scan) => scan,
        Err(err) => {
            println!("Error opening file {}:{}", validation_file_name, err);
            std::process::exit(1);
        }
    };
    let CompilationUnit { statements, name } = match parse::<_, CompilationUnit>(scan) {
        Ok(compilation_unit) => compilation_unit,
        Err(err) => {
            println!("Error parsing file: {}", err);
            std::process::exit(1);
        }
    };

    println!("Errors encountered during compilation:");
    let statements = evaluate(statements)
        .into_iter()
        .filter(|stmnt| {
            if let Err(EvaluationError { error_msg }) = stmnt {
                println!("{}", error_msg);
                false
            } else {
                true
            }
        })
        .map(|stmnt| stmnt.unwrap())
        .collect::<Vec<_>>();

    let code = match generate_assembly(statements) {
        Ok(code) => code,
        Err(CodeGenError { error_msg }) => {
            eprintln!("Failed to compile: {}", error_msg);
            std::process::exit(1);
        }
    };

    let mut out = match env::current_dir() {
        Ok(dir) => dir,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };
    out.push(name);
    println!("hello!");

    if let Err(err) = fs::write(out.with_extension("asm"), code) {
        eprintln!("Could not write to output: {}", err);
    }
}
