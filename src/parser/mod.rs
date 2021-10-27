mod table;

use std::iter::Peekable;
use std::mem::discriminant;

use either::{Either, Either::Left, Either::Right};
use log::debug;

use crate::scanner::*;
use table::{NonTerminal, ProductionItem, Table};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    message: String,
}

impl From<&str> for ParseError {
    fn from(message: &str) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl From<String> for ParseError {
    fn from(message: String) -> Self {
        Self { message }
    }
}

impl From<ScannerError> for ParseError {
    fn from(error: ScannerError) -> Self {
        Self {
            message: format!(
                "[{}:{}] {}",
                error.location.line, error.location.column, error.message
            ),
        }
    }
}

pub fn parse(scan: Peekable<Scanner>) -> Result<()> {
    let mut production_stack: Vec<ProductionItem> = Vec::new();
    production_stack.push(Right(Token::EOF));
    production_stack.push(Left(NonTerminal::Goal));
    let table = Table {};

    'outer: for item in scan {
        let word = &item?;
        loop {
            debug!("\nStack:{:?}\nWord: {}", production_stack, word);
            match production_stack.last().unwrap() {
                Right(Token::EOF) if word == &Token::EOF => return Ok(()),
                Right(terminal) if discriminant(terminal) == discriminant(word) => {
                    production_stack.pop();
                    continue 'outer;
                }
                Right(bad_terminal) => {
                    return Err(
                        format!("Error: Expected {}, but found {}", bad_terminal, word).into(),
                    )
                }
                Left(non_terminal) => {
                    if let Some(mut new_items) = table.at(non_terminal, word) {
                        production_stack.pop();
                        production_stack.append(&mut new_items);
                    } else {
                        return Err(format!(
                            "[{}] Unexpected toke {}",
                            word.format_location(),
                            word
                        )
                        .into());
                    }
                }
            }
        }
    }
    unreachable!("Should not make it out of the loop!");
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_errors_on_bad_first_character() {
        let scan = Scanner::from_text("~");
        let out = parse(scan);
        assert_eq!(
            out,
            Err(ParseError {
                message: "[1:1] Unexpected token ~, (0x7E)".into()
            })
        )
    }

    #[test]
    fn single_number_does_not_error() {
        let scan = Scanner::from_text("1");
        assert!(parse(scan).is_ok());
    }

    #[test]
    fn single_operation_does_not_error() {
        let scan = Scanner::from_text("1+1");
        assert!(parse(scan).is_ok());
    }

    #[test]
    fn operation_chain_does_not_error() {
        let scan = Scanner::from_text("Hello + world * 3");
        assert!(parse(scan).is_ok());
    }

    #[test]
    fn parenthetical_number_does_not_error() {
        let scan = Scanner::from_text("(1)");
        assert!(parse(scan).is_ok());
    }

    #[test]
    fn operation_chain_with_parenthasese_does_not_error() {
        let scan = Scanner::from_text("1+(1-1)*1");
        assert!(parse(scan).is_ok())
    }

    #[test]
    fn sequential_numbers_error() {
        let scan = Scanner::from_text("1 1");
        assert!(parse(scan).is_err());
    }

    #[test]
    fn trailing_operator_errors() {
        let scan = Scanner::from_text("1 + 1 -");
        assert!(parse(scan).is_err());
    }
}
