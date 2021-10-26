mod table;

use std::iter::Peekable;

use either::{Either, Either::Left, Either::Right};
use log::debug;

use crate::scanner::*;
use table::Table;

#[derive(Debug, PartialEq, Clone)]
enum NonTerminal {
    Goal,
    Expr,
    ExprPrime,
    Term,
    TermPrime,
    Factor,
}

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

type Item = Either<NonTerminal, Token>;
pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse(scan: Peekable<Scanner>) -> Result<()> {
    let mut stack: Vec<Item> = Vec::new();
    stack.push(Right(Token::EOF));
    stack.push(Left(NonTerminal::Goal));
    let table = Table {};

    'outer: for item in scan {
        let word = item?;
        loop {
            debug!("\nStack:{:?}\nWord: {:?}", stack, word);
            match stack.last().unwrap() {
                Right(Token::EOF) if word == Token::EOF => return Ok(()),
                Right(terminal) if terminal == &word => {
                    stack.pop();
                    continue 'outer;
                }
                Right(bad_terminal) => {
                    return Err(
                        format!("Error: Expected {:?}, but found {:?}", bad_terminal, word).into(),
                    )
                }
                Left(non_terminal) => {
                    if let Some(mut new_items) = table.at(&non_terminal, &word) {
                        stack.pop();
                        stack.append(&mut new_items);
                    } else {
                        return Err(
                            format!("Error expanding {:?} with {:?}", non_terminal, word).into(),
                        );
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
        let out = parse(scan);
        assert!(out.is_ok());
    }
}
