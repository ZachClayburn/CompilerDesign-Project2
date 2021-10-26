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

type Item = Either<NonTerminal, Option<Token>>;
pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse(mut scan: Peekable<Scanner>) -> Result<()> {
    let mut word = match scan.next() {
        Some(Err(error)) => return Err(error.into()),
        Some(Ok(token)) => Some(token),
        None => None,
    };
    let mut stack: Vec<Item> = Vec::new();
    stack.push(Right(None)); // None is represents eof
    stack.push(Left(NonTerminal::Goal));
    let table = Table {};
    let mut focus = match stack.last() {
        Some(item) => item.clone(),
        None => todo!(),
    };
    loop {
        debug!("{:?}", stack);
        match focus {
            Right(None) if word.is_none() => return Ok(()),
            Right(terminal) if terminal == word => {
                stack.pop();
                word = match scan.next() {
                    Some(Err(error)) => return Err(error.into()),
                    Some(Ok(token)) => Some(token),
                    None => None,
                }
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
        focus = match stack.last() {
            Some(item) => item.clone(),
            None => todo!(),
        };
    }
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
}
