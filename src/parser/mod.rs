use std::iter::Peekable;

use crate::scanner::*;
use either::{Either, Either::Left, Either::Right};

#[derive(Debug, PartialEq, Clone)]
enum NonTerminal {
    Goal,
    Expr,
    ExperPrime,
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
    let mut word: Item = Right(match scan.next() {
        Some(Err(error)) => return Err(error.into()),
        Some(Ok(token)) => Some(token),
        None => None,
    });
    let mut stack: Vec<Item> = Vec::new();
    stack.push(Right(None)); // None is represents eof
    stack.push(Left(NonTerminal::Goal));
    let focus = match stack.last() {
        Some(item) => item.clone(),
        None => todo!(),
    };
    loop {
        if let (Right(None), Right(None)) = (&focus, &word) {
            return Ok(());
        } else if focus.is_left() || focus.as_ref().map_right(|x| x.is_none()).right_or(false) {
            if focus == word {
                stack.pop();
                word = Right(match scan.next() {
                    Some(Err(error)) => return Err(error.into()),
                    Some(Ok(token)) => Some(token),
                    None => None,
                })
            } else {
                return Err(format!("Expected {:?}, but found {:?}", focus, word).into());
            }
        } else {
            // TODO
        }
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
