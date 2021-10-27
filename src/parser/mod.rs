mod ast;
mod table;

use crate::scanner::*;
use ast::{ExpressionIr, ValueItem};
use either::Either::{self, Left, Right};
use log::debug;
use std::fmt::Display;
use std::num::ParseIntError;
use std::{iter::Peekable, mem::discriminant};
use table::{NonTerminal, Table};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
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

impl From<ParseIntError> for ParseError {
    fn from(_: ParseIntError) -> Self {
        todo!()
    }
}

pub fn parse(scan: Peekable<Scanner>) -> Result<ExpressionIr> {
    let mut production_stack = vec![Right(Token::EOF), Left(NonTerminal::Goal)];
    let mut value_stack: Vec<ValueItem> = Vec::new();
    let table = Table {};

    'outer: for item in scan {
        let word = &item?;
        loop {
            debug!(
                "\nStack:{:?}\nValueStack:{:?}\nWord: {}",
                production_stack, value_stack, word
            );
            match production_stack.last().unwrap() {
                Right(Token::EOF) if word == &Token::EOF => match value_stack.len() {
                    1 => {
                        if let Left(final_value) = value_stack.pop().unwrap() {
                            return Ok(final_value);
                        } else {
                            return Err("Error extracting final value".into());
                        }
                    }
                    x => {
                        return Err(format!(
                            "Expected a single value left in the value stack, but found {}",
                            x
                        )
                        .into())
                    }
                },
                Right(terminal) if discriminant(terminal) == discriminant(word) => {
                    let token = production_stack.pop().unwrap().unwrap_right();
                    value_stack.push(Right(token));
                    continue 'outer;
                }
                Right(bad_terminal) => {
                    return Err(
                        format!("Error: Expected {}, but found {}", bad_terminal, word).into(),
                    )
                }
                Left(non_terminal) => {
                    if let &NonTerminal::Reduction(op) = non_terminal {
                        value_stack = op(value_stack)?;
                        production_stack.pop();
                    } else if let Some(new_items) = table.at(non_terminal, word) {
                        production_stack.pop();
                        for new_item in new_items.into_iter().rev() {
                            production_stack.push(new_item);
                        }
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
    use ast::*;
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

    #[ignore]
    #[test]
    fn parenthetical_number_does_not_error() {
        let scan = Scanner::from_text("(1)");
        assert!(parse(scan).is_ok());
    }

    #[ignore]
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

    #[test]
    fn single_number_parses_correctly() {
        let scan = Scanner::from_text("1");
        let out = parse(scan).unwrap();
        let expected = ExpressionIr::NumberLiteral(1);
        assert_eq!(out, expected);
    }

    #[test]
    fn single_variable_parses_correctly() {
        let scan = Scanner::from_text("a");
        let out = parse(scan).unwrap();
        let expected = ExpressionIr::Variable("a".into());
        assert_eq!(out, expected);
    }

    #[test]
    fn single_addition_parses_correctly() {
        use ExpressionIr::*;
        let scan = Scanner::from_text("a+b");
        let out = parse(scan).unwrap();
        let expected = BinaryOperation(
            Box::new(Variable("a".into())),
            BinaryOperator::Plus,
            Box::new(Variable("b".into())),
        );
        assert_eq!(out, expected);
    }

    #[test]
    fn single_subtraction_parses_correctly() {
        use ExpressionIr::*;
        let scan = Scanner::from_text("a-b");
        let out = parse(scan).unwrap();
        let expected = BinaryOperation(
            Box::new(Variable("a".into())),
            BinaryOperator::Minus,
            Box::new(Variable("b".into())),
        );
        assert_eq!(out, expected);
    }

    #[test]
    fn single_multiplication_parses_correctly() {
        use ExpressionIr::*;
        let scan = Scanner::from_text("a*b");
        let out = parse(scan).unwrap();
        let expected = BinaryOperation(
            Box::new(Variable("a".into())),
            BinaryOperator::Multiply,
            Box::new(Variable("b".into())),
        );
        assert_eq!(out, expected);
    }

    #[test]
    fn single_division_parses_correctly() {
        use ExpressionIr::*;
        let scan = Scanner::from_text("a/b");
        let out = parse(scan).unwrap();
        let expected = BinaryOperation(
            Box::new(Variable("a".into())),
            BinaryOperator::Divide,
            Box::new(Variable("b".into())),
        );
        assert_eq!(out, expected);
    }

    #[test]
    fn operation_chain_parses_correctly() {
        use ExpressionIr::*;
        let scan = Scanner::from_text("Hello + world * 3");
        let out = parse(scan).unwrap();
        let expected = BinaryOperation(
            Box::new(Variable("Hello".into())),
            BinaryOperator::Plus,
            Box::new(BinaryOperation(
                Box::new(Variable("world".into())),
                BinaryOperator::Multiply,
                Box::new(NumberLiteral(3)),
            )),
        );
        assert_eq!(out, expected);
    }
}
