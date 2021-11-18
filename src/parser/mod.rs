pub mod ast;
pub mod errors;
mod table;

use crate::scanner::*;
use ast::{ValueItem, AST};
use either::Either::{self, Left, Right};
pub use errors::ParseError;
use log::debug;
use std::{convert::TryFrom, iter::Peekable, mem::discriminant};
use table::{NonTerminal, Table};

pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse_expression(scan: Peekable<Scanner>) -> Result<ast::Expression> {
    parse((scan, NonTerminal::Expr, Token::EOF))
}

pub struct ParseArgs {
    scan: Peekable<Scanner>,
    start: NonTerminal,
    end: Token,
}

impl Default for ParseArgs {
    fn default() -> Self {
        Self {
            scan: Scanner::from_text(""),
            start: NonTerminal::Prog,
            end: Token::EOF,
        }
    }
}

impl From<Peekable<Scanner>> for ParseArgs {
    fn from(scan: Peekable<Scanner>) -> Self {
        Self {
            scan,
            ..Self::default()
        }
    }
}

impl From<(Peekable<Scanner>, NonTerminal, Token)> for ParseArgs {
    fn from(tuple: (Peekable<Scanner>, NonTerminal, Token)) -> Self {
        let (scan, start, end) = tuple;
        Self { scan, start, end }
    }
}

pub fn parse<A, R>(args: A) -> Result<R>
where
    A: Into<ParseArgs>,
    R: TryFrom<AST, Error = ParseError>,
{
    let ParseArgs {
        mut scan,
        start,
        end,
    } = args.into();
    let mut production_stack = vec![Right(end), Left(start)];
    let mut value_stack: Vec<ValueItem> = Vec::new();
    let table = Table::new();

    'outer: while let Some(item) = scan.next() {
        let word = item?;
        let next_word = scan.peek();
        loop {
            debug!(
                "\nStack:[{}]\nValueStack:[{}]\nWord: {}\nnext Word: {:?}",
                production_stack
                    .iter()
                    .map(|i| format!("{}", i))
                    .fold(String::new(), |acc, x| acc + &x + " ,"),
                value_stack
                    .iter()
                    .map(|i| format!("{}", i))
                    .fold(String::new(), |acc, x| acc + &x + " ,"),
                word,
                next_word
            );
            match production_stack.last().unwrap() {
                Right(Token::EOF) if word == Token::EOF => match value_stack.len() {
                    1 => {
                        if let Left(final_value) = value_stack.pop().unwrap() {
                            return R::try_from(final_value);
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
                Right(terminal) if discriminant(terminal) == discriminant(&word) => {
                    let _ = production_stack.pop().unwrap().unwrap_right();
                    value_stack.push(Right(word));
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
                    } else if let Some(new_items) = table.at(non_terminal, &word) {
                        production_stack.pop();
                        for new_item in new_items.into_iter().rev() {
                            production_stack.push(new_item);
                        }
                    } else {
                        return Err(format!(
                            "[{}] Unexpected token {} (was expecting {})",
                            word.format_location(),
                            word,
                            non_terminal
                        )
                        .into());
                    }
                }
            }
        }
    }
    unreachable!("Should not make it out of the loop!");
}

pub fn veiw_table_info() {
    Table::new().print_info();
}

#[cfg(test)]
mod test {
    use super::*;
    use ast::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_errors_on_bad_first_character() {
        let scan = Scanner::from_text("~");
        let out = parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF));
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
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_ok());
    }

    #[test]
    fn single_operation_does_not_error() {
        let scan = Scanner::from_text("1+1");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_ok());
    }

    #[test]
    fn operation_chain_does_not_error() {
        let scan = Scanner::from_text("a*b-c+d");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_ok());
    }

    #[test]
    fn parenthetical_number_does_not_error() {
        let scan = Scanner::from_text("(1)");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_ok());
    }

    #[test]
    fn operation_chain_with_parenthasese_does_not_error() {
        let scan = Scanner::from_text("1+(1-1)*1");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_ok())
    }

    #[test]
    fn sequential_numbers_error() {
        let scan = Scanner::from_text("1 1");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_err());
    }

    #[test]
    fn trailing_operator_errors() {
        let scan = Scanner::from_text("1 + 1 -");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_err());
    }

    #[test]
    fn single_number_parses_correctly() {
        let scan = Scanner::from_text("1");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(Expression::NumberLiteral(1));
        assert_eq!(out, expected);
    }

    #[test]
    fn single_variable_parses_correctly() {
        let scan = Scanner::from_text("a");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(Expression::Variable("a".into()));
        assert_eq!(out, expected);
    }

    #[test]
    fn single_addition_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("a+b");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(Variable("a".into())),
            Operator::Plus,
            Box::new(Variable("b".into())),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn single_subtraction_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("a-b");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(Variable("a".into())),
            Operator::Minus,
            Box::new(Variable("b".into())),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn single_multiplication_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("a*b");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(Variable("a".into())),
            Operator::Multiply,
            Box::new(Variable("b".into())),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn single_division_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("a/b");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(Variable("a".into())),
            Operator::Divide,
            Box::new(Variable("b".into())),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn operation_chain_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("a*b-c+d");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(BinaryOperation(
                Box::new(BinaryOperation(
                    Box::new(Variable("a".into())),
                    Operator::Multiply,
                    Box::new(Variable("b".into())),
                )),
                Operator::Minus,
                Box::new(Variable("c".into())),
            )),
            Operator::Plus,
            Box::new(Variable("d".into())),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn parenthetical_variable_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("(a)");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(Variable("a".into()));
        assert_eq!(out, expected);
    }

    #[test]
    fn literal_expressions_can_collapse() {
        let scan = Scanner::from_text("2*3-4+5");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(Expression::NumberLiteral(7));
        assert_eq!(out, expected);
    }

    #[test]
    fn fails_when_number_is_too_large() {
        // TODO Change this back to assuming i32 when I fix the integer type I use
        let scan = Scanner::from_text(format!("{}", (i64::MAX as i128) + 1).as_str());
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_err());
    }

    #[test]
    fn expressions_format_correctly() {
        let scan = Scanner::from_text("a*b+c/d-(e)");
        let out = format!(
            "{}",
            parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).unwrap()
        );
        let expected = "(((a * b) + (c / d)) - e)";
        assert_eq!(out, expected);
    }

    #[test]
    fn literal_division_by_zero_does_not_panic() {
        use Expression::*;
        let scan = Scanner::from_text("1/0");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(NumberLiteral(1)),
            Operator::Divide,
            Box::new(NumberLiteral(0)),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn negative_numbers_parse_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("-1");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(NumberLiteral(-1));
        assert_eq!(out, expected);
    }

    #[test]
    fn negative_variables_parse_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("-a");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(UnaryOperation(
            Operator::Minus,
            Box::new(Variable("a".into())),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn minus_adjacent_to_another_minus_fails() {
        let scan = Scanner::from_text("1--1");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_err());
    }

    #[test]
    fn minus_adjacent_to_plus_fails() {
        let scan = Scanner::from_text("1+-1");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_err());
    }

    #[test]
    fn single_float_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("12.34");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(FloatLiteral(12.34));
        assert_eq!(out, expected);
    }

    #[test]
    fn floating_point_numbers_can_collapse() {
        use Expression::*;
        let scan = Scanner::from_text("1.0+2.0");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(FloatLiteral(3.0));
        assert_eq!(out, expected);
    }

    #[test]
    fn negative_floating_point_numbers_parse_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("-12.34");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(FloatLiteral(-12.34));
        assert_eq!(out, expected);
    }

    #[test]
    fn single_exponent_parses_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("a^b");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(Variable("a".into())),
            Operator::Power,
            Box::new(Variable("b".into())),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn integer_exponents_parse_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("2^3");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(NumberLiteral(8));
        assert_eq!(out, expected);
    }

    #[test]
    fn float_exponents_parse_correctly() {
        use Expression::*;
        let scan = Scanner::from_text("2.0^3.0");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(FloatLiteral(8.0));
        assert_eq!(out, expected);
    }

    #[test]
    fn parsing_lone_comment_fails_gracefully() {
        let scan = Scanner::from_text("//");
        assert!(parse::<_, Expression>((scan, NonTerminal::Expr, Token::EOF)).is_err());
    }

    #[test]
    fn trailing_comment_doesnt_cause_failure() {
        use Expression::*;
        let scan = Scanner::from_text("X//");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(Variable("X".into()));
        assert_eq!(out, expected);
    }

    #[test]
    fn integer_power_overflow_fails_gracefully() {
        use Expression::*;
        let scan = Scanner::from_text("8^88");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(NumberLiteral(8)),
            Operator::Power,
            Box::new(NumberLiteral(88)),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn integer_multiply_overflow_fails_gracefully() {
        use Expression::*;
        let scan = Scanner::from_text("20^8*20^8");
        let out = parse((scan, NonTerminal::Expr, Token::EOF));
        let expected = Ok(BinaryOperation(
            Box::new(NumberLiteral(25600000000)),
            Operator::Multiply,
            Box::new(NumberLiteral(25600000000)),
        ));
        assert_eq!(out, expected);
    }

    #[test]
    fn minimal_program_parses_correctly() {
        let scan = Scanner::from_text("program a; begin end.");
        let out = parse(scan);
        let expected = Ok(CompilationUnit {
            name: "a".into(),
            statements: vec![],
        });
        assert_eq!(out, expected);
    }

    #[test]
    fn assignment_expression_parses_correctly() {
        let scan = Scanner::from_text("num a = 1;");
        let out = parse((scan, NonTerminal::Assignment, Token::EOF));
        let expected = Ok(Statement::NumAssignment {
            name: "a".into(),
            expression: Expression::NumberLiteral(1),
        });
        assert_eq!(out, expected);
    }

    #[test]
    fn program_with_statements_parses_correctly() {
        let scan = Scanner::from_text(indoc! {"
            program foo; begin
                num a = 1 + 7;
                num b = 2 * a;
            end.
        "});
        let out = parse(scan);
        let expected = Ok(CompilationUnit {
            name: "foo".into(),
            statements: vec![
                Statement::NumAssignment {
                    name: "a".into(),
                    expression: Expression::NumberLiteral(8),
                },
                Statement::NumAssignment {
                    name: "b".into(),
                    expression: Expression::BinaryOperation(
                        Box::new(Expression::NumberLiteral(2)),
                        Operator::Multiply,
                        Box::new(Expression::Variable("a".into())),
                    ),
                },
            ],
        });
        assert_eq!(out, expected);
    }

    #[test]
    fn floating_point_assignments_can_parse_correctly() {
        let scan = Scanner::from_text("ish a = 1.0;");
        let out = parse((scan, NonTerminal::Assignment, Token::EOF));
        let expected = Ok(Statement::IshAssignment {
            name: "a".into(),
            expression: Expression::FloatLiteral(1.0),
        });
        assert_eq!(out, expected);
    }

    #[test]
    fn procedures_can_parse_correctly() {
        let scan =
            Scanner::from_text("num procedure ident(num x) { num result = x; return result; }");
        let out = parse((scan, NonTerminal::ProcDeclaration, Token::EOF));
        let expected = Ok(Statement::ProcedureDeclaration {
            name: "a".into(),
            params: vec![Param::Num("x".into())],
            statements: vec![Statement::NumAssignment {
                name: "result".into(),
                expression: Expression::Variable("x".into()),
            }],
            return_expression: Expression::Variable("result".into()),
        });
        assert_eq!(out, expected);
    }
}
