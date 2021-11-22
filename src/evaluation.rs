use crate::parser::ast::{Expression, Operator, Statement};
use std::convert::TryInto;

#[derive(Debug, PartialEq)]
pub struct EvaluationError {
    pub error_msg: String,
}

impl Into<EvaluationError> for String {
    fn into(self) -> EvaluationError {
        EvaluationError { error_msg: self }
    }
}

type Result<T> = std::result::Result<T, EvaluationError>;

pub fn evaluate(statements: Vec<Statement>) -> Vec<Result<Statement>> {
    let mut evaluated = Vec::new();

    for statement in statements {
        match statement {
            Statement::Declaration {
                name_and_type,
                expression,
            } => match evaluate_expression(expression) {
                Ok(expression) => evaluated.push(Ok(Statement::Declaration {
                    name_and_type,
                    expression,
                })),
                Err(err) => evaluated.push(Err(err)),
            },
            unsuported => evaluated.push(Ok(unsuported)),
        }
    }

    evaluated
}

fn evaluate_expression(expr: Expression) -> Result<Expression> {
    use Expression::*;
    let expr = match expr {
        BinaryOperation(lhs, op, rhs) => match (*lhs, *rhs) {
            (NumberLiteral(lhs), NumberLiteral(rhs)) => match op {
                Operator::Plus => NumberLiteral(lhs + rhs),
                Operator::Minus => NumberLiteral(lhs - rhs),
                Operator::Multiply => {
                    if let Some(result) = lhs.checked_mul(rhs) {
                        NumberLiteral(result)
                    } else {
                        return Err(format!("Could not multiply {} and {}", lhs, rhs).into());
                    }
                }
                Operator::Divide => {
                    if let Some(result) = lhs.checked_div(rhs) {
                        NumberLiteral(result)
                    } else {
                        return Err(format!("Could not divide {} by {}", lhs, rhs).into());
                    }
                }
                Operator::Power => {
                    let rhs = match rhs.try_into() {
                        Ok(rhs) => rhs,
                        Err(_) => {
                            return Err(format!("{} cannot be used as an exponent", rhs).into())
                        }
                    };
                    if let Some(result) = lhs.checked_pow(rhs) {
                        NumberLiteral(result)
                    } else {
                        return Err(
                            format!("Could not raise {} to the power of {}", lhs, rhs).into()
                        );
                    }
                }
            },
            (FloatLiteral(lhs), FloatLiteral(rhs)) => match op {
                Operator::Plus => FloatLiteral(lhs + rhs),
                Operator::Minus => FloatLiteral(lhs - rhs),
                Operator::Multiply => FloatLiteral(lhs * rhs),
                Operator::Divide => FloatLiteral(lhs / rhs),
                Operator::Power => FloatLiteral(lhs.powf(rhs)),
            },
            (lhs, rhs) => BinaryOperation(Box::new(lhs), op, Box::new(rhs)),
        },
        unsupported => unsupported,
    };
    Ok(expr)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::ast::{CompilationUnit, Expression, TypedVar};
    use crate::parser::parse;
    use crate::scanner::Scanner;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_int_math_statements_can_be_evaluated() {
        let scan = Scanner::from_text(indoc! {"
            program test; begin
            num a = 1 + 1;
            num b = 2 * 2;
            num c = 3 / 3;
            num d = 4 - 4;
            num e = 5 ^ 5;
            end.
        "});
        let CompilationUnit {
            statements: parsed, ..
        } = parse(scan).unwrap();

        let out = evaluate(parsed);

        let expected = vec![
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("a".into()),
                expression: Expression::NumberLiteral(2),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("b".into()),
                expression: Expression::NumberLiteral(4),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("c".into()),
                expression: Expression::NumberLiteral(1),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("d".into()),
                expression: Expression::NumberLiteral(0),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("e".into()),
                expression: Expression::NumberLiteral(3125),
            }),
        ];

        assert_eq!(out, expected);
    }

    #[test]
    fn simple_float_math_statements_can_be_evaluated() {
        let scan = Scanner::from_text(indoc! {"
            program test; begin
            ish a = 1.0 + 1.0;
            ish b = 2.0 * 2.0;
            ish c = 3.0 / 3.0;
            ish d = 4.0 - 4.0;
            ish e = 5.0 ^ 5.0;
            end.
        "});
        let CompilationUnit {
            statements: parsed, ..
        } = parse(scan).unwrap();

        let out = evaluate(parsed);

        let expected = vec![
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("a".into()),
                expression: Expression::FloatLiteral(2.0),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("b".into()),
                expression: Expression::FloatLiteral(4.0),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("c".into()),
                expression: Expression::FloatLiteral(1.0),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("d".into()),
                expression: Expression::FloatLiteral(0.0),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("e".into()),
                expression: Expression::FloatLiteral(3125.0),
            }),
        ];

        assert_eq!(out, expected);
    }
}
