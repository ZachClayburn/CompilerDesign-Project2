mod symbol_table;

use crate::parser::ast::{Expression, Operator, Statement, TypedVar};
use std::convert::TryInto;
use symbol_table::{SymbolTable, TableItem};

#[derive(Debug, PartialEq, Clone)]
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
    let mut symbols = SymbolTable::new();
    evaluate_statments(statements, &mut symbols)
}

fn evaluate_statments(
    statements: Vec<Statement>,
    symbols: &mut SymbolTable,
) -> Vec<Result<Statement>> {
    let mut evaluated = Vec::new();
    for statement in statements {
        let result = match statement {
            Statement::Declaration {
                name_and_type,
                expression,
            } => match evaluate_expression(expression, symbols) {
                Ok(expression) => {
                    if let Err(err) = symbols.add_expression(&name_and_type, &expression) {
                        evaluated.push(Err(err));
                        continue;
                    }
                    Ok(Statement::Declaration {
                        name_and_type,
                        expression,
                    })
                }
                Err(err) => Err(err),
            },
            Statement::ProcedureDeclaration {
                name_and_type,
                params,
                statements,
            } => {
                if let Err(err) =
                    symbols.add_procedure(&name_and_type, params.to_vec(), statements.to_vec())
                {
                    evaluated.push(Err(err));
                }
                Ok(Statement::ProcedureDeclaration {
                    name_and_type,
                    params,
                    statements,
                })
            }
            unsuported => Ok(unsuported),
        };
        evaluated.push(result);
    }

    evaluated
}

fn evaluate_expression(expr: Expression, mut symbols: &mut SymbolTable) -> Result<Expression> {
    use Expression::*;
    let expr = match expr {
        BinaryOperation(lhs, op, rhs) => {
            let lhs = evaluate_expression(*lhs, &mut symbols)?;
            let rhs = evaluate_expression(*rhs, &mut symbols)?;
            match (lhs, rhs) {
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
            }
        }
        Variable(name) => match symbols.get_value(&name)? {
            TableItem::NumVariable(val) => NumberLiteral((*val).into()), // TODO remove this when I fix the types
            TableItem::IshVariable(val) => FloatLiteral(*val),
            TableItem::NumProcedure { .. } => todo!(),
            TableItem::IshProcedure { .. } => todo!(),
        },
        NumberLiteral(num) => NumberLiteral(num),
        FloatLiteral(num) => FloatLiteral(num),
        UnaryOperation(Operator::Minus, boxed_expr) => {
            let expression = evaluate_expression(*boxed_expr, symbols)?;
            match expression {
                NumberLiteral(num) => NumberLiteral(-num),
                FloatLiteral(num) => FloatLiteral(-num),
                bad_expr => return Err(format!("Cannot negate {}", bad_expr).into()),
            }
        }
        ProcedureCall { name, args } => match symbols.get_value(&name)? {
            TableItem::NumProcedure { params, statements } => {
                let params = params.clone();
                let statements = statements.clone();
                evaluate_procedure_call(ProcedureType::Num, args, params, statements, &mut symbols)?
            }
            TableItem::IshProcedure { params, statements } => {
                let params = params.clone();
                let statements = statements.clone();
                evaluate_procedure_call(ProcedureType::Ish, args, params, statements, symbols)?
            }
            _ => return Err(format!("{} is value, but is called as a procedure", name).into()),
        },
        unsupported => {
            return Err(format!(
                "Attempting to evaluate unsupported expression {}",
                unsupported
            )
            .into())
        }
    };
    Ok(expr)
}

enum ProcedureType {
    Num,
    Ish,
}

fn evaluate_procedure_call(
    return_type: ProcedureType,
    args: Vec<Expression>,
    params: Vec<TypedVar>,
    statements: Vec<Statement>,
    symbols: &mut SymbolTable,
) -> Result<Expression> {
    if params.len() != args.len() {
        return Err(format!(
            "Expected {} arguments, but found {}",
            params.len(),
            args.len()
        )
        .into());
    }

    let mut args_eval = Vec::new();
    for arg in args {
        args_eval.push(evaluate_expression(arg, symbols)?);
    }

    symbols.push_scope(return_type);
    for (arg, param) in args_eval.iter().zip(params) {
        symbols.add_expression(&param, arg)?;
    }

    let evaluated = evaluate_statments(statements, symbols);

    if let Some(err) = evaluated.iter().find_map(|x| x.as_ref().err()) {
        return Err(err.clone());
    };

    let return_value = if let Some(expr) = evaluated.iter().find_map(|x| {
        if let Ok(Statement::ReturnStatement(expr)) = x {
            Some(expr)
        } else {
            None
        }
    }) {
        Ok(expr.clone())
    } else {
        Err("No return statement found!".to_string().into())
    };
    symbols.pop_scope()?;
    return_value
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

    #[test]
    fn nested_expressions_can_be_evaluated() {
        let scan = Scanner::from_text(indoc! {"
            program test; begin
            num a = 1 + 1 + 1;
            ish b = 2.0 + 4.0 / 2.0;
            end.
        "});
        let CompilationUnit {
            statements: parsed, ..
        } = parse(scan).unwrap();

        let out = evaluate(parsed);

        let expected = vec![
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("a".into()),
                expression: Expression::NumberLiteral(3),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("b".into()),
                expression: Expression::FloatLiteral(4.0),
            }),
        ];

        assert_eq!(out, expected);
    }

    #[test]
    fn negatives_can_be_evaluated() {
        let scan = Scanner::from_text(indoc! {"
            program test; begin
            num a = -1;
            end.
        "});
        let CompilationUnit {
            statements: parsed, ..
        } = parse(scan).unwrap();

        let out = evaluate(parsed);

        let expected = vec![Ok(Statement::Declaration {
            name_and_type: TypedVar::Num("a".into()),
            expression: Expression::NumberLiteral(-1),
        })];

        assert_eq!(out, expected);
    }
    #[test]
    fn variables_can_be_evaluated() {
        let scan = Scanner::from_text(indoc! {"
            program test; begin
            num a = 1;
            num b = 2;
            num c = a + b;
            ish aish = 1.0;
            ish bish = 2.0;
            ish cish = aish + bish;
            end.
        "});
        let CompilationUnit {
            statements: parsed, ..
        } = parse(scan).unwrap();

        let out = evaluate(parsed);

        let expected = vec![
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("a".into()),
                expression: Expression::NumberLiteral(1),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("b".into()),
                expression: Expression::NumberLiteral(2),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("c".into()),
                expression: Expression::NumberLiteral(3),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("aish".into()),
                expression: Expression::FloatLiteral(1.0),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("bish".into()),
                expression: Expression::FloatLiteral(2.0),
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Ish("cish".into()),
                expression: Expression::FloatLiteral(3.0),
            }),
        ];

        assert_eq!(out, expected);
    }

    #[test]
    fn can_evaluate_simple_procedures() {
        let scan = Scanner::from_text(indoc! {"
            program test; begin
                num procedure zero() {
                    return 0;
                }
                num a = zero();
            end.
        "});
        let CompilationUnit {
            statements: parsed, ..
        } = parse(scan).unwrap();

        let out = evaluate(parsed);

        let expected = vec![
            Ok(Statement::ProcedureDeclaration {
                name_and_type: TypedVar::Num("zero".into()),
                params: vec![],
                statements: vec![Statement::ReturnStatement(Expression::NumberLiteral(0))],
            }),
            Ok(Statement::Declaration {
                name_and_type: TypedVar::Num("a".into()),
                expression: Expression::NumberLiteral(0),
            }),
        ];

        assert_eq!(out, expected);
    }
}
