use super::*;
use crate::scanner::Token;
use crate::scanner::TokenInfo;
use either::{Left, Right};
use num::checked_pow;
use std::convert::TryInto;

pub fn get_reduction_name(function_pointer: &ReductionOp) -> &'static str {
    match *function_pointer {
        x if x == reduce_value => "reduce_value()",
        x if x == reduce_binary_op => "reduce_binary_op()",
        x if x == reduce_parenthetical => "reduce_parenthetical()",
        x if x == reduce_unary_operator => "reduce_unary_operator()",
        x if x == reduce_program => "reduce_program()",
        x if x == reduce_statement_list => "reduce_statement_list()",
        x if x == reduce_assignment => "reduce_assignment()",
        _ => panic!("unknown reduction"),
    }
}

pub fn reduce_value(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Expression::*;
    use AST::*;
    match stack.pop() {
        Some(Right(Token::Number(info))) => {
            let number = info.content.parse()?;
            stack.push(Left(Expr(NumberLiteral(number))));
            Ok(stack)
        }
        Some(Right(Token::Identifier(info))) => {
            stack.push(Left(Expr(Variable(info.content))));
            Ok(stack)
        }
        Some(Right(Token::Float(info))) => {
            let number = info.content.parse()?;
            stack.push(Left(Expr(FloatLiteral(number))));
            Ok(stack)
        }
        Some(unexpected) => Err(format!(
            "Expectied a number or variable, but {} was found",
            unexpected
        )
        .into()),
        None => Err("Attempting to reduce a value, but the stack is empty!".into()),
    }
}

pub fn reduce_binary_op(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Expression::*;
    use Token::*;
    use AST::*;
    let rhs = match stack.pop() {
        Some(Left(Expr(expr))) => expr,
        Some(bad) => return Err(format!("Expected an expression, found {}", bad).into()),
        None => return Err("Missing ) while trying to reduce parenthetical".into()),
    };
    let op = match stack.pop().unwrap().unwrap_right() {
        Plus(_) => Operator::Plus,
        Minus(_) => Operator::Minus,
        Star(_) => Operator::Multiply,
        Div(_) => Operator::Divide,
        Pow(_) => Operator::Power,
        bad => return Err(format!("{} not a valid binary operator", bad).into()),
    };
    let lhs = match stack.pop() {
        Some(Left(Expr(expr))) => expr,
        Some(bad) => return Err(format!("Expected an expression, found {}", bad).into()),
        None => return Err("Missing ) while trying to reduce parenthetical".into()),
    };

    let expr = match (lhs, rhs) {
        (NumberLiteral(lhs), NumberLiteral(rhs)) => match op {
            Operator::Plus => NumberLiteral(lhs + rhs),
            Operator::Minus => NumberLiteral(lhs - rhs),
            Operator::Multiply => lhs.checked_mul(rhs).map_or_else(
                || {
                    BinaryOperation(
                        Box::new(NumberLiteral(lhs)),
                        Operator::Multiply,
                        Box::new(NumberLiteral(rhs)),
                    )
                },
                |result| NumberLiteral(result),
            ),
            Operator::Power => checked_pow(lhs, rhs.try_into()?).map_or_else(
                || {
                    BinaryOperation(
                        Box::new(NumberLiteral(lhs)),
                        Operator::Power,
                        Box::new(NumberLiteral(rhs)),
                    )
                },
                |result| NumberLiteral(result),
            ),
            Operator::Divide if rhs != 0 => NumberLiteral(lhs / rhs),
            op => BinaryOperation(
                Box::new(NumberLiteral(lhs)),
                op,
                Box::new(NumberLiteral(rhs)),
            ),
        },
        (FloatLiteral(lhs), FloatLiteral(rhs)) => match op {
            Operator::Plus => FloatLiteral(lhs + rhs),
            Operator::Minus => FloatLiteral(lhs - rhs),
            Operator::Multiply => FloatLiteral(lhs * rhs),
            Operator::Divide => FloatLiteral(lhs / rhs),
            Operator::Power => FloatLiteral(lhs.powf(rhs)),
        },
        (lhs, rhs) => BinaryOperation(Box::new(lhs), op, Box::new(rhs)),
    };
    stack.push(Left(Expr(expr)));
    Ok(stack)
}

pub fn reduce_parenthetical(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    match stack.pop() {
        Some(Right(Token::RParen(_))) => (),
        Some(bad) => return Err(format!("Expected ), found {}", bad).into()),
        None => return Err("Missing ) while trying to reduce parenthetical".into()),
    };
    let expr = match stack.pop() {
        Some(Left(expr)) => expr,
        Some(bad) => return Err(format!("Expected expression, found {}", bad).into()),
        None => return Err("Missing expression while trying to reduce parenthetical".into()),
    };
    match stack.pop() {
        Some(Right(Token::LParen(_))) => (),
        Some(bad) => return Err(format!("Expected (, found {}", bad).into()),
        None => return Err("Missing ( while trying to reduce parenthetical".into()),
    };

    stack.push(Left(expr));
    Ok(stack)
}

pub fn reduce_unary_operator(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use AST::*;
    let expr = match stack.pop() {
        Some(Left(Expr(expr))) => expr,
        Some(bad) => return Err(format!("Expected expression, found {}", bad).into()),
        None => return Err("Missing expression while trying to reduce unary operator".into()),
    };
    match stack.pop() {
        Some(Right(Token::Minus(_))) => (),
        Some(bad) => return Err(format!("Expected -, found {}", bad).into()),
        None => return Err("Missing - while trying to reduce unary operator".into()),
    };

    stack.push(Left(Expr(match expr {
        Expression::NumberLiteral(value) => Expression::NumberLiteral(-value),
        Expression::FloatLiteral(value) => Expression::FloatLiteral(-value),
        exp => Expression::UnaryOperation(Operator::Minus, Box::new(exp)),
    })));
    Ok(stack)
}

pub fn reduce_program(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(Dot(_))) => (),
        Some(bad) => return Err(format!("Expected ., but found {}", bad).into()),
        None => return Err(format!("Missing . while trying to reduce program").into()),
    };

    match stack.pop() {
        Some(Right(End(_))) => (),
        Some(bad) => return Err(format!("Expected end, but found {}", bad).into()),
        None => return Err(format!("Missing end while trying to reduce program").into()),
    };

    let statents = match stack.pop() {
        Some(Left(AST::StmntList(list))) => list,
        Some(bad) => return Err(format!("Expected Statement List, but found {}", bad).into()),
        None => {
            return Err(format!("Missing Statement List while trying to reduce program").into())
        }
    };

    match stack.pop() {
        Some(Right(Begin(_))) => (),
        Some(bad) => return Err(format!("Expected begin, but found {}", bad).into()),
        None => return Err(format!("Missing begin while trying to reduce program").into()),
    };

    match stack.pop() {
        Some(Right(Semicolon(_))) => (),
        Some(bad) => return Err(format!("Expected ;, but found {}", bad).into()),
        None => return Err(format!("Missing ; while trying to reduce program").into()),
    };

    let id = match stack.pop() {
        Some(Right(Identifier(TokenInfo { content, .. }))) => content,
        Some(bad) => return Err(format!("Expected program name, but found {}", bad).into()),
        None => return Err(format!("Missing program name while trying to reduce program").into()),
    };

    match stack.pop() {
        Some(Right(Program(_))) => (),
        Some(bad) => return Err(format!("Expected program, but found {}", bad).into()),
        None => return Err(format!("Missing program while trying to reduce program").into()),
    };

    stack.push(Left(AST::Prog(CompilationUnit {
        name: id,
        statements: statents,
    })));

    Ok(stack)
}

pub fn reduce_statement_list(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    let mut statements = vec![];

    loop {
        if let Some(top) = stack.pop() {
            if let Left(AST::Stmnt(stmnt)) = top {
                statements.push(stmnt);
            } else {
                stack.push(top);
                break;
            }
        } else {
            break;
        }
    }

    statements.reverse();

    stack.push(Left(AST::StmntList(statements)));
    Ok(stack)
}

pub fn reduce_assignment(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    use Token::*;

    match stack.pop() {
        Some(Right(Semicolon(_))) => (),
        Some(bad) => return Err(format!("Expected ;, but found {}", bad).into()),
        None => return Err(format!("Missing ; while trying to reduce program").into()),
    };

    let expression = match stack.pop() {
        Some(Left(AST::Expr(expression))) => expression,
        Some(bad) => return Err(format!("Expected expression, but found {}", bad).into()),
        None => return Err(format!("Missing expression while trying to reduce program").into()),
    };

    match stack.pop() {
        Some(Right(Assign(_))) => (),
        Some(bad) => return Err(format!("Expected =, but found {}", bad).into()),
        None => return Err(format!("Missing = while trying to reduce program").into()),
    };

    let name = match stack.pop() {
        Some(Right(Identifier(TokenInfo { content, .. }))) => content,
        Some(bad) => return Err(format!("Expected name, but found {}", bad).into()),
        None => return Err(format!("Missing name while trying to reduce program").into()),
    };

    match stack.pop() {
        Some(Right(Num(_))) => stack.push(Left(AST::Stmnt(Statement::NumAssignment {
            name,
            expression,
        }))),
        Some(Right(Ish(_))) => stack.push(Left(AST::Stmnt(Statement::IshAssignment {
            name,
            expression,
        }))),
        Some(bad) => return Err(format!("Expected Num, but found {}", bad).into()),
        None => return Err(format!("Missing Num while trying to reduce program").into()),
    };

    Ok(stack)
}
