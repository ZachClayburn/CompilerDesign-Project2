use super::{Either, Left, Result, Right, Token};
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperator::Plus => '+',
                BinaryOperator::Minus => '-',
                BinaryOperator::Multiply => '*',
                BinaryOperator::Divide => '/',
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionIr {
    NumberLiteral(i32),
    Variable(String),
    BinaryOperation(Box<Self>, BinaryOperator, Box<Self>),
}

impl Display for ExpressionIr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionIr::NumberLiteral(num) => write!(f, "{}", num),
            ExpressionIr::Variable(name) => write!(f, "{}", name),
            ExpressionIr::BinaryOperation(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
        }
    }
}

pub(super) type ValueItem = Either<ExpressionIr, Token>;
pub(super) type ReductionOp = fn(Vec<ValueItem>) -> Result<Vec<ValueItem>>;

pub fn reduce_value(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    match stack.pop() {
        Some(Right(Token::Number(info))) => {
            let number = info.content.parse()?;
            stack.push(Left(ExpressionIr::NumberLiteral(number)));
            Ok(stack)
        }
        Some(Right(Token::Identifier(info))) => {
            stack.push(Left(ExpressionIr::Variable(info.content)));
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
    use super::Token::*;
    use ExpressionIr::*;
    match &stack[..] {
        [] | [_] | [_, _] => {
            return Err("Insufficient values to reduce binary operation".into());
        }
        [.., Left(_), Right(Plus(_) | Minus(_) | Star(_) | Div(_)), Left(_)] => (),
        [.., wrong1, wrong2, wrong3] => {
            return Err(format!(
                "Incorrect types for binary operation reduction: {}, {}, {}",
                wrong1, wrong2, wrong3
            )
            .into());
        }
    };
    let rhs = stack.pop().unwrap().unwrap_left();
    let op = match stack.pop().unwrap().unwrap_right() {
        Plus(_) => BinaryOperator::Plus,
        Minus(_) => BinaryOperator::Minus,
        Star(_) => BinaryOperator::Multiply,
        Div(_) => BinaryOperator::Divide,
        bad => unreachable!("sould not be able to have {} as the operator", bad),
    };
    let lhs = stack.pop().unwrap().unwrap_left();

    let expr = match (lhs, rhs) {
        (NumberLiteral(lhs), NumberLiteral(rhs)) => match op {
            BinaryOperator::Plus => NumberLiteral(lhs + rhs),
            BinaryOperator::Minus => NumberLiteral(lhs - rhs),
            BinaryOperator::Multiply => NumberLiteral(lhs * rhs),
            BinaryOperator::Divide if rhs != 0 => NumberLiteral(lhs / rhs),
            op => BinaryOperation(
                Box::new(NumberLiteral(lhs)),
                op,
                Box::new(NumberLiteral(rhs)),
            ),
        },
        (lhs, rhs) => BinaryOperation(Box::new(lhs), op, Box::new(rhs)),
    };
    stack.push(Left(expr));
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
