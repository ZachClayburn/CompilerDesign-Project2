use super::{Either, Left, Result, Right, Token};
use std::convert::TryInto;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operator::Plus => '+',
                Operator::Minus => '-',
                Operator::Multiply => '*',
                Operator::Divide => '/',
                Operator::Power => '^',
            }
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionIr {
    NumberLiteral(i64), // TODO Switch this back to i32 when I can gracefully fail at exponetiation
    FloatLiteral(f32),
    Variable(String),
    BinaryOperation(Box<Self>, Operator, Box<Self>),
    UnaryOperation(Operator, Box<Self>),
}

impl Display for ExpressionIr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionIr::NumberLiteral(num) => write!(f, "{}", num),
            ExpressionIr::FloatLiteral(num) => write!(f, "{}", num),
            ExpressionIr::Variable(name) => write!(f, "{}", name),
            ExpressionIr::BinaryOperation(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            ExpressionIr::UnaryOperation(op, exp) => write!(f, "{} {}", op, exp),
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
        Some(Right(Token::Float(info))) => {
            let number = info.content.parse()?;
            stack.push(Left(ExpressionIr::FloatLiteral(number)));
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
    let rhs = match stack.pop() {
        Some(Left(expr)) => expr,
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
        Some(Left(expr)) => expr,
        Some(bad) => return Err(format!("Expected an expression, found {}", bad).into()),
        None => return Err("Missing ) while trying to reduce parenthetical".into()),
    };

    let expr = match (lhs, rhs) {
        (NumberLiteral(lhs), NumberLiteral(rhs)) => match op {
            Operator::Plus => NumberLiteral(lhs + rhs),
            Operator::Minus => NumberLiteral(lhs - rhs),
            Operator::Multiply => NumberLiteral(lhs * rhs),
            Operator::Power => NumberLiteral(lhs.pow(rhs.try_into()?)),
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
            Operator::Power => FloatLiteral(lhs.powf(rhs)), // TODO Gracefully fail at overflow
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

pub fn reduce_unary_operator(mut stack: Vec<ValueItem>) -> Result<Vec<ValueItem>> {
    let expr = match stack.pop() {
        Some(Left(expr)) => expr,
        Some(bad) => return Err(format!("Expected expression, found {}", bad).into()),
        None => return Err("Missing expression while trying to reduce unary operator".into()),
    };
    match stack.pop() {
        Some(Right(Token::Minus(_))) => (),
        Some(bad) => return Err(format!("Expected -, found {}", bad).into()),
        None => return Err("Missing - while trying to reduce unary operator".into()),
    };

    stack.push(Left(match expr {
        ExpressionIr::NumberLiteral(value) => ExpressionIr::NumberLiteral(-value),
        ExpressionIr::FloatLiteral(value) => ExpressionIr::FloatLiteral(-value),
        exp => ExpressionIr::UnaryOperation(Operator::Minus, Box::new(exp)),
    }));
    Ok(stack)
}
