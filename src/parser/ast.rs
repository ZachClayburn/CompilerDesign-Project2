use super::{Either, Left, Result, Right, Token};
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
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
            }
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionIr {
    NumberLiteral(i32),
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
        Plus(_) => Operator::Plus,
        Minus(_) => Operator::Minus,
        Star(_) => Operator::Multiply,
        Div(_) => Operator::Divide,
        bad => unreachable!("sould not be able to have {} as the operator", bad),
    };
    let lhs = stack.pop().unwrap().unwrap_left();

    let expr = match (lhs, rhs) {
        (NumberLiteral(lhs), NumberLiteral(rhs)) => match op {
            Operator::Plus => NumberLiteral(lhs + rhs),
            Operator::Minus => NumberLiteral(lhs - rhs),
            Operator::Multiply => NumberLiteral(lhs * rhs),
            Operator::Divide if rhs != 0 => NumberLiteral(lhs / rhs),
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
        exp => ExpressionIr::UnaryOperation(Operator::Minus, Box::new(exp)),
    }));
    Ok(stack)
}
