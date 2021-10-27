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
