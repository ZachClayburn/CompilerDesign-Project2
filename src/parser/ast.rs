use super::{Either, Left, ParseError, Result, Right, Token};
use num::checked_pow;
use std::{convert::{TryFrom, TryInto}, fmt::Display};

#[derive(Debug, PartialEq)]
pub enum AST {
    Expr(Expression),
}

impl Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expression) => write!(f, "{}", expression),
        }
    }
}

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
pub enum Expression {
    NumberLiteral(i64), // TODO Switch this back to i32 when I can gracefully fail at exponetiation
    FloatLiteral(f32),
    Variable(String),
    BinaryOperation(Box<Self>, Operator, Box<Self>),
    UnaryOperation(Operator, Box<Self>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::NumberLiteral(num) => write!(f, "{}", num),
            Expression::FloatLiteral(num) => write!(f, "{}", num),
            Expression::Variable(name) => write!(f, "{}", name),
            Expression::BinaryOperation(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expression::UnaryOperation(op, exp) => write!(f, "{} {}", op, exp),
        }
    }
}

impl TryFrom<AST> for Expression {
    type Error = ParseError;

    fn try_from(value: AST) -> Result<Self> {
        match value {
            AST::Expr(expr) => Ok(expr),
        }
    }
}

pub(super) type ValueItem = Either<AST, Token>;
pub(super) type ReductionOp = fn(Vec<ValueItem>) -> Result<Vec<ValueItem>>;

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
    use super::Token::*;
    use Expression::*;
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
