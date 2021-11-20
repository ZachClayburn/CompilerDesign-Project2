use super::{ParseError, Result, AST};
use itertools::Itertools;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, PartialEq)]
pub enum Expression {
    NumberLiteral(i64), // TODO Switch this back to i32 when I can gracefully fail at exponetiation
    FloatLiteral(f32),
    Variable(String),
    BinaryOperation(Box<Self>, Operator, Box<Self>),
    UnaryOperation(Operator, Box<Self>),
    ProcedureCall { name: String, args: Vec<Expression> },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::NumberLiteral(num) => write!(f, "{}", num),
            Expression::FloatLiteral(num) => write!(f, "{}", num),
            Expression::Variable(name) => write!(f, "{}", name),
            Expression::BinaryOperation(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expression::UnaryOperation(op, exp) => write!(f, "{} {}", op, exp),
            Self::ProcedureCall { name, args } => {
                write!(f, "{}({})", name, args.iter().format(", "))
            }
        }
    }
}

impl TryFrom<AST> for Expression {
    type Error = ParseError;

    fn try_from(value: AST) -> Result<Self> {
        match value {
            AST::Expr(expr) => Ok(expr),
            bad => Err(format!("Expected Expression, but {} was produced!", bad.get_name()).into()),
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
