use super::{Expression, ParseError, Result, AST};
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, PartialEq)]
pub enum Statement {
    NumAssignment {
        name: String,
        expression: Expression,
    },
    IshAssignment {
        name: String,
        expression: Expression,
    },
    ProcedureDeclaration {
        name: String,
        params: Vec<Param>,
        statements: Vec<Statement>,
        return_expression: Expression,
    },
    ReturnStatement(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Param {
    Num(String),
    Ish(String),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::NumAssignment { name, expression }
            | Statement::IshAssignment { name, expression } => {
                write!(f, "num {} = {};", name, expression)
            }
            Statement::ProcedureDeclaration {
                name,
                params,
                statements,
                return_expression,
            } => todo!("You need to finish Display for Procedure declarations!"),
            Self::ReturnStatement(expr) => write!(f, "return {};", expr),
        }
    }
}

impl TryFrom<AST> for Statement {
    type Error = ParseError;

    fn try_from(value: AST) -> Result<Self> {
        match value {
            AST::Stmnt(statement) => Ok(statement),
            bad => Err(format!("Expected Statement, but {} was produced!", bad.get_name()).into()),
        }
    }
}
