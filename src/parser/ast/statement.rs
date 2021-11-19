use super::{Expression, ParseError, Result, AST};
use itertools::Itertools;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Declaration {
        name_and_type: TypedVar,
        expression: Expression,
    },
    ProcedureDeclaration {
        name_and_type: TypedVar,
        params: Vec<TypedVar>,
        statements: Vec<Statement>,
    },
    ReturnStatement(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Declaration {
                name_and_type: name,
                expression,
            } => {
                write!(f, "{} = {};", name, expression)
            }
            Statement::ProcedureDeclaration {
                name_and_type,
                params,
                statements,
            } => write!(
                f,
                "{}({}) {{{}}}",
                match name_and_type {
                    TypedVar::Num(name) => format!("num procedure {}", name),
                    TypedVar::Ish(name) => format!("ish procedure {}", name),
                },
                params.iter().format(", "),
                statements.iter().format("\n")
            ),
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

#[derive(Debug, PartialEq)]
pub enum TypedVar {
    Num(String),
    Ish(String),
}

impl Display for TypedVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(name) => write!(f, "num {}", name),
            Self::Ish(name) => write!(f, "ish {}", name),
        }
    }
}
