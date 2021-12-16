use super::{Expression, ParseError, Result, AST};
use itertools::Itertools;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, PartialEq, Clone)]
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
    PrintStatement(PrintExpr),
    ReadStatement(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrintExpr {
    Num(String),
    Ish(String),
    String(String),
}

impl Display for PrintExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrintExpr::Num(name) => write!(f, "printNum {}", name),
            PrintExpr::Ish(name) => write!(f, "printIsh {}", name),
            PrintExpr::String(name) => write!(f, "printString {}", name),
        }
    }
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
                "{}({}) {{\n    {}\n}}",
                match name_and_type {
                    TypedVar::Num(name) => format!("num procedure {}", name),
                    TypedVar::Ish(name) => format!("ish procedure {}", name),
                },
                params.iter().format(", "),
                statements.iter().format("\n    ")
            ),
            Self::ReturnStatement(expr) => write!(f, "return {};", expr),
            Self::PrintStatement(print_expr) => write!(f, "{};", print_expr),
            Self::ReadStatement(var) => write!(f, "readNum {}", var),
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
