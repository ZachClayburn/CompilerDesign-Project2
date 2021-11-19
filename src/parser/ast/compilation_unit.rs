use super::{ParseError, Result, Statement, AST};
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, PartialEq)]
pub struct CompilationUnit {
    pub name: String,
    pub statements: Vec<Statement>,
}

impl Display for CompilationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program: {}", self.name)
    }
}

impl TryFrom<AST> for CompilationUnit {
    type Error = ParseError;

    fn try_from(value: AST) -> Result<Self> {
        match value {
            AST::Prog(prog) => Ok(prog),
            bad => Err(format!("Expected Program, but {} was produced!", bad.get_name()).into()),
        }
    }
}
