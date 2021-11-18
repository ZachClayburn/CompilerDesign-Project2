mod compilation_unit;
mod expression;
pub mod reductions;
mod statement;

use super::{Either, ParseError, Result, Token};
pub use compilation_unit::CompilationUnit;
pub use expression::{Expression, Operator};
pub use statement::Statement;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum AST {
    Expr(Expression),
    Prog(CompilationUnit),
    Stmnt(Statement),
    StmntList(Vec<Statement>),
}

impl AST {
    fn get_name(&self) -> &'static str {
        match self {
            AST::Expr(..) => "Expression",
            AST::Prog(..) => "Program",
            AST::Stmnt(..) => "Statement",
            AST::StmntList(..) => "Statement List",
        }
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expression) => write!(f, "{}", expression),
            Self::Prog(program) => write!(f, "{}", program),
            Self::Stmnt(statement) => write!(f, "{}", statement),
            Self::StmntList(list) => write!(
                f,
                "{}",
                list.iter()
                    .map(|s| format!("{}\n", s))
                    .fold("".to_string(), |accum, s| accum + &s)
            ),
        }
    }
}

pub(super) type ValueItem = Either<AST, Token>;
pub(super) type ReductionOp = fn(Vec<ValueItem>) -> Result<Vec<ValueItem>>;
