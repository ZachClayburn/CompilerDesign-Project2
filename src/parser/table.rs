use log::trace;

use super::{ast, Either, Left, Right, Token};
use ast::{reduce_binary_op, reduce_parenthetical, reduce_value, ReductionOp};

#[derive(Debug, PartialEq, Clone)]
pub(super) enum NonTerminal {
    Goal,
    Expr,
    ExprPrime,
    Term,
    TermPrime,
    Factor,
    Reduction(ReductionOp),
}

pub(super) type ProductionItem = Either<NonTerminal, Token>;

pub struct Table {}

impl Table {
    pub(super) fn at(&self, focus: &NonTerminal, word: &Token) -> Option<Vec<ProductionItem>> {
        use NonTerminal::*;
        use Token::*;
        match (focus, word) {
            (Goal, LParen(_)) | (Goal, Number(_)) | (Goal, Identifier(_)) => {
                trace!("Running rule 0");
                Some(vec![Left(Expr)])
            }
            (Expr, LParen(_)) | (Expr, Number(_)) | (Expr, Identifier(_)) => {
                trace!("Running rule 1");
                Some(vec![Left(Term), Left(ExprPrime)])
            }
            (ExprPrime, Plus(info)) => {
                trace!("Running rule 2");
                Some(vec![
                    Right(Plus(*info)),
                    Left(Term),
                    Left(Reduction(reduce_binary_op)),
                    Left(ExprPrime),
                ])
            }
            (ExprPrime, Minus(info)) => {
                trace!("Running rule 3");
                Some(vec![
                    Right(Minus(*info)),
                    Left(Term),
                    Left(Reduction(reduce_binary_op)),
                    Left(ExprPrime),
                ])
            }
            (ExprPrime, RParen(_)) | (ExprPrime, EOF) => {
                trace!("Running rule 4");
                Some(vec![])
            }
            (Term, LParen(_)) | (Term, Number(_)) | (Term, Identifier(_)) => {
                trace!("Running rule 5");
                Some(vec![Left(Factor), Left(TermPrime)])
            }
            (TermPrime, Star(info)) => {
                trace!("Running rule 6");
                Some(vec![
                    Right(Star(*info)),
                    Left(Factor),
                    Left(Reduction(reduce_binary_op)),
                    Left(TermPrime),
                ])
            }
            (TermPrime, Div(info)) => {
                trace!("Running rule 7");
                Some(vec![
                    Right(Div(*info)),
                    Left(Factor),
                    Left(Reduction(reduce_binary_op)),
                    Left(TermPrime),
                ])
            }
            (TermPrime, RParen(_))
            | (TermPrime, EOF)
            | (TermPrime, Plus(_))
            | (TermPrime, Minus(_)) => {
                trace!("Running rule 8");
                Some(vec![])
            }
            (Factor, LParen(info)) => {
                trace!("Running rule 9");
                Some(vec![
                    Right(LParen(*info)),
                    Left(Expr),
                    Right(RParen(*info)),
                    Left(Reduction(reduce_parenthetical)),
                ])
            }
            (Factor, Number(info)) => {
                trace!("Running rule 10");
                Some(vec![
                    Right(Number(info.clone())),
                    Left(Reduction(reduce_value)),
                ])
            }
            (Factor, Identifier(info)) => {
                trace!("Running rule 11");
                Some(vec![
                    Right(Identifier(info.clone())),
                    Left(Reduction(reduce_value)),
                ])
            }
            (_, _) => None,
        }
    }
}
