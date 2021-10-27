use log::trace;

use super::{ast, Either, Left, Right, Token};
use ast::{reduce_value, ReductionOp};

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
                Some(vec![Left(ExprPrime), Left(Term)])
            }
            (ExprPrime, Plus(info)) => {
                trace!("Running rule 2");
                Some(vec![Left(ExprPrime), Left(Term), Right(Plus(*info))])
            }
            (ExprPrime, Minus(info)) => {
                trace!("Running rule 3");
                Some(vec![Left(ExprPrime), Left(Term), Right(Minus(*info))])
            }
            (ExprPrime, RParen(_)) | (ExprPrime, EOF) => {
                trace!("Running rule 4");
                Some(vec![])
            }
            (Term, LParen(_)) | (Term, Number(_)) | (Term, Identifier(_)) => {
                trace!("Running rule 5");
                Some(vec![Left(TermPrime), Left(Factor)])
            }
            (TermPrime, Star(info)) => {
                trace!("Running rule 6");
                Some(vec![Left(TermPrime), Left(Factor), Right(Star(*info))])
            }
            (TermPrime, Div(info)) => {
                trace!("Running rule 7");
                Some(vec![Left(TermPrime), Left(Factor), Right(Div(*info))])
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
                Some(vec![Right(RParen(*info)), Left(Expr), Right(LParen(*info))])
            }
            (Factor, Number(info)) => {
                trace!("Running rule 10");
                Some(vec![
                    Left(Reduction(reduce_value)),
                    Right(Number(info.clone())),
                ])
            }
            (Factor, Identifier(info)) => {
                trace!("Running rule 11");
                Some(vec![
                    Left(Reduction(reduce_value)),
                    Right(Identifier(info.clone())),
                ])
            }
            (_, _) => None,
        }
    }
}
