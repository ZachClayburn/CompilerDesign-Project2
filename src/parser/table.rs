use log::{info, trace};

use super::{
    Item, Left,
    NonTerminal::{self, *},
    Right,
    Token::{self, *},
};

pub struct Table {}

impl Table {
    pub(super) fn at(&self, focus: &NonTerminal, word: &Token) -> Option<Vec<Item>> {
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
            (Factor, LParen(_info)) => {
                trace!("Running rule 9");
                todo!("I need to figure out how to add the ) here")
            }
            (Factor, Number(info)) => {
                trace!("Running rule 10");
                Some(vec![Right(Number(info.clone()))])
            }
            (Factor, Identifier(info)) => {
                trace!("Running rule 11");
                Some(vec![Right(Identifier(info.clone()))])
            }
            (_, _) => None,
        }
    }
}
