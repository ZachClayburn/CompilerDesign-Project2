use log::trace;

use super::{
    Item, Left,
    NonTerminal::{self, *},
    Right,
    Token::{self, *},
};

pub struct Table {}

impl Table {
    pub(super) fn at(&self, focus: &NonTerminal, word: &Option<Token>) -> Option<Vec<Item>> {
        match (focus, word) {
            (Goal, Some(LParen(_))) | (Goal, Some(Number(_))) | (Goal, Some(Identifier(_))) => {
                trace!("Running rule 0");
                Some(vec![Left(Expr)])
            }
            (Expr, Some(LParen(_))) | (Expr, Some(Number(_))) | (Expr, Some(Identifier(_))) => {
                trace!("Running rule 1");
                Some(vec![Left(Term), Left(ExprPrime)])
            }
            (ExprPrime, Some(Plus(info))) => {
                trace!("Running rule 2");
                Some(vec![Right(Some(Plus(*info))), Left(Term), Left(ExprPrime)])
            }
            (ExprPrime, Some(Minus(info))) => {
                trace!("Running rule 3");
                Some(vec![Right(Some(Minus(*info))), Left(Term), Left(ExprPrime)])
            }
            (ExprPrime, Some(RParen(_))) | (ExprPrime, None) => {
                trace!("Running rule 4");
                Some(vec![])
            }
            (Term, Some(LParen(_))) | (Term, Some(Number(_))) | (Term, Some(Identifier(_))) => {
                trace!("Running rule 5");
                Some(vec![Left(Factor), Left(TermPrime)])
            }
            (TermPrime, Some(Star(info))) => {
                trace!("Running rule 6");
                Some(vec![
                    Right(Some(Star(*info))),
                    Left(Factor),
                    Left(TermPrime),
                ])
            }
            (TermPrime, Some(Div(info))) => {
                trace!("Running rule 7");
                Some(vec![Right(Some(Div(*info))), Left(Factor), Left(TermPrime)])
            }
            (TermPrime, Some(RParen(_)))
            | (TermPrime, None)
            | (TermPrime, Some(Plus(_)))
            | (TermPrime, Some(Minus(_))) => {
                trace!("Running rule 8");
                Some(vec![])
            }
            (Factor, Some(LParen(_info))) => {
                trace!("Running rule 9");
                todo!("I need to figure out how to add the ) here")
            }
            (Factor, Some(Number(info))) => {
                trace!("Running rule 10");
                Some(vec![Right(Some(Number(info.clone())))])
            }
            (Factor, Some(Identifier(info))) => {
                trace!("Running rule 11");
                Some(vec![Right(Some(Identifier(info.clone())))])
            }
            (_, _) => None,
        }
    }
}
