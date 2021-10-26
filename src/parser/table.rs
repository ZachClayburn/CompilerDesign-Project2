use super::{Item, Left, NonTerminal, Right, Token};
use NonTerminal::*;
use Token::*;

pub struct Table {}

impl Table {
    pub(super) fn at(&self, focus: &NonTerminal, word: &Option<Token>) -> Option<Vec<Item>> {
        match (focus, word) {
            (Goal, Some(LParen(_))) | (Goal, Some(Number(_))) | (Goal, Some(Identifier(_))) => {
                Some(vec![Left(Expr)])
            }
            (Expr, Some(LParen(_))) | (Expr, Some(Number(_))) | (Expr, Some(Identifier(_))) => {
                Some(vec![Left(Term), Left(ExprPrime)])
            }
            (ExprPrime, Some(Plus(info))) => {
                Some(vec![Right(Some(Plus(*info))), Left(Term), Left(ExprPrime)])
            }
            (ExprPrime, Some(Minus(info))) => {
                Some(vec![Right(Some(Minus(*info))), Left(Term), Left(ExprPrime)])
            }
            (ExprPrime, Some(RParen(_))) | (ExprPrime, None) => Some(vec![]),
            (Term, Some(LParen(_))) | (Term, Some(Number(_))) | (Term, Some(Identifier(_))) => {
                Some(vec![Left(Factor), Left(TermPrime)])
            }
            (TermPrime, Some(Star(info))) => Some(vec![
                Right(Some(Star(*info))),
                Left(Factor),
                Left(TermPrime),
            ]),
            (TermPrime, Some(Div(info))) => {
                Some(vec![Right(Some(Div(*info))), Left(Factor), Left(TermPrime)])
            }
            (TermPrime, Some(RParen(_)))
            | (TermPrime, None)
            | (TermPrime, Some(Plus(_)))
            | (TermPrime, Some(Minus(_))) => Some(vec![]),
            (Factor, Some(LParen(_info))) => todo!("I need to figure out how to add the ) here"),
            (Factor, Some(Number(info))) => Some(vec![Right(Some(Number(info.clone())))]),
            (Factor, Some(Identifier(info))) => Some(vec![Right(Some(Identifier(info.clone())))]),
            (_, _) => None,
        }
    }
}
