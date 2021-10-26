use super::{Item, NonTerminal, Token};
use NonTerminal::*;
use Token::*;

pub struct Table {}

impl Table {
    pub fn at(&self, focus: &NonTerminal, word: &Option<Token>) -> Option<Vec<Item>> {
        match (focus, word) {
            (Goal, Some(LParen(_)))
            | (
                Goal,
                Some(Number {
                    content: _,
                    start: _,
                    stop: _,
                }),
            )
            | (
                Goal,
                Some(Identifier {
                    content: _,
                    start: _,
                    stop: _,
                }),
            ) => todo!("0"),
            (_, _) => None,
        }
    }
}
