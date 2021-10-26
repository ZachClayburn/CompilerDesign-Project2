use super::{Item, NonTerminal, Token};
use NonTerminal::*;
use Token::*;

pub struct Table {}

impl Table {
    pub fn at(&self, focus: &NonTerminal, word: &Option<Token>) -> Option<Vec<Item>> {
        match (focus, word) {
            (Goal, Some(LParen(_))) | (Goal, Some(Number(_))) | (Goal, Some(Identifier(_))) => {
                todo!("0")
            }
            (_, _) => None,
        }
    }
}
