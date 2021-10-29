use log::trace;

use super::{ast, Either, Left, Right, Token};
use ast::{reduce_binary_op, reduce_parenthetical, reduce_value, ReductionOp};
use std::collections::{HashMap, HashSet};

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

pub fn compute_first_set<'a>(
    terminals: Vec<&'a str>,
    non_terminals: Vec<&'a str>,
    productions: Vec<(&'a str, Vec<&'a str>)>,
) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut first = HashMap::from([("EOF", HashSet::from(["EOF"])), ("", HashSet::from([""]))]);

    for terminal in &terminals {
        first.entry(terminal).or_insert(HashSet::from([*terminal]));
    }

    for non_terminal in &non_terminals {
        first.entry(non_terminal).or_insert(HashSet::new());
    }

    loop {
        let last_loop = first.clone();
        for (terminal, items) in &productions {
            println!("{}, {:?}", terminal, items);
            if !items.is_empty()
                && items
                    .iter()
                    .all(|x| terminals.contains(x) || non_terminals.contains(x) || x == &"")
            {
                let k = items.len() - 1;
                let mut rhs = first.get(items[0]).unwrap() - &HashSet::from([""]);
                let mut i = 0;
                loop {
                    match first.get(items[i]) {
                        Some(stuff) if stuff.contains("") && i + 1 <= k => {
                            rhs = &rhs | &(first.get(items[i + 1]).unwrap() - &HashSet::from([""]));
                            i += 1;
                        }
                        _ => break,
                    }
                }
                println!("i = {}, k = {}", i, k);
                if i == k && first.get(items[k]).unwrap().contains("") {
                    println!(r#"adding "" "#);
                    rhs = &rhs | &HashSet::from([""]);
                }
                let updated = first.entry(&terminal).or_default();
                *updated = &*updated | &rhs;
            }
        }
        if last_loop == first {
            break;
        }
    }

    first
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn can_replicate_books_first_set() {
        let terminals = vec!["+", "-", "*", "/", "(", ")", "name", "num"];

        let non_terminals = vec!["Goal", "Expr", "Expr'", "Term", "Term'", "Factor"];

        let productions = vec![
            ("Goal", vec!["Expr"]),
            ("Expr", vec!["Term", "Expr'"]),
            ("Expr'", vec!["+", "Term", "Expr'"]),
            ("Expr'", vec!["-", "Term", "Expr'"]),
            ("Expr'", vec![""]),
            ("Term", vec!["Factor", "Term'"]),
            ("Term'", vec!["*", "Factor", "Term'"]),
            ("Term'", vec!["/", "Factor", "Term'"]),
            ("Term'", vec![""]),
            ("Factor", vec!["(", "Expr", ")"]),
            ("Factor", vec!["name"]),
            ("Factor", vec!["num"]),
        ];

        let first = {
            let mut temp = compute_first_set(terminals, non_terminals, productions)
                .drain()
                .map(|(a, mut b)| {
                    let mut b = b.drain().collect::<Vec<_>>();
                    b.sort();
                    (a, b)
                })
                .collect::<Vec<_>>();
            temp.sort_by(|(a, _), (b, _)| a.cmp(b));
            temp
        };

        let expected = {
            let mut temp = vec![
                ("+", vec!["+"]),
                ("-", vec!["-"]),
                ("*", vec!["*"]),
                ("/", vec!["/"]),
                ("(", vec!["("]),
                (")", vec![")"]),
                ("name", vec!["name"]),
                ("num", vec!["num"]),
                ("EOF", vec!["EOF"]),
                ("", vec![""]),
                ("Goal", vec!["(", "name", "num"]),
                ("Expr", vec!["(", "name", "num"]),
                ("Expr'", vec!["", "+", "-"]),
                ("Term", vec!["(", "name", "num"]),
                ("Term'", vec!["", "*", "/"]),
                ("Factor", vec!["(", "name", "num"]),
            ];
            temp.sort_by(|(a, _), (b, _)| a.cmp(b));
            temp
        };
        assert_eq!(first, expected);
    }
}
