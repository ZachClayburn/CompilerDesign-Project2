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

type TokenSets<'a> = HashMap<&'a str, HashSet<&'a str>>;

fn compute_first_set<'a>(
    terminals: &Vec<&'a str>,
    non_terminals: &Vec<&'a str>,
    productions: &Vec<(&'a str, Vec<&'a str>)>,
) -> TokenSets<'a> {
    let mut first = HashMap::from([("EOF", HashSet::from(["EOF"])), ("", HashSet::from([""]))]);

    for terminal in terminals {
        first.entry(terminal).or_insert(HashSet::from([*terminal]));
    }

    for non_terminal in non_terminals {
        first.entry(non_terminal).or_insert(HashSet::new());
    }

    loop {
        let last_loop = first.clone();
        for (non_terminal, items) in productions {
            let k = items.len() - 1;
            let mut rhs = first.get(items[0]).unwrap() - &HashSet::from([""]);
            for i in 0..(k + 1) {
                if i + 1 <= k {
                    rhs = &rhs
                        | &(match first.get(items[i]) {
                            Some(stuff) if stuff.contains("") => {
                                first.get(items[i + 1]).unwrap() - &HashSet::from([""])
                            }
                            _ => break,
                        });
                } else if first.get(items[k]).unwrap().contains("") {
                    rhs = &rhs | &HashSet::from([""]);
                }
            }
            let updated = first.entry(&non_terminal).or_default();
            *updated = &*updated | &rhs;
        }
        if last_loop == first {
            break;
        }
    }
    first
}

fn compute_follow_set<'a>(
    non_terminals: &Vec<&'a str>,
    productions: &Vec<(&'a str, Vec<&'a str>)>,
    first: &TokenSets<'a>,
) -> TokenSets<'a> {
    let mut follow = TokenSets::default();

    follow
        .entry(non_terminals.first().unwrap())
        .or_insert(["EOF"].into());

    for non_terminal in non_terminals.iter().skip(1) {
        follow.entry(non_terminal).or_default();
    }

    loop {
        let last_loop = follow.clone();
        for (non_terminal, items) in productions {
            let mut trailer = follow.get(non_terminal).unwrap().clone();
            for item in items.iter().rev() {
                if non_terminals.contains(item) {
                    let updated = follow.entry(&item).or_default();
                    *updated = &*updated | &trailer;
                    trailer = match first.get(item) {
                        Some(set) if set.contains("") => &trailer | &(set - &HashSet::from([""])),
                        Some(set) => set.clone(),
                        _ => unreachable!("{} wasn't in the first sets", item),
                    }
                } else {
                    trailer = first.get(item).unwrap().clone();
                }
            }
        }
        if last_loop == follow {
            break;
        }
    }
    follow
}

fn compute_lookup_table(
    terminals: &Vec<&str>,
    non_terminals: &Vec<&str>,
    productions: &Vec<(&str, Vec<&str>)>,
    first: &TokenSets,
    follow: &TokenSets,
) -> HashMap<(usize, usize), usize> {
    let mut lookup = HashMap::new();
    let terminals = {
        let mut temp = vec!["EOF"];
        temp.append(&mut terminals.clone());
        temp
    };

    for (productoion_num, (non_terminal, items)) in productions.iter().enumerate() {
        let beta = items.first().unwrap();
        let first_at_beta = first.get(beta).unwrap();
        let first_plus = if !first_at_beta.contains("") {
            first_at_beta.clone()
        } else {
            first_at_beta | follow.get(non_terminal).unwrap()
        };
        for terminal in first_plus.iter().filter(|x| !x.is_empty()) {
            lookup
                .entry((
                    non_terminals
                        .iter()
                        .position(|x| x == non_terminal)
                        .unwrap_or_else(|| panic!("Error finding non-terminal {:?}", non_terminal)),
                    terminals
                        .iter()
                        .position(|x| x == terminal)
                        .unwrap_or_else(|| panic!("Error finding terminal {:?}", terminal)),
                ))
                .or_insert(productoion_num);
        }
    }

    lookup
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn token_sets_to_sorted_vectors(mut sets: TokenSets) -> Vec<(&str, Vec<&str>)> {
        let mut temp = sets
            .drain()
            .map(|(a, mut b)| {
                let mut b = b.drain().collect::<Vec<_>>();
                b.sort();
                (a, b)
            })
            .collect::<Vec<_>>();
        temp.sort_by(|(a, _), (b, _)| a.cmp(b));
        temp
    }

    fn sorted(mut input: Vec<&str>) -> Vec<&str> {
        input.sort();
        input
    }

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

        let first = token_sets_to_sorted_vectors(compute_first_set(
            &terminals,
            &non_terminals,
            &productions,
        ));

        let expected = vec![
            ("", vec![""]),
            ("(", vec!["("]),
            (")", vec![")"]),
            ("*", vec!["*"]),
            ("+", vec!["+"]),
            ("-", vec!["-"]),
            ("/", vec!["/"]),
            ("EOF", vec!["EOF"]),
            ("Expr", vec!["(", "name", "num"]),
            ("Expr'", vec!["", "+", "-"]),
            ("Factor", vec!["(", "name", "num"]),
            ("Goal", vec!["(", "name", "num"]),
            ("Term", vec!["(", "name", "num"]),
            ("Term'", vec!["", "*", "/"]),
            ("name", vec!["name"]),
            ("num", vec!["num"]),
        ];
        assert_eq!(first, expected);
    }

    #[test]
    fn can_replicate_books_follow_set() {
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

        let first = compute_first_set(&terminals, &non_terminals, &productions);

        let follow =
            token_sets_to_sorted_vectors(compute_follow_set(&non_terminals, &productions, &first));

        let expected = vec![
            ("Expr", sorted(vec!["EOF", ")"])),
            ("Expr'", sorted(vec!["EOF", ")"])),
            ("Factor", sorted(vec!["EOF", ")", "+", "-", "*", "/"])),
            ("Goal", sorted(vec!["EOF"])),
            ("Term", sorted(vec!["EOF", ")", "+", "-"])),
            ("Term'", sorted(vec!["EOF", ")", "+", "-"])),
        ];
        assert_eq!(follow, expected);
    }

    #[test]
    fn can_replicate_books_lookup_table() {
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

        let first = compute_first_set(&terminals, &non_terminals, &productions);

        let follow = compute_follow_set(&non_terminals, &productions, &first);

        let lookup = {
            let mut temp =
                compute_lookup_table(&terminals, &non_terminals, &productions, &first, &follow)
                    .drain()
                    .collect::<Vec<_>>();
            temp.sort();
            temp.iter()
                .map(|((a, b), c)| format!("(({}, {}), {})", a, b, c))
                .collect::<Vec<_>>()
        };

        let expected = vec![
            "((0, 5), 0)",
            "((0, 7), 0)",
            "((0, 8), 0)",
            "((1, 5), 1)",
            "((1, 7), 1)",
            "((1, 8), 1)",
            "((2, 0), 4)",
            "((2, 1), 2)",
            "((2, 2), 3)",
            "((2, 6), 4)",
            "((3, 5), 5)",
            "((3, 7), 5)",
            "((3, 8), 5)",
            "((4, 0), 8)",
            "((4, 1), 8)",
            "((4, 2), 8)",
            "((4, 3), 6)",
            "((4, 4), 7)",
            "((4, 6), 8)",
            "((5, 5), 9)",
            "((5, 7), 10)",
            "((5, 8), 11)",
        ];

        assert_eq!(lookup, expected);
    }
}
