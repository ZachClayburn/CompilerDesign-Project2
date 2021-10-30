use super::{ast, Either, Left, Right, Token};
use ast::{
    reduce_binary_op, reduce_parenthetical, reduce_unary_operator, reduce_value, ReductionOp,
};
use log::{trace, warn};
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Clone)]
pub(super) enum NonTerminal {
    Goal,
    Expr,
    ExprPrime,
    Term,
    TermPrime,
    Factor,
    Atom,
    Reduction(ReductionOp),
}

pub(super) type ProductionItem = Either<NonTerminal, Token>;

pub struct Table {
    lookup: HashMap<(usize, usize), usize>,
}

impl Table {
    pub(super) fn new() -> Self {
        let terminals = vec!["+", "-", "*", "/", "(", ")", "name", "num"];

        let non_terminals = vec!["Goal", "Expr", "Expr'", "Term", "Term'", "Factor", "Atom"];

        let productions = vec![
            /* 0*/ ("Goal", vec!["Expr"]),
            /* 1*/ ("Expr", vec!["Term", "Expr'"]),
            /* 2*/ ("Expr'", vec!["+", "Term", "Expr'"]),
            /* 3*/ ("Expr'", vec!["-", "Term", "Expr'"]),
            /* 4*/ ("Expr'", vec![""]),
            /* 5*/ ("Term", vec!["Factor", "Term'"]),
            /* 6*/ ("Term'", vec!["*", "Factor", "Term'"]),
            /* 7*/ ("Term'", vec!["/", "Factor", "Term'"]),
            /* 8*/ ("Term'", vec![""]),
            /* 9*/ ("Factor", vec!["(", "Expr", ")"]),
            /*10*/ ("Factor", vec!["Atom"]),
            /*11*/ ("Factor", vec!["-", "Atom"]),
            /*12*/ ("Atom", vec!["num"]),
            /*13*/ ("Atom", vec!["name"]),
        ];

        let first = compute_first_set(&terminals, &non_terminals, &productions);

        let follow = compute_follow_set(&non_terminals, &productions, &first);

        let lookup =
            compute_lookup_table(&terminals, &non_terminals, &productions, &first, &follow);
        Self { lookup }
    }

    fn get_focus_number(&self, non_terminal: &NonTerminal) -> Option<usize> {
        let number = match non_terminal {
            NonTerminal::Goal => 0,
            NonTerminal::Expr => 1,
            NonTerminal::ExprPrime => 2,
            NonTerminal::Term => 3,
            NonTerminal::TermPrime => 4,
            NonTerminal::Factor => 5,
            NonTerminal::Atom => 6,
            NonTerminal::Reduction(_) => {
                warn!("trying to assign a focust number to a reduction operation!");
                return None;
            }
        };
        trace!("Assigning {:?} the focus number {}", non_terminal, number);
        Some(number)
    }

    fn get_word_number(&self, terminal: &Token) -> Option<usize> {
        let number = match terminal {
            Token::EOF => 0,
            Token::Plus(_) => 1,
            Token::Minus(_) => 2,
            Token::Star(_) => 3,
            Token::Div(_) => 4,
            Token::LParen(_) => 5,
            Token::RParen(_) => 6,
            Token::Identifier(_) => 7,
            Token::Number(_) => 8,
            unexpected => {
                warn!("Trying to assign a word number to {}", unexpected);
                return None;
            }
        };
        trace!("Assigning {} the word number {}", terminal, number);
        Some(number)
    }

    pub(super) fn at(&self, focus: &NonTerminal, word: &Token) -> Option<Vec<ProductionItem>> {
        use NonTerminal::*;
        use Token::*;
        match self
            .lookup
            .get(&(self.get_focus_number(focus)?, self.get_word_number(word)?))?
        {
            0 => {
                trace!("Running rule 0");
                Some(vec![Left(Expr)])
            }
            1 => {
                trace!("Running rule 1");
                Some(vec![Left(Term), Left(ExprPrime)])
            }
            2 => {
                trace!("Running rule 2");
                Some(vec![
                    Right(Plus(<_>::default())),
                    Left(Term),
                    Left(Reduction(reduce_binary_op)),
                    Left(ExprPrime),
                ])
            }
            3 => {
                trace!("Running rule 3");
                Some(vec![
                    Right(Minus(<_>::default())),
                    Left(Term),
                    Left(Reduction(reduce_binary_op)),
                    Left(ExprPrime),
                ])
            }
            4 => {
                trace!("Running rule 4");
                Some(vec![])
            }
            5 => {
                trace!("Running rule 5");
                Some(vec![Left(Factor), Left(TermPrime)])
            }
            6 => {
                trace!("Running rule 6");
                Some(vec![
                    Right(Star(<_>::default())),
                    Left(Factor),
                    Left(Reduction(reduce_binary_op)),
                    Left(TermPrime),
                ])
            }
            7 => {
                trace!("Running rule 7");
                Some(vec![
                    Right(Div(<_>::default())),
                    Left(Factor),
                    Left(Reduction(reduce_binary_op)),
                    Left(TermPrime),
                ])
            }
            8 => {
                trace!("Running rule 8");
                Some(vec![])
            }
            9 => {
                trace!("Running rule 9");
                Some(vec![
                    Right(LParen(<_>::default())),
                    Left(Expr),
                    Right(RParen(<_>::default())),
                    Left(Reduction(reduce_parenthetical)),
                ])
            }
            10 => {
                trace!("Running rule 10");
                Some(vec![Left(Atom)])
            }
            11 => {
                trace!("Running rule 11");
                Some(vec![
                    Right(Minus(<_>::default())),
                    Left(Atom),
                    Left(Reduction(reduce_unary_operator)),
                ])
            }
            12 => {
                trace!("Running rule 12");
                Some(vec![
                    Right(Number(<_>::default())),
                    Left(Reduction(reduce_value)),
                ])
            }
            13 => {
                trace!("Running rule 13");
                Some(vec![
                    Right(Identifier(<_>::default())),
                    Left(Reduction(reduce_value)),
                ])
            }
            _ => None,
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
            ("Factor", vec!["num"]),
            ("Factor", vec!["name"]),
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
            ("Factor", vec!["num"]),
            ("Factor", vec!["name"]),
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
            ("Factor", vec!["num"]),
            ("Factor", vec!["name"]),
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
            "((5, 7), 11)",
            "((5, 8), 10)",
        ];

        assert_eq!(lookup, expected);
    }
}
