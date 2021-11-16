use super::{ast, Either, Left, Right, Token};
use ast::{
    reduce_assignment, reduce_binary_op, reduce_parenthetical, reduce_program,
    reduce_statement_list, reduce_unary_operator, reduce_value, ReductionOp,
};
use log::{error, trace, warn};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::mem::discriminant;

#[derive(Debug, PartialEq, Clone, Copy, Eq, PartialOrd, Ord)]
pub(super) enum NonTerminal {
    Goal,
    Prog,
    StatementList,
    NumAssignment,
    Expr,
    ExprPrime,
    Term,
    TermPrime,
    Power,
    PowerPrime,
    Factor,
    Atom,
    Reduction(ReductionOp),
}

impl Display for NonTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Error;
        match self {
            NonTerminal::Goal => write!(f, "Goal"),
            NonTerminal::Prog => write!(f, "Prog"),
            NonTerminal::StatementList => write!(f, "StatementList"),
            NonTerminal::NumAssignment => write!(f, "NumAssignment"),
            NonTerminal::Expr => write!(f, "Expr"),
            NonTerminal::ExprPrime => write!(f, "Expr'"),
            NonTerminal::Term => write!(f, "Term"),
            NonTerminal::TermPrime => write!(f, "Term'"),
            NonTerminal::Power => write!(f, "Power"),
            NonTerminal::PowerPrime => write!(f, "Power'"),
            NonTerminal::Factor => write!(f, "Factor"),
            NonTerminal::Atom => write!(f, "Atom"),
            NonTerminal::Reduction(_) => {
                error!("Cannot format a reduction!");
                Err(Error {})
            }
        }
    }
}

pub(super) type ProductionItem = Either<NonTerminal, Token>;

pub struct Table {
    lookup: HashMap<(usize, usize), usize>,
    rules: Vec<Vec<ProductionItem>>,
    terminals: Vec<Token>,
    non_terminals: Vec<NonTerminal>,
}

impl Table {
    pub(super) fn new() -> Self {
        use NonTerminal::*;
        use Token::*;
        let productions = {
            vec![
                (Goal, vec![Left(Expr)]),
                (
                    Prog,
                    vec![
                        Right(Program(<_>::default())),
                        Right(Identifier(<_>::default())),
                        Right(Semicolon(<_>::default())),
                        Right(Begin(<_>::default())),
                        Left(StatementList),
                        Right(End(<_>::default())),
                        Right(Dot(<_>::default())),
                        Left(Reduction(reduce_program)),
                    ],
                ),
                (StatementList, vec![Left(NumAssignment), Left(StatementList)]),
                (StatementList, vec![Left(Reduction(reduce_statement_list))]),
                (
                    NumAssignment,
                    vec![
                        Right(Num(<_>::default())),
                        Right(Identifier(<_>::default())),
                        Right(Assign(<_>::default())),
                        Left(Expr),
                        Right(Semicolon(<_>::default())),
                        Left(Reduction(reduce_assignment)),
                    ],
                ),
                (Expr, vec![Left(Term), Left(ExprPrime)]),
                (
                    ExprPrime,
                    vec![
                        Right(Plus(<_>::default())),
                        Left(Term),
                        Left(Reduction(reduce_binary_op)),
                        Left(ExprPrime),
                    ],
                ),
                (
                    ExprPrime,
                    vec![
                        Right(Minus(<_>::default())),
                        Left(Term),
                        Left(Reduction(reduce_binary_op)),
                        Left(ExprPrime),
                    ],
                ),
                (ExprPrime, vec![]),
                (Term, vec![Left(Power), Left(TermPrime)]),
                (
                    TermPrime,
                    vec![
                        Right(Star(<_>::default())),
                        Left(Power),
                        Left(Reduction(reduce_binary_op)),
                        Left(TermPrime),
                    ],
                ),
                (
                    TermPrime,
                    vec![
                        Right(Div(<_>::default())),
                        Left(Power),
                        Left(Reduction(reduce_binary_op)),
                        Left(TermPrime),
                    ],
                ),
                (TermPrime, vec![]),
                (Power, vec![Left(Factor), Left(PowerPrime)]),
                (
                    PowerPrime,
                    vec![
                        Right(Pow(<_>::default())),
                        Left(Factor),
                        Left(Reduction(reduce_binary_op)),
                        Left(PowerPrime),
                    ],
                ),
                (PowerPrime, vec![]),
                (
                    Factor,
                    vec![
                        Right(LParen(<_>::default())),
                        Left(Expr),
                        Right(RParen(<_>::default())),
                        Left(Reduction(reduce_parenthetical)),
                    ],
                ),
                (Factor, vec![Left(Atom)]),
                (
                    Factor,
                    vec![
                        Right(Minus(<_>::default())),
                        Left(Atom),
                        Left(Reduction(reduce_unary_operator)),
                    ],
                ),
                (
                    Atom,
                    vec![Right(Number(<_>::default())), Left(Reduction(reduce_value))],
                ),
                (
                    Atom,
                    vec![
                        Right(Identifier(<_>::default())),
                        Left(Reduction(reduce_value)),
                    ],
                ),
                (
                    Atom,
                    vec![Right(Float(<_>::default())), Left(Reduction(reduce_value))],
                ),
            ]
        };
        let non_terminals = {
            let mut temp = productions.iter().map(|(nt, _)| *nt).collect::<Vec<_>>();
            temp.sort();
            temp.dedup();
            temp
        };
        let rules = productions
            .iter()
            .map(|(_, rule)| rule.clone())
            .collect::<Vec<_>>();
        let terminals = {
            let mut temp = rules
                .iter()
                .flatten()
                .filter_map(|i| i.clone().right())
                .collect::<Vec<_>>();
            temp.sort();
            temp.dedup();
            temp
        };

        let formatted_terminals = terminals
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<_>>();
        let terminal_strings = formatted_terminals.iter().map(|s| s.as_str()).collect();

        let formatted_non_terminals = non_terminals
            .iter()
            .map(|nt| format!("{}", nt))
            .collect::<Vec<_>>();
        let non_terminal_strings = formatted_non_terminals.iter().map(|s| s.as_str()).collect();

        let formatted_productions = productions
            .iter()
            .map(|(lhs, rhs)| {
                (format!("{}", lhs), {
                    let temp = rhs
                        .iter()
                        .filter_map(|item| match item {
                            Left(Reduction(_)) => None,
                            Left(item) => Some(format!("{}", item)),
                            Right(item) => Some(format!("{}", item)),
                        })
                        .collect::<Vec<_>>();
                    if temp.is_empty() {
                        vec!["".into()]
                    } else {
                        temp
                    }
                })
            })
            .collect::<Vec<_>>();
        let production_strings = formatted_productions
            .iter()
            .map(|(lhs, rhs)| (lhs.as_str(), rhs.iter().map(|s| s.as_str()).collect()))
            .collect::<Vec<_>>();

        let first = compute_first_set(
            &terminal_strings,
            &non_terminal_strings,
            &production_strings,
        );

        let follow = compute_follow_set(&non_terminal_strings, &production_strings, &first);

        let lookup = compute_lookup_table(
            &terminal_strings,
            &non_terminal_strings,
            &production_strings,
            &first,
            &follow,
        );
        Self {
            lookup,
            rules,
            terminals,
            non_terminals,
        }
    }

    fn get_focus_number(&self, non_terminal: &NonTerminal) -> Option<usize> {
        let d = discriminant(non_terminal);
        if let NonTerminal::Reduction(_) = non_terminal {
            warn!("trying to assign a focus number to a reduction operation!");
            None
        } else if let Some(number) = self.non_terminals.iter().position(|x| discriminant(x) == d) {
            trace!("Assigning {} the focus number {}", non_terminal, number);
            Some(number)
        } else {
            warn!(
                "trying to assign a focus number to the unknown non-terminal {}",
                non_terminal
            );
            None
        }
    }

    fn get_word_number(&self, terminal: &Token) -> Option<usize> {
        let d = discriminant(terminal);
        let number = if let Token::EOF = terminal {
            0
        } else if let Some(number) = self.terminals.iter().position(|x| discriminant(x) == d) {
            number + 1
        } else {
            warn!("Trying to assign a word number to {}", terminal);
            return None;
        };
        trace!("Assigning {} the word number {}", terminal, number);
        Some(number)
    }

    pub(super) fn at(&self, focus: &NonTerminal, word: &Token) -> Option<Vec<ProductionItem>> {
        let &index = self
            .lookup
            .get(&(self.get_focus_number(focus)?, self.get_word_number(word)?))?;
        trace!("Runnin rule {}", index);
        self.rules.get(index).map(|x| x.clone())
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
            for i in 0..=k {
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
