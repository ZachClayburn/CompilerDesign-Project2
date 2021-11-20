mod location;
mod tokens;

pub use location::Location;
use peeking_take_while::PeekableExt;
use std::char;
use std::iter::Peekable;
use std::vec::IntoIter;
use std::{fs, io};
pub use tokens::{Token, TokenInfo};

/// An iterable struct that produces the tokens of the given file
pub struct Scanner {
    raw_text: Peekable<IntoIter<char>>,
    location: Location,
    done: bool,
}

#[derive(Debug, PartialEq)]
pub struct ScannerError {
    pub message: String,
    pub location: Location,
}

impl Scanner {
    pub fn from_text(text: &str) -> Peekable<Self> {
        Self {
            raw_text: text.chars().collect::<Vec<_>>().into_iter().peekable(),
            location: Location::default(),
            done: false,
        }
        .peekable()
    }

    #[allow(dead_code)]
    pub fn from_file(file_path: &str) -> io::Result<Peekable<Self>> {
        Ok(Self::from_text(&fs::read_to_string(file_path)?))
    }
}

pub type Result<T> = std::result::Result<T, ScannerError>;

fn is_newline(c: &char) -> bool {
    c == &'\n'
}

impl Iterator for Scanner {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else {
            Some(Ok(match self.next_impl() {
                Some(Ok(token)) => token,
                None => {
                    self.done = true;
                    Token::EOF
                }
                Some(Err(error)) => {
                    self.done = true;
                    return Some(Err(error));
                }
            }))
        }
    }
}

impl Scanner {
    fn next_impl(&mut self) -> Option<<Self as Iterator>::Item> {
        loop {
            let mut c = self.raw_text.next()?;
            self.location.next_col();
            let token = match c {
                c if c.is_whitespace() => {
                    if is_newline(&c) {
                        self.location.next_line();
                    }
                    continue;
                }
                '(' => Ok(Token::LParen(self.location)),
                ')' => Ok(Token::RParen(self.location)),
                '[' => Ok(Token::LBracket(self.location)),
                ']' => Ok(Token::RBracket(self.location)),
                '{' => Ok(Token::LBrace(self.location)),
                '}' => Ok(Token::RBrace(self.location)),
                ';' => Ok(Token::Semicolon(self.location)),
                '+' if self.raw_text.next_if_eq(&'-').is_some() => Err(ScannerError {
                    message: "+- is not allowed in the language!".into(),
                    location: self.location,
                }),
                '+' => Ok(Token::Plus(self.location)),
                '-' if self.raw_text.next_if_eq(&'-').is_some() => {
                    let next_token = Ok(Token::Decrement(self.location));
                    self.location.next_col();
                    next_token
                }
                '-' => Ok(Token::Minus(self.location)),
                '*' => Ok(Token::Star(self.location)),
                '/' if self.raw_text.next_if_eq(&'/').is_some() => {
                    while !is_newline(&c) {
                        c = self.raw_text.next()?
                    }
                    self.location.next_line();
                    continue;
                }
                '/' if self.raw_text.next_if_eq(&'*').is_some() => {
                    let start_pos = self.location.clone();
                    self.location.next_col();
                    loop {
                        let next_char = self.raw_text.next();
                        if let None = next_char {
                            return Some(Err(ScannerError {
                                message: "Unterminated block comment".into(),
                                location: start_pos,
                            }));
                        }
                        self.location.next_col();
                        c = next_char?;
                        if is_newline(&c) {
                            self.location.next_line();
                        }
                        if c == '*' && self.raw_text.next_if_eq(&'/').is_some() {
                            self.location.next_col();
                            break;
                        }
                    }
                    continue;
                }
                '/' => Ok(Token::Div(self.location)),
                '^' => Ok(Token::Pow(self.location)),
                ',' => Ok(Token::Comma(self.location)),
                '<' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::LessEqual(self.location));
                    self.location.next_col();
                    next_token
                }
                '<' => (Ok(Token::Less(self.location))),
                '>' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::GreaterEqual(self.location));
                    self.location.next_col();
                    next_token
                }
                '>' => (Ok(Token::Greater(self.location))),
                '=' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::Equal(self.location));
                    self.location.next_col();
                    next_token
                }
                '=' => (Ok(Token::Assign(self.location))),
                '!' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::NotEqual(self.location));
                    self.location.next_col();
                    next_token
                }
                '.' if self.raw_text.next_if_eq(&'.').is_some() => {
                    let next_token = Ok(Token::DoubleDot(self.location));
                    self.location.next_col();
                    next_token
                }
                '.' => (Ok(Token::Dot(self.location))),
                '"' => {
                    let start = self.location.clone();
                    let content: String = self
                        .raw_text
                        .by_ref()
                        .peeking_take_while(|x| x != &'"')
                        .collect();
                    let newlines = content.matches('\n').count();
                    self.location.advance_line(newlines);
                    if let Some((_, tail)) = content.rsplit_once('\n') {
                        self.location.advance_col(tail.len() + 1);
                    } else {
                        self.location.advance_col(content.len() + 1);
                    }
                    if let Some('"') = self.raw_text.next_if_eq(&'"') {
                        Ok(Token::StringLiteral(TokenInfo {
                            content,
                            start,
                            stop: self.location,
                        }))
                    } else {
                        self.done = true;
                        Err(ScannerError {
                            message: "Unterminated string!".into(),
                            location: start,
                        })
                    }
                }
                number if number.is_digit(10) => {
                    let start = self.location.clone();
                    let first_char = number.to_string();
                    let rest: String = self
                        .raw_text
                        .by_ref()
                        .peeking_take_while(|x| x.is_digit(10))
                        .collect();
                    self.location.advance_col(rest.len());
                    let content = first_char + &rest;
                    match self.raw_text.peek() {
                        Some(letter) if letter.is_alphabetic() => Err(ScannerError {
                            message: "Invalid Number".into(),
                            location: start,
                        }),
                        Some(dot) if dot == &'.' => {
                            self.raw_text.next();
                            self.location.next_col();
                            let rest: String = self
                                .raw_text
                                .by_ref()
                                .peeking_take_while(|x| x.is_digit(10))
                                .collect();
                            self.location.advance_col(rest.len());
                            let content = content + "." + &rest;
                            match self.raw_text.peek() {
                                Some(letter) if letter.is_alphabetic() => Err(ScannerError {
                                    message: "Invalid Number".into(),
                                    location: start,
                                }),
                                _ => Ok(Token::Float(TokenInfo {
                                    content,
                                    start,
                                    stop: self.location,
                                })),
                            }
                        }
                        _ => Ok(Token::Number(TokenInfo {
                            content,
                            start,
                            stop: self.location,
                        })),
                    }
                }
                letter if letter.is_ascii_alphabetic() => {
                    let start = self.location.clone();
                    let first_char = letter.to_string();
                    let rest: String = self
                        .raw_text
                        .by_ref()
                        .peeking_take_while(|x| x.is_ascii_alphanumeric() || x == &'_')
                        .collect();
                    self.location.advance_col(rest.len());
                    let content = first_char + &rest;
                    match content.as_str() {
                        "program" => Ok(Token::Program(start)),
                        "begin" => Ok(Token::Begin(start)),
                        "end" => Ok(Token::End(start)),
                        "switch" => Ok(Token::Switch(start)),
                        "case" => Ok(Token::Case(start)),
                        "default" => Ok(Token::Default(start)),
                        "write" => Ok(Token::Write(start)),
                        "read" => Ok(Token::Read(start)),
                        "for" => Ok(Token::For(start)),
                        "to" => Ok(Token::To(start)),
                        "step" => Ok(Token::Step(start)),
                        "do" => Ok(Token::Do(start)),
                        "if" => Ok(Token::If(start)),
                        "then" => Ok(Token::Then(start)),
                        "else" => Ok(Token::Else(start)),
                        "array" => Ok(Token::Array(start)),
                        "procedure" => Ok(Token::Procedure(start)),
                        "num" => Ok(Token::Num(start)),
                        "ish" => Ok(Token::Ish(start)),
                        "string" => Ok(Token::StringKeyWord(start)),
                        "return" => Ok(Token::Return(start)),
                        _ => Ok(Token::Identifier(TokenInfo {
                            content,
                            start,
                            stop: self.location,
                        })),
                    }
                }
                unexpected => Err(ScannerError {
                    message: format!(
                        "Unexpected token {}, ({:#X})",
                        unexpected, unexpected as i32
                    ),
                    location: self.location,
                }),
            };
            return Some(token);
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::path::PathBuf;
    use Token::*;

    fn get_test_dir() -> PathBuf {
        [env!("CARGO_MANIFEST_DIR"), "resources", "test"]
            .iter()
            .collect()
    }

    #[test]
    fn can_create_scanner_from_text() {
        let _ = Scanner::from_text("Here is some text!");
    }

    #[test]
    fn creating_a_scanner_from_non_existing_file_fails() {
        let mut d = get_test_dir();
        d.push("Not_A_Real_File");
        d.set_extension("txt");
        let scan = Scanner::from_file(&d.into_os_string().into_string().unwrap());
        assert!(scan.is_err())
    }

    #[test]
    fn creating_a_scanner_from_an_existing_file_works() {
        let mut d = get_test_dir();
        d.push("I_AM_A_REAL_FILE");
        d.set_extension("txt");
        let scan = Scanner::from_file(&d.into_os_string().into_string().unwrap());
        assert!(scan.is_ok())
    }

    #[test]
    fn can_lex_single_symbol_tokens() {
        let scan = Scanner::from_text("()[]{};+ */^,");
        let tokens: Vec<Token> = scan.map(|x| x.unwrap()).collect();
        let expected = vec![
            LParen(Location { line: 1, column: 1 }),
            RParen(Location { line: 1, column: 2 }),
            LBracket(Location { line: 1, column: 3 }),
            RBracket(Location { line: 1, column: 4 }),
            LBrace(Location { line: 1, column: 5 }),
            RBrace(Location { line: 1, column: 6 }),
            Semicolon(Location { line: 1, column: 7 }),
            Plus(Location { line: 1, column: 8 }),
            Star(Location {
                line: 1,
                column: 10,
            }),
            Div(Location {
                line: 1,
                column: 11,
            }),
            Pow(Location {
                line: 1,
                column: 12,
            }),
            Comma(Location {
                line: 1,
                column: 13,
            }),
            EOF,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_skip_whitespace() {
        let has_whitespace = indoc! { ";   \t;
        ;
        " };
        let scan = Scanner::from_text(has_whitespace);
        let tokens: Vec<Token> = scan.map(|x| x.unwrap()).collect();
        let expected = vec![
            Semicolon(Location { line: 1, column: 1 }),
            Semicolon(Location { line: 1, column: 6 }),
            Semicolon(Location { line: 2, column: 1 }),
            EOF,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_lex_one_or_two_character_tokens() {
        let scan = Scanner::from_text("< <= > >= = == != . .. - --");
        let tokens: Vec<Token> = scan.map(|x| x.unwrap()).collect();
        let expected = vec![
            Less(Location { line: 1, column: 1 }),
            LessEqual(Location { line: 1, column: 3 }),
            Greater(Location { line: 1, column: 6 }),
            GreaterEqual(Location { line: 1, column: 8 }),
            Assign(Location {
                line: 1,
                column: 11,
            }),
            Equal(Location {
                line: 1,
                column: 13,
            }),
            NotEqual(Location {
                line: 1,
                column: 16,
            }),
            Dot(Location {
                line: 1,
                column: 19,
            }),
            DoubleDot(Location {
                line: 1,
                column: 21,
            }),
            Minus(Location {
                line: 1,
                column: 24,
            }),
            Decrement(Location {
                line: 1,
                column: 26,
            }),
            EOF,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_lex_line_comments() {
        let comment_string = indoc! {"
            //This line is a comment!
            / // That / is not
            //This is as well
        "};
        let scan = Scanner::from_text(comment_string);
        let tokens: Vec<Token> = scan.map(|x| x.unwrap()).collect();
        let expected = vec![Div(Location { line: 2, column: 1 }), EOF];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_lex_block_comments() {
        let block_comment_string = indoc! {"
        /* This is a block comment, I should just keep going
         * skiping whatever I see, even if I see / or *
         * in fact the only thing that will stop me is those two
         * characters back it back likt this */
        /
        /* I should work on a single line */
        = /* I should be able to see tokens before and after me */ =
        /* This is an unterminated block comment, and should produce an error
        "};
        let scan = Scanner::from_text(block_comment_string);
        let tokens: Vec<<super::Scanner as Iterator>::Item> = scan.collect();
        let expected = vec![
            Ok(Div(Location { line: 5, column: 1 })),
            Ok(Assign(Location { line: 7, column: 1 })),
            Ok(Assign(Location {
                line: 7,
                column: 60,
            })),
            Err(ScannerError {
                message: "Unterminated block comment".into(),
                location: Location { line: 8, column: 1 },
            }),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn can_lex_strings() {
        let string_string = indoc! {r#"
            "This is a string" "The next string will be empty" ""
            "This string spans
            multiple lines"
            "This string is not termintated, and should result in an error
        "#};
        let scan = Scanner::from_text(string_string);
        let tokens: Vec<<super::Scanner as Iterator>::Item> = scan.collect();
        let expected = vec![
            Ok(StringLiteral(TokenInfo {
                content: "This is a string".to_string(),
                start: Location { line: 1, column: 1 },
                stop: Location {
                    line: 1,
                    column: 18,
                },
            })),
            Ok(StringLiteral(TokenInfo {
                content: "The next string will be empty".to_string(),
                start: Location {
                    line: 1,
                    column: 20,
                },
                stop: Location {
                    line: 1,
                    column: 50,
                },
            })),
            Ok(StringLiteral(TokenInfo {
                content: "".to_string(),
                start: Location {
                    line: 1,
                    column: 52,
                },
                stop: Location {
                    line: 1,
                    column: 53,
                },
            })),
            Ok(StringLiteral(TokenInfo {
                content: "This string spans\nmultiple lines".to_string(),
                start: Location { line: 2, column: 1 },
                stop: Location {
                    line: 3,
                    column: 15,
                },
            })),
            Err(ScannerError {
                message: "Unterminated string!".into(),
                location: Location { line: 4, column: 1 },
            }),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_lex_numbers() {
        let scan = Scanner::from_text("1 123 12a");
        let tokens: Vec<<super::Scanner as Iterator>::Item> = scan.collect();
        let expected = vec![
            Ok(Number(TokenInfo {
                content: "1".to_string(),
                start: Location { line: 1, column: 1 },
                stop: Location { line: 1, column: 1 },
            })),
            Ok(Number(TokenInfo {
                content: "123".to_string(),
                start: Location { line: 1, column: 3 },
                stop: Location { line: 1, column: 5 },
            })),
            Err(ScannerError {
                message: "Invalid Number".into(),
                location: Location { line: 1, column: 7 },
            }),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_lex_identifyers() {
        let scan = Scanner::from_text("notAKeyword Hasnumbers123 has_underscore");
        let tokens: Vec<Token> = scan.map(|x| x.unwrap()).collect();
        let expected = vec![
            Identifier(TokenInfo {
                content: "notAKeyword".to_string(),
                start: Location { line: 1, column: 1 },
                stop: Location {
                    line: 1,
                    column: 11,
                },
            }),
            Identifier(TokenInfo {
                content: "Hasnumbers123".to_string(),
                start: Location {
                    line: 1,
                    column: 13,
                },
                stop: Location {
                    line: 1,
                    column: 25,
                },
            }),
            Identifier(TokenInfo {
                content: "has_underscore".to_string(),
                start: Location {
                    line: 1,
                    column: 27,
                },
                stop: Location {
                    line: 1,
                    column: 40,
                },
            }),
            EOF,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_lex_keywords() {
        let scan = Scanner::from_text(indoc! {"
            program
            begin
            end
            switch
            case
            default
            write
            read
            for
            to
            step
            do
            if
            then
            else
            array
            procedure
            num
            ish
            string
            return"
        });
        let tokens: Vec<Token> = scan.map(|x| x.unwrap()).collect();
        let expected = vec![
            Program(Location { line: 1, column: 1 }),
            Begin(Location { line: 2, column: 1 }),
            End(Location { line: 3, column: 1 }),
            Switch(Location { line: 4, column: 1 }),
            Case(Location { line: 5, column: 1 }),
            Default(Location { line: 6, column: 1 }),
            Write(Location { line: 7, column: 1 }),
            Read(Location { line: 8, column: 1 }),
            For(Location { line: 9, column: 1 }),
            To(Location {
                line: 10,
                column: 1,
            }),
            Step(Location {
                line: 11,
                column: 1,
            }),
            Do(Location {
                line: 12,
                column: 1,
            }),
            If(Location {
                line: 13,
                column: 1,
            }),
            Then(Location {
                line: 14,
                column: 1,
            }),
            Else(Location {
                line: 15,
                column: 1,
            }),
            Array(Location {
                line: 16,
                column: 1,
            }),
            Procedure(Location {
                line: 17,
                column: 1,
            }),
            Num(Location {
                line: 18,
                column: 1,
            }),
            Ish(Location {
                line: 19,
                column: 1,
            }),
            StringKeyWord(Location {
                line: 20,
                column: 1,
            }),
            Return(Location {
                line: 21,
                column: 1,
            }),
            EOF,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn plus_minus_is_rejected() {
        let scan = Scanner::from_text("+-");
        let tokens = scan.collect::<Vec<_>>();
        let expected = vec![Err(ScannerError {
            message: "+- is not allowed in the language!".into(),
            location: Location { line: 1, column: 1 },
        })];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn can_parse_floating_point_numbers() {
        let scan = Scanner::from_text(indoc! {"
            12.34
            123.
        "});
        let tokens: Vec<Token> = scan.map(|x| x.unwrap()).collect();
        let expected = vec![
            Float(TokenInfo {
                content: "12.34".into(),
                start: Location { line: 1, column: 1 },
                stop: Location { line: 1, column: 5 },
            }),
            Float(TokenInfo {
                content: "123.".into(),
                start: Location { line: 2, column: 1 },
                stop: Location { line: 2, column: 4 },
            }),
            EOF,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn float_with_trailing_illegal_char_rejected() {
        let scan = Scanner::from_text("12.34a");
        let tokens = scan.collect::<Vec<_>>();
        let expected = vec![Err(ScannerError {
            message: "Invalid Number".into(),
            location: Location { line: 1, column: 1 },
        })];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn correctly_handles_eof_with_trailing_comment() {
        let scan = Scanner::from_text("X//");
        let tokens = scan.map(|x| x.unwrap()).collect::<Vec<_>>();
        let expected = vec![
            Identifier(TokenInfo {
                content: "X".into(),
                start: Location { line: 1, column: 1 },
                stop: Location { line: 1, column: 1 },
            }),
            EOF,
        ];
        assert_eq!(tokens, expected);
    }
}
