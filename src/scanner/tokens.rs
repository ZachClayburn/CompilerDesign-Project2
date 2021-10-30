use super::Location;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    pub content: String,
    pub start: Location,
    pub stop: Location,
}

impl Default for TokenInfo {
    fn default() -> Self {
        Self {
            content: "".into(),
            start: <_>::default(),
            stop: <_>::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Complex tokens
    Number(TokenInfo),
    Float(TokenInfo),
    Identifier(TokenInfo),
    StringLiteral(TokenInfo),
    // keywords
    Program(Location),
    Begin(Location),
    End(Location),
    Switch(Location),
    Case(Location),
    Default(Location),
    Write(Location),
    Read(Location),
    For(Location),
    To(Location),
    Step(Location),
    Do(Location),
    If(Location),
    Then(Location),
    Else(Location),
    Array(Location),
    Procedure(Location),
    Num(Location),
    StringKeyWord(Location),
    Return(Location),
    // Symbols
    LParen(Location),
    RParen(Location),
    LBracket(Location),
    RBracket(Location),
    LBrace(Location),
    RBrace(Location),
    Semicolon(Location),
    Assign(Location),
    Plus(Location),
    Minus(Location),
    Decrement(Location),
    Star(Location),
    Div(Location),
    Pow(Location),
    Less(Location),
    Greater(Location),
    LessEqual(Location),
    GreaterEqual(Location),
    Equal(Location),
    NotEqual(Location),
    Dot(Location),
    DoubleDot(Location),
    Comma(Location),
    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(TokenInfo {
                content,
                start: _,
                stop: _,
            }) => write!(f, "Number: {}", content),
            Token::Float(TokenInfo {
                content,
                start: _,
                stop: _,
            }) => write!(f, "Float: {}", content),
            Token::Identifier(TokenInfo {
                content,
                start: _,
                stop: _,
            }) => write!(f, "Identifier: {}", content),
            Token::StringLiteral(TokenInfo {
                content,
                start: _,
                stop: _,
            }) => write!(f, "StringLiteral: {}", content),
            Token::Program(_) => write!(f, "Program"),
            Token::Begin(_) => write!(f, "Begin"),
            Token::End(_) => write!(f, "End"),
            Token::Switch(_) => write!(f, "Switch"),
            Token::Case(_) => write!(f, "Case"),
            Token::Default(_) => write!(f, "Default"),
            Token::Write(_) => write!(f, "Write"),
            Token::Read(_) => write!(f, "Read"),
            Token::For(_) => write!(f, "For"),
            Token::To(_) => write!(f, "To"),
            Token::Step(_) => write!(f, "Step"),
            Token::Do(_) => write!(f, "Do"),
            Token::If(_) => write!(f, "If"),
            Token::Then(_) => write!(f, "Then"),
            Token::Else(_) => write!(f, "Else"),
            Token::Array(_) => write!(f, "Array"),
            Token::Procedure(_) => write!(f, "Procedure"),
            Token::Num(_) => write!(f, "Num"),
            Token::StringKeyWord(_) => write!(f, "String"),
            Token::Return(_) => write!(f, "Return"),
            Token::LParen(_) => write!(f, "("),
            Token::RParen(_) => write!(f, ")"),
            Token::LBracket(_) => write!(f, "["),
            Token::RBracket(_) => write!(f, "]"),
            Token::LBrace(_) => write!(f, "{}", "{"),
            Token::RBrace(_) => write!(f, "{}", "}"),
            Token::Semicolon(_) => write!(f, ";"),
            Token::Assign(_) => write!(f, "="),
            Token::Plus(_) => write!(f, "+"),
            Token::Minus(_) => write!(f, "-"),
            Token::Decrement(_) => write!(f, "--"),
            Token::Star(_) => write!(f, "*"),
            Token::Div(_) => write!(f, "/"),
            Token::Pow(_) => write!(f, "^"),
            Token::Less(_) => write!(f, "<"),
            Token::Greater(_) => write!(f, ">"),
            Token::LessEqual(_) => write!(f, "<="),
            Token::GreaterEqual(_) => write!(f, ">="),
            Token::Equal(_) => write!(f, "=="),
            Token::NotEqual(_) => write!(f, "!="),
            Token::Dot(_) => write!(f, "."),
            Token::DoubleDot(_) => write!(f, ".."),
            Token::Comma(_) => write!(f, ","),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl Token {
    pub fn format_location(&self) -> String {
        match self {
            Token::Number(TokenInfo {
                content: _,
                start: loc,
                stop: _,
            })
            | Token::Float(TokenInfo {
                content: _,
                start: loc,
                stop: _,
            })
            | Token::Identifier(TokenInfo {
                content: _,
                start: loc,
                stop: _,
            })
            | Token::StringLiteral(TokenInfo {
                content: _,
                start: loc,
                stop: _,
            })
            | Token::Program(loc)
            | Token::Begin(loc)
            | Token::End(loc)
            | Token::Switch(loc)
            | Token::Case(loc)
            | Token::Default(loc)
            | Token::Write(loc)
            | Token::Read(loc)
            | Token::For(loc)
            | Token::To(loc)
            | Token::Step(loc)
            | Token::Do(loc)
            | Token::If(loc)
            | Token::Then(loc)
            | Token::Else(loc)
            | Token::Array(loc)
            | Token::Procedure(loc)
            | Token::Num(loc)
            | Token::StringKeyWord(loc)
            | Token::Return(loc)
            | Token::LParen(loc)
            | Token::RParen(loc)
            | Token::LBracket(loc)
            | Token::RBracket(loc)
            | Token::LBrace(loc)
            | Token::RBrace(loc)
            | Token::Semicolon(loc)
            | Token::Assign(loc)
            | Token::Plus(loc)
            | Token::Minus(loc)
            | Token::Decrement(loc)
            | Token::Star(loc)
            | Token::Div(loc)
            | Token::Pow(loc)
            | Token::Less(loc)
            | Token::Greater(loc)
            | Token::LessEqual(loc)
            | Token::GreaterEqual(loc)
            | Token::Equal(loc)
            | Token::NotEqual(loc)
            | Token::Dot(loc)
            | Token::DoubleDot(loc)
            | Token::Comma(loc) => format!("{}:{}", loc.line, loc.column),
            Token::EOF => format!("EOF"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::scanner::Scanner;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn tokens_format_correctly() {
        let scan = Scanner::from_text(
            r#"
            program begin end switch case default write read for to step do if then else array
            procedure num string return ()[]{};=+ -*/^<><=>===.!=..,
            identifier 1234 12.34
            "String"
            "#,
        );
        let formatted = scan
            .map(|x| x.unwrap())
            .map(|x| format!("{}", x))
            .collect::<Vec<_>>();
        let expected = vec![
            "Program",
            "Begin",
            "End",
            "Switch",
            "Case",
            "Default",
            "Write",
            "Read",
            "For",
            "To",
            "Step",
            "Do",
            "If",
            "Then",
            "Else",
            "Array",
            "Procedure",
            "Num",
            "String",
            "Return",
            "(",
            ")",
            "[",
            "]",
            "{",
            "}",
            ";",
            "=",
            "+",
            "-",
            "*",
            "/",
            "^",
            "<",
            ">",
            "<=",
            ">=",
            "==",
            ".",
            "!=",
            "..",
            ",",
            "Identifier: identifier",
            "Number: 1234",
            "Float: 12.34",
            "StringLiteral: String",
            "EOF",
        ];
        assert_eq!(formatted, expected);
    }

    #[test]
    fn token_location_formats_correctly() {
        let scan = Scanner::from_text(indoc! {r#"
            .   .
            . . .
                hello 1234
             "hi"
        "#});
        let formatted = scan
            .map(|x| x.unwrap().format_location())
            .collect::<Vec<_>>();
        let expected = vec!["1:1", "1:5", "2:1", "2:3", "2:5", "3:5", "3:11", "4:2", "EOF"];
        assert_eq!(formatted, expected);
    }
}
