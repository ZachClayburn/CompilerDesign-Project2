use super::Location;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    Ish(Location),
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
            Token::Program(_) => "Program".fmt(f),
            Token::Begin(_) => "Begin".fmt(f),
            Token::End(_) => "End".fmt(f),
            Token::Switch(_) => "Switch".fmt(f),
            Token::Case(_) => "Case".fmt(f),
            Token::Default(_) => "Default".fmt(f),
            Token::Write(_) => "Write".fmt(f),
            Token::Read(_) => "Read".fmt(f),
            Token::For(_) => "For".fmt(f),
            Token::To(_) => "To".fmt(f),
            Token::Step(_) => "Step".fmt(f),
            Token::Do(_) => "Do".fmt(f),
            Token::If(_) => "If".fmt(f),
            Token::Then(_) => "Then".fmt(f),
            Token::Else(_) => "Else".fmt(f),
            Token::Array(_) => "Array".fmt(f),
            Token::Procedure(_) => "Procedure".fmt(f),
            Token::Num(_) => "Num".fmt(f),
            Token::Ish(_) => "Ish".fmt(f),
            Token::StringKeyWord(_) => "String".fmt(f),
            Token::Return(_) => "Return".fmt(f),
            Token::LParen(_) => "(".fmt(f),
            Token::RParen(_) => ")".fmt(f),
            Token::LBracket(_) => "[".fmt(f),
            Token::RBracket(_) => "]".fmt(f),
            Token::LBrace(_) => write!(f, "{}", "{"),
            Token::RBrace(_) => write!(f, "{}", "}"),
            Token::Semicolon(_) => ";".fmt(f),
            Token::Assign(_) => "=".fmt(f),
            Token::Plus(_) => "+".fmt(f),
            Token::Minus(_) => "-".fmt(f),
            Token::Decrement(_) => "--".fmt(f),
            Token::Star(_) => "*".fmt(f),
            Token::Div(_) => "/".fmt(f),
            Token::Pow(_) => "^".fmt(f),
            Token::Less(_) => "<".fmt(f),
            Token::Greater(_) => ">".fmt(f),
            Token::LessEqual(_) => "<=".fmt(f),
            Token::GreaterEqual(_) => ">=".fmt(f),
            Token::Equal(_) => "==".fmt(f),
            Token::NotEqual(_) => "!=".fmt(f),
            Token::Dot(_) => ".".fmt(f),
            Token::DoubleDot(_) => "..".fmt(f),
            Token::Comma(_) => ",".fmt(f),
            Token::EOF => "EOF".fmt(f),
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
            | Token::Ish(loc)
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
        let expected = vec![
            "1:1", "1:5", "2:1", "2:3", "2:5", "3:5", "3:11", "4:2", "EOF",
        ];
        assert_eq!(formatted, expected);
    }
}
