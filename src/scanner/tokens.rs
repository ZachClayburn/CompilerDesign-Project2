use super::Location;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Complex tokens
    Number {
        content: String,
        start: Location,
        stop: Location,
    },
    Identifier {
        content: String,
        start: Location,
        stop: Location,
    },
    StringLiteral {
        content: String,
        start: Location,
        stop: Location,
    },
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
    String(Location),
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
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number {
                content,
                start: _,
                stop: _,
            } => write!(f, "Number: {}", content),
            Token::Identifier {
                content,
                start: _,
                stop: _,
            } => write!(f, "Identifier: {}", content),
            Token::StringLiteral {
                content,
                start: _,
                stop: _,
            } => write!(f, "StringLiteral: {}", content),
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
            Token::String(_) => write!(f, "String"),
            Token::Return(_) => write!(f, "Return"),
            Token::LParen(_) => write!(f, "LParen"),
            Token::RParen(_) => write!(f, "RParen"),
            Token::LBracket(_) => write!(f, "LBracket"),
            Token::RBracket(_) => write!(f, "RBracket"),
            Token::LBrace(_) => write!(f, "LBrace"),
            Token::RBrace(_) => write!(f, "RBrace"),
            Token::Semicolon(_) => write!(f, "Semicolon"),
            Token::Assign(_) => write!(f, "Assign"),
            Token::Plus(_) => write!(f, "Plus"),
            Token::Minus(_) => write!(f, "Minus"),
            Token::Star(_) => write!(f, "Star"),
            Token::Div(_) => write!(f, "Div"),
            Token::Pow(_) => write!(f, "Pow"),
            Token::Less(_) => write!(f, "Less"),
            Token::Greater(_) => write!(f, "Greater"),
            Token::LessEqual(_) => write!(f, "LessEqual"),
            Token::GreaterEqual(_) => write!(f, "GreaterEqual"),
            Token::Equal(_) => write!(f, "Equal"),
            Token::NotEqual(_) => write!(f, "NotEqual"),
            Token::Dot(_) => write!(f, "Dot"),
            Token::DoubleDot(_) => write!(f, "DoubleDot"),
            Token::Comma(_) => write!(f, "Comma"),
        }
    }
}

impl Token {
    pub fn format_location(&self) -> String {
        match self {
            Token::Number {
                content: _,
                start: loc,
                stop: _,
            }
            | Token::Identifier {
                content: _,
                start: loc,
                stop: _,
            }
            | Token::StringLiteral {
                content: _,
                start: loc,
                stop: _,
            }
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
            | Token::String(loc)
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
        }
    }
}
