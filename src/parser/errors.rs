use crate::scanner::ScannerError;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError, TryFromIntError};

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl From<&str> for ParseError {
    fn from(message: &str) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl From<String> for ParseError {
    fn from(message: String) -> Self {
        Self { message }
    }
}

impl From<ScannerError> for ParseError {
    fn from(error: ScannerError) -> Self {
        Self {
            message: format!(
                "[{}:{}] {}",
                error.location.line, error.location.column, error.message
            ),
        }
    }
}

impl From<ParseIntError> for ParseError {
    fn from(err: ParseIntError) -> Self {
        err.to_string().into()
    }
}

impl From<ParseFloatError> for ParseError {
    fn from(err: ParseFloatError) -> Self {
        err.to_string().into()
    }
}

impl From<TryFromIntError> for ParseError {
    fn from(err: TryFromIntError) -> Self {
        err.to_string().into()
    }
}
