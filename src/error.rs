use serde::{de, ser};
use std::fmt::{self, Display};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub(crate) err: Box<ErrorImpl>,
}
impl Error {
    pub fn code(&self) -> &ErrorCode {
        &self.err.code
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let err = &self.err;
        write!(
            f,
            "{}:{} {}",
            err.location.line, err.location.column, err.code
        )
    }
}
impl std::error::Error for Error {}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Self {
            err: Box::new(ErrorImpl {
                code: ErrorCode::Message(msg.to_string().into()),
                location: Location::default(), // TODO: maybe try parsing a location like serde_json
            }),
        }
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Self {
            err: Box::new(ErrorImpl {
                code: ErrorCode::Message(msg.to_string().into()),
                location: Location::default(), // TODO: maybe try parsing a location like serde_json
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ErrorImpl {
    pub(crate) code: ErrorCode,
    pub(crate) location: Location,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct Location {
    pub line: usize,
    pub column: usize,
}
impl Location {
    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }
}
impl Default for Location {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnexpectedCharacter,
    TrailingCharacters,
    Eof,
    ExpectedBoolean,
    ExpectedInteger,
    ExpectedFloat,
    ExpectedNumber,
    ExpectedString,
    ExpectedChar,
    ExpectedNull,
    ExpectedArray,
    ExpectedMap,
    ExpectedEnum,
    ExpectedPlusOrMinus,
    UnclosedBlockComment,
    UnclosedString,
    UnclosedArray,
    UnclosedMap,
    EscapeInvalid,
    CharCodeInvalid(u32),
    NumberOutOfRange,
    Message(Box<str>),
}
impl ErrorCode {
    pub(crate) fn at(self, location: Location) -> Error {
        Error {
            err: Box::new(ErrorImpl {
                code: self,
                location,
            }),
        }
    }
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter => f.write_str("an unexpected character was encountered"),
            Self::TrailingCharacters => f.write_str("there where leftover characters in the input"),
            Self::Eof => f.write_str("unexpected end of file"),
            Self::ExpectedBoolean => f.write_str("expected a boolean"),
            Self::ExpectedInteger => f.write_str("expected an integer"),
            Self::ExpectedFloat => f.write_str("expected a float"),
            Self::ExpectedNumber => f.write_str("expected a number"),
            Self::ExpectedString => f.write_str("expected a string"),
            Self::ExpectedChar => {
                f.write_str("expected a char in the form of a single-character string")
            }
            Self::ExpectedNull => f.write_str("expected null"),
            Self::ExpectedArray => f.write_str("expected an array"),
            Self::ExpectedMap => f.write_str("expected a map"),
            Self::ExpectedEnum => f.write_str("expected an enum"),
            Self::ExpectedPlusOrMinus => {
                f.write_str("expected a plus '+' or a minus '-' at the start of the exponent")
            }
            Self::UnclosedBlockComment => {
                f.write_str("a block comment was opened '/*' but not closed '*/'")
            }
            Self::UnclosedString => f.write_str("a string was started '\"' but not ended '\"'"),
            Self::UnclosedArray => f.write_str("an array was opened '[' but not closed ']'"),
            Self::UnclosedMap => f.write_str("a map was opened '{' but not closed '}'"),
            Self::EscapeInvalid => f.write_str("an escape code was invalid"),
            Self::CharCodeInvalid(code) => {
                write!(f, "the char code {:#06x} is not a valid character", code)
            }
            Self::NumberOutOfRange => {
                write!(f, "a number was not in the range of the expected type")
            }
            Self::Message(msg) => f.write_str(msg),
        }
    }
}
