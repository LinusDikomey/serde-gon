use std::fmt::{self, Display};
use serde::{de, ser};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    TrailingCharacters,
    Eof,
    ExpectedBoolean,
    ExpectedInteger,
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
    Message(String),
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Self::Message(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Self::Message(msg.to_string())
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TrailingCharacters => f.write_str("there where leftover characters in the input"),
            Self::Eof => f.write_str("unexpected end of file"),
            Self::ExpectedBoolean => f.write_str("expected a boolean"),
            Self::ExpectedInteger => f.write_str("expected an integer"),
            Self::ExpectedString => f.write_str("expected a string"),
            Self::ExpectedChar => f.write_str("expected a char in the form of a single-character string"),
            Self::ExpectedNull => f.write_str("expected null"),
            Self::ExpectedArray => f.write_str("expected an array"),
            Self::ExpectedMap => f.write_str("expected a map"),
            Self::ExpectedEnum => f.write_str("expected an enum"),
            Self::ExpectedPlusOrMinus => f.write_str("expected a plus '+' or a minus '-' at the start of the exponent"),
            Self::UnclosedBlockComment => f.write_str("a block comment was opened '/*' but not closed '*/'"),
            Self::UnclosedString => f.write_str("a string was started '\"' but not ended '\"'"),
            Self::UnclosedArray => f.write_str("an array was opened '[' but not closed ']'"),
            Self::UnclosedMap => f.write_str("a map was opened '{' but not closed '}'"),
            Self::EscapeInvalid => f.write_str("an escape code was invalid"),
            Self::CharCodeInvalid(code) => write!(f, "the char code {:#06x} is not a valid character", code),
            Self::Message(msg) => f.write_str(msg),
        }
    }
}

impl std::error::Error for Error { }
