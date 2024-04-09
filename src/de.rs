use std::borrow::Cow;
use std::ops::{AddAssign, DivAssign, MulAssign, Neg, SubAssign};
use std::str::FromStr;

use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess,
    Visitor,
};
use serde::Deserialize;

use crate::error::{ErrorCode, Location};
use crate::{Error, Number, Result};

pub fn from_str<'a, T: Deserialize<'a>>(input: &'a str) -> Result<T> {
    let mut deserializer = Deserializer::from_str(input);
    let t = T::deserialize(&mut deserializer)?;
    deserializer.eat_whitespace()?;
    if deserializer.input.is_empty() {
        Ok(t)
    } else {
        Err(ErrorCode::TrailingCharacters.at(deserializer.location))
    }
}

enum Sign {
    Positive,
    Negative,
}

pub struct Deserializer<'de> {
    input: &'de str,
    location: Location,
    /// Save the initial length to check if the deserializer is still at the start for top-level
    /// maps without braces. This might be better tracked with a bool `at_start`.
    initial_length: usize,
}
impl<'de> Deserializer<'de> {
    pub fn from_str(input: &'de str) -> Self {
        Deserializer {
            input,
            location: Location::default(),
            initial_length: input.len(),
        }
    }
}

fn is_unquoted_string_char(c: char) -> bool {
    !c.is_whitespace() && !matches!(c, '"' | '[' | ']' | '{' | '}' | ',' | ':')
}

impl<'de> Deserializer<'de> {
    fn is_at_start(&self) -> bool {
        self.input.len() == self.initial_length
    }

    /// Tries to skip whitespace and comments and returns true if anything was skipped.
    fn eat_whitespace(&mut self) -> Result<bool> {
        let mut ate_something = false;
        loop {
            if self.input.starts_with("//") {
                match self.input.split_once('\n') {
                    Some((_comment, rest)) => {
                        self.location.line += 1;
                        self.location.column = 1;
                        self.input = rest;
                    }
                    None => {
                        self.location.column += self.input.len();
                        self.input = "";
                    }
                }
            } else if let Some(rest) = self.input.strip_prefix("/*") {
                self.location.column += 2;
                self.input = rest;
                let mut seen_star = false;
                let mut chars = self.input.char_indices();
                loop {
                    let next = chars.next();
                    match next {
                        Some((_, '\n')) => {
                            self.location.line += 1;
                            self.location.column = 1;
                        }
                        Some((_, '*')) => {
                            seen_star = true;
                            self.location.column += 1;
                        }
                        Some((i, '/')) if seen_star => {
                            self.input = &self.input[i + 1..];
                            break;
                        }
                        Some((_, other)) => self.location.column += other.len_utf8(),
                        None => {
                            return Err(ErrorCode::UnclosedBlockComment.at(self.location));
                        }
                    }
                }
            } else if self.peek_char().is_ok_and(char::is_whitespace) {
                self.next_char()?;
            } else {
                break;
            }
            ate_something = true;
        }
        Ok(ate_something)
    }

    /// Consumes a null if the input starts with one and returns true if it did.
    fn eat_null(&mut self) -> Result<bool> {
        self.eat_whitespace()?;

        let null_found = self.input.starts_with("null")
            && self.input[4..]
                .chars()
                .next()
                .map_or(true, |c| !is_unquoted_string_char(c));

        if null_found {
            self.input = &self.input[4..];
        }
        Ok(null_found)
    }

    fn peek_char(&mut self) -> Result<char> {
        self.input
            .chars()
            .next()
            .ok_or(ErrorCode::Eof.at(self.location))
    }

    fn next_char(&mut self) -> Result<char> {
        let ch = self.peek_char()?;
        self.input = &self.input[ch.len_utf8()..];
        if ch == '\n' {
            self.location.next_line();
        } else {
            self.location.column += ch.len_utf8();
        }
        Ok(ch)
    }

    fn skip_if(&mut self, c: char) -> bool {
        if let Some(actual) = self.input.chars().next() {
            if actual == c {
                self.input = &self.input[c.len_utf8()..];
                if c == '\n' {
                    self.location.next_line();
                } else {
                    self.location.column += c.len_utf8();
                }
                return true;
            }
        }
        false
    }

    fn parse_bool(&mut self) -> Result<bool> {
        self.eat_whitespace()?;

        enum State {
            Empty,
            T,
            Tr,
            Tru,
            F,
            Fa,
            Fal,
            Fals,
        }
        use State::*;

        let mut state = Empty;
        let value = loop {
            state = match (state, self.next_char()?) {
                (Empty, 't') => T,
                (T, 'r') => Tr,
                (Tr, 'u') => Tru,
                (Tru, 'e') => break true,
                (Empty, 'f') => F,
                (F, 'a') => Fa,
                (Fa, 'l') => Fal,
                (Fal, 's') => Fals,
                (Fals, 'e') => break false,
                _ => return Err(ErrorCode::ExpectedBoolean.at(self.location)),
            }
        };
        if self.peek_char().map_or(false, is_unquoted_string_char) {
            // If more characters come after, this isn't a boolean but an arbitrary string just
            // starting with the string "true" or "false", meaning this should error.
            return Err(ErrorCode::ExpectedBoolean.at(self.location));
        }
        Ok(value)
    }

    fn parse_optional_exponent(&mut self) -> Result<Option<(Sign, u32)>> {
        (self.skip_if('e') || self.skip_if('E'))
            .then(|| {
                let sign = match self.next_char()? {
                    '-' => Sign::Negative,
                    '+' => Sign::Positive,
                    _ => return Err(ErrorCode::ExpectedPlusOrMinus.at(self.location)),
                };
                let first_digit = self.next_char()?;
                if !matches!(first_digit, '0'..='9') {
                    return Err(ErrorCode::ExpectedInteger.at(self.location));
                }
                let mut value = first_digit as u32 - b'0' as u32;
                while self.peek_char().is_ok_and(|c| matches!(c, '0'..='9')) {
                    value *= 10;
                    value += self.next_char().unwrap() as u32 - b'0' as u32; // char is peeked so unwrap is fine here
                }
                Ok((sign, value))
            })
            .transpose()
    }

    fn parse_unsigned<T>(&mut self) -> Result<T>
    where
        T: AddAssign<T> + MulAssign<T> + DivAssign<T> + From<u8> + TryFrom<u64>,
    {
        self.eat_whitespace()?;

        let mut int = match self.next_char()? {
            ch @ '0'..='9' => T::from(ch as u8 - b'0'),
            _ => return Err(ErrorCode::ExpectedInteger.at(self.location)),
        };
        loop {
            match self.input.chars().next() {
                Some(ch @ '0'..='9') => {
                    self.input = &self.input[1..];
                    self.location.column += 1;
                    int *= T::from(10);
                    int += T::from(ch as u8 - b'0');
                }
                // This is not a number, just a string starting with digits.
                Some(c) if is_unquoted_string_char(c) => {
                    return Err(ErrorCode::ExpectedInteger.at(self.location))
                }
                _ => break,
            }
        }
        if let Some((sign, value)) = self.parse_optional_exponent()? {
            let factor = 10u64
                .pow(value)
                .try_into()
                .map_err(|_| ErrorCode::NumberOutOfRange.at(self.location))?;
            match sign {
                Sign::Positive => int *= factor,
                Sign::Negative => int /= factor,
            }
        }
        Ok(int)
    }

    fn parse_signed<T>(&mut self) -> Result<T>
    where
        T: Neg<Output = T>
            + AddAssign<T>
            + SubAssign<T>
            + MulAssign<T>
            + DivAssign<T>
            + From<i8>
            + TryFrom<u64>,
    {
        self.eat_whitespace()?;

        let negative = self.skip_if('-');
        let mut int = match self.next_char()? {
            ch @ '0'..='9' => {
                let digit = T::from((ch as u8 - b'0') as i8);
                if negative {
                    -digit
                } else {
                    digit
                }
            }
            _ => {
                return Err(ErrorCode::ExpectedInteger.at(self.location));
            }
        };
        loop {
            match self.input.chars().next() {
                Some(ch @ '0'..='9') => {
                    self.input = &self.input[1..];
                    self.location.column += 1;
                    int *= T::from(10);
                    let digit = T::from((ch as u8 - b'0') as i8);
                    if negative {
                        int -= digit;
                    } else {
                        int += digit;
                    }
                }
                // This is not a number, just a string starting with digits.
                Some(c) if is_unquoted_string_char(c) => {
                    return Err(ErrorCode::ExpectedInteger.at(self.location))
                }
                _ => break,
            }
        }

        if let Some((sign, value)) = self.parse_optional_exponent()? {
            let factor = 10u64
                .pow(value)
                .try_into()
                .map_err(|_| ErrorCode::NumberOutOfRange.at(self.location))?;
            match sign {
                Sign::Positive => int *= factor,
                Sign::Negative => int /= factor,
            }
        }

        Ok(int)
    }

    fn parse_float<T>(&mut self) -> Result<T>
    where
        T: FromStr,
    {
        self.eat_whitespace()?;

        // PERF: parsing floats to a string first isn't optimal.
        // validating the float and passing a slice should definitely work
        let mut s = String::new();
        if self.skip_if('-') {
            s.push('-');
        }
        let mut dot_seen = false;
        match self.next_char()? {
            c @ '1'..='9' => {
                s.push(c);
                while matches!(self.peek_char(), Ok('0'..='9')) {
                    s.push(self.next_char().unwrap());
                }
            }
            // leading zero is not allowed, don't expect more chars before the dot/exponent
            '0' => s.push('0'),
            // we allow leading dot number notation like .5 even though json doesn't
            '.' => {
                s.push('.');
                dot_seen = true;
            }
            _ => return Err(ErrorCode::ExpectedFloat.at(self.location)),
        }
        if !dot_seen && self.skip_if('.') {
            s.push('.');
            while matches!(self.peek_char(), Ok('0'..='9')) {
                s.push(self.next_char().unwrap());
            }
        }
        if let Some((sign, exponent)) = self.parse_optional_exponent()? {
            let sign = match sign {
                Sign::Positive => '+',
                Sign::Negative => '-',
            };
            use std::fmt::Write;
            write!(s, "E{sign}{exponent}").unwrap();
        }
        match T::from_str(&s) {
            Ok(x) => Ok(x),
            // We have validated the float so it has a format rust can parse
            Err(_) => unreachable!(),
        }
    }

    fn parse_string(&mut self) -> Result<Cow<'de, str>> {
        self.eat_whitespace()?;

        match self.peek_char()? {
            '"' => self.parse_quoted_string(),
            c if is_unquoted_string_char(c) => Ok(Cow::Borrowed(self.parse_unquoted_string()?)),
            _ => Err(ErrorCode::ExpectedString.at(self.location)),
        }
    }

    fn parse_unquoted_string(&mut self) -> Result<&'de str> {
        debug_assert!(self
            .input
            .chars()
            .next()
            .is_some_and(is_unquoted_string_char));

        let end_of_string = self
            .input
            .char_indices()
            .skip(1)
            .find(|(_, c)| !is_unquoted_string_char(*c));
        if let Some((end, _)) = end_of_string {
            let s = &self.input[..end];
            self.input = &self.input[end..];
            self.location.column += end;
            Ok(s)
        } else {
            // string continues to end of file
            let s = self.input;
            self.location.column += s.len();
            self.input = "";
            Ok(s)
        }
    }

    fn parse_quoted_string(&mut self) -> Result<Cow<'de, str>> {
        let start_quote = self.next_char();
        // Start quote should already be checked by the function calling `parse_quoted_string`.
        debug_assert_eq!(start_quote, Ok('"'));

        let mut chars = self.input.match_indices(&['"', '\\']);

        match chars
            .next()
            .ok_or(ErrorCode::UnclosedString.at(self.location))?
        {
            (i, "\"") => {
                let content = &self.input[..i];
                self.input = &self.input[i + 1..];
                self.location.column += i + 1;
                Ok(Cow::Borrowed(content))
            }
            (i, "\\") => {
                let mut s = String::from(&self.input[..i]);
                self.input = &self.input[i..];
                self.location.column += i;
                self.parse_quoted_escaped_string_contents(&mut s)?;
                Ok(Cow::Owned(s))
            }
            _ => unreachable!(), // all patterns are covered
        }
    }

    fn parse_quoted_escaped_string_contents(&mut self, s: &mut String) -> Result<()> {
        let mut chars = self.input.char_indices();
        loop {
            let (i, c) = chars
                .next()
                .ok_or(ErrorCode::UnclosedString.at(self.location))?;
            if c == '\n' {
                self.location.next_line();
            } else {
                self.location.column += c.len_utf8();
            }
            match c {
                '\\' => match chars
                    .next()
                    .ok_or(ErrorCode::UnclosedString.at(self.location))?
                    .1
                {
                    '"' => s.push('"'),
                    '\\' => s.push('\\'),
                    '/' => s.push('/'),
                    'b' => s.push('\x08'), // backspace
                    'f' => s.push('\x0C'), // form feed
                    'n' => s.push('\n'),
                    'r' => s.push('\r'),
                    't' => s.push('\t'),
                    'u' => {
                        // hex escape code
                        let mut hex_digit = || {
                            let c = chars
                                .next()
                                .ok_or(ErrorCode::UnclosedString.at(self.location))?
                                .1;
                            match c {
                                '0'..='9' => Ok(c as u32 - b'0' as u32),
                                'a'..='f' => Ok(c as u32 - b'a' as u32 + 10),
                                'A'..='F' => Ok(c as u32 - b'A' as u32 + 10),
                                _ => Err(ErrorCode::EscapeInvalid.at(self.location)),
                            }
                        };
                        let value = (1..=4)
                            .rev()
                            .map(|i| hex_digit().map(|d| d * 16u32.pow(i)))
                            .sum::<Result<u32>>()?;
                        s.push(
                            char::from_u32(value)
                                .ok_or(ErrorCode::CharCodeInvalid(value).at(self.location))?,
                        );
                    }
                    _ => return Err(ErrorCode::EscapeInvalid.at(self.location)),
                },
                '"' => {
                    self.input = &self.input[i + 1..];
                    return Ok(());
                }
                c => s.push(c),
            }
        }
    }
}

macro_rules! deserialize_number {
    ($parse_method: ident: $($method: ident $visit_method: ident,)*) => {
        $(
            fn $method<V>(self, visitor: V) -> Result<V::Value>
            where
                V: Visitor<'de>,
            {
                visitor.$visit_method(self.$parse_method()?)
            }
        )*
    };
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let start = self.is_at_start();
        self.eat_whitespace()?;

        match self.peek_char()? {
            '"' => {
                let input = self.input;
                let key_or_str = self.parse_quoted_string()?;
                if start
                    && self.peek_char().is_ok_and(|c| {
                        matches!(c, ':' | '{' | '[' | '"') || is_unquoted_string_char(c)
                    })
                {
                    // PERF: This parses the key twice.
                    // this is definitely a top-level map, go back and just call deserialize_map.
                    self.input = input;
                    self.deserialize_map(visitor)
                } else {
                    match key_or_str {
                        Cow::Owned(s) => visitor.visit_string(s),
                        Cow::Borrowed(s) => visitor.visit_str(s),
                    }
                }
            }
            '[' => self.deserialize_seq(visitor),
            '{' => self.deserialize_map(visitor),
            c if is_unquoted_string_char(c) => match self.parse_unquoted_string()? {
                "null" => visitor.visit_none(),
                "true" => visitor.visit_bool(true),
                "false" => visitor.visit_bool(false),
                key_or_str => {
                    let input = self.input;
                    self.eat_whitespace()?;
                    if start
                        && self.peek_char().is_ok_and(|c| {
                            matches!(c, ':' | '{' | '[' | '"') || is_unquoted_string_char(c)
                        })
                    {
                        // this is definitely a top-level map, go back and just call deserialize_map.
                        // PERF: This parses the key twice.
                        self.input = input;
                        self.deserialize_map(visitor)
                    } else {
                        // this might still be a number, check that
                        if let Ok(number) = Number::try_from(key_or_str) {
                            number.visit(visitor)
                        } else {
                            visitor.visit_borrowed_str(key_or_str)
                        }
                    }
                }
            },
            _ => Err(ErrorCode::UnexpectedCharacter.at(self.location)),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(self.parse_bool()?)
    }

    deserialize_number! {
        parse_signed:
            deserialize_i8 visit_i8,
            deserialize_i16 visit_i16,
            deserialize_i32 visit_i32,
            deserialize_i64 visit_i64,
    }
    deserialize_number! {
        parse_unsigned:
            deserialize_u8 visit_u8,
            deserialize_u16 visit_u16,
            deserialize_u32 visit_u32,
            deserialize_u64 visit_u64,
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f32(self.parse_float()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f64(self.parse_float()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // chars are just single-character strings
        let s = self.parse_string()?;
        let c = s
            .chars()
            .next()
            .ok_or(ErrorCode::ExpectedChar.at(self.location))?;
        if s.chars().next().is_some() {
            // string contains multiple chars
            return Err(ErrorCode::ExpectedChar.at(self.location));
        }

        visitor.visit_char(c)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // This tries to respect the choice of borrowing, but if the string
        // was escaped, we just pass a String.
        match self.parse_string()? {
            Cow::Owned(s) => visitor.visit_string(s),
            Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    // The `Serializer` implementation on the previous page serialized byte
    // arrays as GON arrays of bytes. Handle that representation here.
    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        todo!("deserialize bytes")
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        todo!("deserialize byte buf")
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if self.eat_null()? {
            return visitor.visit_none();
        }
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if self.eat_null()? {
            visitor.visit_unit()
        } else {
            Err(ErrorCode::ExpectedNull.at(self.location))
        }
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.eat_whitespace()?;
        // Parse the opening bracket of the sequence.
        if self.skip_if('[') {
            // Give the visitor access to each element of the sequence.
            let value = visitor.visit_seq(&mut *self)?;

            self.eat_whitespace()?;
            if self.skip_if(']') {
                Ok(value)
            } else {
                Err(ErrorCode::UnclosedArray.at(self.location))
            }
        } else {
            Err(ErrorCode::ExpectedArray.at(self.location))
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let is_at_start = self.is_at_start();
        self.eat_whitespace()?;
        // Parse the opening brace of the map.
        if self.peek_char().is_ok_and(|c| c == '{') {
            let opening_brace = self.next_char();
            debug_assert_eq!(opening_brace, Ok('{'));
            // Give the visitor access to each entry of the map.
            let value = visitor.visit_map(Map {
                de: self,
                eof_closes: false,
            })?;
            self.eat_whitespace()?;
            if self.next_char()? == '}' {
                Ok(value)
            } else {
                Err(ErrorCode::UnclosedMap.at(self.location))
            }
        } else if is_at_start {
            // braces can be left out at the top level
            visitor.visit_map(Map {
                de: self,
                eof_closes: true,
            })
        } else {
            Err(ErrorCode::ExpectedMap.at(self.location))
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.eat_whitespace()?;
        if self
            .peek_char()
            .map(|c| c == '"' || is_unquoted_string_char(c))?
        {
            // Visit a unit variant.
            visitor.visit_enum(self.parse_string()?.into_deserializer())
        } else if self.next_char()? == '{' {
            // Visit a newtype variant, tuple variant, or struct variant.
            let value = visitor.visit_enum(Enum::new(self))?;
            // Parse the matching close brace.
            if self.next_char()? == '}' {
                Ok(value)
            } else {
                Err(ErrorCode::UnclosedMap.at(self.location))
            }
        } else {
            // TODO: top-level omitted {} ?
            Err(ErrorCode::ExpectedEnum.at(self.location))
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

impl<'de> SeqAccess<'de> for Deserializer<'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        self.eat_whitespace()?;
        // Check if there are no more elements.
        if self.peek_char()? == ']' {
            return Ok(None);
        }
        // Deserialize an array element.
        let elem = seed.deserialize(&mut *self)?;
        self.eat_whitespace()?;
        self.skip_if(',');
        Ok(Some(elem))
    }
}

// In order to handle the case that the braces are implicit around a top-level map, we need a
// separate struct.
struct Map<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    eof_closes: bool,
}

// `MapAccess` is provided to the `Visitor` to give it the ability to iterate
// through entries of the map.
impl<'de, 'a> MapAccess<'de> for Map<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        self.de.eat_whitespace()?;
        // Check if there are no more entries.
        match self.de.peek_char() {
            Ok('}') if !self.eof_closes => return Ok(None),
            Ok(_) => {}
            Err(err) => return if self.eof_closes { Ok(None) } else { Err(err) },
        }
        // Deserialize a map key.
        let elem = seed.deserialize(&mut *self.de)?;

        Ok(Some(elem))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        self.de.eat_whitespace()?;
        // Optionally parse a colon.
        self.de.skip_if(':');
        // Deserialize a map value.
        let value = seed.deserialize(&mut *self.de)?;

        self.de.eat_whitespace()?;
        // Optionally parse a comma.
        self.de.skip_if(',');

        Ok(value)
    }
}

struct Enum<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> Enum<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Enum { de }
    }
}

// `EnumAccess` is provided to the `Visitor` to give it the ability to determine
// which variant of the enum is supposed to be deserialized.
//
// Note that all enum deserialization methods in Serde refer exclusively to the
// "externally tagged" enum representation.
impl<'de, 'a> EnumAccess<'de> for Enum<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: DeserializeSeed<'de>,
    {
        // The `deserialize_enum` method parsed a `{` character so we are
        // currently inside of a map. The seed will be deserializing itself from
        // the key of the map.
        let val = seed.deserialize(&mut *self.de)?;
        self.de.eat_whitespace()?;
        // Parse the colon separating map key from value.
        if self.de.peek_char().is_ok_and(|c| c == ':') {
            self.de.input = &self.de.input[1..];
        }
        Ok((val, self))
    }
}

// `VariantAccess` is provided to the `Visitor` to give it the ability to see
// the content of the single variant that it decided to deserialize.
impl<'de, 'a> VariantAccess<'de> for Enum<'a, 'de> {
    type Error = Error;

    // If the `Visitor` expected this variant to be a unit variant, the input
    // should have been the plain string case handled in `deserialize_enum`.
    fn unit_variant(self) -> Result<()> {
        Err(ErrorCode::ExpectedString.at(self.de.location))
    }

    // Newtype variants are represented in GON as `{ NAME: VALUE }` so
    // deserialize the value here.
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(self.de)
    }

    // Tuple variants are represented in GON as `{ NAME: [DATA...] }` so
    // deserialize the sequence of data here.
    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.de, visitor)
    }

    // Struct variants are represented in GON as `{ NAME: { K: V, ... } }` so
    // deserialize the inner map here.
    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.de, visitor)
    }
}

#[cfg(test)]
mod tests {
    use super::from_str;
    use super::ErrorCode;
    use crate::error::Location;
    use serde::Deserialize;

    #[test]
    fn test_struct() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct Test {
            int: u32,
            seq: Vec<String>,
        }

        let expected = Test {
            int: 1,
            seq: vec!["a".to_owned(), "b".to_owned()],
        };

        let compact = r#"{"int":1,"seq":[a,"b"]}"#;
        assert_eq!(expected, from_str(compact).unwrap());

        let with_whitespace = r#" {
        
            "int" : /* a comment */ 1
            ,
            "seq" :  [ a  ,  "b"  ]  } // another comment "#;
        assert_eq!(expected, from_str(with_whitespace).unwrap());

        let with_implicit_top_level_map = r#" // comment here to make sure there is whitespace at the start
        
            "int" : /* a comment */ 1
            ,
            "seq" :  [ a  ,  "b"  ]   // another comment "#;
        assert_eq!(expected, from_str(with_implicit_top_level_map).unwrap());
    }

    #[test]
    fn test_enum() {
        #[derive(Deserialize, PartialEq, Debug)]
        enum E {
            Unit,
            Newtype(u32),
            Tuple(u32, u32),
            Struct { a: u32 },
        }

        let j = r#""Unit""#;
        let expected = E::Unit;
        assert_eq!(expected, from_str(j).unwrap());

        let j = r#"{"Newtype":1}"#;
        let expected = E::Newtype(1);
        assert_eq!(expected, from_str(j).unwrap());

        let j = r#"{"Tuple":[1,2]}"#;
        let expected = E::Tuple(1, 2);
        assert_eq!(expected, from_str(j).unwrap());

        let j = r#"{"Struct":{"a":1}}"#;
        let expected = E::Struct { a: 1 };
        assert_eq!(expected, from_str(j).unwrap());
    }

    #[test]
    fn float_parsing() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Floats {
            my_f32: f32,
            my_f64: f64,
            whole_number: f64,
            negative_float: f64,
        }

        let j = r#" my_f32 0.314159E+1 my_f64 1.234567 whole_number 4 negative_float -4.7 "#;
        let expected = Floats {
            my_f32: 3.14159,
            my_f64: 1.234567,
            whole_number: 4.0,
            negative_float: -4.7,
        };
        assert_eq!(expected, from_str(j).unwrap());
    }

    #[test]
    fn error_location() {
        #[derive(Deserialize, Debug)]
        #[allow(dead_code)]
        struct Values {
            name: String,
            age: u32,
        }

        // age should be a u32 but we give a float here
        let j = "/* comment at the start */ name \"John Doe\"// end of line comment\n  age 12.5";
        let err = from_str::<Values>(j).unwrap_err();
        assert_eq!(err.code(), &ErrorCode::ExpectedInteger);
        let location = err.err.location;
        // check that the location is correct
        assert!(matches!(
            location,
            Location {
                line: 2,
                column: 7..=11, // error should be in the range of the float literal
            }
        ));
        // test the formatting of error messages
        assert_eq!(
            err.to_string(),
            format!("2:{} {}", location.column, ErrorCode::ExpectedInteger)
        );
    }
}
