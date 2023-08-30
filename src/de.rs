use std::borrow::Cow;
use std::ops::{AddAssign, MulAssign, Neg, SubAssign};

use serde::Deserialize;
use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
    VariantAccess, Visitor,
};

use crate::{Error, Result};

pub fn from_str<'a, T: Deserialize<'a>>(input: &'a str) -> Result<T> {
    let mut deserializer = Deserializer::from_str(input);
    let t = T::deserialize(&mut deserializer)?;
    deserializer.eat_whitespace()?;
    if deserializer.input.is_empty() {
        Ok(t)
    } else {
        Err(Error::TrailingCharacters)
    }
}

enum Sign { Positive, Negative }

pub struct Deserializer<'de> {
    input: &'de str,
    /// Save the initial length to check if the deserializer is still at the start for top-level
    /// maps without braces. This might be better tracked with a bool `at_start`.
    initial_length: usize,
}
impl<'de> Deserializer<'de> {
    pub fn from_str(input: &'de str) -> Self {
        Deserializer { input, initial_length: input.len() }
    }
}

fn is_unquoted_string_char(c: char) -> bool {
    !c.is_whitespace() && !matches!(c,  '"' | '[' | ']' | '{' | '}' | ',' | ':')
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
                self.input = self.input.split_once('\n').map_or("", |(_comment, rest_of_file)| rest_of_file);
            } else if self.input.starts_with("/*") {
                self.input = self.input.split_once("*/").ok_or(Error::UnclosedBlockComment)?.1;
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

        let null_found = 
            self.input.starts_with("null")
            && self.input[4..].chars().next().map_or(true, |c| !is_unquoted_string_char(c));

        if null_found {
            self.input = &self.input[4..];
        }
        Ok(null_found)
    }

    fn peek_char(&mut self) -> Result<char> {
        self.input.chars().next().ok_or(Error::Eof)
    }

    fn next_char(&mut self) -> Result<char> {
        let ch = self.peek_char()?;
        self.input = &self.input[ch.len_utf8()..];
        Ok(ch)
    }

    fn skip_if(&mut self, c: char) -> bool {
        if let Some(actual) = self.input.chars().next() {
            if actual == c {
                self.input = &self.input[c.len_utf8()..];
                return true;
            }
        }
        false
    }

    fn parse_bool(&mut self) -> Result<bool> {
        self.eat_whitespace()?;

        enum State { Empty, T, Tr, Tru, F, Fa, Fal, Fals }
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
                _ => return Err(Error::ExpectedBoolean)
            }
        };
        if self.peek_char().map_or(false, is_unquoted_string_char) {
            // If more characters come after, this isn't a boolean but an arbitrary string just
            // starting with the string "true" or "false", meaning this should error.
            return Err(Error::ExpectedBoolean)
        }
        Ok(value)
    }

    fn parse_optional_exponent(&mut self) -> Result<Option<(Sign, u64)>> {
        (self.skip_if('e') || self.skip_if('E')).then(|| {
            self.input = &self.input[1..];
            let sign = match self.next_char()? {
                '-' => Sign::Negative,
                '+' => Sign::Positive,
                _ => return Err(Error::ExpectedPlusOrMinus),
            };
            let first_digit = self.next_char()?;
            if !matches!(first_digit, '0'..='9') {
                return Err(Error::ExpectedInteger);
            }
            let mut value = first_digit as u64 - b'0' as u64;
            while self.peek_char().is_ok_and(|c| matches!(c, '0'..='9')) {
                value *= 10;
                value += self.next_char().unwrap() as u64 - b'0' as u64; // char is peeked so unwrap is fine here
            }
            Ok((sign, value))
        }).transpose()
    }

    fn parse_unsigned<T>(&mut self) -> Result<T>
    where
        T: AddAssign<T> + MulAssign<T> + From<u8>,
    {
        self.eat_whitespace()?;

        let mut int = match self.next_char()? {
            ch @ '0'..='9' => T::from(ch as u8 - b'0'),
            _ => return Err(Error::ExpectedInteger),
        };
        loop {
            match self.input.chars().next() {
                Some(ch @ '0'..='9') => {
                    self.input = &self.input[1..];
                    int *= T::from(10);
                    int += T::from(ch as u8 - b'0');
                }
                // This is not a number, just a string starting with digits.
                Some(c) if is_unquoted_string_char(c) => return Err(Error::ExpectedInteger),
                _ => break
            }
        }
        if let Some((_sign, _value)) = self.parse_optional_exponent()? {
            todo!("apply exponents");
        }
        Ok(int)
    }

    fn parse_signed<T>(&mut self) -> Result<T>
    where
        T: Neg<Output = T> + AddAssign<T> + SubAssign<T> + MulAssign<T> + From<i8>,
    {
        self.eat_whitespace()?;

        let negative = self.peek_char() == Ok('-');
        if negative {
            // This doesn't error since we peeked the char before.
            _ = self.next_char();
        }
        let mut int = match self.next_char()? {
            ch @ '0'..='9' => {
                let digit = T::from((ch as u8 - b'0') as i8);
                if negative { -digit } else { digit }
            }
            _ => {
                return Err(Error::ExpectedInteger);
            }
        };
        loop {
            match self.input.chars().next() {
                Some(ch @ '0'..='9') => {
                    self.input = &self.input[1..];
                    int *= T::from(10);
                    let digit = T::from((ch as u8 - b'0') as i8);
                    if negative {
                        int -= digit;
                    } else {
                        int += digit;
                    }
                }
                // This is not a number, just a string starting with digits.
                Some(c) if is_unquoted_string_char(c) => return Err(Error::ExpectedInteger),
                _ => break
            }
        }

        if let Some((_sign, _value)) = self.parse_optional_exponent()? {
            todo!("apply exponents");
        }

        Ok(int)
    }

    fn parse_string(&mut self) -> Result<Cow<'de, str>> {
        self.eat_whitespace()?;

        match self.peek_char()? {
            '"' => self.parse_quoted_string(),
            c if is_unquoted_string_char(c) => {
                Ok(Cow::Borrowed(self.parse_unquoted_string()?))
            }
            _ => Err(Error::ExpectedString)
        }
    }

    fn parse_unquoted_string(&mut self) -> Result<&'de str> {
        debug_assert!(self.input.chars().next().is_some_and(is_unquoted_string_char));

        let end_of_string = self.input
            .char_indices()
            .skip(1)
            .find(|(_, c)| !is_unquoted_string_char(*c));
        if let Some((end, _)) = end_of_string {
            let s = &self.input[..end];
            self.input = &self.input[end..];
            Ok(s)
        } else {
            // string continues to end of file
            let s = self.input;
            self.input = "";
            Ok(s)
        }
    }
    
    fn parse_quoted_string(&mut self) -> Result<Cow<'de, str>> {
        let start_quote = self.next_char();
        // Start quote should already be checked by the function calling `parse_quoted_string`.
        debug_assert_eq!(start_quote, Ok('"'));

        let mut chars = self.input.match_indices(&['"', '\\']);

        match chars.next().ok_or(Error::UnclosedString)? {
            (i, "\"") => {
                let content = &self.input[..i];
                self.input = &self.input[i+1..];
                return Ok(Cow::Borrowed(content))
            }
            (i, "\\") => {
                let mut s = String::from(&self.input[..i]);
                self.input = &self.input[i..];
                self.parse_quoted_escaped_string_contents(&mut s)?;
                return Ok(Cow::Owned(s));
            }
            _ => unreachable!() // all patterns are covered
        }
    }

    fn parse_quoted_escaped_string_contents(&mut self, s: &mut String) -> Result<()> {
        let mut chars = self.input.char_indices();
        loop {
            let (i, c) = chars.next().ok_or(Error::UnclosedString)? ;
            match c {
                '\\' => match chars.next().ok_or(Error::UnclosedString)?.1 {
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
                            let c = chars.next().ok_or(Error::UnclosedString)?.1;
                            match c {
                                '0'..='9' => Ok(c as u32 - b'0' as u32),
                                'a'..='f' => Ok(c as u32 - b'a' as u32 + 10),
                                'A'..='F' => Ok(c as u32 - b'A' as u32 + 10),
                                _ => Err(Error::EscapeInvalid)
                            }
                        };
                        let value = (1..=4)
                            .rev()
                            .map(|i| hex_digit().map(|d| d * 16u32.pow(i)))
                            .sum::<Result<u32>>()?;
                        s.push(char::from_u32(value).ok_or(Error::CharCodeInvalid(value))?);
                    }
                    _ => return Err(Error::EscapeInvalid)
                }
                '"' => {
                    self.input = &self.input[i+1..];
                    return Ok(())
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
        self.eat_whitespace()?;

        match self.peek_char()? {
            '"' => self.deserialize_string(visitor),
            _ => todo!("find out how to implement"),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(self.parse_bool()?)
    }

    deserialize_number!{
        parse_signed:
            deserialize_i8 visit_i8,
            deserialize_i16 visit_i16,
            deserialize_i32 visit_i32,
            deserialize_i64 visit_i64,
    }
    deserialize_number!{
        parse_unsigned:
            deserialize_u8 visit_u8,
            deserialize_u16 visit_u16,
            deserialize_u32 visit_u32,
            deserialize_u64 visit_u64,
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value>
    where
         V: Visitor<'de>,
    {
        todo!("floats")
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value>
    where
         V: Visitor<'de>,
    {
        todo!("floats")
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // chars are just single-character strings
        let s = self.parse_string()?;
        let c = s.chars().next().ok_or(Error::ExpectedChar)?;
        if s.chars().next().is_some() {
            // string contains multiple chars
            return Err(Error::ExpectedChar);
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
            Err(Error::ExpectedNull)
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value>
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
        if self.next_char()? == '[' {
            // Give the visitor access to each element of the sequence.
            let value = visitor.visit_seq(&mut *self)?;

            self.eat_whitespace()?;
            if self.next_char()? == ']' {
                Ok(value)
            } else {
                Err(Error::UnclosedArray)
            }

        } else {
            Err(Error::ExpectedArray)
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
            let value = visitor.visit_map(Map { de: self, eof_closes: false })?;
            self.eat_whitespace()?;
            if self.next_char()? == '}' {
                Ok(value)
            } else {
                Err(Error::UnclosedMap)
            }
        } else if is_at_start {
            // braces can be left out at the top level
            visitor.visit_map(Map { de: self, eof_closes: true })
        } else {
            return Err(Error::ExpectedMap);
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
        if self.peek_char().map(|c| c == '"' || is_unquoted_string_char(c))? {
            // Visit a unit variant.
            visitor.visit_enum(self.parse_string()?.into_deserializer())
        } else if self.next_char()? == '{' {
            // Visit a newtype variant, tuple variant, or struct variant.
            let value = visitor.visit_enum(Enum::new(self))?;
            // Parse the matching close brace.
            if self.next_char()? == '}' {
                Ok(value)
            } else {
                Err(Error::UnclosedMap)
            }
        } else {
            // TODO: top-level omitted {} ?
            Err(Error::ExpectedEnum)
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
            Err(err) => if self.eof_closes {
                return Ok(None);
            } else {
                return Err(err);
            }
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
        Err(Error::ExpectedString)
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
    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.de, visitor)
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize;
    use super::from_str;

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
}
