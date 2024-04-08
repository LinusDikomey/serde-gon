use std::collections::BTreeMap;
use std::ops::Index;

use crate::Error;

mod visitor;

#[derive(Clone, Debug, Default, PartialEq)]
pub enum Value {
    #[default]
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Map(BTreeMap<String, Value>),
}
impl Index<&str> for Value {
    type Output = Self;

    fn index(&self, index: &str) -> &Self::Output {
        match self {
            Self::Map(map) => map.get(index).unwrap_or(&Value::Null),
            _ => &Value::Null,
        }
    }
}
impl Index<String> for Value {
    type Output = Self;

    fn index(&self, index: String) -> &Self::Output {
        &self[index.as_str()]
    }
}
impl Index<usize> for Value {
    type Output = Self;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Self::Array(array) => array.get(index).unwrap_or(&Value::Null),
            _ => &Value::Null,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Number(NumberRepr);
impl From<u8> for Number {
    fn from(value: u8) -> Self {
        Self(NumberRepr::Unsigned(value.into()))
    }
}
impl From<u16> for Number {
    fn from(value: u16) -> Self {
        Self(NumberRepr::Unsigned(value.into()))
    }
}
impl From<u32> for Number {
    fn from(value: u32) -> Self {
        Self(NumberRepr::Unsigned(value.into()))
    }
}
impl From<u64> for Number {
    fn from(value: u64) -> Self {
        Self(NumberRepr::Unsigned(value))
    }
}
impl From<i8> for Number {
    fn from(value: i8) -> Self {
        Self(if value < 0 {
            NumberRepr::Signed(value.into())
        } else {
            NumberRepr::Unsigned(value as u64)
        })
    }
}
impl From<i16> for Number {
    fn from(value: i16) -> Self {
        Self(if value < 0 {
            NumberRepr::Signed(value.into())
        } else {
            NumberRepr::Unsigned(value as u64)
        })
    }
}
impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Self(if value < 0 {
            NumberRepr::Signed(value.into())
        } else {
            NumberRepr::Unsigned(value as u64)
        })
    }
}
impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self(if value < 0 {
            NumberRepr::Signed(value)
        } else {
            NumberRepr::Unsigned(value as u64)
        })
    }
}
impl From<f32> for Number {
    fn from(value: f32) -> Self {
        Self(NumberRepr::Float(value.into()))
    }
}
impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self(NumberRepr::Float(value))
    }
}
impl TryFrom<&str> for Number {
    type Error = Error;

    fn try_from(_value: &str) -> Result<Self, Self::Error> {
        todo!("implement any number parsing")
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum NumberRepr {
    Float(f64),
    Unsigned(u64),
    Signed(i64),
}

#[cfg(test)]
mod tests {
    use crate::{Number, Value};

    #[test]
    fn deserialize_to_values() {
        let s = r#"
            // comment at the start
            {
                "a": 3,
                "b": 4.5,
                "c": [6, "hello"],
                // comment
                "d": { "e": null }
            }
        "#;
        let v: Value = s.parse().unwrap();
        assert_eq!(v["a"], Value::Number(Number::from(3)));
        assert_eq!(v["b"], Value::Number(Number::from(4.5)));
        assert_eq!(v["c"][0], Value::Number(Number::from(6)));
        assert_eq!(v["c"][1], Value::String("hello".to_owned()));
        assert_eq!(v["d"]["e"], Value::Null);
    }
}
