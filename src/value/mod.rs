use std::collections::BTreeMap;
use std::ops::Index;

use serde::de;

use crate::{Error, Result};

mod ser;
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
impl Number {
    pub(crate) fn visit<'de, V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.0 {
            NumberRepr::Unsigned(n) => visitor.visit_u64(n),
            NumberRepr::Signed(n) => visitor.visit_i64(n),
            NumberRepr::Float(n) => visitor.visit_f64(n),
        }
    }
}
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
    type Error = ();

    fn try_from(mut s: &str) -> std::result::Result<Self, Self::Error> {
        let mut negative = false;
        if s.chars().peekable().next() == Some('-') {
            negative = true;
            s = &s[1..];
        }
        match s.chars().next().ok_or(())? {
            '0' => parse_number(&s[1..], negative, 0),
            c @ '1'..='9' => {
                s = &s[1..];
                let mut significand = (c as u8 - b'0') as u64;
                loop {
                    match s.chars().next() {
                        Some(c @ '0'..='9') => {
                            s = &s[1..];
                            let digit = (c as u8 - b'0') as u64;
                            match significand
                                .checked_mul(10)
                                .and_then(|x| x.checked_add(digit))
                            {
                                Some(new) => {
                                    significand = new;
                                }
                                None => {
                                    return Ok(Number(NumberRepr::Float(parse_long_integer(
                                        s,
                                        negative,
                                        significand,
                                    )?)))
                                }
                            }
                        }
                        _ => return parse_number(s, negative, significand),
                    }
                }
            }
            _ => Err(()),
        }
    }
}

fn parse_number(s: &str, negative: bool, significand: u64) -> std::result::Result<Number, ()> {
    Ok(Number(match s.chars().next() {
        Some('.') => NumberRepr::Float(parse_decimal(&s[1..], negative, significand, 0)?),
        Some('e' | 'E') => NumberRepr::Float(parse_exponent(&s[1..], negative, significand, 0)?),
        Some(_) => return Err(()),
        None => {
            if negative {
                let neg = (significand as i64).wrapping_neg();
                if neg >= 0 {
                    NumberRepr::Float(-(significand as f64))
                } else {
                    NumberRepr::Signed(neg)
                }
            } else {
                NumberRepr::Unsigned(significand)
            }
        }
    }))
}

fn parse_decimal(
    mut s: &str,
    negative: bool,
    mut significand: u64,
    exponent_before_decimal_point: i32,
) -> std::result::Result<f64, ()> {
    let mut exponent_after_decimal_point = 0;
    while let Some(c @ '0'..='9') = s.chars().next() {
        let digit = (c as u8 - b'0') as u64;
        match significand
            .checked_mul(10)
            .and_then(|x| x.checked_add(digit))
        {
            Some(x) => significand = x,
            None => {
                let exponent = exponent_before_decimal_point + exponent_after_decimal_point;
                return parse_decimal_overflow(s, negative, significand, exponent);
            }
        }
        s = &s[1..];
        exponent_after_decimal_point -= 1;
    }

    if exponent_after_decimal_point == 0 {
        return Err(());
    }
    let exponent = exponent_before_decimal_point + exponent_after_decimal_point;
    match s.chars().next() {
        Some('e' | 'E') => parse_exponent(&s[1..], negative, significand, exponent),
        Some(_) => Err(()),
        None => f64_from_parts(negative, significand, exponent),
    }
}

fn parse_exponent(
    mut s: &str,
    negative: bool,
    significand: u64,
    starting_exp: i32,
) -> std::result::Result<f64, ()> {
    let positive_exp = match s.chars().next().ok_or(())? {
        '+' => {
            s = &s[1..];
            true
        }
        '-' => {
            s = &s[1..];
            false
        }
        _ => true,
    };

    let next = s.chars().next().ok_or(())?;
    s = &s[1..];
    let mut exp = match next {
        c @ '0'..='9' => (c as u8 - b'0') as i32,
        _ => return Err(()),
    };

    while let Some(c @ '0'..='9') = s.chars().next() {
        s = &s[1..];
        let digit = (c as u8 - b'0') as i32;

        match exp.checked_mul(10).and_then(|x| x.checked_add(digit)) {
            Some(x) => exp = x,
            None => {
                let zero_significand = significand == 0;
                return parse_exponent_overflow(s, negative, zero_significand, positive_exp);
            }
        }
    }

    let final_exp = if positive_exp {
        starting_exp.saturating_add(exp)
    } else {
        starting_exp.saturating_sub(exp)
    };

    f64_from_parts(negative, significand, final_exp)
}

static POW10: [f64; 309] = [
    1e000, 1e001, 1e002, 1e003, 1e004, 1e005, 1e006, 1e007, 1e008, 1e009, //
    1e010, 1e011, 1e012, 1e013, 1e014, 1e015, 1e016, 1e017, 1e018, 1e019, //
    1e020, 1e021, 1e022, 1e023, 1e024, 1e025, 1e026, 1e027, 1e028, 1e029, //
    1e030, 1e031, 1e032, 1e033, 1e034, 1e035, 1e036, 1e037, 1e038, 1e039, //
    1e040, 1e041, 1e042, 1e043, 1e044, 1e045, 1e046, 1e047, 1e048, 1e049, //
    1e050, 1e051, 1e052, 1e053, 1e054, 1e055, 1e056, 1e057, 1e058, 1e059, //
    1e060, 1e061, 1e062, 1e063, 1e064, 1e065, 1e066, 1e067, 1e068, 1e069, //
    1e070, 1e071, 1e072, 1e073, 1e074, 1e075, 1e076, 1e077, 1e078, 1e079, //
    1e080, 1e081, 1e082, 1e083, 1e084, 1e085, 1e086, 1e087, 1e088, 1e089, //
    1e090, 1e091, 1e092, 1e093, 1e094, 1e095, 1e096, 1e097, 1e098, 1e099, //
    1e100, 1e101, 1e102, 1e103, 1e104, 1e105, 1e106, 1e107, 1e108, 1e109, //
    1e110, 1e111, 1e112, 1e113, 1e114, 1e115, 1e116, 1e117, 1e118, 1e119, //
    1e120, 1e121, 1e122, 1e123, 1e124, 1e125, 1e126, 1e127, 1e128, 1e129, //
    1e130, 1e131, 1e132, 1e133, 1e134, 1e135, 1e136, 1e137, 1e138, 1e139, //
    1e140, 1e141, 1e142, 1e143, 1e144, 1e145, 1e146, 1e147, 1e148, 1e149, //
    1e150, 1e151, 1e152, 1e153, 1e154, 1e155, 1e156, 1e157, 1e158, 1e159, //
    1e160, 1e161, 1e162, 1e163, 1e164, 1e165, 1e166, 1e167, 1e168, 1e169, //
    1e170, 1e171, 1e172, 1e173, 1e174, 1e175, 1e176, 1e177, 1e178, 1e179, //
    1e180, 1e181, 1e182, 1e183, 1e184, 1e185, 1e186, 1e187, 1e188, 1e189, //
    1e190, 1e191, 1e192, 1e193, 1e194, 1e195, 1e196, 1e197, 1e198, 1e199, //
    1e200, 1e201, 1e202, 1e203, 1e204, 1e205, 1e206, 1e207, 1e208, 1e209, //
    1e210, 1e211, 1e212, 1e213, 1e214, 1e215, 1e216, 1e217, 1e218, 1e219, //
    1e220, 1e221, 1e222, 1e223, 1e224, 1e225, 1e226, 1e227, 1e228, 1e229, //
    1e230, 1e231, 1e232, 1e233, 1e234, 1e235, 1e236, 1e237, 1e238, 1e239, //
    1e240, 1e241, 1e242, 1e243, 1e244, 1e245, 1e246, 1e247, 1e248, 1e249, //
    1e250, 1e251, 1e252, 1e253, 1e254, 1e255, 1e256, 1e257, 1e258, 1e259, //
    1e260, 1e261, 1e262, 1e263, 1e264, 1e265, 1e266, 1e267, 1e268, 1e269, //
    1e270, 1e271, 1e272, 1e273, 1e274, 1e275, 1e276, 1e277, 1e278, 1e279, //
    1e280, 1e281, 1e282, 1e283, 1e284, 1e285, 1e286, 1e287, 1e288, 1e289, //
    1e290, 1e291, 1e292, 1e293, 1e294, 1e295, 1e296, 1e297, 1e298, 1e299, //
    1e300, 1e301, 1e302, 1e303, 1e304, 1e305, 1e306, 1e307, 1e308,
];

fn f64_from_parts(
    negative: bool,
    significand: u64,
    mut exponent: i32,
) -> std::result::Result<f64, ()> {
    let mut f = significand as f64;
    loop {
        match POW10.get(exponent.wrapping_abs() as usize) {
            Some(&pow) => {
                if exponent >= 0 {
                    f *= pow;
                    if f.is_infinite() {
                        return Err(());
                    }
                } else {
                    f /= pow;
                }
                break;
            }
            None => {
                if f == 0.0 {
                    break;
                }
                if exponent >= 0 {
                    return Err(());
                }
                f /= 1e308;
                exponent += 308;
            }
        }
    }
    Ok(if negative { -f } else { f })
}

#[cold]
#[inline(never)]
fn parse_long_integer(
    mut s: &str,
    negative: bool,
    significand: u64,
) -> std::result::Result<f64, ()> {
    let mut exponent = 0;
    loop {
        match s.chars().next() {
            Some('0'..='9') => {
                s = &s[1..];
                // This could overflow... if your integer is gigabytes long.
                // Ignore that possibility.
                exponent += 1;
            }
            Some('.') => {
                return parse_decimal(&s[1..], negative, significand, exponent);
            }
            Some('e' | 'E') => {
                return parse_exponent(&s[1..], negative, significand, exponent);
            }
            Some(_) => return Err(()),
            None => {
                return f64_from_parts(negative, significand, exponent);
            }
        }
    }
}

#[cold]
#[inline(never)]
fn parse_decimal_overflow(
    mut s: &str,
    negative: bool,
    significand: u64,
    exponent: i32,
) -> std::result::Result<f64, ()> {
    // The next multiply/add would overflow, so just ignore all further
    // digits.
    while let Some('0'..='9') = s.chars().next() {
        s = &s[1..];
    }

    match s.chars().next() {
        Some('e' | 'E') => parse_exponent(&s[1..], negative, significand, exponent),
        Some(_) => Err(()),
        _ => f64_from_parts(negative, significand, exponent),
    }
}

#[cold]
#[inline(never)]
fn parse_exponent_overflow(
    mut s: &str,
    negative: bool,
    zero_significand: bool,
    positive_exp: bool,
) -> std::result::Result<f64, ()> {
    // Error instead of +/- infinity.
    if !zero_significand && positive_exp {
        return Err(());
    }

    while let Some('0'..='9') = s.chars().next() {
        s = &s[1..];
    }
    Ok(if negative { -0.0 } else { 0.0 })
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
