use crate::Value;

use super::NumberRepr;

impl serde::Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Value::Null => serializer.serialize_unit(),
            &Value::Bool(b) => serializer.serialize_bool(b),
            Value::Number(n) => match n.0 {
                NumberRepr::Unsigned(n) => serializer.serialize_u64(n),
                NumberRepr::Signed(n) => serializer.serialize_i64(n),
                NumberRepr::Float(n) => serializer.serialize_f64(n),
            },
            Value::String(s) => serializer.serialize_str(s),
            Value::Array(v) => v.serialize(serializer),
            Value::Map(m) => {
                use serde::ser::SerializeMap;
                let mut map = serializer.serialize_map(Some(m.len()))?;
                for (k, v) in m {
                    map.serialize_entry(k, v)?;
                }
                map.end()
            }
        }
    }
}
