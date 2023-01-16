//! Deserialize values from environment variables.

use std::fmt;

use serde::de::IntoDeserializer;

pub mod parse;

/// Error type only for deserialization of env values.
///
/// Semantically private, only public as it's used in the API of the `internal`
/// module. Gets converted into `ErrorKind::EnvDeserialization` before reaching
/// the real public API.
#[derive(PartialEq, Eq)]
#[doc(hidden)]
pub struct DeError(pub(crate) String);

impl std::error::Error for DeError {}

impl fmt::Debug for DeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for DeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl serde::de::Error for DeError {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self(msg.to_string())
    }
}

/// Deserializer type. Semantically private (see `DeError`).
#[doc(hidden)]
pub struct Deserializer {
    value: String,
}

impl Deserializer {
    pub(crate) fn new(value: String) -> Self {
        Self { value }
    }
}

macro_rules! deserialize_via_parse {
    ($method:ident, $visit_method:ident, $int:ident) => {
        fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: serde::de::Visitor<'de>,
        {
            let s = self.value.trim();
            let v = s.parse().map_err(|e| {
                DeError(format!(
                    concat!("invalid value '{}' for type ", stringify!($int), ": {}"),
                    s, e,
                ))
            })?;
            visitor.$visit_method(v)
        }
    };
}

impl<'de> serde::Deserializer<'de> for Deserializer {
    type Error = DeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.value.into_deserializer().deserialize_any(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        let v = match self.value.trim() {
            "1" | "true" | "TRUE" => true,
            "0" | "false" | "FALSE" => false,
            other => return Err(DeError(format!("invalid value for bool: '{other}'"))),
        };

        visitor.visit_bool(v)
    }

    deserialize_via_parse!(deserialize_i8, visit_i8, i8);
    deserialize_via_parse!(deserialize_i16, visit_i16, i16);
    deserialize_via_parse!(deserialize_i32, visit_i32, i32);
    deserialize_via_parse!(deserialize_i64, visit_i64, i64);
    deserialize_via_parse!(deserialize_u8, visit_u8, u8);
    deserialize_via_parse!(deserialize_u16, visit_u16, u16);
    deserialize_via_parse!(deserialize_u32, visit_u32, u32);
    deserialize_via_parse!(deserialize_u64, visit_u64, u64);
    deserialize_via_parse!(deserialize_f32, visit_f32, f32);
    deserialize_via_parse!(deserialize_f64, visit_f64, f64);

    fn deserialize_newtype_struct<V>(
        self,
        _: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    serde::forward_to_deserialize_any! {
        char str string
        bytes byte_buf
        unit unit_struct
        map
        option
        struct
        identifier
        ignored_any

        // TODO: think about manually implementing these
        enum
        seq
        tuple tuple_struct
    }
}

mod formatter {
    use std::{
        collections::VecDeque,
        fmt::{self, Write},
    };

    use crate::{meta::Expr, template::Formatter, Config, FormatOptions};

    #[derive(Default)]
    struct EnvFormatter {
        buffer: String,
        nested_stack: VecDeque<&'static str>,
        current_env_field: Option<&'static str>,
        current_env_optional: Option<bool>,
    }

    /// Helper to emit `meta::Expr` into JSON5.
    struct PrintExpr(&'static Expr);
    impl From<&'static Expr> for PrintExpr {
        fn from(expr: &'static Expr) -> Self {
            Self(expr)
        }
    }
    impl fmt::Display for PrintExpr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self.0 {
                Expr::Str(str_) => write!(f, "{}", str_),
                Expr::Float(float_) => write!(f, "{}", float_),
                Expr::Integer(integer_) => write!(f, "{}", integer_),
                Expr::Bool(true) => write!(f, "1"),
                Expr::Bool(false) => write!(f, "0"),
                Expr::Array(_array) => write!(f, "Not Supported"),
                Expr::Map(_map) => write!(f, "Not Supported"),
            }
        }
    }

    impl Formatter for EnvFormatter {
        type ExprPrinter = PrintExpr;

        fn buffer(&mut self) -> &mut String {
            &mut self.buffer
        }

        fn finish(self) -> String {
            self.buffer
        }

        fn comment(&mut self, comment: impl fmt::Display) {
            writeln!(self.buffer, "#{comment}").unwrap()
        }

        fn env_comment(&mut self, env_key: &'static str) {
            assert!(self.current_env_field.is_none());
            self.current_env_field = Some(env_key);
        }

        fn default_or_required_comment(&mut self, default_value: Option<&'static Expr>) {
            self.current_env_optional = Some(default_value.is_some());
        }

        fn disabled_field(
            &mut self,
            name: &'static str,
            value: Option<&'static crate::meta::Expr>,
        ) {
            let env_key = match self.current_env_field.take() {
                Some(env_key) => {
                    self.buffer.truncate(self.buffer.len().saturating_sub(2)); // Remove empty comment-line
                    env_key
                }
                None => {
                    return;
                }
            };
            match self.current_env_optional.take() {
                Some(true) | None => writeln!(
                    self.buffer,
                    "#Param is optional\n#{env_key}=",
                    env_key = env_key
                ),
                Some(false) => writeln!(self.buffer, "{env_key}=", env_key = env_key),
            }
            .unwrap();

            match value.map(PrintExpr) {
                Some(v) => self.comment(format_args!(
                    "Sets the '{name}' field in the configuration (default value is {v})"
                )),
                None => self.comment(format_args!("Sets the '{name}' field in the configuration")),
            };
        }

        fn start_nested(&mut self, name: &'static str, doc: &[&'static str]) {
            self.nested_stack.push_back(name);
            self.comment(format!("Start '{name}'"));
            doc.iter().for_each(|doc_line| self.comment(doc_line));
        }

        fn end_nested(&mut self) {
            let name = self
                .nested_stack
                .pop_back()
                .expect("Unreachable, `end_nested` called only after `start_nested`");
            self.comment(format!("End '{name}'"));
        }
    }

    pub fn template<C: Config>(options: FormatOptions) -> String {
        let mut out = EnvFormatter::default();
        crate::template::format::<C>(&mut out, options);
        out.finish()
    }

    #[cfg(test)]
    mod tests {
        use crate as confique;
        use crate::Config;

        use super::*;

        #[test]
        fn test_simple_format() {
            #[allow(dead_code)]
            #[derive(Debug, Config)]
            struct Conf {
                /// This is url
                #[config(env = "URL", default = "0.0.0.0")]
                url: String,
                /// This is ports
                /// Separate by ','
                #[config(env = "PORTS", parse_env = crate::env::parse::list_by_comma)]
                ports: Vec<u16>,
                /// This is names
                /// Separate by '|'
                #[config(env = "NAMES", parse_env = crate::env::parse::list_by_sep::<'|', _, _>)]
                names: Vec<String>,
                /// This is optional surnames
                /// Separate by ';'
                #[config(env = "SURNAMES", parse_env = crate::env::parse::list_by_sep::<';', _, _>)]
                surnames: Option<Vec<String>>,
            }

            let template = super::template::<Conf>(FormatOptions::default());
            panic!("{}", template);
        }
    }
}

pub use formatter::template;

#[cfg(test)]
mod tests;
