use std::rc::Rc;
use core::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BasicString {
	/// A UTF-8 string
	String(Rc<String>),
	/// A single Unicode char
	Char(char),
	/// A string with no characters
	EmptyString,
}

impl Display for BasicString {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::String(value) => write!(formatter, "{value}"),
			Self::Char(value) => write!(formatter, "{value}"),
			Self::EmptyString => write!(formatter, ""),
		}
	}
}