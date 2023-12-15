use std::rc::Rc;
use core::fmt::Display;

use crate::get_rc_only_or_clone;

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum BasicString {
	/// A UTF-8 string
	String(Rc<String>),
	/// A single Unicode char
	Char(char),
	/// A string with no characters
	EmptyString,
}

impl BasicString {
	/// Makes a value compact.
	pub fn compact(self) -> Self {
		match self {
			Self::String(value) => {
				if value.is_empty() {
					Self::EmptyString
				}
				else {
					let mut iter = value.chars();
					let first_char = iter.next().unwrap();
					match iter.next() {
						Some(..) => Self::String(value),
						None => Self::Char(first_char),
					}
				}
			}
			Self::Char(..) | Self::EmptyString => self,
		}
	}

	/// Creates a new string from `self` concatenated with `right_value`.
	pub fn concatenate(self, right_value: Self) -> Self {
		match (self, right_value) {
			(Self::EmptyString, other) | (other, Self::EmptyString) => other,
			(Self::String(left_string_value), Self::Char(right_char_value)) => {
				let mut new_string = get_rc_only_or_clone(left_string_value);
				new_string.push(right_char_value);
				Self::String(Rc::new(new_string))
			}
			(Self::Char(left_char_value), Self::String(right_string_value)) => Self::String(Rc::new(format!("{left_char_value}{right_string_value}"))),
			(Self::Char(left_char_value), Self::Char(right_char_value)) => Self::String(Rc::new(format!("{left_char_value}{right_char_value}"))),
			(Self::String(left_string_value), Self::String(right_string_value)) => {
				let mut new_string = get_rc_only_or_clone(left_string_value);
				new_string.extend(right_string_value.chars());
				Self::String(Rc::new(new_string))
			}
		}
	}

	pub fn repeat(self, times: usize) -> Self {
		match times {
			0 => Self::EmptyString,
			1 => self,
			_ => match self {
				Self::String(value) => Self::String(Rc::new(value.repeat(times))),
				Self::Char(value) => Self::String(Rc::new(value.to_string().repeat(times))),
				Self::EmptyString => Self::EmptyString,
			}
		}
	}

	pub const SPACE: Self = Self::Char(' ');
	pub const NEW_LINE: Self = Self::Char('\n');
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