use strum::IntoEnumIterator;
use std::collections::HashMap;

use strum_macros::EnumIter;

use crate::Main;

#[derive(Debug, Clone, Copy, EnumIter)]
#[repr(u8)]
pub enum TypeRestriction {
	Any,
	Number,
	Integer,
	Float,
	String,
	Boolean,
}

impl TypeRestriction {
	/// Takes a char and returns the type restriction that is associated with the char if it is a symbol for a type restriction.
	pub fn from_suffix_char(main_data: &mut Main, suffix: char) -> Option<Self> {
		main_data.char_to_type_restriction_mapping.get(&suffix).copied()
	}

	/// Returns the suffix character for the type restriction or None if it does not have one
	pub const fn get_type_restriction_suffix(&self) -> Option<char> {
		match self {
			Self::Any => None,
			Self::Number => Some('#'),
			Self::Integer => Some('%'),
			Self::Float => Some('~'),
			Self::String => Some('$'),
			Self::Boolean => Some('?'),
		}
	}

	/// Returns a hashmap mapping char suffixes to type restrictions
	pub fn get_char_to_type_restruction_mapping() -> HashMap<char, Self> {
		let mut out = HashMap::new();
		for type_restruction in Self::iter() {
			let symbol = type_restruction.get_type_restriction_suffix();
			match symbol {
				Some(symbol) => out.insert(symbol, type_restruction),
				None => continue,
			};
		}
		out
	}
}