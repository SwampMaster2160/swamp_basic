use strum::IntoEnumIterator;
use std::collections::HashMap;

use strum_macros::EnumIter;

use crate::Main;

#[derive(Debug, Clone, Copy, EnumIter)]
#[repr(u8)]
pub enum Separator {
	OpenBracket,
	ClosedBracket,
	Comma,
	Semicolon,
	Colon,
}

impl Separator {
	/// Takes a char and returns the seperator that is associated with the char if it is a symbol for a separator.
	pub fn from_char(main_data: &mut Main, suffix: char) -> Option<Self> {
		main_data.char_to_separator_mapping.get(&suffix).copied()
	}

	/// Returns the separator character
	pub const fn get_symbol_char(&self) -> char {
		match self {
			Self::OpenBracket => '(',
			Self::ClosedBracket => ')',
			Self::Comma => ',',
			Self::Semicolon => ';',
			Self::Colon => ':',
		}
	}

	/// Returns a hashmap mapping chars to separators
	pub fn get_char_to_separator_mapping() -> HashMap<char, Self> {
		Self::iter()
			.map(|sep| (sep.get_symbol_char(), sep))
			.collect()
	}
}