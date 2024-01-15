use strum::IntoEnumIterator;
use std::collections::{HashMap, HashSet};

use strum_macros::EnumIter;

use crate::Main;

#[derive(Debug, Clone, Copy, EnumIter, PartialEq, Eq)]
#[repr(u8)]
pub enum Operator {
	AddConcatenate,
	MinusNegate,
	Multiply,
	Divide,
	FlooredDivide,
	Exponent,
	EqualToAssign,
	EqualTo,
	NotEqualTo,
	LessThan,
	GreaterThan,
	LessThanOrEqualTo,
	GreaterThanOrEqualTo,
	Modulus,
	And,
	Or,
	ExclusiveOr,
	Not,
}

impl Operator {
	const NON_ALPHABETIC_CHARACTER_SET: [char; 11] = ['+', '-', '*', '/', '^', '=', '<', '>', '&', '|', '!'];

	/// Takes a string and returns the operator that is associated with the string if it is a name or alias for an operator.
	#[inline(always)]
	pub fn from_str(main_data: &Main, string: &str) -> Option<Self> {
		main_data.string_to_operator_mapping.get(string.to_lowercase().as_str()).copied()
	}

	#[inline(always)]
	const fn get_names(self) -> (&'static str, &'static[&'static str]) {
		match self {
			Self::AddConcatenate => ("+", &[]),
			Self::MinusNegate => ("-", &[]),
			Self::Multiply => ("*", &[]),
			Self::Divide => ("/", &[]),
			Self::FlooredDivide => ("//", &[]),
			Self::Exponent => ("^", &["pow"]),
			Self::EqualToAssign => ("=", &[]),
			Self::EqualTo => ("==", &[]),
			Self::NotEqualTo => ("<>", &["!="]),
			Self::LessThan => ("<", &[]),
			Self::GreaterThan => (">", &[]),
			Self::LessThanOrEqualTo => ("<=", &["=<"]),
			Self::GreaterThanOrEqualTo => (">=", &["=>"]),
			Self::Modulus => ("%", &["mod", "modulus"]),
			Self::And => ("&", &["and"]),
			Self::Or => ("|", &["or", "inclusiveor"]),
			Self::ExclusiveOr => ("xor", &["eor", "exclusiveor"]),
			Self::Not => ("!", &["not"]),
		}
	}

	pub const fn get_name(self) -> &'static str {
		self.get_names().0
	}

	#[inline(always)]
	/// Returns a hashmap mapping operator names and aliases to operators
	pub fn get_string_to_operator_mapping() -> HashMap<&'static str, Self> {
		let mut out = HashMap::new();
		for operator in Self::iter() {
			let (operator_name, operator_aliases) = operator.get_names();
			out.insert(operator_name, operator);
			for operator_alias in operator_aliases {
				out.insert(operator_alias, operator);
			}
		}
		out
	}

	pub const fn get_precedence_priority() -> &'static[&'static[Self]] {
		&[
			&[Self::Exponent],
			&[Self::Multiply, Self::Divide, Self::FlooredDivide, Self::Modulus],
			&[Self::AddConcatenate, Self::MinusNegate],
			&[Self::EqualTo, Self::EqualToAssign, Self::NotEqualTo, Self::LessThan, Self::LessThanOrEqualTo, Self::GreaterThan, Self::GreaterThanOrEqualTo],
			&[Self::And],
			&[Self::ExclusiveOr],
			&[Self::Or],
		]
	}

	#[inline(always)]
	pub fn is_char_in_non_alphabetic_character_set(main: &Main, chr: char) -> bool {
		main.operator_character_set.contains(&chr)
	}

	#[inline(always)]
	pub fn get_character_set() -> HashSet<char> {
		Self::NON_ALPHABETIC_CHARACTER_SET
			.into_iter()
			.collect()
	}
}