use num_traits::Zero;
use strum::IntoEnumIterator;
use std::{collections::HashMap, fmt::Display};

use strum_macros::EnumIter;

use crate::{Main, error::BasicError, scalar_value::{scalar_value::ScalarValue, integer::BasicInteger, string::BasicString}};

#[derive(Debug, Clone, Copy, EnumIter, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TypeRestriction {
	Any,
	RealNumber,
	Integer,
	Float,
	String,
	Boolean,
	ComplexFloat,
	Number,
}

impl TypeRestriction {
	const SUFFIX_CHARS: [char; 5] = ['#', '%', '~', '$', '?'];

	pub fn default_value(self) -> ScalarValue {
		match self {
			Self::Any | Self::Number | Self::RealNumber | Self::Integer => ScalarValue::Integer(BasicInteger::zero()),
			Self::Float | Self::ComplexFloat => ScalarValue::Float(0.0),
			Self::Boolean => ScalarValue::Boolean(false),
			Self::String => ScalarValue::String(BasicString::EmptyString),
		}
	}

	/// Takes a char and returns the type restriction that is associated with the char if it is a symbol for a type restriction.
	#[inline(always)]
	pub fn from_string_with_suffix<'a>(main_data: &'a Main, name_with_suffix: &'a str) -> Result<(&'a str, Self), BasicError> {
		let suffix_start_index = match name_with_suffix.find(Self::SUFFIX_CHARS) {
			Some(index) => index,
			None => return Ok((name_with_suffix, Self::Any)),
		};
		let name_without_suffix = &name_with_suffix[..suffix_start_index];
		let suffix = &name_with_suffix[suffix_start_index..];
		let type_restriction = *main_data.string_to_type_restriction_mapping.get(suffix)
			.ok_or(BasicError::InvalidTypeRestriction(suffix.to_string()))?;
		Ok((name_without_suffix, type_restriction))
	}

	/// Returns the suffix character for the type restriction or None if it does not have one
	pub const fn get_type_restriction_suffix(&self) -> Option<&'static str> {
		match self {
			Self::Any => None,
			Self::RealNumber => Some("#"),
			Self::Integer => Some("%"),
			Self::Float => Some("~"),
			Self::String => Some("$"),
			Self::Boolean => Some("?"),
			Self::Number => Some("##"),
			Self::ComplexFloat => Some("~~"),
			//Self::GaussianInteger => Some("%%"),
		}
	}

	/// Returns a hashmap mapping char suffixes to type restrictions
	#[inline(always)]
	pub fn get_string_to_type_restruction_mapping() -> HashMap<&'static str, Self> {
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

impl Display for TypeRestriction {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TypeRestriction::Any => write!(formatter, "any"),
			TypeRestriction::RealNumber => write!(formatter, "real number"),
			TypeRestriction::Integer => write!(formatter, "integer"),
			TypeRestriction::Float => write!(formatter, "floating-point number"),
			TypeRestriction::String => write!(formatter, "string"),
			TypeRestriction::Boolean => write!(formatter, "boolean"),
			TypeRestriction::ComplexFloat => write!(formatter, "complex floating-point number"),
			//TypeRestriction::GaussianInteger => write!(formatter, "gaussian integer"),
			TypeRestriction::Number => write!(formatter, "number"),
		}
	}
}