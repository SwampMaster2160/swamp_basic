use std::collections::HashMap;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::Main;

#[derive(Debug, Clone, Copy, EnumIter)]
#[repr(u8)]
pub enum BuiltInFunction {
	AbsoluteValue,
	Arctangent,
	Cosine,
	Exponential,
	Integer,
	Logarithm,
	Random,
	Sign,
	Sine,
	SquareRoot,
	Tangent,
}

impl BuiltInFunction {
	/// Takes a string and returns the built-in function that is associated with the string if it is a name or alias for a built-in function.
	#[inline(always)]
	pub fn from_str(main_data: &mut Main, string: &str) -> Option<Self> {
		main_data.string_to_built_in_keyword_mapping.get(string.to_lowercase().as_str()).copied()
	}

	/// Returns the name of the built-in function and a list of aliases.
	#[inline(always)]
	const fn get_names(self) -> (&'static str, &'static[&'static str]) {
		match self {
			Self::AbsoluteValue => ("abs", &["absolutevalue"]),
			Self::Arctangent => ("atn", &["atan", "arctangent"]),
			Self::Cosine => ("cos", &["cosine"]),
			Self::Exponential => ("exp", &["exponential"]),
			Self::Integer => ("int", &["integer"]),
			Self::Logarithm => ("log", &["ln", "logarithm"]),
			Self::Random => ("rnd", &["rand", "random"]),
			Self::Sign => ("sgn", &["sign", "signum"]),
			Self::Sine => ("sin", &["sine"]),
			Self::SquareRoot => ("sqr", &["sqrt", "squareroot"]),
			Self::Tangent => ("tan", &["tangent"]),
		}
	}

	/// Returns a hashmap mapping built-in function names and aliases to built-in functions
	#[inline(always)]
	pub fn get_string_to_built_in_function_mapping() -> HashMap<&'static str, BuiltInFunction> {
		let mut out = HashMap::new();
		for keyword in BuiltInFunction::iter() {
			let (keyword_name, keyword_aliases) = keyword.get_names();
			out.insert(keyword_name, keyword);
			for keyword_alias in keyword_aliases {
				out.insert(keyword_alias, keyword);
			}
		}
		out
	}
}