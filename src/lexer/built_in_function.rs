use std::collections::HashMap;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::Main;

#[derive(Debug, Clone, Copy, EnumIter, PartialEq, Eq)]
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
	True,
	False,
	Pi,
	EulersNumber,
	ImaginaryUnit,
	Sum,
	Product,
}

impl BuiltInFunction {
	/// Takes a string and returns the built-in function that is associated with the string if it is a name or alias for a built-in function.
	#[inline(always)]
	pub fn from_str(main_data: &Main, string: &str) -> Option<Self> {
		main_data.string_to_built_in_function_mapping.get(string.to_lowercase().as_str()).copied()
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
			Self::True => ("true", &[]),
			Self::False => ("false", &[]),
			Self::Pi => ("pi", &[]),
			Self::EulersNumber => ("e", &["eulersnumber"]),
			Self::ImaginaryUnit => ("i", &["imaginaryunit", "imaginary"]),
			Self::Sum => ("sum", &[]),
			Self::Product => ("prod", &["product"]),
		}
	}

	pub const fn get_name(self) -> &'static str {
		let (name, _) = self.get_names();
		name
	}

	/// Returns a hashmap mapping built-in function names and aliases to built-in functions
	#[inline(always)]
	pub fn get_string_to_built_in_function_mapping() -> HashMap<&'static str, BuiltInFunction> {
		let mut out = HashMap::new();
		for function in BuiltInFunction::iter() {
			let (function_name, function_aliases) = function.get_names();
			out.insert(function_name, function);
			for function_alias in function_aliases {
				out.insert(function_alias, function);
			}
		}
		out
	}
}