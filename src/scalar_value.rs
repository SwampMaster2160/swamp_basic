use std::rc::Rc;

use num::{BigInt, complex::Complex64};
use strum_macros::EnumDiscriminants;

use crate::lexer::type_restriction::TypeRestriction;

#[derive(Debug, Clone, PartialEq, EnumDiscriminants)]
pub enum ScalarValue {
	/// An integer of any magnitude
	BigInteger(Rc<BigInt>),
	/// A size or index
	Size(usize),
	/// An unsigned byte
	Byte(u8),
	/// A 64-bit floating point number
	Float(f64),
	/// A complex number with 64-bit float real and imaginary parts
	Complex(Complex64),
	/// A UTF-8 string
	String(Rc<String>),
	/// A single Unicode char
	Char(char),
	/// A string with no characters
	EmptyString,
	/// A boolean
	Bool(bool),
}

impl ScalarValue {
	fn is_integer(&self) -> bool {
		matches!(self, Self::BigInteger(_) | Self::Size(_) | Self::Byte(_))
	}

	fn is_float(&self) -> bool {
		matches!(self, Self::Float(_) | Self::Complex(_))
	}

	/// Returns true if the value conforms to the type restriction.
	pub fn conforms_to_type_restriction(&self, restriction: TypeRestriction) -> bool {
		match restriction {
			TypeRestriction::Any => true,
			TypeRestriction::Integer => self.is_integer(),
			TypeRestriction::Float => self.is_float(),
			TypeRestriction::Number => self.is_integer() || self.is_float(),
			TypeRestriction::String => matches!(self, Self::String(_) | Self::Char(_) | ScalarValue::EmptyString),
			TypeRestriction::Boolean => matches!(self, Self::Bool(_)),
		}
	}
}