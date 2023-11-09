use std::rc::Rc;

use num::{BigInt, complex::Complex64, bigint::Sign, ToPrimitive};
use strum_macros::EnumDiscriminants;

use crate::{lexer::type_restriction::TypeRestriction, error::BasicError};

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

	/// Given a length of a container, will return Ok(index) if the value can be used as a index for the container or returns an error otherwise
	pub fn as_index(&self, container_length: usize) -> Result<usize, BasicError> {
		match self {
			Self::Size(index) => {
				let index = *index;
				match index < container_length {
					true => Ok(index),
					false => Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				}
			}
			ScalarValue::Byte(index) => {
				// A usize is at least 16 bits wide
				let index = *index as usize;
				match index < container_length {
					true => Ok(index),
					false => Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				}
			}
			ScalarValue::BigInteger(index) => {
				// If the index is negative, add the length of the container to convert it from the -container_length..0 range to the 0..container_length range
				let index = match index.sign() == Sign::Minus {
					false => index.clone(),
					true => Rc::new(index.as_ref().clone() + BigInt::from(container_length))
				};
				// The index is invalid if it is not between 0 and usize::MAX
				let index_as_usize = match index.to_usize() {
					Some(index) => index,
					None => return Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				};
				// The index is invalid if it is not less than the container length
				match index_as_usize < container_length {
					true => Ok(index_as_usize),
					false => Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				}
			}
			_ => Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::Integer)),
		}
	}
}