use std::{rc::Rc, fmt::Display};

use num::{BigInt, complex::Complex64, bigint::{Sign, ToBigInt}, ToPrimitive, Zero};
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
	/// An unsigned 32-bit integer
	U32(u32),
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
		matches!(self, Self::BigInteger(_) | Self::Size(_) | Self::Byte(_) | Self::U32(_))
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
			ScalarValue::U32(index) => {
				let index = match (*index).try_into() {
					Ok(index) => index,
					Err(_) => return Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				};
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

	pub fn as_big_int(self) -> Result<BigInt, BasicError> {
		let out: BigInt = match self {
			Self::BigInteger(value) => match Rc::try_unwrap(value) {
				Ok(only_value) => only_value,
				Err(value) => value.as_ref().clone(),
			}
			ScalarValue::Byte(value) => value.into(),
			ScalarValue::Size(value) => value.into(),
			ScalarValue::U32(value) => value.into(),
			_ => return Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::Integer)),
		};
		Ok(out)
	}

	/// Casts a the value to conform to the given type restriction.
	pub fn cast_to(&self, cast_to: TypeRestriction) -> Result<Self, BasicError> {
		match (self, cast_to) {
			// Do nothing to a value if it already conforms to the type restriction
			(_, TypeRestriction::Any) |
			(Self::BigInteger(..) | Self::Size(..) | Self::Byte(..) | Self::U32(..), TypeRestriction::Integer | TypeRestriction::Number) |
			(Self::Float(..) | Self::Complex(..), TypeRestriction::Float | TypeRestriction::Number) |
			(Self::String(..) | Self::Char(..) | Self::EmptyString, TypeRestriction::String) |
			(Self::Bool(..), TypeRestriction::Boolean) => Ok(self.clone()),
			// To string
			(_, TypeRestriction::String) => Ok(Self::String(Rc::new(self.to_string()))),
			// To boolean

			// strings yes/no, y/n, true/false, t/f and 1/0 convert to booleans true/false
			(Self::String(string), TypeRestriction::Boolean) => match string {
				_ if string.eq_ignore_ascii_case("true") || string.eq_ignore_ascii_case("t") ||
				string.eq_ignore_ascii_case("yes") || string.eq_ignore_ascii_case("y") ||string.eq_ignore_ascii_case("1") => Ok(ScalarValue::Bool(true)),

				_ if string.eq_ignore_ascii_case("false") || string.eq_ignore_ascii_case("f") ||
				string.eq_ignore_ascii_case("no") || string.eq_ignore_ascii_case("n") || string.eq_ignore_ascii_case("0") => Ok(ScalarValue::Bool(false)),

				_ => Err(BasicError::UnableToCast(self.clone(), cast_to)),
			}
			(Self::Char(value), TypeRestriction::Boolean) => match value {
				_ if value.eq_ignore_ascii_case(&'t') || value.eq_ignore_ascii_case(&'y') || value.eq_ignore_ascii_case(&'1') => Ok(ScalarValue::Bool(true)),
				_ if value.eq_ignore_ascii_case(&'f') || value.eq_ignore_ascii_case(&'n') || value.eq_ignore_ascii_case(&'0') => Ok(ScalarValue::Bool(false)),
				_ => Err(BasicError::UnableToCast(self.clone(), cast_to)),
			}

			// Numbers are false if 0 or true otherwise
			(Self::BigInteger(value), TypeRestriction::Boolean) => Ok(Self::Bool(value.sign() != Sign::NoSign)),
			(Self::Size(value), TypeRestriction::Boolean) => Ok(Self::Bool(*value != 0)),
			(Self::Byte(value), TypeRestriction::Boolean) => Ok(Self::Bool(*value != 0)),
			(Self::U32(value), TypeRestriction::Boolean) => Ok(Self::Bool(*value != 0)),
			(Self::Float(value), TypeRestriction::Boolean) => Ok(Self::Bool(*value != 0.)),
			(Self::Complex(value), TypeRestriction::Boolean) => Ok(Self::Bool(*value != Complex64{ re: 0., im: 0. })),
			// To float
			(Self::Char(value), TypeRestriction::Float | TypeRestriction::Number) if *value == '.' => Ok(Self::Float(0.)),
			(Self::Char(value), TypeRestriction::Float | TypeRestriction::Number) if *value == 'i' => Ok(Self::Complex(Complex64::i())),
			// To integer
			(Self::Bool(value), TypeRestriction::Integer | TypeRestriction::Number) => Ok(Self::Byte(*value as u8)),
			(Self::Char(value), TypeRestriction::Integer | TypeRestriction::Number) => Ok(Self::Byte(value.to_digit(10).ok_or(BasicError::UnableToCast(self.clone(), cast_to))? as u8)),
			(Self::String(value), TypeRestriction::Integer) => match value.parse::<BigInt>() {
				Ok(value) => Ok(Self::BigInteger(Rc::new(value))),
				Err(_) => Err(BasicError::UnableToCast(self.clone(), cast_to)),
			}
			(Self::Float(value), TypeRestriction::Integer) => match value.to_bigint() {
				Some(value) => Ok(Self::BigInteger(Rc::new(value))),
				None => Err(BasicError::UnableToCast(self.clone(), cast_to)),
			}
			(Self::Complex(value), TypeRestriction::Integer) if value.im.is_zero() => match value.re.to_bigint() {
				Some(value) => Ok(Self::BigInteger(Rc::new(value))),
				None => Err(BasicError::UnableToCast(self.clone(), cast_to)),
			}
			// To float
			(Self::Bool(value), TypeRestriction::Float) => Ok(Self::Float(*value as u8 as f64)),
			(Self::Char(value), TypeRestriction::Float) => Ok(Self::Float(value.to_digit(10).ok_or(BasicError::UnableToCast(self.clone(), cast_to))? as u8 as f64)),
			(Self::String(value), TypeRestriction::Float) => match value.parse::<Complex64>() {
				Ok(value) => Ok(Self::Complex(value)),
				Err(_) => Err(BasicError::UnableToCast(self.clone(), cast_to)),
			}
			// To number
			(Self::String(value), TypeRestriction::Number) => match value.parse::<BigInt>() {
				Ok(value) => Ok(Self::BigInteger(Rc::new(value))),
				Err(_) => match value.parse::<Complex64>() {
					Ok(value) => Ok(Self::Complex(value)),
					Err(_) => Err(BasicError::UnableToCast(self.clone(), cast_to)),
				}
			}

			_ => Err(BasicError::UnableToCast(self.clone(), cast_to)),
		}
	}
}

impl Display for ScalarValue {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BigInteger(value) => write!(formatter, "{value}"),
			Self::Size(value) => write!(formatter, "{value}"),
			Self::Byte(value) => write!(formatter, "{value}"),
			Self::U32(value) => write!(formatter, "{value}"),
			Self::Bool(value) => write!(formatter, "{value}"),
			Self::Float(value) => write!(formatter, "{value}"),
			Self::String(value) => write!(formatter, "{value}"),
			Self::EmptyString => write!(formatter, ""),
			Self::Char(value) => write!(formatter, "{value}"),
			Self::Complex(value) => write!(formatter, "{value}"),
		}
	}
}