use std::{fmt::Display, rc::Rc};

use num::{BigInt, Complex, complex::Complex64};
use num_traits::Zero;

use crate::{lexer::type_restriction::TypeRestriction, error::BasicError};

use super::{integer::BasicInteger, string::BasicString};

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum ScalarValue {
	Integer(BasicInteger),
	Float(f64),
	Bool(bool),
	String(BasicString),
	ComplexFloat(Complex64),
	GaussianInteger(Complex<BasicInteger>),
}

impl ScalarValue {
	fn is_integer(&self) -> bool {
		match self {
			Self::Integer(..) => true,
			ScalarValue::GaussianInteger(value) => value.im.is_zero(),
			_ => false,
		}
	}

	fn is_float(&self) -> bool {
		match self {
			Self::Float(..) => true,
			ScalarValue::ComplexFloat(value) => value.im.is_zero(),
			_ => false,
		}
	}

	fn is_gaussian_integer(&self) -> bool {
		matches!(self, Self::GaussianInteger(..) | Self::Integer(..))
	}

	fn is_complex_float(&self) -> bool {
		matches!(self, Self::ComplexFloat(..) | Self::Float(..))
	}

	/// Returns true if the value conforms to the type restriction.
	pub fn conforms_to_type_restriction(&self, restriction: TypeRestriction) -> bool {
		match restriction {
			TypeRestriction::Any => true,
			TypeRestriction::Integer => self.is_integer(),
			TypeRestriction::Float => self.is_float(),
			TypeRestriction::RealNumber => self.is_integer() || self.is_float(),
			TypeRestriction::String => matches!(self, Self::String(_)),
			TypeRestriction::Boolean => matches!(self, Self::Bool(_)),
			TypeRestriction::GaussianInteger => self.is_gaussian_integer(),
			TypeRestriction::ComplexFloat => self.is_complex_float(),
			TypeRestriction::ComplexNumber => self.is_complex_float() || self.is_gaussian_integer(),
		}
	}

	/// Makes a value compact.
	pub fn compact(self) -> Self {
		// TODO
		self
	}

	/*/// Casts a the value to conform to the given type restriction.
	pub fn cast_to(&self, cast_to: TypeRestriction) -> Result<Self, BasicError> {
		match (self, cast_to) {
			// Do nothing to a value if it already conforms to the type restriction
			(_, TypeRestriction::Any) |
			(Self::BigInteger(..) | Self::Size(..) | Self::Byte(..) | Self::U32(..), TypeRestriction::Integer | TypeRestriction::RealNumber) |
			(Self::Float(..) | Self::Complex(..), TypeRestriction::Float | TypeRestriction::RealNumber) |
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
			(Self::Char(value), TypeRestriction::Float | TypeRestriction::RealNumber) if *value == '.' => Ok(Self::Float(0.)),
			(Self::Char(value), TypeRestriction::Float | TypeRestriction::RealNumber) if *value == 'i' => Ok(Self::Complex(Complex64::i())),
			// To integer
			(Self::Bool(value), TypeRestriction::Integer | TypeRestriction::RealNumber) => Ok(Self::Byte(*value as u8)),
			(Self::Char(value), TypeRestriction::Integer | TypeRestriction::RealNumber) => Ok(Self::Byte(value.to_digit(10).ok_or(BasicError::UnableToCast(self.clone(), cast_to))? as u8)),
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
			(Self::String(value), TypeRestriction::RealNumber) => match value.parse::<BigInt>() {
				Ok(value) => Ok(Self::BigInteger(Rc::new(value))),
				Err(_) => match value.parse::<Complex64>() {
					Ok(value) => Ok(Self::Complex(value)),
					Err(_) => Err(BasicError::UnableToCast(self.clone(), cast_to)),
				}
			}

			_ => Err(BasicError::UnableToCast(self.clone(), cast_to)),
		}
	}*/

	pub fn add_concatenate(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) | (other, Self::ComplexFloat(complex_float_value)) => {
				let converted: Complex64 = other.try_into()?;
				Self::ComplexFloat(complex_float_value + converted)
			}
			(Self::GaussianInteger(left_value), Self::GaussianInteger(right_value)) =>
				Self::GaussianInteger(Complex::new(left_value.re + right_value.re, left_value.im + right_value.im)),
			(Self::GaussianInteger(gaussian_integer_value), Self::Integer(integer_value)) |
			(Self::Integer(integer_value), Self::GaussianInteger(gaussian_integer_value)) =>
				Self::GaussianInteger(Complex::new(gaussian_integer_value.re + integer_value, gaussian_integer_value.im)),
			(Self::GaussianInteger(gaussian_integer_value), Self::Float(float_value)) |
			(Self::Float(float_value), Self::GaussianInteger(gaussian_integer_value)) => {
				let gaussian_real: f64 = gaussian_integer_value.re.into();
				Self::ComplexFloat(Complex64::new(gaussian_real + float_value, gaussian_integer_value.im.into()))
			}
			(Self::Float(float_value), other) | (other, Self::Float(float_value)) => {
				let converted: f64 = other.try_into()?;
				Self::Float(float_value + converted)
			}
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value + right_value),
			(Self::String(left_value), Self::String(right_value)) => todo!(),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::ComplexNumber)),
		})
	}
}

impl TryInto<Rc<BigInt>> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<Rc<BigInt>, Self::Error> {
		match self {
			Self::Integer(value) => value.try_into(),
			Self::GaussianInteger(value) if value.im.is_zero() => value.re.try_into(),
			_ => return Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::Integer)),
		}
	}
}

impl TryInto<f64> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<f64, Self::Error> {
		Ok(match self {
			Self::Float(value) => value,
			Self::Integer(value) => value.into(),
			Self::ComplexFloat(value) if value.im.is_zero() => value.re,
			Self::GaussianInteger(value) if value.im.is_zero() => value.re.into(),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Float)),
		})
	}
}

impl TryInto<Complex64> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<Complex64, Self::Error> {
		Ok(match self {
			Self::Float(value) => value.into(),
			Self::Integer(value) => Complex64::new(value.into(), 0.0),
			Self::ComplexFloat(value) => value,
			Self::GaussianInteger(value) => Complex::new(value.re.into(), value.im.into()),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Float)),
		})
	}
}

impl Display for ScalarValue {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Integer(value) => write!(formatter, "{value}"),
			Self::Bool(value) => write!(formatter, "{value}"),
			Self::Float(value) => write!(formatter, "{value}"),
			Self::String(value) => write!(formatter, "{value}"),
			Self::ComplexFloat(value) if value.im.is_zero() => write!(formatter, "{}", value.re),
			Self::ComplexFloat(value) => write!(formatter, "{} + {}i", value.re, value.im),
			Self::GaussianInteger(value) if value.im.is_zero() => write!(formatter, "{}", value.re),
			Self::GaussianInteger(value) => write!(formatter, "{} + {}i", value.re, value.im),
		}
	}
}