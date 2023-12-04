use std::{fmt::Display, rc::Rc};

use num::{BigInt, complex::Complex64};
use num_traits::{Zero, CheckedDiv};

use crate::{lexer::type_restriction::TypeRestriction, error::BasicError};

use super::{integer::BasicInteger, string::BasicString};

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum ScalarValue {
	Integer(BasicInteger),
	Float(f64),
	Boolean(bool),
	String(BasicString),
	ComplexFloat(Complex64),
}

impl ScalarValue {
	fn is_complex_float(&self) -> bool {
		matches!(self, Self::ComplexFloat(..) | Self::Float(..))
	}

	/// Returns true if the value conforms to the type restriction.
	pub fn conforms_to_type_restriction(&self, restriction: TypeRestriction) -> bool {
		match restriction {
			TypeRestriction::Any => true,
			TypeRestriction::Integer => matches!(self, Self::Integer(..)),
			TypeRestriction::Float => matches!(self, Self::Float(..)),
			TypeRestriction::RealNumber => matches!(self, Self::Integer(..) | ScalarValue::Float(..)),
			TypeRestriction::String => matches!(self, Self::String(_)),
			TypeRestriction::Boolean => matches!(self, Self::Boolean(_)),
			TypeRestriction::ComplexFloat => self.is_complex_float(),
			TypeRestriction::Number => self.is_complex_float() || matches!(self, Self::Integer(..)),
		}
	}

	/// Makes a value compact.
	pub fn compact(self) -> Self {
		match self {
			ScalarValue::ComplexFloat(value) if value.im.is_zero() => ScalarValue::Float(value.re),
			_ => self,
		}
	}

	pub fn add_concatenate(self, rhs: Self, return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		let result = match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) | (other, Self::ComplexFloat(complex_float_value)) =>
				Self::ComplexFloat(complex_float_value + other.to_complex64()?).compact(),
			(Self::Float(float_value), other) | (other, Self::Float(float_value)) => Self::Float(float_value + other.to_f64()?),
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value + right_value),

			(Self::String(left_value), Self::String(right_value)) => Self::String(left_value.concatenate(right_value)),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		};
		if !result.conforms_to_type_restriction(return_type_restriction) {
			return Err(BasicError::TypeMismatch(result, return_type_restriction));
		}
		Ok(result)
	}

	pub fn sub(self, rhs: Self, return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		let result = match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) => Self::ComplexFloat(complex_float_value - other.to_complex64()?).compact(),
			(other, Self::ComplexFloat(complex_float_value)) => Self::ComplexFloat(other.to_complex64()? - complex_float_value).compact(),

			(Self::Float(float_value), other) => Self::Float(float_value - other.to_f64()?),
			(other, Self::Float(float_value)) => Self::Float(other.to_f64()? - float_value),

			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value - right_value),

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		};
		if !result.conforms_to_type_restriction(return_type_restriction) {
			return Err(BasicError::TypeMismatch(result, return_type_restriction));
		}
		Ok(result)
	}

	pub fn mul(self, rhs: Self, return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		let result = match (self.clone(), rhs) {
			(Self::Integer(integer_value), Self::String(string_value)) | (Self::String(string_value), Self::Integer(integer_value)) => {
				let usize_value: usize = integer_value.try_into()?;
				Self::String(string_value.repeat(usize_value))
			},
			(Self::ComplexFloat(complex_float_value), other) | (other, Self::ComplexFloat(complex_float_value)) =>
				Self::ComplexFloat(complex_float_value * other.to_complex64()?).compact(),
			(Self::Float(float_value), other) | (other, Self::Float(float_value)) => Self::Float(float_value * other.to_f64()?),
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value * right_value),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		};
		if !result.conforms_to_type_restriction(return_type_restriction) {
			return Err(BasicError::TypeMismatch(result, return_type_restriction));
		}
		Ok(result)
	}

	pub fn div(self, rhs: Self, return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		let result = match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) => Self::ComplexFloat(complex_float_value / other.to_complex64()?).compact(),
			(other, Self::ComplexFloat(complex_float_value)) => Self::ComplexFloat(other.to_complex64()? / complex_float_value).compact(),

			(Self::Float(float_value), other) => Self::Float(float_value / other.to_f64()?),
			(other, Self::Float(float_value)) => Self::Float(other.to_f64()? / float_value),

			(Self::Integer(left_value), Self::Integer(right_value)) => match return_type_restriction {
				TypeRestriction::Integer => match left_value.checked_div(&right_value) {
					Some(value) => Self::Integer(value),
					None => return Err(BasicError::DivisionByZero),
				}
				_ => match left_value.clone().can_divide_by_exact(right_value.clone()) {
					Some(true) => Self::Integer(left_value.checked_div(&right_value).unwrap()),
					_ => Self::Float(left_value.to_f64() / right_value.to_f64()),
				}
			}

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		};
		if !result.conforms_to_type_restriction(return_type_restriction) {
			return Err(BasicError::TypeMismatch(result, return_type_restriction));
		}
		Ok(result)
	}

	pub fn modulus(self, _rhs: Self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn pow(self, _rhs: Self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn and(self, _rhs: Self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn xor(self, _rhs: Self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn or(self, _rhs: Self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn abs(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn atan(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn cos(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn sin(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn tan(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn random(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn integer(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn log(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn neg(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn not(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn sqrt(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn sign(self, _return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn to_f64(self) -> Result<f64, BasicError> {
		Ok(match self {
			Self::Float(value) => value,
			Self::Integer(value) => value.to_f64(),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::RealNumber)),
		})
	}

	pub fn to_complex64(self) -> Result<Complex64, BasicError> {
		Ok(match self {
			Self::Float(value) => value.into(),
			Self::Integer(value) => Complex64::new(value.to_f64(), 0.0),
			Self::ComplexFloat(value) => value,
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::ComplexFloat)),
		})
	}
}

impl TryInto<Rc<BigInt>> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<Rc<BigInt>, Self::Error> {
		match self {
			Self::Integer(value) => Ok(value.into()),
			_ => return Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::Integer)),
		}
	}
}

impl Display for ScalarValue {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Integer(value) => write!(formatter, "{value}"),
			Self::Boolean(value) => write!(formatter, "{value}"),
			Self::Float(value) => write!(formatter, "{value}"),
			Self::String(value) => write!(formatter, "{value}"),
			Self::ComplexFloat(value) => write!(formatter, "{} + {}i", value.re, value.im),
		}
	}
}