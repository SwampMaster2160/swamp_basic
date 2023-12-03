use std::{fmt::Display, rc::Rc};

use num::{BigInt, Complex, complex::Complex64};
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
	GaussianInteger(Complex<BasicInteger>),
}

impl ScalarValue {
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
			TypeRestriction::Integer => matches!(self, Self::Integer(..)),
			TypeRestriction::Float => matches!(self, Self::Float(..)),
			TypeRestriction::RealNumber => matches!(self, Self::Integer(..) | ScalarValue::Float(..)),
			TypeRestriction::String => matches!(self, Self::String(_)),
			TypeRestriction::Boolean => matches!(self, Self::Boolean(_)),
			TypeRestriction::GaussianInteger => self.is_gaussian_integer(),
			TypeRestriction::ComplexFloat => self.is_complex_float(),
			TypeRestriction::ComplexNumber => self.is_complex_float() || self.is_gaussian_integer(),
		}
	}

	/// Makes a value compact.
	pub fn compact(self) -> Self {
		match self {
			ScalarValue::ComplexFloat(value) if value.im.is_zero() => ScalarValue::Float(value.re),
			ScalarValue::GaussianInteger(value) if value.im.is_zero() => ScalarValue::Integer(value.re),
			_ => self,
		}
	}

	pub fn add_concatenate(self, rhs: Self, return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		let result = match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) | (other, Self::ComplexFloat(complex_float_value)) => {
				let converted: Complex64 = other.try_into()?;
				Self::ComplexFloat(complex_float_value + converted).compact()
			}
			(Self::GaussianInteger(_), Self::Float(float_value)) | (Self::Float(float_value), Self::GaussianInteger(_)) => {
				let converted: Complex64 = self.try_into()?;
				Self::ComplexFloat(converted + Complex64::new(float_value, 0.)).compact()
			}
			(Self::GaussianInteger(gaussian_integer_value), other) | (other, Self::GaussianInteger(gaussian_integer_value)) => {
				let converted: Complex<BasicInteger> = other.try_into()?;
				Self::GaussianInteger(gaussian_integer_value + converted).compact()
			}
			(Self::Float(float_value), other) | (other, Self::Float(float_value)) => {
				let converted: f64 = other.try_into()?;
				Self::Float(float_value + converted)
			}
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value + right_value),
			(Self::String(left_value), Self::String(right_value)) => Self::String(left_value.concatenate(right_value)),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::ComplexNumber)),
		};
		if !result.conforms_to_type_restriction(return_type_restriction) {
			return Err(BasicError::TypeMismatch(result, return_type_restriction));
		}
		Ok(result)
	}

	pub fn sub(self, rhs: Self, return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		let result = match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) => {
				let converted: Complex64 = other.try_into()?;
				Self::ComplexFloat(complex_float_value - converted).compact()
			}
			(other, Self::ComplexFloat(complex_float_value)) => {
				let converted: Complex64 = other.try_into()?;
				Self::ComplexFloat(converted - complex_float_value).compact()
			}

			(Self::GaussianInteger(_), Self::Float(float_value)) => {
				let converted: Complex64 = self.try_into()?;
				Self::ComplexFloat(converted - Complex64::new(float_value, 0.)).compact()
			}
			(Self::Float(float_value), Self::GaussianInteger(_)) => {
				let converted: Complex64 = self.try_into()?;
				Self::ComplexFloat(Complex64::new(float_value, 0.) - converted).compact()
			}

			(Self::GaussianInteger(complex_float_value), other) => {
				let converted: Complex<BasicInteger> = other.try_into()?;
				Self::GaussianInteger(complex_float_value - converted).compact()
			}
			(other, Self::GaussianInteger(complex_float_value)) => {
				let converted: Complex<BasicInteger> = other.try_into()?;
				Self::GaussianInteger(converted - complex_float_value).compact()
			}

			(Self::Float(float_value), other) => {
				let converted: f64 = other.try_into()?;
				Self::Float(float_value - converted)
			}
			(other, Self::Float(float_value)) => {
				let converted: f64 = other.try_into()?;
				Self::Float(converted - float_value)
			}

			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value - right_value),

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::ComplexNumber)),
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
			(Self::ComplexFloat(complex_float_value), other) | (other, Self::ComplexFloat(complex_float_value)) => {
				let converted: Complex64 = other.try_into()?;
				Self::ComplexFloat(complex_float_value * converted).compact()
			}
			(Self::GaussianInteger(_), Self::Float(float_value)) | (Self::Float(float_value), Self::GaussianInteger(_)) => {
				let converted: Complex64 = self.try_into()?;
				Self::ComplexFloat(converted * Complex64::new(float_value, 0.)).compact()
			}
			(Self::GaussianInteger(gaussian_integer_value), other) | (other, Self::GaussianInteger(gaussian_integer_value)) => {
				let converted: Complex<BasicInteger> = other.try_into()?;
				Self::GaussianInteger(gaussian_integer_value * converted).compact()
			}
			(Self::Float(float_value), other) | (other, Self::Float(float_value)) => {
				let converted: f64 = other.try_into()?;
				Self::Float(float_value * converted)
			}
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value * right_value),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::ComplexNumber)),
		};
		if !result.conforms_to_type_restriction(return_type_restriction) {
			return Err(BasicError::TypeMismatch(result, return_type_restriction));
		}
		Ok(result)
	}

	pub fn div(self, rhs: Self, return_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		let result = match (self.clone(), rhs) {
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
			_ => todo!()
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
}

impl TryInto<Rc<BigInt>> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<Rc<BigInt>, Self::Error> {
		match self {
			Self::Integer(value) => Ok(value.into()),
			//Self::GaussianInteger(value) if value.im.is_zero() => value.re.try_into(),
			_ => return Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::Integer)),
		}
	}
}

impl TryInto<f64> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<f64, Self::Error> {
		Ok(match self {
			Self::Float(value) => value,
			Self::Integer(value) => value.to_f64(),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Float)),
		})
	}
}

impl TryInto<Complex64> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<Complex64, Self::Error> {
		Ok(match self {
			Self::Float(value) => value.into(),
			Self::Integer(value) => Complex64::new(value.to_f64(), 0.0),
			Self::ComplexFloat(value) => value,
			Self::GaussianInteger(value) => Complex::new(value.re.to_f64(), value.im.to_f64()),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::ComplexFloat)),
		})
	}
}

impl TryInto<Complex<BasicInteger>> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<Complex<BasicInteger>, Self::Error> {
		Ok(match self {
			Self::Integer(value) => Complex::new(value, BasicInteger::zero()),
			Self::GaussianInteger(value) => Complex::new(value.re.into(), value.im.into()),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::GaussianInteger)),
		})
	}
}

impl Display for ScalarValue {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Integer(value) => write!(formatter, "{value}"),
			Self::Boolean(value) => write!(formatter, "{value}"),
			Self::Float(value) => write!(formatter, "{value}"),
			Self::String(value) => write!(formatter, "{value}"),
			Self::ComplexFloat(value) if value.im.is_zero() => write!(formatter, "{}", value.re),
			Self::ComplexFloat(value) => write!(formatter, "{} + {}i", value.re, value.im),
			Self::GaussianInteger(value) if value.im.is_zero() => write!(formatter, "{}", value.re),
			Self::GaussianInteger(value) => write!(formatter, "{} + {}i", value.re, value.im),
		}
	}
}