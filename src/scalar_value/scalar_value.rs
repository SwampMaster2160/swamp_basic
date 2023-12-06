use std::{fmt::Display, rc::Rc, f64::consts::{PI, E}};

use num::{BigInt, complex::Complex64, bigint::ToBigInt};
use num_traits::{Zero, CheckedDiv, CheckedRem, Pow, One};

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

	pub fn add_concatenate(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) | (other, Self::ComplexFloat(complex_float_value)) =>
				Self::ComplexFloat(complex_float_value + other.to_complex64()?).compact(),
			(Self::Float(float_value), other) | (other, Self::Float(float_value)) => Self::Float(float_value + other.to_f64()?),
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value + right_value),

			(Self::String(left_value), Self::String(right_value)) => Self::String(left_value.concatenate(right_value)),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		})
	}

	pub fn subtract(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) => Self::ComplexFloat(complex_float_value - other.to_complex64()?).compact(),
			(other, Self::ComplexFloat(complex_float_value)) => Self::ComplexFloat(other.to_complex64()? - complex_float_value).compact(),

			(Self::Float(float_value), other) => Self::Float(float_value - other.to_f64()?),
			(other, Self::Float(float_value)) => Self::Float(other.to_f64()? - float_value),

			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value - right_value),

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		})
	}

	pub fn multiply(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::Integer(integer_value), Self::String(string_value)) | (Self::String(string_value), Self::Integer(integer_value)) => {
				let usize_value: usize = integer_value.try_into()?;
				Self::String(string_value.repeat(usize_value))
			},
			(Self::ComplexFloat(complex_float_value), other) | (other, Self::ComplexFloat(complex_float_value)) =>
				Self::ComplexFloat(complex_float_value * other.to_complex64()?).compact(),
			(Self::Float(float_value), other) | (other, Self::Float(float_value)) => Self::Float(float_value * other.to_f64()?),
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value * right_value),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		})
	}

	pub fn divide(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) => Self::ComplexFloat(complex_float_value / other.to_complex64()?).compact(),
			(other, Self::ComplexFloat(complex_float_value)) => Self::ComplexFloat(other.to_complex64()? / complex_float_value).compact(),

			(Self::Float(float_value), other) => Self::Float(float_value / other.to_f64()?),
			(other, Self::Float(float_value)) => Self::Float(other.to_f64()? / float_value),

			(Self::Integer(left_value), Self::Integer(right_value)) => match left_value.clone().can_divide_by_exact(right_value.clone()) {
				Some(true) => Self::Integer(left_value.checked_div(&right_value).unwrap()),
				_ => Self::Float(left_value.to_f64() / right_value.to_f64()),
			}

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		})
	}

	pub fn floored_divide(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value.checked_div(&right_value)
				.ok_or(BasicError::DivisionByZero)?),

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Integer)),
		})
	}

	pub fn modulus(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) => Self::ComplexFloat(complex_float_value % other.to_complex64()?).compact(),
			(other, Self::ComplexFloat(complex_float_value)) => Self::ComplexFloat(other.to_complex64()? % complex_float_value).compact(),

			(Self::Float(float_value), other) => Self::Float(float_value % other.to_f64()?),
			(other, Self::Float(float_value)) => Self::Float(other.to_f64()? % float_value),

			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value.checked_rem(&right_value)
				.ok_or(BasicError::DivisionByZero)?),

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		})
	}

	pub fn power(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self.clone(), rhs) {
			(Self::ComplexFloat(complex_float_value), other) => Self::ComplexFloat(complex_float_value.powc(other.to_complex64()?)).compact(),
			(other, Self::ComplexFloat(complex_float_value)) => Self::ComplexFloat(other.to_complex64()?.powc(complex_float_value)).compact(),

			(Self::Float(float_value), other) => Self::Float(float_value.powf(other.to_f64()?)),
			(other, Self::Float(float_value)) => Self::Float(other.to_f64()?.powf(float_value)),

			(Self::Integer(left_value), Self::Integer(right_value)) => match left_value.clone().pow(right_value.clone()) {
				Some(result) => Self::Integer(result),
				None => Self::Float(left_value.to_f64().powf(right_value.to_f64())),
			}

			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		})
	}

	pub fn and(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self, rhs) {
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value & right_value),
			(Self::Boolean(left_value), Self::Boolean(right_value)) => Self::Boolean(left_value && right_value),
			(left, _) => return Err(BasicError::TypeMismatch(left, TypeRestriction::Integer)),
		})
	}

	pub fn xor(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self, rhs) {
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value ^ right_value),
			(Self::Boolean(left_value), Self::Boolean(right_value)) => Self::Boolean(left_value != right_value),
			(left, _) => return Err(BasicError::TypeMismatch(left, TypeRestriction::Integer)),
		})
	}

	pub fn or(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(match (self, rhs) {
			(Self::Integer(left_value), Self::Integer(right_value)) => Self::Integer(left_value | right_value),
			(Self::Boolean(left_value), Self::Boolean(right_value)) => Self::Boolean(left_value || right_value),
			(left, _) => return Err(BasicError::TypeMismatch(left, TypeRestriction::Integer)),
		})
	}

	pub fn equal_to(self, _rhs: Self) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn not_equal_to(self, _rhs: Self) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn less_than(self, _rhs: Self) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn less_than_or_equal_to(self, _rhs: Self) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn greater_than(self, _rhs: Self) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn greater_than_or_equal_to(self, _rhs: Self) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn absolute_value(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn arctangent(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn cosine(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn sine(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn tangent(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn random_1_argument(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn integer(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn natural_logarithm(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn negate(self) -> Result<Self, BasicError> {
		match self {
			Self::Integer(value) => Ok(Self::Integer(-value)),
			Self::Float(value) => Ok(Self::Float(-value)),
			Self::ComplexFloat(value) => Ok(Self::ComplexFloat(-value)),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		}
	}

	pub fn not(self) -> Result<Self, BasicError> {
		match self {
			Self::Integer(value) => Ok(Self::Integer(!value)),
			Self::Boolean(value) => Ok(Self::Boolean(!value)),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Integer)),
		}
	}

	pub fn square_root(self, _type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn sign(self, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		Ok(match (self, type_restriction) {
			(Self::Integer(value), TypeRestriction::Any | TypeRestriction::Integer | TypeRestriction::Number | TypeRestriction::RealNumber) => Self::Integer(value.sign()),
			(Self::Integer(value), TypeRestriction::Float | TypeRestriction::ComplexFloat) => Self::Float(value.sign_f64()),
			(Self::Integer(value), TypeRestriction::Boolean) => Self::Boolean(value.sign_bit()),

			(Self::Float(value), TypeRestriction::Any | TypeRestriction::Number | TypeRestriction::RealNumber | TypeRestriction::Float | TypeRestriction::ComplexFloat) => Self::Float(value.signum()),
			(Self::Float(value), TypeRestriction::Integer) => Self::Integer(match value {
				value if value == 0.0 => BasicInteger::zero(),
				value if value < 0.0 => BasicInteger::BigInteger(Rc::new((-1).to_bigint().unwrap())),
				_ => BasicInteger::one(),
			}),
			(Self::Float(value), TypeRestriction::Boolean) => Self::Boolean(value.is_sign_negative()),
			(value, _) => return Err(BasicError::TypeMismatch(value, TypeRestriction::RealNumber)),
		})
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

	pub fn get_random_no_arguments(_type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		return Err(BasicError::FeatureNotYetSupported)
	}

	pub fn true_value() -> Self {
		Self::Boolean(true)
	}

	pub fn false_value() -> Self {
		Self::Boolean(false)
	}

	pub fn pi() -> Self {
		Self::Float(PI)
	}

	pub fn eulers_number() -> Self {
		Self::Float(E)
	}

	pub fn imaginary_unit() -> Self {
		Self::ComplexFloat(Complex64::new(0.0, 1.0))
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