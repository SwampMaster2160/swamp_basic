use std::{fmt::Display, rc::Rc, f64::consts::{PI, E}};

use num::{BigInt, complex::Complex64, bigint::ToBigInt};
use num_traits::{Zero, CheckedDiv, CheckedRem, Pow, One, Signed};
use rand::Rng;

use crate::{lexer::type_restriction::TypeRestriction, error::BasicError};

use super::{integer::BasicInteger, string::BasicString};

#[derive(Debug, Clone)]
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

	pub fn less_than(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(Self::Boolean(match (self, rhs) {
			(Self::Float(float_value), other) => float_value < other.to_f64()?,
			(other, Self::Float(float_value)) => other.to_f64()? < float_value,

			(Self::Integer(left_value), Self::Integer(right_value)) => left_value < right_value,

			(left, _) => return Err(BasicError::TypeMismatch(left, TypeRestriction::RealNumber)),
		}))
	}

	pub fn less_than_or_equal_to(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(Self::Boolean(match (self, rhs) {
			(Self::Float(float_value), other) => float_value <= other.to_f64()?,
			(other, Self::Float(float_value)) => other.to_f64()? <= float_value,

			(Self::Integer(left_value), Self::Integer(right_value)) => left_value <= right_value,

			(left, _) => return Err(BasicError::TypeMismatch(left, TypeRestriction::RealNumber)),
		}))
	}

	pub fn greater_than(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(Self::Boolean(match (self, rhs) {
			(Self::Float(float_value), other) => float_value > other.to_f64()?,
			(other, Self::Float(float_value)) => other.to_f64()? > float_value,

			(Self::Integer(left_value), Self::Integer(right_value)) => left_value > right_value,

			(left, _) => return Err(BasicError::TypeMismatch(left, TypeRestriction::RealNumber)),
		}))
	}

	pub fn greater_than_or_equal_to(self, rhs: Self) -> Result<Self, BasicError> {
		Ok(Self::Boolean(match (self, rhs) {
			(Self::Float(float_value), other) => float_value >= other.to_f64()?,
			(other, Self::Float(float_value)) => other.to_f64()? >= float_value,

			(Self::Integer(left_value), Self::Integer(right_value)) => left_value >= right_value,

			(left, _) => return Err(BasicError::TypeMismatch(left, TypeRestriction::RealNumber)),
		}))
	}

	pub fn absolute_value(self) -> Result<Self, BasicError> {
		Ok(match self {
			Self::Integer(value) => Self::Integer(value.abs()),
			Self::Float(value) => Self::Float(value.abs()),
			Self::ComplexFloat(value) => Self::Float(value.norm()),
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::Number)),
		})
	}

	pub fn arctangent(self, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		Ok(match (self, type_restriction) {
			(Self::ComplexFloat(value), _) => Self::ComplexFloat(value.atan()).compact(),
			(Self::Float(value), _) => Self::Float(value.atan()),
			(Self::Integer(value), _) if value.is_zero() && !matches!(type_restriction, TypeRestriction::Float | TypeRestriction::ComplexFloat) => Self::Integer(BasicInteger::zero()),
			(Self::Integer(value), _) => Self::Float(value.to_f64().atan()),
			(other, _) => return Err(BasicError::TypeMismatch(other, TypeRestriction::Number)),
		})
	}

	pub fn cosine(self, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		Ok(match (self, type_restriction) {
			(Self::ComplexFloat(value), _) => Self::ComplexFloat(value.cos()).compact(),
			(Self::Float(value), _) => Self::Float(value.cos()),
			(Self::Integer(value), _) if value.is_zero() && !matches!(type_restriction, TypeRestriction::Float | TypeRestriction::ComplexFloat) => Self::Integer(BasicInteger::one()),
			(Self::Integer(value), _) => Self::Float(value.to_f64().cos()),
			(other, _) => return Err(BasicError::TypeMismatch(other, TypeRestriction::Number)),
		})
	}

	pub fn sine(self, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		Ok(match (self, type_restriction) {
			(Self::ComplexFloat(value), _) => Self::ComplexFloat(value.sin()).compact(),
			(Self::Float(value), _) => Self::Float(value.sin()),
			(Self::Integer(value), _) if value.is_zero() && !matches!(type_restriction, TypeRestriction::Float | TypeRestriction::ComplexFloat) => Self::Integer(BasicInteger::zero()),
			(Self::Integer(value), _) => Self::Float(value.to_f64().sin()),
			(other, _) => return Err(BasicError::TypeMismatch(other, TypeRestriction::Number)),
		})
	}

	pub fn tangent(self, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		Ok(match (self, type_restriction) {
			(Self::ComplexFloat(value), _) => Self::ComplexFloat(value.tan()).compact(),
			(Self::Float(value), _) => Self::Float(value.tan()),
			(Self::Integer(value), _) if value.is_zero() && !matches!(type_restriction, TypeRestriction::Float | TypeRestriction::ComplexFloat) => Self::Integer(BasicInteger::zero()),
			(Self::Integer(value), _) => Self::Float(value.to_f64().tan()),
			(other, _) => return Err(BasicError::TypeMismatch(other, TypeRestriction::Number)),
		})
	}

	pub fn integer(self) -> Result<Self, BasicError> {
		Ok(match self.clone() {
			Self::Boolean(value) => ScalarValue::Integer(BasicInteger::from_bool(value)),
			Self::Float(value) => ScalarValue::Integer(BasicInteger::BigInteger(Rc::new(value.floor().to_bigint().ok_or(BasicError::InvalidValue(Self::Float(value)))?)).compact()),
			Self::Integer(value) => Self::Integer(value),
			Self::String(string_value) => match BasicInteger::from_basic_string(string_value) {
				Some(value) => Self::Integer(value),
				None => return Err(BasicError::InvalidValue(self)),
			},
			Self::ComplexFloat(..) => return Err(BasicError::TypeMismatch(self, TypeRestriction::RealNumber)),
		})
	}

	pub fn logarithm(self, base: Self, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		Ok(match (self, base, type_restriction) {
			(other, Self::ComplexFloat(base), _) => Self::ComplexFloat(other.to_complex64()?.ln() / base.ln()).compact(),
			(Self::ComplexFloat(value), base, _) => Self::ComplexFloat(value.log(base.to_f64()?)).compact(),

			(Self::Float(value), base, _) => Self::ComplexFloat(Self::Float(value).to_complex64()?.log(base.to_f64()?)).compact(),
			(other, Self::Float(base), _) => Self::ComplexFloat(other.to_complex64()?.log(base)).compact(),

			(Self::Integer(value), Self::Integer(base), TypeRestriction::Integer) =>
				Self::Integer(value.clone().log_floor(base).ok_or(BasicError::InvalidValue(Self::Integer(value)))?),
			(Self::Integer(value), Self::Integer(base), _) => match value.clone().log_exact(base.clone()) {
				Some(result) => Self::Integer(result),
				None => Self::ComplexFloat(Complex64::new(value.to_f64(), 0.).log(Self::Integer(base).to_f64()?)).compact()
			}

			_ => todo!(),
		})
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

	pub fn square_root(self, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		Ok(match (self, type_restriction) {
			(Self::ComplexFloat(value), _) => Self::ComplexFloat(value.sqrt()).compact(),
			(Self::Float(value), _) => match value < 0. {
				true => Self::ComplexFloat(Complex64::new(0., value.abs().sqrt())),
				false => Self::Float(value.sqrt()),
			}
			(Self::Integer(value), TypeRestriction::Float | TypeRestriction::ComplexFloat) => Self::Float(value.to_f64().sqrt()),
			(Self::Integer(value), TypeRestriction::Integer) => match value.clone().floor_sqrt() {
				None => return Err(BasicError::InvalidValue(Self::Integer(value))),
				Some(result) => Self::Integer(result),
			}
			(Self::Integer(value), _) => match value.clone().exact_sqrt() {
				Some(result) => Self::Integer(result),
				None => match (&value).is_negative() {
					true => Self::ComplexFloat(Complex64::new(0., value.to_f64().abs().sqrt())),
					false => Self::Float(value.to_f64().sqrt()),
				}
			}
			(value, _) => return Err(BasicError::TypeMismatch(value, TypeRestriction::Number)),
		})
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

	pub fn get_type_restriction(&self) -> TypeRestriction {
		match self {
			Self::Boolean(_) => TypeRestriction::Boolean,
			Self::ComplexFloat(_) => TypeRestriction::ComplexFloat,
			Self::Float(_) => TypeRestriction::Float,
			Self::Integer(_) => TypeRestriction::Integer,
			Self::String(_) => TypeRestriction::String,
		}
	}

	pub fn to_complex64(self) -> Result<Complex64, BasicError> {
		Ok(match self {
			Self::Float(value) => value.into(),
			Self::Integer(value) => Complex64::new(value.to_f64(), 0.0),
			Self::ComplexFloat(value) => value,
			_ => return Err(BasicError::TypeMismatch(self, TypeRestriction::ComplexFloat)),
		})
	}

	pub fn get_random(min: Option<ScalarValue>, max: Option<ScalarValue>, type_restriction: TypeRestriction) -> Result<Self, BasicError> {
		match type_restriction {
			TypeRestriction::Any | TypeRestriction::ComplexFloat | TypeRestriction::Number | TypeRestriction::RealNumber => {
				let min = match min {
					None => 0.0,
					Some(value) => value.to_f64()?,
				};
				let max = match max {
					None => 1.0,
					Some(value) => value.to_f64()?,
				};
				let range = min..max;
				if range.is_empty() || !min.is_finite() || ! max.is_finite() {
					return Err(BasicError::InvalidRange(Self::Float(min), Self::Float(max)))
				}
				let mut rng = rand::thread_rng();
				Ok(ScalarValue::Float(rng.gen_range(range)))
			}
			TypeRestriction::Integer => {
				let min = match min {
					None => BasicInteger::zero(),
					Some(Self::Integer(value)) => value,
					Some(bad_value) => return Err(BasicError::TypeMismatch(bad_value, TypeRestriction::Integer)),
				};
				let max = match max {
					None => BasicInteger::SmallInteger(2),
					Some(Self::Integer(value)) => value,
					Some(bad_value) => return Err(BasicError::TypeMismatch(bad_value, TypeRestriction::Integer)),
				};
				Ok(ScalarValue::Integer(BasicInteger::get_random(min, max)?))
			}
			TypeRestriction::Boolean => {
				if min.is_some() || max.is_some() {
					return Err(BasicError::InvalidArgumentCount)
				}
				let mut rng = rand::thread_rng();
				Ok(ScalarValue::Boolean(rng.gen()))
			}
			_ => return Err(BasicError::InvalidTypeRestriction(format!("{}", type_restriction.get_type_restriction_suffix().unwrap()))),
		}
	}

	pub fn is_negative(&self) -> Result<bool, BasicError> {
		Ok(match self {
			Self::Integer(value) => value.is_negative(),
			Self::Float(value) => value.is_negative(),
			_ => return Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::RealNumber)),
		})
	}

	pub const TRUE: Self = Self::Boolean(true);
	pub const FALSE: Self = Self::Boolean(false);
	pub const PI: Self = Self::Float(PI);
	pub const EULERS_NUMBER: Self = Self::Float(E);
	pub const IMAGINARY_UNIT: Self = Self::ComplexFloat(Complex64::new(0.0, 1.0));
	pub const SPACE: Self = Self::String(BasicString::SPACE);
	pub const NEW_LINE: Self = Self::String(BasicString::NEW_LINE);
	pub const ONE: Self = Self::Integer(BasicInteger::SmallInteger(1));
}

/// Check if two f64 values are equal.
/// All NaN values are equal.
fn f64_eq_all_nans_are_eq(left: f64, right: f64) -> bool {
	match left.is_nan() {
		false => left == right,
		true => right.is_nan(),
	}
}

impl PartialEq for ScalarValue {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::ComplexFloat(left_value), Self::ComplexFloat(right_value)) =>
			f64_eq_all_nans_are_eq(left_value.re, right_value.re) && f64_eq_all_nans_are_eq(left_value.im, right_value.im),
			(Self::ComplexFloat(..), _) | (_, Self::ComplexFloat(..)) => false,
			(Self::Float(float_value), other_value) | (other_value, Self::Float(float_value)) => match other_value.clone().to_f64() {
				// All Nan's are equal.
				Ok(converted_value) => f64_eq_all_nans_are_eq(*float_value, converted_value),
				Err(_) => false,
			}
			(Self::Integer(left_value), Self::Integer(right_value)) => left_value == right_value,
			(Self::Boolean(left_value), Self::Boolean(right_value)) => left_value == right_value,
			(Self::String(left_value), Self::String(right_value)) => left_value == right_value,
			_ => false,
		}
	}
}

impl Eq for ScalarValue {}

impl TryInto<Rc<BigInt>> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<Rc<BigInt>, Self::Error> {
		match self {
			Self::Integer(value) => Ok(value.into()),
			_ => return Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::Integer)),
		}
	}
}

impl TryInto<bool> for ScalarValue {
	type Error = BasicError;

	fn try_into(self) -> Result<bool, Self::Error> {
		match self {
			Self::Boolean(value) => Ok(value),
			_ => return Err(BasicError::TypeMismatch(self.clone(), TypeRestriction::Boolean)),
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