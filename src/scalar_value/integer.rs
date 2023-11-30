use std::{rc::Rc, fmt::Display, ops::{Add, Sub}};

use num::{BigInt, bigint::{Sign, ToBigInt}};
use num_traits::{Zero, ToPrimitive};

use crate::{error::BasicError, get_rc_only_or_clone};

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BasicInteger {
	/// Zero
	Zero,
	/// Should be negative
	NegativeSmallInteger(isize),
	/// Should be 1 or greater
	PositiveSmallInteger(usize),
	/// A big integer, should be greater than usize::MAX or less than isize::MIN
	BigInteger(Rc<BigInt>),
}

impl BasicInteger {
	/// Returns if the value is equal to zero
	pub fn is_zero(&self) -> bool {
		match self {
			Self::BigInteger(value) => value.is_zero(),
			Self::NegativeSmallInteger(value) => value.is_zero(),
			Self::Zero => true,
			Self::PositiveSmallInteger(value) => value.is_zero(),
		}
	}

	/// Makes a value compact.
	pub fn compact(self) -> Self {
		match self {
			Self::Zero => Self::Zero,
			Self::NegativeSmallInteger(value) => match value {
				0 => Self::Zero,
				positive_value if value > 0 => Self::PositiveSmallInteger(positive_value as usize),
				other => Self::NegativeSmallInteger(other),
			}
			Self::PositiveSmallInteger(value) => match value {
				0 => Self::Zero,
				other => Self::PositiveSmallInteger(other),
			}
			Self::BigInteger(value) if value.is_zero() => Self::Zero,
			Self::BigInteger(value) => {
				match value.to_usize() {
					Some(value) => Self::PositiveSmallInteger(value),
					None => match value.to_isize() {
						Some(value) => Self::NegativeSmallInteger(value),
						None => Self::BigInteger(value),
					}
				}
			}
		}
	}

	/// Given a length of a container, will return Ok(index) if the value can be used as a index for the container or returns an error otherwise.
	pub fn as_index(&self, container_length: usize) -> Result<usize, BasicError> {
		match self {
			Self::PositiveSmallInteger(index) => {
				let index = *index;
				// The index is invalid if it is not less than the container length
				match index < container_length {
					true => Ok(index),
					false => Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				}
			}
			Self::NegativeSmallInteger(negative_index) => {
				// Add the length of the container to convert it from the -container_length..0 range to the 0..container_length range
				let index_as_usize = match container_length.checked_add_signed(*negative_index) {
					Some(index) => index,
					None => return Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				};
				// The index is invalid if it is not less than the container length
				match index_as_usize < container_length {
					true => Ok(index_as_usize),
					false => Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				}
			}
			Self::BigInteger(index) => {
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
			Self::Zero => {
				match container_length {
					0 => Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
					_ => Ok(0),
				}
			}
		}
	}
}

impl TryInto<Rc<BigInt>> for BasicInteger {
	type Error = BasicError;

	fn try_into(self) -> Result<Rc<BigInt>, Self::Error> {
		Ok(match self {
			Self::BigInteger(value) => value.clone(),
			Self::NegativeSmallInteger(value) => Rc::new(value.into()),
			Self::PositiveSmallInteger(value) => Rc::new(value.into()),
			Self::Zero => Rc::new(BigInt::zero()),
		})
	}
}

impl Into<f64> for BasicInteger {
	fn into(self) -> f64 {
		match self {
			Self::Zero => 0.0,
			Self::NegativeSmallInteger(value) => value as f64,
			Self::PositiveSmallInteger(value) => value as f64,
			Self::BigInteger(value) => match value.to_f64() {
				Some(value) => value,
				None => match value.sign() {
					Sign::NoSign => 0.0,
					Sign::Plus => f64::INFINITY,
					Sign::Minus => f64::NEG_INFINITY,
				}
			}
		}
	}
}

impl Display for BasicInteger {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BigInteger(value) => write!(formatter, "{value}"),
			Self::PositiveSmallInteger(value) => write!(formatter, "{value}"),
			Self::NegativeSmallInteger(value) => write!(formatter, "{value}"),
			Self::Zero => write!(formatter, "0"),
		}
	}
}

impl Add for BasicInteger {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Zero + any
			(Self::Zero, other) | (other, Self::Zero) => other,
			// Positive + positive
			(Self::PositiveSmallInteger(left_value), Self::PositiveSmallInteger(right_value)) => match left_value.checked_add(right_value) {
				Some(result) => Self::PositiveSmallInteger(result),
				None => Self::BigInteger(Rc::new(left_value.to_bigint().unwrap() + right_value.to_bigint().unwrap())),
			}
			// Negative + negative
			(Self::NegativeSmallInteger(left_value), Self::NegativeSmallInteger(right_value)) => match left_value.checked_add(right_value) {
				Some(result) => Self::NegativeSmallInteger(result),
				None => Self::BigInteger(Rc::new(left_value.to_bigint().unwrap() + right_value.to_bigint().unwrap())),
			}
			// Negative + positive
			(Self::PositiveSmallInteger(positive_value), Self::NegativeSmallInteger(negative_value)) |
			(Self::NegativeSmallInteger(negative_value), Self::PositiveSmallInteger(positive_value)) => match positive_value.checked_add_signed(negative_value) {
				Some(non_negative_result) => match non_negative_result {
					0 => Self::Zero,
					positive_result => Self::PositiveSmallInteger(positive_result)
				},
				None => Self::NegativeSmallInteger(negative_value.wrapping_add_unsigned(positive_value)),
			}
			// Positive + big integer
			(Self::PositiveSmallInteger(positive_value), Self::BigInteger(big_value)) |
			(Self::BigInteger(big_value), Self::PositiveSmallInteger(positive_value)) => match big_value.sign() {
				Sign::NoSign => panic!(),
				Sign::Plus => Self::BigInteger(Rc::new(big_value.as_ref() + positive_value.to_bigint().unwrap())),
				Sign::Minus => Self::BigInteger(Rc::new(big_value.as_ref() + positive_value.to_bigint().unwrap())).compact(),
			}
			// Negative + big integer
			(Self::NegativeSmallInteger(negative_value), Self::BigInteger(big_value)) |
			(Self::BigInteger(big_value), Self::NegativeSmallInteger(negative_value)) => match big_value.sign() {
				Sign::NoSign => panic!(),
				Sign::Plus => Self::BigInteger(Rc::new(big_value.as_ref() + negative_value.to_bigint().unwrap())).compact(),
				Sign::Minus => Self::BigInteger(Rc::new(big_value.as_ref() + negative_value.to_bigint().unwrap())),
			}
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(left_value.as_ref() + right_value.as_ref())),
		}
	}
}

const SIGNED_MIN_ABS: usize = isize::MIN.abs_diff(0);

impl Sub for BasicInteger {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Any - zero
			(other, Self::Zero) => other,
			// Zero - positive
			(Self::Zero, Self::PositiveSmallInteger(positive_value)) => match 0isize.checked_sub_unsigned(positive_value) {
				Some(result) => Self::NegativeSmallInteger(result),
				None => Self::BigInteger(Rc::new(BigInt::zero() - positive_value.to_bigint().unwrap())),
			}
			// Zero - negative
			(Self::Zero, Self::NegativeSmallInteger(negative_value)) => Self::PositiveSmallInteger(0usize.wrapping_sub(negative_value as usize)),
			// Positive - positive
			(Self::PositiveSmallInteger(left_value), Self::PositiveSmallInteger(right_value)) if left_value == right_value => Self::Zero,
			(Self::PositiveSmallInteger(left_value), Self::PositiveSmallInteger(right_value)) => match left_value.checked_sub(right_value) {
				Some(result) => Self::PositiveSmallInteger(result),
				None => match right_value - left_value < SIGNED_MIN_ABS {
					true => Self::NegativeSmallInteger(left_value.wrapping_sub(right_value) as isize),
					false => Self::BigInteger(Rc::new(left_value.to_bigint().unwrap() - right_value.to_bigint().unwrap())),
				}
			}
			// Negative - negative
			(Self::NegativeSmallInteger(left_value), Self::NegativeSmallInteger(right_value)) => match left_value.wrapping_sub(right_value) {
				value if value < 0 => Self::NegativeSmallInteger(value as isize),
				value if value == 0 => Self::Zero,
				value => Self::PositiveSmallInteger(value as usize),
			}
			// Negative - positive
			(Self::NegativeSmallInteger(negative_value), Self::PositiveSmallInteger(positive_value)) => match negative_value.checked_sub_unsigned(positive_value) {
				Some(result) => Self::NegativeSmallInteger(result),
				None => Self::BigInteger(Rc::new(negative_value.to_bigint().unwrap() - positive_value.to_bigint().unwrap())),
			}
			// Positive - negative
			(Self::PositiveSmallInteger(positive_value), Self::NegativeSmallInteger(negative_value)) => match positive_value.checked_add(negative_value.abs_diff(0)) {
				Some(result) => Self::PositiveSmallInteger(result),
				None => Self::BigInteger(Rc::new(positive_value.to_bigint().unwrap() - negative_value.to_bigint().unwrap())),
			},
			// Big - positive
			(Self::BigInteger(big_value), Self::PositiveSmallInteger(positive_value)) => Self::BigInteger(Rc::new(get_rc_only_or_clone(big_value) - positive_value.to_bigint().unwrap())),
			// Big - negative
			(Self::BigInteger(big_value), Self::NegativeSmallInteger(positive_value)) => Self::BigInteger(Rc::new(get_rc_only_or_clone(big_value) - positive_value.to_bigint().unwrap())),
			// Zero - big
			(Self::Zero, Self::BigInteger(big_value)) => Self::BigInteger(Rc::new(BigInt::zero() - get_rc_only_or_clone(big_value))),
			// Positive - big
			(Self::PositiveSmallInteger(positive_value), Self::BigInteger(big_value)) => Self::BigInteger(Rc::new(positive_value.to_bigint().unwrap() - get_rc_only_or_clone(big_value))),
			// Negative - big
			(Self::NegativeSmallInteger(negative_value), Self::BigInteger(big_value)) => Self::BigInteger(Rc::new(negative_value.to_bigint().unwrap() - get_rc_only_or_clone(big_value))),
			// Big - big
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(get_rc_only_or_clone(left_value) - get_rc_only_or_clone(right_value))),
		}
	}
}

/*impl Num for BasicInteger {
	type FromStrRadixErr;

	fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
		todo!()
	}
}*/