use std::{rc::Rc, fmt::Display, ops::{Add, Sub, Neg, Mul, Div}, num::NonZeroUsize};

use num::{BigInt, bigint::{Sign, ToBigInt}};
use num_traits::{Zero, ToPrimitive, CheckedDiv};

use crate::{error::BasicError, get_rc_only_or_clone};

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BasicInteger {
	/// Zero
	Zero,
	/// Should be 1 or greater
	SmallInteger(NonZeroUsize),
	/// A big integer, should be greater than usize::MAX or less than 0
	BigInteger(Rc<BigInt>),
}

impl BasicInteger {
	/// Returns if the value is equal to zero
	pub fn is_zero(&self) -> bool {
		match self {
			Self::BigInteger(value) => value.is_zero(),
			Self::Zero => true,
			//Self::SmallInteger(value) => value.is_zero(),
			Self::SmallInteger(_) => false,
		}
	}

	/// Makes a value compact.
	pub fn compact(self) -> Self {
		match self {
			/*Self::Zero => Self::Zero,
			Self::SmallInteger(value) => match value {
				0 => Self::Zero,
				other => Self::SmallInteger(other),
			}*/
			Self::BigInteger(value) => {
				match value.to_usize() {
					Some(value) => match NonZeroUsize::try_from(value) {
						Ok(value) => Self::SmallInteger(value),
						Err(_) => Self::Zero,
					},
					None => Self::BigInteger(value),
				}
			}
			other => other,
		}
	}

	/// Given a length of a container, will return Ok(index) if the value can be used as a index for the container or returns an error otherwise.
	pub fn as_index(&self, container_length: usize) -> Result<usize, BasicError> {
		match self {
			Self::SmallInteger(index) => {
				let index = index.get();
				// The index is invalid if it is not less than the container length
				match index < container_length {
					true => Ok(index),
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

	pub fn is_multiple_of(self, other: Self) -> Option<bool> {
		match (self, other) {
			// Any, Zero
			(_, Self::Zero) => None,
			// Zero, any
			(Self::Zero, _) => Some(true),
			// Small, small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => Some(left_value.get() % right_value.get() == 0),
			// Big, small
			(Self::BigInteger(left_value), Self::SmallInteger(right_value)) => Some(left_value.as_ref() % right_value.get().to_bigint().unwrap() == BigInt::zero()),
			// Small, big
			(Self::SmallInteger(left_value), Self::BigInteger(right_value)) => Some(left_value.get().to_bigint().unwrap() % right_value.as_ref() == BigInt::zero()),
			// Big, big
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Some(left_value.as_ref() % right_value.as_ref() == BigInt::zero())
		}
	}
}

impl TryInto<Rc<BigInt>> for BasicInteger {
	type Error = BasicError;

	fn try_into(self) -> Result<Rc<BigInt>, Self::Error> {
		Ok(match self {
			Self::BigInteger(value) => value.clone(),
			Self::SmallInteger(value) => Rc::new(value.get().into()),
			Self::Zero => Rc::new(BigInt::zero()),
		})
	}
}

impl Into<f64> for BasicInteger {
	fn into(self) -> f64 {
		match self {
			Self::Zero => 0.0,
			Self::SmallInteger(value) => value.get() as f64,
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

impl TryInto<usize> for BasicInteger {
	type Error = ();

	fn try_into(self) -> Result<usize, Self::Error> {
		match self {
			Self::Zero => Ok(0),
			Self::SmallInteger(value) => Ok(value.get()),
			Self::BigInteger(..) => Err(()),
		}
	}
}

impl Display for BasicInteger {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BigInteger(value) => write!(formatter, "{value}"),
			Self::SmallInteger(value) => write!(formatter, "{value}"),
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
			// Small + small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => match left_value.checked_add(right_value.get()) {
				Some(result) => Self::SmallInteger(result),
				None => Self::BigInteger(Rc::new(left_value.get().to_bigint().unwrap() + right_value.get().to_bigint().unwrap())),
			}
			// Small + big integer
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) |
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Self::BigInteger(Rc::new(big_value.as_ref() + small_value.get().to_bigint().unwrap())).compact(),
			// Big integer + big integer
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(left_value.as_ref() + right_value.as_ref())).compact(),
		}
	}
}

impl Sub for BasicInteger {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Any - zero
			(other, Self::Zero) => other,
			// Zero - small
			(Self::Zero, Self::SmallInteger(small_value)) => Self::BigInteger(Rc::new(BigInt::zero() - small_value.get().to_bigint().unwrap())),
			// Small - small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) if left_value == right_value => Self::Zero,
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => match left_value.get().checked_sub(right_value.get()) {
				Some(result) => match NonZeroUsize::try_from(result) {
					Ok(result) => Self::SmallInteger(result),
					Err(..) => Self::Zero,
				}
				None => Self::BigInteger(Rc::new(left_value.get().to_bigint().unwrap() - right_value.get().to_bigint().unwrap())),
			}
			// Big - small
			(Self::BigInteger(small_value), Self::SmallInteger(positive_value)) => Self::BigInteger(Rc::new(get_rc_only_or_clone(small_value) - positive_value.get().to_bigint().unwrap())).compact(),
			// Zero - big
			(Self::Zero, Self::BigInteger(big_value)) => Self::BigInteger(Rc::new(BigInt::zero() - get_rc_only_or_clone(big_value))).compact(),
			// Small - big
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) => Self::BigInteger(Rc::new(small_value.get().to_bigint().unwrap() - get_rc_only_or_clone(big_value))).compact(),
			// Big - big
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(get_rc_only_or_clone(left_value) - get_rc_only_or_clone(right_value))).compact(),
		}
	}
}

impl Mul for BasicInteger {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Zero * any
			(Self::Zero, _) | (_, Self::Zero) => Self::Zero,
			// Small * small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => match left_value.checked_mul(right_value) {
				Some(result) => Self::SmallInteger(result),
				None => Self::BigInteger(Rc::new(left_value.get().to_bigint().unwrap() * right_value.get().to_bigint().unwrap())),
			}
			// Small * big integer
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) |
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Self::BigInteger(Rc::new(big_value.as_ref() * small_value.get().to_bigint().unwrap())),
			// Big integer * big integer
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(left_value.as_ref() * right_value.as_ref())).compact(),
		}
	}
}

impl Div for BasicInteger {
	type Output = Self;

	fn div(self, _: Self) -> Self::Output {
		unimplemented!()
	}
}

impl CheckedDiv for BasicInteger {
	fn checked_div(&self, rhs: &Self) -> Option<Self> {
		match (self, rhs) {
			// Any / zero
			(_, Self::Zero) => None,
			// Zero / any
			(Self::Zero, _) => Some(Self::Zero),
			// Small / small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => Some(match NonZeroUsize::try_from(left_value.get() / right_value.get()) {
				Ok(result) => Self::SmallInteger(result),
				Err(..) => Self::Zero,
			}),
			// Small / big integer
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) => Some(match big_value.sign() {
				Sign::NoSign => panic!(),
				Sign::Plus => Self::Zero,
				Sign::Minus => Self::BigInteger(Rc::new(small_value.get().to_bigint().unwrap() / big_value.as_ref())),
			}),
			// Big integer / big integer
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Some(Self::BigInteger(Rc::new(left_value.as_ref() / right_value.as_ref())).compact()),
			// Big integer / small
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Some(Self::BigInteger(Rc::new(big_value.as_ref() / small_value.get().to_bigint().unwrap())).compact()),
		}
	}
}

impl Neg for BasicInteger {
	type Output = Self;

	fn neg(self) -> Self::Output {
		match self {
			Self::Zero => Self::Zero,
			Self::SmallInteger(value) => Self::BigInteger(Rc::new(-(value.get().to_bigint().unwrap()))),
			BasicInteger::BigInteger(value) => Self::BigInteger(Rc::new(-get_rc_only_or_clone(value))).compact(),
		}
	}
}

/*impl Pow<Self> for BasicInteger {
	type Output = Self;

	fn pow(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Any ^ 0
			(_, Self::Zero) => Self::ONE,
			// 0 ^ any
			(Self::Zero, _) => Self::Zero,
			// Big ^ big
			(Self::BigInteger(big_value), Self::BigInteger(big_exponent)) => Self::BigInteger(Rc::new(big_value.as_ref().pow(*big_exponent.as_ref()))).compact(),
		}
	}
}*/

/*impl Num for BasicInteger {
	type FromStrRadixErr;

	fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
		todo!()
	}
}*/