use std::{rc::Rc, fmt::Display, ops::{Add, Sub, Neg, Mul, Div, Rem}};

use num::{BigInt, bigint::{Sign, ToBigInt}, BigUint};
use num_traits::{Zero, ToPrimitive, CheckedDiv, Num, One, CheckedRem, Pow, pow, checked_pow};

use crate::{error::BasicError, get_rc_only_or_clone};

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BasicInteger {
	/// Small integer
	SmallInteger(usize),
	/// A big integer, should be greater than usize::MAX or less than 0
	BigInteger(Rc<BigInt>),
}

impl BasicInteger {
	/// Makes a value compact.
	pub fn compact(self) -> Self {
		match self {
			Self::BigInteger(value) => match value.to_usize() {
				Some(value) => Self::SmallInteger(value),
				None => Self::BigInteger(value),
			}
			other => other,
		}
	}

	/// Given a length of a container, will return Ok(index) if the value can be used as a index for the container or returns an error otherwise.
	pub fn as_index(&self, container_length: usize) -> Result<usize, BasicError> {
		match self {
			Self::SmallInteger(index) => {
				let index = *index;
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
		}
	}

	pub fn to_f64(self) -> f64 {
		match self {
			Self::SmallInteger(value) => value as f64,
			Self::BigInteger(value) => match value.to_f64() {
				Some(value) => value,
				None => match value.sign() {
					Sign::NoSign => panic!(),
					Sign::Plus => f64::INFINITY,
					Sign::Minus => f64::NEG_INFINITY,
				}
			}
		}
	}

	pub fn can_divide_by_exact(self, other: Self) -> Option<bool> {
		match (self, other) {
			// Check for % by 0
			(_, Self::SmallInteger(0)) => None,
			// Small, small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => Some(left_value % right_value == 0),
			// Big, small
			(Self::BigInteger(left_value), Self::SmallInteger(right_value)) => Some(left_value.as_ref() % right_value.to_bigint().unwrap() == BigInt::zero()),
			// Small, big
			(Self::SmallInteger(left_value), Self::BigInteger(right_value)) => Some(left_value.to_bigint().unwrap() % right_value.as_ref() == BigInt::zero()),
			// Big, big
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Some(left_value.as_ref() % right_value.as_ref() == BigInt::zero())
		}
	}
}

impl Into<Rc<BigInt>> for BasicInteger {
	fn into(self) -> Rc<BigInt> {
		match self {
			Self::BigInteger(value) => value.clone(),
			Self::SmallInteger(value) => Rc::new(value.into()),
		}
	}
}

impl TryInto<usize> for BasicInteger {
	type Error = BasicError;

	fn try_into(self) -> Result<usize, Self::Error> {
		match self {
			Self::SmallInteger(value) => Ok(value),
			Self::BigInteger(..) => Err(BasicError::InvalidSize(self)),
		}
	}
}

impl Display for BasicInteger {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BigInteger(value) => write!(formatter, "{value}"),
			Self::SmallInteger(value) => write!(formatter, "{value}"),
		}
	}
}

impl Add for BasicInteger {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Small + small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => match left_value.checked_add(right_value) {
				Some(result) => Self::SmallInteger(result),
				None => Self::BigInteger(Rc::new(left_value.to_bigint().unwrap() + right_value.to_bigint().unwrap())),
			}
			// Small + big integer
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) |
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Self::BigInteger(Rc::new(big_value.as_ref() + small_value.to_bigint().unwrap())).compact(),
			// Big integer + big integer
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(left_value.as_ref() + right_value.as_ref())).compact(),
		}
	}
}

impl Sub for BasicInteger {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Small - small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => match left_value.checked_sub(right_value) {
				Some(result) => Self::SmallInteger(result),
				None => Self::BigInteger(Rc::new(left_value.to_bigint().unwrap() - right_value.to_bigint().unwrap())),
			}
			// Big - small
			(Self::BigInteger(small_value), Self::SmallInteger(positive_value)) => Self::BigInteger(Rc::new(get_rc_only_or_clone(small_value) - positive_value.to_bigint().unwrap())).compact(),
			// Small - big
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) => Self::BigInteger(Rc::new(small_value.to_bigint().unwrap() - get_rc_only_or_clone(big_value))).compact(),
			// Big - big
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(get_rc_only_or_clone(left_value) - get_rc_only_or_clone(right_value))).compact(),
		}
	}
}

impl Mul for BasicInteger {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// Small * small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => match left_value.checked_mul(right_value) {
				Some(result) => Self::SmallInteger(result),
				None => Self::BigInteger(Rc::new(left_value.to_bigint().unwrap() * right_value.to_bigint().unwrap())),
			}
			// Small * big integer
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) |
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Self::BigInteger(Rc::new(big_value.as_ref() * small_value.to_bigint().unwrap())).compact(),
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
			(_, Self::SmallInteger(0)) => None,
			// Small / small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => Some(Self::SmallInteger(left_value / right_value)),
			// Small / big integer
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) => Some(match big_value.sign() {
				Sign::NoSign => panic!(),
				Sign::Plus => Self::SmallInteger(0),
				Sign::Minus => Self::BigInteger(Rc::new(small_value.to_bigint().unwrap() / big_value.as_ref())),
			}),
			// Big integer / big integer
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Some(Self::BigInteger(Rc::new(left_value.as_ref() / right_value.as_ref())).compact()),
			// Big integer / small
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Some(Self::BigInteger(Rc::new(big_value.as_ref() / small_value.to_bigint().unwrap())).compact()),
		}
	}
}

impl Neg for BasicInteger {
	type Output = Self;

	fn neg(self) -> Self::Output {
		match self {
			Self::SmallInteger(0) => self,
			Self::SmallInteger(value) => Self::BigInteger(Rc::new(-(value.to_bigint().unwrap()))),
			Self::BigInteger(value) => Self::BigInteger(Rc::new(-value.as_ref())).compact(),
		}
	}
}

impl CheckedRem for BasicInteger {
	fn checked_rem(&self, rhs: &Self) -> Option<Self> {
		match (self, rhs) {
			// Any / zero
			(_, Self::SmallInteger(0)) => None,
			// Small / small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => Some(Self::SmallInteger(left_value % right_value)),
			// Small / big integer
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) => Some(Self::BigInteger(Rc::new(small_value.to_bigint().unwrap() % big_value.as_ref())).compact()),
			// Big integer / big integer
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Some(Self::BigInteger(Rc::new(left_value.as_ref() % right_value.as_ref())).compact()),
			// Big integer / small
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Some(Self::BigInteger(Rc::new(big_value.as_ref() % small_value.to_bigint().unwrap())).compact()),
		}
	}
}

impl Rem for BasicInteger {
	type Output = Self;

	fn rem(self, _rhs: Self) -> Self::Output {
		unimplemented!()
	}
}

impl One for BasicInteger {
	fn one() -> Self {
		Self::SmallInteger(1.try_into().unwrap())
	}
}

impl Zero for BasicInteger {
	fn zero() -> Self {
		Self::SmallInteger(0)
	}

	fn is_zero(&self) -> bool {
		self == &Self::SmallInteger(0)
	}
}

impl Pow<Self> for BasicInteger {
	type Output = Option<Self>;

	fn pow(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			// 1 ^ any
			(Self::SmallInteger(1), _) => Some(Self::SmallInteger(1)),
			// Small ^ small
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => Some(match checked_pow(left_value, right_value) {
				Some(result) => Self::SmallInteger(result),
				None => Self::BigInteger(Rc::new(pow(left_value.to_bigint().unwrap(), right_value))),
			}),
			// Big ^ small
			(Self::BigInteger(big_value), Self::SmallInteger(small_value)) => Some(Self::BigInteger(Rc::new(pow(get_rc_only_or_clone(big_value), small_value)))),
			// Big ^ big
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => match right_value.to_biguint() {
				Some(mut exp) => {
					let mut base = get_rc_only_or_clone(left_value);
				
					while (&exp & BigUint::one()).is_zero() {
						base *= base.clone();
						exp >>= 1;
					}
					if exp.is_one() {
						return Some(Self::BigInteger(Rc::new(base)).compact());
					}
				
					let mut acc = base.clone();
					while exp > BigUint::one() {
						exp >>= 1;
						base *= base.clone();
						if (&exp & BigUint::one()).is_one() {
							acc *= base.clone();
						}
					}
					return Some(Self::BigInteger(Rc::new(acc)).compact());
				}
				None => None,
			}
			// Small ^ big
			(Self::SmallInteger(small_value), Self::BigInteger(big_value)) => match big_value.to_biguint() {
				Some(mut exp) => {
					let mut base = small_value.to_bigint().unwrap();
				
					while (&exp & BigUint::one()).is_zero() {
						base *= base.clone();
						exp >>= 1;
					}
					if exp.is_one() {
						return Some(Self::BigInteger(Rc::new(base)).compact());
					}
				
					let mut acc = base.clone();
					while exp > BigUint::one() {
						exp >>= 1;
						base *= base.clone();
						if (&exp & BigUint::one()).is_one() {
							acc *= base.clone();
						}
					}
					return Some(Self::BigInteger(Rc::new(acc)).compact());
				}
				None => None,
			}
		}
	}
}

impl Num for BasicInteger {
	type FromStrRadixErr = ();

	fn from_str_radix(_str: &str, _radix: u32) -> Result<Self, Self::FromStrRadixErr> {
		unimplemented!()
	}
}