use std::{rc::Rc, fmt::Display, ops::Add};

use num::{BigInt, bigint::{Sign, ToBigInt}};
use num_traits::{Zero, ToPrimitive};

use crate::error::BasicError;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BasicInteger {
	/// Zero
	Zero,
	/// A 64-bit signed integer
	SmallInteger(i64),
	/// A size or index
	Size(usize),
	/// An integer of any magnitude
	BigInteger(Rc<BigInt>),
}

impl BasicInteger {
	pub fn is_zero(&self) -> bool {
		match self {
			Self::BigInteger(value) => value.is_zero(),
			Self::SmallInteger(value) => value.is_zero(),
			Self::Zero => true,
			Self::Size(value) => value.is_zero(),
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
			Self::SmallInteger(index) => {
				let index = match (*index).try_into() {
					Ok(index) => index,
					Err(_) => return Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				};
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

	/*pub fn as_big_int(self) -> Result<BigInt, BasicError> {
		let out: BigInt = match self {
			Self::BigInteger(value) => match Rc::try_unwrap(value) {
				Ok(only_value) => only_value,
				Err(value) => value.as_ref().clone(),
			}
			Self::Byte(value) => value.into(),
			Self::Size(value) => value.into(),
			Self::U32(value) => value.into(),
			Self::Zero => BigInt::zero(),
		};
		Ok(out)
	}*/
}

impl TryInto<Rc<BigInt>> for BasicInteger {
	type Error = BasicError;

	fn try_into(self) -> Result<Rc<BigInt>, Self::Error> {
		Ok(match self {
			Self::BigInteger(value) => value.clone(),
			Self::SmallInteger(value) => Rc::new(value.into()),
			Self::Size(value) => Rc::new(value.into()),
			Self::Zero => Rc::new(BigInt::zero()),
		})
	}
}

impl Display for BasicInteger {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BigInteger(value) => write!(formatter, "{value}"),
			Self::Size(value) => write!(formatter, "{value}"),
			Self::SmallInteger(value) => write!(formatter, "{value}"),
			Self::Zero => write!(formatter, "0"),
		}
	}
}

impl Add for BasicInteger {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Self::Zero, other) | (other, Self::Zero) => other,
			(Self::SmallInteger(left_value), Self::SmallInteger(right_value)) => match left_value.checked_add(left_value) {
				Some(result) => Self::SmallInteger(result),
				None => Self::BigInteger(Rc::new((left_value as i128 + right_value as i128).to_bigint().unwrap())),
			}
			(Self::Size(left_value), Self::Size(right_value)) => match left_value.checked_add(left_value) {
				Some(result) => {
					Self::Size(result)
				},
				None => match usize::BITS < 63 {
					true => Self::SmallInteger(left_value as i64 + right_value as i64),
					false => Self::BigInteger(Rc::new(left_value.to_bigint().unwrap() + right_value.to_bigint().unwrap())),
				}
			}
			(Self::BigInteger(left_value), Self::BigInteger(right_value)) => Self::BigInteger(Rc::new(left_value.as_ref() + right_value.as_ref())),
			_ => todo!(),
		}
	}
}