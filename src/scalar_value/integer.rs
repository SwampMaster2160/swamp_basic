use std::{rc::Rc, fmt::Display};

use num::{BigInt, bigint::Sign};
use num_traits::{Zero, ToPrimitive};

use crate::error::BasicError;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BasicInteger {
	/// Zero
	Zero,
	/// An integer of any magnitude
	BigInteger(Rc<BigInt>),
	/// A size or index
	Size(usize),
	/// An unsigned byte
	Byte(u8),
	/// An unsigned 32-bit integer
	U32(u32),
}

impl BasicInteger {
	pub fn is_zero(&self) -> bool {
		match self {
			Self::BigInteger(value) => value.is_zero(),
			Self::Byte(value) => value.is_zero(),
			Self::U32(value) => value.is_zero(),
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
			Self::Byte(index) => {
				// A usize is at least 16 bits wide
				let index = *index as usize;
				match index < container_length {
					true => Ok(index),
					false => Err(BasicError::IndexOutOfBounds(self.clone(), container_length)),
				}
			}
			Self::U32(index) => {
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

	pub fn as_big_int(self) -> Result<BigInt, BasicError> {
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
	}
}

impl Display for BasicInteger {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::BigInteger(value) => write!(formatter, "{value}"),
			Self::Size(value) => write!(formatter, "{value}"),
			Self::Byte(value) => write!(formatter, "{value}"),
			Self::U32(value) => write!(formatter, "{value}"),
			Self::Zero => write!(formatter, "0"),
		}
	}
}