use std::fmt::Display;

use num_traits::Zero;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum BasicFloat {
	/// A 32-bit floating point number
	Float32(f32),
	/// A 64-bit floating point number
	Float64(f64),
}

impl BasicFloat {
	pub fn is_zero(&self) -> bool {
		match self {
			Self::Float32(value) => value.is_zero(),
			Self::Float64(value) => value.is_zero(),
		}
	}
}

impl Display for BasicFloat {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Float32(value) => write!(formatter, "{value}"),
			Self::Float64(value) => write!(formatter, "{value}"),
		}
	}
}