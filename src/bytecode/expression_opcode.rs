use num_derive::FromPrimitive;

use crate::error::BasicError;

#[derive(FromPrimitive, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ExpressionOpcode {
	//End = 0,
	NumericalLiteral = 1,
	StringLiteral,
	SumConcatenate,
	Product,
	Subtract,
	Divide,
	FlooredDivide,
	Modulus,
	Exponent,
	And,
	Or,
	ExclusiveOr,
	Negate,
	Not,
	EqualTo,
	NotEqualTo,
	LessThan,
	GreaterThan,
	LessThanOrEqualTo,
	GreaterThanOrEqualTo,
	AbsoluteValue,
	Arctangent,
	Cosine,
	Exponential,
	Integer,
	Logarithm,
	Random,
	Sign,
	Sine,
	SquareRoot,
	Tangent,
	GetRealNumber,
	GetInteger,
	GetFloat,
	GetString,
	GetBoolean,
	GetComplexFloat,
	GetNumber,
	True,
	False,
	Pi,
	EulersNumber,
	ImaginaryUnit,
	LoadScalarAny,
	LoadScalarRealNumber,
	LoadScalarInteger,
	LoadScalarFloat,
	LoadScalarString,
	LoadScalarBoolean,
	LoadScalarComplexFloat,
	LoadScalarNumber,
	GetArrayValueAny,
	GetArrayValueRealNumber,
	GetArrayValueInteger,
	GetArrayValueFloat,
	GetArrayValueString,
	GetArrayValueBoolean,
	GetArrayValueComplexFloat,
	GetArrayValueNumber,
	CallUserFunctionAny,
	CallUserFunctionRealNumber,
	CallUserFunctionInteger,
	CallUserFunctionFloat,
	CallUserFunctionString,
	CallUserFunctionBoolean,
	CallUserFunctionComplexFloat,
	CallUserFunctionNumber,
	Space,
	NewLine,
	FromStartOrToEnd,
	OneElement,
}

impl ExpressionOpcode {
	/// Get a `ExpressionOpcode` from a `u8` value.
	pub fn from_u8(opcode_id: u8) -> Result<Option<Self>, BasicError> {
		Ok(match opcode_id {
			0 => None,
			_ => Some(num::FromPrimitive::from_u8(opcode_id).ok_or_else(|| BasicError::InvalidExpressionOpcode(opcode_id))?),
		})
	}

	/// Get a `ExpressionOpcode` from a `Option<u8>` value.
	pub fn from_option_u8(opcode_id: Option<u8>) -> Result<Option<Self>, BasicError> {
		match opcode_id {
			Some(opcode_id) => Self::from_u8(opcode_id),
			None => Err(BasicError::ExpectedExpressionOpcodeButProgramEnd),
		}
	}

	/// Get a `ExpressionOpcode` from a `u8` value that should not be zero.
	pub fn from_non_zero_u8(opcode_id: u8) -> Result<Self, BasicError> {
		Self::from_u8(opcode_id)?
			.ok_or_else(|| BasicError::InvalidNullExpressionOpcode)
	}

	/// Get a `ExpressionOpcode` from a `Option<u8>` value that should not be zero.
	pub fn from_non_zero_option_u8(opcode_id: Option<u8>) -> Result<Self, BasicError> {
		match opcode_id {
			Some(opcode_id) => Self::from_non_zero_u8(opcode_id),
			None => Err(BasicError::ExpectedExpressionOpcodeButProgramEnd),
		}
	}
}