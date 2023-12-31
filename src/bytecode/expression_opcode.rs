use num_derive::FromPrimitive;

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
	CallUserFunctionOrGetArrayValueAny,
	CallUserFunctionOrGetArrayValueRealNumber,
	CallUserFunctionOrGetArrayValueInteger,
	CallUserFunctionOrGetArrayValueFloat,
	CallUserFunctionOrGetArrayValueString,
	CallUserFunctionOrGetArrayValueBoolean,
	CallUserFunctionOrGetArrayValueComplexFloat,
	CallUserFunctionOrGetArrayValueNumber,
	Space,
	NewLine,
}