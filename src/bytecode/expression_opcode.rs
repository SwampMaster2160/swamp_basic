use num_derive::FromPrimitive;

#[derive(FromPrimitive, Clone, Copy)]
#[repr(u8)]
pub enum ExpressionOpcode {
	//End = 0,
	NumericalLiteral = 1,
	StringLiteral,
	SumConcatenate,
	Product,
	Subtract,
	Divide,
	Modulus,
	Exponent,
	And,
	Or,
	ExclusiveOr,
	Negate,
	Not,
	EqualToAssign,
	EqualTo,
	NotEqualTo,
	LessThan,
	GreaterThan,
	LessThanOrEqualTo,
	GreaterThanOrEqualTo,
}