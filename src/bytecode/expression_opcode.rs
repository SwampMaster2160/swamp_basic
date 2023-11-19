use num_derive::FromPrimitive;

#[derive(FromPrimitive)]
#[repr(u8)]
pub enum ExpressionOpcode {
	//End = 0,
	NumericalLiteral,
	StringLiteral,
}