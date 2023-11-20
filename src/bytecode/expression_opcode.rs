use num_derive::FromPrimitive;

#[derive(FromPrimitive, Clone, Copy)]
#[repr(u8)]
pub enum ExpressionOpcode {
	//End = 0,
	NumericalLiteral = 1,
	StringLiteral,
}