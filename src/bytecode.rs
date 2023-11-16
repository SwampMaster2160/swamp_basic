use num_derive::FromPrimitive;

#[derive(FromPrimitive)]
#[repr(u8)]
pub enum Bytecode {
	End = 0,
	NumericalLiteral,
	StringLiteral,
	Print,
	Goto,
	Run,
	List,
}