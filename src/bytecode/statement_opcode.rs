use num_derive::FromPrimitive;

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum StatementOpcode {
	End = 0,
	Print,
	Goto,
	Run,
	List,
}