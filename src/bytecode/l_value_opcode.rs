use num_derive::FromPrimitive;

#[derive(FromPrimitive, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum LValueOpcode {
	End = 0,
	ScalarAny,
	ScalarRealNumber,
	ScalarInteger,
	ScalarFloat,
	ScalarString,
	ScalarBoolean,
	ScalarComplexFloat,
	ScalarNumber,
	ArrayElementAny,
	ArrayElementRealNumber,
	ArrayElementInteger,
	ArrayElementFloat,
	ArrayElementString,
	ArrayElementBoolean,
	ArrayElementComplexFloat,
	ArrayElementNumber,
}