use std::collections::HashMap;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::Main;

#[derive(Debug, Clone, Copy, EnumIter)]
#[repr(u8)]
pub enum Keyword {
	Base,
	Data,
	Define,
	Dimension,
	End,
	For,
	Go,
	GoSubroutine,
	Goto,
	If,
	Input,
	Let,
	Next,
	On,
	Option,
	Print,
	Randomize,
	Read,
	Remark,
	Restore,
	Return,
	Step,
	Stop,
	Subroutine,
	Then,
	To,
}

impl Keyword {
	pub fn from_str(main_data: &mut Main, string: &str) -> Option<Self> {
		main_data.string_to_keyword_mapping.get(string).copied()
	}

	pub fn get_string_to_keyword_mapping() -> HashMap<String, Keyword> {
		for keyword in Keyword::iter() {
			
		}
		HashMap::new()
	}
}