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
	/// Takes a string and returns the keyword that is associated with the string if it is a name or alias for a keyword.
	pub fn from_str(main_data: &mut Main, string: &str) -> Option<Self> {
		main_data.string_to_keyword_mapping.get(string.to_lowercase().as_str()).copied()
	}

	/// Returns the name of the keyword and a list of aliases.
	pub const fn get_names(self) -> (&'static str, &'static[&'static str]) {
		match self {
			Self::Base => ("base", &[]),
			Self::Data => ("data", &["dat"]),
			Self::Define => ("def", &["define"]),
			Self::Dimension => ("dim", &["dimension"]),
			Self::End => ("end", &[]),
			Self::For => ("for", &[]),
			Self::Go => ("go", &[]),
			Self::GoSubroutine => ("gosub", &["gosubroutine"]),
			Self::Goto => ("goto", &[]),
			Self::If => ("if", &[]),
			Self::Input => ("input", &[]),
			Self::Let => ("let", &[]),
			Self::Next => ("next", &[]),
			Self::On => ("on", &[]),
			Self::Option => ("option", &[]),
			Self::Print => ("print", &[]),
			Self::Randomize => ("randomize", &[]),
			Self::Read => ("read", &[]),
			Self::Remark => ("rem", &["remark"]),
			Self::Restore => ("restore", &[]),
			Self::Return => ("return", &["ret"]),
			Self::Step => ("step", &[]),
			Self::Stop => ("stop", &[]),
			Self::Subroutine => ("sub", &["subroutine"]),
			Self::Then => ("then", &[]),
			Self::To => ("to", &[]),
		}
	}

	/// Returns a hashmap mapping keyword names and aliases to keywords
	pub fn get_string_to_keyword_mapping() -> HashMap<&'static str, Self> {
		let mut out = HashMap::new();
		for keyword in Self::iter() {
			let (keyword_name, keyword_aliases) = keyword.get_names();
			out.insert(keyword_name, keyword);
			for keyword_alias in keyword_aliases {
				out.insert(keyword_alias, keyword);
			}
		}
		out
	}
}