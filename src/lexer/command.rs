use std::{collections::HashMap, iter::once};

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::Main;

#[derive(Debug, Clone, Copy, EnumIter, PartialEq, Eq)]
#[repr(u8)]
pub enum Command {
	Base,
	Continue,
	Data,
	Define,
	Dimension,
	End,
	Else,
	For,
	Function,
	Go,
	GoSubroutine,
	Goto,
	If,
	Input,
	Let,
	List,
	Next,
	On,
	Option,
	Print,
	Randomize,
	Read,
	Remark,
	Restore,
	Return,
	Run,
	Step,
	Stop,
	Subroutine,
	Then,
	To,
	Save,
	Load,
}

impl Command {
	/// Takes a string and returns the command that is associated with the string if it is a name or alias for a command.
	#[inline(always)]
	pub fn from_str(main_data: &Main, string: &str) -> Option<Self> {
		main_data.string_to_command_mapping.get(string.to_lowercase().as_str()).copied()
	}

	/// Returns the name of the command and a list of aliases.
	#[inline(always)]
	const fn get_names(self) -> (&'static str, &'static[&'static str]) {
		match self {
			Self::Base => ("base", &[]),
			Self::Data => ("data", &["dat"]),
			Self::Define => ("def", &["define"]),
			Self::Dimension => ("dim", &["dimension"]),
			Self::End => ("end", &[]),
			Self::Else => ("else", &[]),
			Self::For => ("for", &[]),
			Self::Go => ("go", &[]),
			Self::GoSubroutine => ("gosub", &["gosubroutine"]),
			Self::Goto => ("goto", &[]),
			Self::If => ("if", &[]),
			Self::Input => ("input", &[]),
			Self::Let => ("let", &[]),
			Self::List => ("list", &[]),
			Self::Next => ("next", &[]),
			Self::On => ("on", &[]),
			Self::Option => ("option", &[]),
			Self::Print => ("print", &[]),
			Self::Randomize => ("randomize", &[]),
			Self::Read => ("read", &[]),
			Self::Remark => ("rem", &["remark"]),
			Self::Restore => ("restore", &[]),
			Self::Return => ("return", &["ret"]),
			Self::Run => ("run", &[]),
			Self::Step => ("step", &[]),
			Self::Stop => ("stop", &[]),
			Self::Subroutine => ("sub", &["subroutine"]),
			Self::Then => ("then", &[]),
			Self::To => ("to", &[]),
			Self::Continue => ("cont", &["continue"]),
			Self::Save => ("save", &[]),
			Self::Load => ("load", &[]),
			Self::Function => ("fn", &["function"]),
		}
	}

	/// Returns the name of the command
	pub const fn get_name(self) -> &'static str {
		self.get_names().0
	}

	/// Returns a hashmap mapping command names and aliases to their command
	#[inline(always)]
	pub fn get_string_to_command_mapping() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|command| (command.get_names(), command))
			.map(|((command_name, command_aliases), command)|
				once((command_name, command))
					.chain(command_aliases.iter()
					.map(move |command_alias| (*command_alias, command)))
			)
			.flatten()
			.collect()
	}
}