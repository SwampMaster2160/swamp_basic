use std::{error::Error, fmt::Display};

#[derive(Debug, Clone)]
pub enum BasicError {
	CharEscapeInvalidChar(char),
	CharEscapeAtLineEnd,
	InvalidNonAlphabeticOperator(String)
}

impl Display for BasicError {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::CharEscapeInvalidChar(chr) => write!(formatter, "Invalid character '{chr}' after '\\' escape character."),
			Self::CharEscapeAtLineEnd => write!(formatter, "'\\' string escape character at end of line."),
			Self::InvalidNonAlphabeticOperator(operator) => write!(formatter, "\"{operator}\" is an invalid non-alphabetic operator."),
		}
	}
}
impl Error for BasicError {}