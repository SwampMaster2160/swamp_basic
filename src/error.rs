use std::{error::Error, fmt::{Display, write}};

#[derive(Debug, Clone)]
pub enum BasicError {
	CharEscapeInvalidChar(char),
}

impl Display for BasicError {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::CharEscapeInvalidChar(chr) => write!(formatter, "Invalid character '{chr}' after '\\' escape character."),
		}
	}
}
impl Error for BasicError {}