use std::{error::Error, fmt::Display};

use num::BigInt;

use crate::{scalar_value::ScalarValue, lexer::type_restriction::TypeRestriction};

#[derive(Debug, Clone)]
pub enum BasicError {
	CharEscapeInvalidChar(char),
	CharEscapeAtLineEnd,
	InvalidNonAlphabeticOperator(String),
	IndexOutOfBounds(ScalarValue, usize),
	TypeMismatch(ScalarValue, TypeRestriction),
	UnableToCast(ScalarValue, TypeRestriction),
	LineNotFound(BigInt),
	ExpectedStatment,
	FeatureNotYetSupported,
}

impl Display for BasicError {
	fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::CharEscapeInvalidChar(chr) => write!(formatter, "Invalid character '{chr}' after '\\' escape character."),
			Self::CharEscapeAtLineEnd => write!(formatter, "'\\' string escape character at end of line."),
			Self::InvalidNonAlphabeticOperator(operator) => write!(formatter, "\"{operator}\" is an invalid non-alphabetic operator."),
			Self::IndexOutOfBounds(value, size) => write!(formatter, "Index {value} is out of bounds for indexee of size {size}."),
			Self::TypeMismatch(value, type_restriction) => write!(formatter, "Value {value} does not conform to type restriction of {type_restriction}."),
			Self::UnableToCast(value, type_restriction) => write!(formatter, "Unable to cast value {value} to conform to type restriction of {type_restriction}."),
			Self::LineNotFound(line) => write!(formatter, "The program does not have a line {line}."),
			Self::ExpectedStatment => write!(formatter, "Expected a statment."),
			Self::FeatureNotYetSupported => write!(formatter, "Feature not yet supported."),
		}
	}
}
impl Error for BasicError {}