use std::{error::Error, fmt::Display};

use num::BigInt;

use crate::{lexer::{type_restriction::TypeRestriction, separator::Separator, command::Command, operator::Operator}, scalar_value::{scalar_value::ScalarValue, integer::BasicInteger}};

#[derive(Debug, Clone)]
pub enum BasicError {
	CharEscapeInvalidChar(char),
	CharEscapeAtLineEnd,
	InvalidNonAlphabeticOperator(String),
	IndexOutOfBounds(BasicInteger, usize),
	TypeMismatch(ScalarValue, TypeRestriction),
	UnableToCast(ScalarValue, TypeRestriction),
	LineNotFound(BigInt),
	ExpectedStatement,
	ExpectedStatementEnd,
	FeatureNotYetSupported,
	TooManyClosingBrackets,
	TooManyOpeningBrackets,
	UnterminatedString,
	InvalidUtf8String,
	InvalidCommandOpcode(u8),
	InvalidFunctionOpcode(u8),
	ExpectedFunctionOpcodeButEnd,
	InvalidNumericalLiteral(String),
	InvalidSeparator(Separator),
	NoOpeningBracketAfterFunction,
	OperatorUsedOnNothing,
	InvalidTypeRestriction(String),
	InvalidMultiCommand(Vec<Command>),
	ExpectedCommand,
	InvalidSingleCommand(Command),
	InvalidBinaryOperatorSymbol(Operator),
	InvalidUnaryOperatorSymbol(Operator),
	InvalidArgumentCount,
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
			Self::ExpectedStatement => write!(formatter, "Expected a statment."),
			Self::ExpectedStatementEnd => write!(formatter, "Expected a statment end."),
			Self::FeatureNotYetSupported => write!(formatter, "Feature not yet supported."),
			Self::TooManyClosingBrackets => write!(formatter, "Too many closing brackets."),
			Self::TooManyOpeningBrackets => write!(formatter, "Too many opening brackets."),
			Self::UnterminatedString => write!(formatter, "Unterminated string."),
			Self::InvalidUtf8String => write!(formatter, "Invalid byte sequence for a UTF-8 string."),
			Self::InvalidCommandOpcode(opcode) => write!(formatter, "Invalid command opcode: {:#04X}.", opcode),
			Self::InvalidFunctionOpcode(opcode) => write!(formatter, "Invalid function opcode: {:#04X}.", opcode),
			Self::ExpectedFunctionOpcodeButEnd => write!(formatter, "Expected function opcode but bytecode ended."),
			Self::InvalidNumericalLiteral(string) => write!(formatter, "Invalid numerical literal: {string}."),
			Self::InvalidSeparator(separator) => write!(formatter, "Invalid separator: {}", separator.get_symbol_char()),
			Self::NoOpeningBracketAfterFunction => write!(formatter, "No opening bracket immediately after function name and type."),
			Self::OperatorUsedOnNothing => write!(formatter, "Operator used on nothing."),
			Self::InvalidTypeRestriction(name) => write!(formatter, "Invalid type restriction: {name}."),
			Self::InvalidMultiCommand(commands) => {
				write!(formatter, "Invalid multi-command: {:?}", commands)
			}
			Self::ExpectedCommand => write!(formatter, "Expected command."),
			Self::InvalidSingleCommand(command) => write!(formatter, "Invalid single command: {:?}.", command),
			Self::InvalidBinaryOperatorSymbol(operator) => write!(formatter, "{:?} can only be used as a unary operator.", operator),
			Self::InvalidUnaryOperatorSymbol(operator) => write!(formatter, "{:?} can only be used as a binary operator.", operator),
			Self::InvalidArgumentCount => write!(formatter, "Invalid argument count."),
		}
	}
}

impl Error for BasicError {}