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
	InvalidValue(ScalarValue),
	TooManyClosingBrackets,
	TooManyOpeningBrackets,
	UnterminatedString,
	InvalidUtf8String,
	InvalidStatementOpcode(u8),
	InvalidExpressionOpcode(u8),
	InvalidLValueOpcode(u8),
	ExpectedExpressionOpcodeButProgramEnd,
	ExpectedStatementOpcodeButProgramEnd,
	ExpectedLValueOpcodeButProgramEnd,
	InvalidNumericalLiteral(String),
	InvalidSeparator(Separator),
	ExpectedSeparator(Separator),
	NoOpeningBracketAfterFunction,
	OperatorUsedOnNothing,
	InvalidTypeRestriction(String),
	InvalidMultiCommand(Vec<Command>),
	ExpectedCommand,
	InvalidSingleCommand(Command),
	InvalidBinaryOperatorSymbol(Operator),
	InvalidUnaryOperatorSymbol(Operator),
	InvalidArgumentCount,
	InvalidSize(BasicInteger),
	InvalidNullStatementOpcode,
	DivisionByZero,
	ExpectedEqualsChar,
	UnexpectedLValueEndOpcode,
	ThenWithoutIf,
	InvalidRange(ScalarValue, ScalarValue),
	ExpectedLValue,
	ToStepNoForLoop,
	NextOnLValueWithoutLoop,
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
			Self::InvalidStatementOpcode(opcode) => write!(formatter, "Invalid command opcode: {:#04X}.", opcode),
			Self::InvalidExpressionOpcode(opcode) => write!(formatter, "Invalid function opcode: {:#04X}.", opcode),
			Self::InvalidLValueOpcode(opcode) => write!(formatter, "Invalid l-value opcode: {:#04X}.", opcode),
			Self::ExpectedExpressionOpcodeButProgramEnd => write!(formatter, "Expected expression opcode but bytecode ended."),
			Self::ExpectedStatementOpcodeButProgramEnd => write!(formatter, "Expected statement opcode but bytecode ended."),
			Self::ExpectedLValueOpcodeButProgramEnd => write!(formatter, "Expected l-value opcode but bytecode ended."),
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
			Self::InvalidSize(size) => write!(formatter, "Invalid size: {size}."),
			Self::InvalidNullStatementOpcode => write!(formatter, "Invalid null statement opcode."),
			Self::DivisionByZero => write!(formatter, "Division by zero."),
			Self::ExpectedEqualsChar => write!(formatter, "Expected '=' character."),
			Self::UnexpectedLValueEndOpcode => write!(formatter, "Unexpected l-value end."),
			Self::ExpectedSeparator(separator) => write!(formatter, "Expected separator \"{}\"", separator.get_symbol_char()),
			Self::ThenWithoutIf => write!(formatter, "A \"then\" or \"else\" was executed without a \"if\" statement being executed since the program start or last gosub call."),
			Self::InvalidValue(value) => write!(formatter, "Invalid value: {value}."),
			Self::InvalidRange(start, end) => write!(formatter, "Invalid range: {start} to {end}."),
			Self::ExpectedLValue => write!(formatter, "Expected l-value."),
			Self::ToStepNoForLoop => write!(formatter, "A \"to\" or \"step\" was executed without a \"for\" loop being executed since the program start or last gosub call."),
			Self::NextOnLValueWithoutLoop => write!(formatter, "A \"next\" was executed on a l-value that is not bound to a for loop."),
		}
	}
}

impl Error for BasicError {}