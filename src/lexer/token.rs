use super::{command::Command, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, separator::Separator, operator::Operator};

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Token {
	Identifier(String, TypeRestriction),
	Command(Command),
	BuiltInFunction(BuiltInFunction, TypeRestriction),
	Separator(Separator),
	Operator(Operator),
	NumericalLiteral(String),
	StringLiteral(String),
	Comment(String),
}