use crate::Main;

use super::{keyword::Keyword, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, separator::Separator, operator::Operator};

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Token {
	Identifier(String, TypeRestriction),
	Keyword(Keyword),
	BuiltInFunction(BuiltInFunction, TypeRestriction),
	Separator(Separator),
	Operator(Operator),
	NumericalLiteral(String),
	StringLiteral(String),
	Comment(String),
}

/// Converts a line of code without a line number into a vector of tokens
pub fn tokenize_line(_main_struct: &mut Main, _line: &str) -> Vec<Token> {
	let mut out = Vec::new();
	//out.push(Token::Identifier("Hi".to_string()));
	out
}