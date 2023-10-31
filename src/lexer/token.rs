use crate::Main;

use super::keyword::Keyword;

#[derive(Debug, Clone)]
pub enum Token {
	Identifier(String),
	Keyword(Keyword),
	Separator,
	Operator,
	NumericalLiteral(String),
	StringLiteral(String),
	Comment(String),
}

pub fn tokenize_line(_main_struct: &mut Main, _line: &str) -> Vec<Token> {
	let mut out = Vec::new();
	out.push(Token::Identifier("Hi".to_string()));
	out
}