use crate::{lexer::{token::Token, separator::Separator, keyword::Keyword}, error::BasicError};

pub fn compile_tokens_to_bytecode(mut tokens: Vec<Token>) -> Result<(Vec<u8>, Option<String>), BasicError> {
	let mut compiled_bytecode = Vec::new();
	let mut comment = None;
	// Separate comment
	if let Some(Token::Comment(_)) = tokens.last() {
		match tokens.pop() {
			Some(Token::Comment(token_comment)) => comment = Some(token_comment),
			_ => unreachable!(),
		}
	}
	if let Some(Token::Keyword(Keyword::Remark)) = tokens.last() {
		tokens.pop();
	}
	// There should be no more remarks
	#[cfg(debug_assertions)]
	if tokens.contains(&Token::Keyword(Keyword::Remark)) {
		panic!();
	}
	// Compile each statment, separated by semicolons
	for statment_tokens in tokens.split(|token| *token == Token::Separator(Separator::Semicolon)) {
		compiled_bytecode.extend(compile_statment_to_bytecode(statment_tokens));
	}
	// Return
	Ok((compiled_bytecode, comment))
}

/// Compiles a statment (does not produce a value) to bytecode.
pub fn compile_statment_to_bytecode(_tokens: &[Token]) -> Vec<u8> {
	todo!()
}