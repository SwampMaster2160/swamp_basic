use crate::{lexer::{token::Token, separator::Separator, keyword::{Keyword, self}}, error::BasicError};

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
	// Compile each statment
	// A statment ends on it's own, when a colon is reached or when the end of the line is reached
	for mut statment_tokens in tokens.split(|token| *token == Token::Separator(Separator::Colon)) {
		while !statment_tokens.is_empty() {
			compiled_bytecode.extend(compile_statment_to_bytecode(&mut statment_tokens)?);
		}
	}
	// Return
	Ok((compiled_bytecode, comment))
}

/// Compiles a statment (does not produce a value) to bytecode.
pub fn compile_statment_to_bytecode(tokens: &mut &[Token]) -> Result<Vec<u8>, BasicError> {
	let mut out = Vec::new();
	// Pop off the first token
	let first_token = &tokens[0];
	*tokens = &tokens[1..];
	// Token should be a keyword
	let keyword = match first_token {
		Token::Keyword(keyword) => keyword,
		Token::Identifier(..) => return Err(BasicError::FeatureNotYetSupported),
		_ => return Err(BasicError::ExpectedStatment),
	};
	//
	
	// Return
	return Ok(out);
}