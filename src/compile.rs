use crate::{lexer::{token::Token, separator::Separator, command::{Command, self}}, error::BasicError, bytecode::Bytecode};

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
	if let Some(Token::Command(Command::Remark)) = tokens.last() {
		tokens.pop();
	}
	// There should be no more remarks
	#[cfg(debug_assertions)]
	if tokens.contains(&Token::Command(Command::Remark)) {
		panic!();
	}
	// Compile each statment
	// A statment ends when another command is reached, when a colon is reached or when the end of the line is reached
	for mut statment_tokens in tokens.split(|token| *token == Token::Separator(Separator::Colon)) {
		while !statment_tokens.is_empty() {
			let end = statment_tokens
				.iter()
				.position(|token| matches!(token, Token::Command(_)))
				.unwrap_or(statment_tokens.len());
			compiled_bytecode.extend(compile_statment_to_bytecode(&statment_tokens[..end])?);
			statment_tokens = &statment_tokens[end..];
		}
	}
	// Return
	Ok((compiled_bytecode, comment))
}

/// Compiles a statment (does not produce a value) to bytecode.
pub fn compile_statment_to_bytecode(tokens: &[Token]) -> Result<Vec<u8>, BasicError> {
	return match &tokens[0] {
		Token::Command(command) => compile_command_to_bytecode(command, &tokens[1..]),
		Token::Identifier(..) => Err(BasicError::FeatureNotYetSupported),
		_ => Err(BasicError::ExpectedStatment),
	}
}

/// Compiles a command to bytecode.
pub fn compile_command_to_bytecode(command: &Command, mut tokens: &[Token]) -> Result<Vec<u8>, BasicError> {
	let mut out = Vec::new();
	match command {
		Command::End => {
			if !tokens.is_empty() {
				return Err(BasicError::ExpectedStatmentEnd);
			}
			out.push(Bytecode::End as u8);
		}
		Command::Print => {
			while !tokens.is_empty() {
				out.extend(compile_expression_to_bytecode(&mut tokens)?);
			}
		}
		Command::Remark => unreachable!(),
		_ => return Err(BasicError::FeatureNotYetSupported),
	}
	Ok(out)
}

/// Compiles an expressing to bytecode.
pub fn compile_expression_to_bytecode(tokens: &mut &[Token]) -> Result<Vec<u8>, BasicError> {
	todo!();
}