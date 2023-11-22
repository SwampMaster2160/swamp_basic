use crate::{lexer::{token::Token, separator::Separator, command::Command}, error::BasicError, bytecode::{statement_opcode::StatementOpcode, expression_opcode::ExpressionOpcode}, parser::ParseTreeElement};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum ExpressionStartSeparator {
	None,
	Coma,
	SemiColon,
}

impl ExpressionStartSeparator {
	pub const fn get_name(self) -> &'static str {
		match self {
			Self::None => "<none>",
			Self::Coma => ",",
			Self::SemiColon => ";",
		}
	}
}

pub fn compile_parse_tree_elements_to_bytecode(mut tokens: &[ParseTreeElement]) -> Result<Vec<u8>, BasicError> {
	let mut compiled_bytecode = Vec::new();
	todo!();
	/*
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
	// Compile each statement
	// A statement ends when another command is reached, when a colon is reached or when the end of the line is reached
	for mut statement_tokens in tokens.split(|token| *token == Token::Separator(Separator::Colon)) {
		while !statement_tokens.is_empty() {
			let end = match statement_tokens
				.iter()
				.skip(1)
				.position(|token| matches!(token, Token::Command(_))) {
					None => statement_tokens.len(),
					Some(end) => end + 1,
				};
			compiled_bytecode.extend(compile_statement_to_bytecode(&statement_tokens[..end])?);
			statement_tokens = &statement_tokens[end..];
		}
	}*/
	// Return
	Ok(compiled_bytecode)
}

/// Compiles a statement (does not produce a value) to bytecode.
pub fn compile_statement_to_bytecode(tokens: &[Token]) -> Result<Vec<u8>, BasicError> {
	return match &tokens[0] {
		Token::Command(command) => compile_command_to_bytecode(command, &tokens[1..]),
		Token::Identifier(..) => Err(BasicError::FeatureNotYetSupported),
		_ => Err(BasicError::ExpectedStatement),
	}
}

/// Compiles a command to bytecode.
pub fn compile_command_to_bytecode(command: &Command, mut tokens: &[Token]) -> Result<Vec<u8>, BasicError> {
	let mut out = Vec::new();
	match command {
		Command::End => {
			if !tokens.is_empty() {
				return Err(BasicError::ExpectedStatementEnd);
			}
			out.push(StatementOpcode::End as u8);
		}
		Command::Print => {
			out.push(StatementOpcode::Print as u8);
			while !tokens.is_empty() {
				let (expression_start_separator, expression_tokens) = extract_expression_tokens(&mut tokens)?;
				if expression_start_separator != ExpressionStartSeparator::None {
					return Err(BasicError::FeatureNotYetSupported);
				}
				out.extend(compile_expression_to_bytecode(&mut expression_tokens.as_slice())?);
			}
			out.push(StatementOpcode::End as u8);
		}
		Command::Run | Command::Goto => {
			out.push(match command {
				Command::Run => StatementOpcode::Run,
				Command::Goto => StatementOpcode::Goto,
				_ => unreachable!(),
			} as u8);
			let (expression_start_separator, expression_tokens) = extract_expression_tokens(&mut tokens)?;
			if !expression_tokens.is_empty() {
				out.extend(compile_expression_to_bytecode(&mut expression_tokens.as_slice())?);
			}
			if expression_start_separator != ExpressionStartSeparator::None {
				return Err(BasicError::InvalidExpressionStartSeparator(expression_start_separator));
			}
			if !tokens.is_empty() {
				return Err(BasicError::ExpectedStatementEnd);
			}
			out.push(StatementOpcode::End as u8);
		}
		/*Command::List => {
			out.push(StatementOpcode::List as u8);
			if !tokens.is_empty() {
				return Err(BasicError::FeatureNotYetSupported);
			}
			out.push(StatementOpcode::End as u8);
		}*/
		Command::Remark => unreachable!(),
		_ => return Err(BasicError::FeatureNotYetSupported),
	}
	Ok(out)
}

/// Compiles a single expression to bytecode.
pub fn compile_expression_to_bytecode(tokens: &mut &[Token]) -> Result<Vec<u8>, BasicError> {
	// TODO: This is a work of concept
	let mut out = Vec::new();
	let token = match tokens.first() {
		Some(token) => token,
		None => return Err(BasicError::FeatureNotYetSupported),
	};
	if tokens.len() != 1 {
		return Err(BasicError::FeatureNotYetSupported);
	}
	match token {
		Token::StringLiteral(string) => {
			out.push(ExpressionOpcode::StringLiteral as u8);
			out.extend(string.as_bytes());
			out.push(0);
		}
		Token::NumericalLiteral(string) => {
			out.push(ExpressionOpcode::NumericalLiteral as u8);
			out.extend(string.as_bytes());
			out.push(0);
		}
		_ => return Err(BasicError::FeatureNotYetSupported),
	}
	Ok(out)
}

/// Takes in a slice of tokens and removes an expression, the start of said expression is separated.
fn extract_expression_tokens(tokens: &mut &[Token]) -> Result<(ExpressionStartSeparator, Vec<Token>), BasicError> {
	let expression_length = get_expression_length(&tokens)?;
	let (extracted_tokens_start, expression_start_separator) = match tokens.get(0) {
		_ if expression_length == 0 => (0, ExpressionStartSeparator::None),
		Some(Token::Separator(Separator::Comma)) => (1, ExpressionStartSeparator::Coma),
		Some(Token::Separator(Separator::Semicolon)) => (1, ExpressionStartSeparator::SemiColon),
		_ => (0, ExpressionStartSeparator::None),
	};
	let extracted_tokens = &tokens[extracted_tokens_start..expression_length];
	*tokens = &tokens[expression_length..];
	Ok((expression_start_separator, extracted_tokens.to_vec()))
}

fn get_expression_length(tokens: &[Token]) -> Result<usize, BasicError> {
	let mut bracket_nesting_depth = 0usize;
	for (index, token) in tokens.iter().enumerate() {
		if matches!(token, Token::Separator(Separator::OpeningBracket)) {
			bracket_nesting_depth += 1;
		}
		if matches!(token, Token::Separator(Separator::ClosingBracket)) {
			bracket_nesting_depth = match bracket_nesting_depth.checked_sub(1) {
				Some(new_depth) => new_depth,
				None => return Err(BasicError::TooManyClosingBrackets),
			};
		}
		if bracket_nesting_depth > 0 {
			continue;
		}
		if index > 0 && matches!(token, Token::Separator(Separator::Comma | Separator::Semicolon)) {
			return Ok(index)
		}
		let next_token = tokens.get(index + 1);
		if matches!(token, Token::Identifier(..) | Token::NumericalLiteral(..) | Token::StringLiteral(..) | Token::Separator(Separator::ClosingBracket)) &&
			matches!(next_token, Some(Token::BuiltInFunction(..) | Token::Identifier(..) | Token::NumericalLiteral(..) | Token::StringLiteral(..) | Token::Separator(Separator::OpeningBracket)))
		{
			return Ok(index + 1)
		}
	}
	match bracket_nesting_depth {
		0 => Ok(tokens.len()),
		_ => Err(BasicError::TooManyOpeningBrackets),
	}
}