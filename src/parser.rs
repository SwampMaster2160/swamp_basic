use std::mem;

use crate::{lexer::{token::Token, separator::Separator, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, operator::Operator, command::Command}, error::BasicError};

#[derive(Debug, Clone)]
pub enum ParseTreeElement {
	UnparsedToken(Token),
	NumericalLiteral(String),
	StringLiteral(String),
	UnaryOperator(Operator, Box<ParseTreeElement>),
	BinaryOperator(Operator, Box<ParseTreeElement>, Box<ParseTreeElement>),
	Identifier(String, TypeRestriction),
	BuiltInFunction(BuiltInFunction, TypeRestriction, Vec<ParseTreeElement>),
	UserDefinedFunction(String, TypeRestriction, Vec<ParseTreeElement>),
	Command(Command, Vec<ParseTreeElement>),
	ExpressionSeparator(Separator),
}

impl ParseTreeElement {
	const fn from_token(token: Token) -> Self {
		Self::UnparsedToken(token)
	}

	pub const fn is_statement(&self) -> bool {
		match self {
			Self::UnparsedToken(_) => false,
			Self::NumericalLiteral(_) => false,
			Self::StringLiteral(_) => false,
			Self::UnaryOperator(_, _) => false,
			Self::Identifier(_, _) => false,
			Self::BinaryOperator(_, _, _) => false,
			Self::BuiltInFunction(_, _, _) => false,
			Self::UserDefinedFunction(_, _, _) => false,
			Self::Command(_, _) => true,
			Self::ExpressionSeparator(_) => false,
		}
	}

	pub const fn is_expression(&self) -> bool {
		match self {
			Self::UnparsedToken(_) => false,
			Self::NumericalLiteral(_) => true,
			Self::StringLiteral(_) => true,
			Self::UnaryOperator(_, _) => true,
			Self::Identifier(_, _) => true,
			Self::BinaryOperator(_, _, _) => true,
			Self::BuiltInFunction(_, _, _) => true,
			Self::UserDefinedFunction(_, _, _) => true,
			Self::Command(_, _) => false,
			Self::ExpressionSeparator(_) => true,
		}
	}
}

/// Parse a BASIC line into trees of parse tree elements
pub fn parse_line(mut tokens: Vec<Token>) -> Result<(Vec<ParseTreeElement>, Option<String>), BasicError> {
	// Separate comment
	let mut comment = None;
	if let Some(Token::Comment(_)) = tokens.last() {
		match tokens.pop() {
			Some(Token::Comment(token_comment)) => comment = Some(token_comment),
			_ => unreachable!(),
		}
	}
	if let Some(Token::Command(Command::Remark)) = tokens.last() {
		tokens.pop();
	}
	// There should be no more remarks or comments
	#[cfg(debug_assertions)]
	if tokens.contains(&Token::Command(Command::Remark)) {
		panic!();
	}
	#[cfg(debug_assertions)]
	for token in tokens.iter() {
		if matches!(token, Token::Comment(_)) {
			panic!();
		}
	}
	// Parse each semicolon separated section
	let mut out = Vec::new();
	for mut statements_tokens in tokens.split(|token| *token == Token::Separator(Separator::Colon)) {
		while !statements_tokens.is_empty() {
			out.push(parse_statement(&mut statements_tokens)?);
		}
	}
	//
	Ok((out, comment))
}

/// Parses and removes a single statement from `tokens`.
fn parse_statement(tokens: &mut &[Token]) -> Result<ParseTreeElement, BasicError> {
	let first_token = tokens.get(0).ok_or(BasicError::ExpectedStatement)?;
	*tokens = &mut &tokens[1..];
	match first_token {
		Token::Identifier(_, _) => return Err(BasicError::FeatureNotYetSupported),
		Token::Command(command) => parse_command(*command, tokens),
		_ => return Err(BasicError::ExpectedStatement),
	}
}

fn parse_command(command: Command, tokens: &mut &[Token]) -> Result<ParseTreeElement, BasicError> {
	Ok(match command {
		Command::Print | Command::Goto | Command::Run | Command::End | Command::GoSubroutine | Command::If | Command::List | Command::On | Command::Return | Command::Stop => {
			// Get the length of the expression
			let command_index = tokens.iter()
				.position(|token| matches!(token, Token::Command(_)))
				.unwrap_or_else(|| tokens.len());
			let mut statements_tokens;
			(statements_tokens, *tokens) = tokens.split_at(command_index);
			// Parse expression
			let expressions_parsed = parse_expressions(&mut statements_tokens)?;
			ParseTreeElement::Command(command, expressions_parsed)
		}
		Command::Then | Command::Else => {
			let statement = parse_statement(tokens)?;
			ParseTreeElement::Command(command, vec![statement])
		}
		_ => return Err(BasicError::FeatureNotYetSupported),
	})
}

/// Parse a list of expressions, commas and semicolons
fn parse_expressions(tokens: &mut &[Token]) -> Result<Vec<ParseTreeElement>, BasicError> {
	let mut out = Vec::new();
	while !tokens.is_empty() {
		// If we have a comma or semicolon then add that to the result
		match &tokens[0] {
			Token::Separator(separator) if matches!(separator, Separator::Comma | Separator::Semicolon) => {
				out.push(ParseTreeElement::ExpressionSeparator(*separator));
				*tokens = &tokens[1..];
				continue;
			}
			_ => {}
		}
		// Find the length of the expression
		let mut bracket_depth = 0usize;
		let mut expression_length = tokens.len();
		for (index, token) in tokens.iter().enumerate() {
			match token {
				Token::Identifier(..) | Token::NumericalLiteral(..) | Token::StringLiteral(..) | Token::BuiltInFunction(..) | Token::Separator(Separator::OpeningBracket)
					if bracket_depth == 0 && index != 0 &&
					matches!(tokens[index - 1], Token::Identifier(..) | Token::NumericalLiteral(..) | Token::StringLiteral(..) | Token::Separator(Separator::ClosingBracket)) &&
					!(matches!(token, Token::Separator(Separator::OpeningBracket)) && matches!(tokens[index - 1], Token::Identifier(..))) =>
				{
					expression_length = index;
					break;
				}
				Token::Separator(Separator::OpeningBracket) => bracket_depth += 1,
				Token::Separator(Separator::ClosingBracket) => bracket_depth = bracket_depth.checked_sub(1)
					.ok_or(BasicError::TooManyClosingBrackets)?,
				Token::Separator(Separator::Comma | Separator::Semicolon) if bracket_depth == 0 => {
					expression_length = index;
					break;
				}
				_ => {}
			}
		}
		// Extract expression
		let expression_tokens;
		(expression_tokens, *tokens) = tokens.split_at(expression_length);
		let expression_parse_tree_elements: Vec<ParseTreeElement> = expression_tokens.into_iter()
			.map(|token| ParseTreeElement::from_token(token.clone()))
			.collect();
		out.push(parse_expression(expression_parse_tree_elements)?);
	}
	Ok(out)
}

/// Takes a slice of parse tree elements and returns the length of the section from the opening bracket to it's matching closing bracket.
///
/// The section includes the opening bracket and the closing bracket and is not any larger.
///
/// The slice should start with an opening bracket.
fn find_bracket_pair_end(tokens: &[ParseTreeElement]) -> Result<usize, BasicError> {
	let mut bracket_depth = 0usize;
	// Go over each token
	for (index, token) in tokens.iter().enumerate() {
		// Opening and closing brackets increase and decrease the bracket depth
		match token {
			ParseTreeElement::UnparsedToken(Token::Separator(separator)) => {
				match separator {
					Separator::OpeningBracket => bracket_depth += 1,
					Separator::ClosingBracket => bracket_depth -= 1,
					_ => {}
				}
			}
			_ => {},
		}
		// If the bracket depth is 0, we've found the end of the bracket pair
		if bracket_depth == 0 {
			return Ok(index + 1);
		}
	}
	// If we reach the end of the tokens without finding the end of the bracket pair, return an error
	Err(BasicError::TooManyOpeningBrackets)
}

/// Parse an expression
fn parse_expression(mut parse_tree_elements: Vec<ParseTreeElement>) -> Result<ParseTreeElement, BasicError> {
	// Parse bracketed pairs and functions
	for index in 0.. {
		// Get each unparsed token
		let parse_tree_element = match parse_tree_elements.get_mut(index) {
			Some(element) => element,
			None => break,
		};
		let token = match parse_tree_element {
			ParseTreeElement::UnparsedToken(token) => token,
			_ => continue,
		};
		// Evaluate bracketed pairs and functions
		match token {
			// An opening bracket
			Token::Separator(Separator::OpeningBracket) => {
				let bracketed_area_length = find_bracket_pair_end(&parse_tree_elements[index..])?;
				let mut bracketed_area: Vec<ParseTreeElement> = parse_tree_elements.drain(index..index + bracketed_area_length)
					.skip(1)
					.collect();
				bracketed_area.pop();
				let bracketed_area_parsed = parse_expression(bracketed_area)?;
				parse_tree_elements.insert(index, bracketed_area_parsed);
			}
			// Other separators are invalid
			Token::Separator(seaprator) => return Err(BasicError::InvalidSeparator(*seaprator)),
			// A function
			Token::BuiltInFunction(function, type_restriction) => {
				let function = *function;
				let type_restriction = *type_restriction;
				let next_token = match parse_tree_elements.get(index + 1) {
					None => return Err(BasicError::NoOpeningBracketAfterFunction),
					Some(token) => token,
				};
				if !matches!(next_token, ParseTreeElement::UnparsedToken(Token::Separator(Separator::OpeningBracket))) {
					return Err(BasicError::NoOpeningBracketAfterFunction)
				}
				let bracketed_area_length = find_bracket_pair_end(&parse_tree_elements[index + 1..])?;
				let mut bracketed_area: Vec<ParseTreeElement> = parse_tree_elements.drain(index..index + 1 + bracketed_area_length)
					.skip(2)
					.collect();
				bracketed_area.pop();
				let bracketed_area_parsed = parse_function_expressions(bracketed_area)?;
				let new_parse_tree_element = ParseTreeElement::BuiltInFunction(function, type_restriction, bracketed_area_parsed);
				parse_tree_elements.insert(index, new_parse_tree_element);
			}
			// User defined functions
			Token::Identifier(name, type_restriction) => {
				let type_restriction = *type_restriction;
				let name = name.clone();
				if !matches!(parse_tree_elements.get(index + 1), Some(ParseTreeElement::UnparsedToken(Token::Separator(Separator::OpeningBracket)))) {
					continue;
				}
				let bracketed_area_length = find_bracket_pair_end(&parse_tree_elements[index + 1..])?;
				let mut bracketed_area: Vec<ParseTreeElement> = parse_tree_elements.drain(index..index + 1 + bracketed_area_length)
					.skip(2)
					.collect();
				bracketed_area.pop();
				let bracketed_area_parsed = parse_function_expressions(bracketed_area)?;
				let new_parse_tree_element = ParseTreeElement::UserDefinedFunction(name, type_restriction, bracketed_area_parsed);
				parse_tree_elements.insert(index, new_parse_tree_element);
			}
			_ => continue,
		}
	}
	// Parse single tokens that do not depend on other tokens
	for parse_tree_element in parse_tree_elements.iter_mut() {
		let token = match parse_tree_element {
			ParseTreeElement::UnparsedToken(token) => token,
			_ => continue,
		};
		match token {
			Token::NumericalLiteral(literal) => *parse_tree_element = ParseTreeElement::NumericalLiteral(mem::take(literal)),
			Token::StringLiteral(literal) => *parse_tree_element = ParseTreeElement::StringLiteral(mem::take(literal)),
			Token::Identifier(name, type_restriction) => *parse_tree_element = ParseTreeElement::Identifier(mem::take(name), *type_restriction),
			_ => continue,
		}
	}
	// Parse unary operators
	for index in (0..parse_tree_elements.len()).rev() {
		let parse_tree_element = &parse_tree_elements[index];
		let operator = match parse_tree_element {
			ParseTreeElement::UnparsedToken(Token::Operator(operator)) => *operator,
			_ => continue,
		};
		if index > 0 && parse_tree_elements[index - 1].is_expression() {
			continue;
		}
		if parse_tree_elements.len() <= index + 1 {
			return Err(BasicError::OperatorUsedOnNothing);
		}
		let operand = parse_tree_elements.remove(index + 1);
		let new_parse_tree_element = ParseTreeElement::UnaryOperator(operator, Box::new(operand));
		parse_tree_elements[index] = new_parse_tree_element;
	}
	// Parse binary operators
	for operators in Operator::get_precedence_priority() {
		let operators = *operators;
		let mut index = 0;
		loop {
			let parse_tree_element = match parse_tree_elements.get(index) {
				Some(element) => element,
				None => break,
			}.clone();
			let operator = match parse_tree_element {
				ParseTreeElement::UnparsedToken(Token::Operator(operator)) => operator,
				_ => {
					index += 1;
					continue;
				}
			};
			if !operators.contains(&operator) {
				index += 1;
				continue;
			}
			if index == 0 {
				return Err(BasicError::OperatorUsedOnNothing);
			}
			let left_operand = &parse_tree_elements.remove(index - 1);
			if !left_operand.is_expression() {
				return Err(BasicError::OperatorUsedOnNothing);
			}
			parse_tree_elements.remove(index - 1);
			if index > parse_tree_elements.len() {
				return Err(BasicError::OperatorUsedOnNothing);
			}
			let right_operand = &parse_tree_elements.remove(index - 1);
			let new_parse_tree_element = ParseTreeElement::BinaryOperator(operator, Box::new(left_operand.clone()), Box::new(right_operand.clone()));
			parse_tree_elements.insert(index - 1, new_parse_tree_element);
		}
	}
	// Return the first element of the parse tree elements
	if parse_tree_elements.len() != 1 {
		return Err(BasicError::TooManyExpressions);
	}
	Ok(parse_tree_elements.pop().unwrap())
}

fn parse_function_expressions(_tokens: Vec<ParseTreeElement>) -> Result<Vec<ParseTreeElement>, BasicError> {
	return Err(BasicError::FeatureNotYetSupported);
}