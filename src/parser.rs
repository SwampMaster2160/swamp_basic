use std::mem;

use crate::{
	lexer::{token::Token, separator::Separator, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, operator::Operator, command::Command},
	error::BasicError
};

#[derive(Debug, Clone)]
/// Parsing a line's tokens will result in a tree of parse tree elements for each statement.
pub enum ParseTreeElement {
	UnparsedToken(Token),
	NumericalLiteral(String),
	StringLiteral(String),
	UnaryOperator(Operator, Box<ParseTreeElement>),
	BinaryOperator(Operator, Box<ParseTreeElement>, Box<ParseTreeElement>),
	Identifier(String, TypeRestriction),
	BuiltInFunction(BuiltInFunction, TypeRestriction, Vec<ParseTreeElement>),
	UserDefinedFunctionOrArrayElement(String, TypeRestriction, Vec<ParseTreeElement>),
	Command(Command, Vec<ParseTreeElement>),
	ExpressionSeparator(Separator),
	Assignment(Box<ParseTreeElement>, Box<ParseTreeElement>)
}

impl ParseTreeElement {
	/// Creates a unparsed new parse tree element from a token.
	#[inline(always)]
	const fn from_token(token: Token) -> Self {
		Self::UnparsedToken(token)
	}

	/// Is the tree element a statement.
	pub const fn is_statement(&self) -> bool {
		match self {
			Self::UnparsedToken(_) => false,
			Self::NumericalLiteral(_) => false,
			Self::StringLiteral(_) => false,
			Self::UnaryOperator(_, _) => false,
			Self::Identifier(_, _) => false,
			Self::BinaryOperator(_, _, _) => false,
			Self::BuiltInFunction(_, _, _) => false,
			Self::UserDefinedFunctionOrArrayElement(_, _, _) => false,
			Self::Command(_, _) => true,
			Self::ExpressionSeparator(_) => false,
			Self::Assignment(_, _) => true
		}
	}

	/// Is the tree element an expression.
	pub const fn is_expression(&self) -> bool {
		match self {
			Self::UnparsedToken(_) => false,
			Self::NumericalLiteral(_) => true,
			Self::StringLiteral(_) => true,
			Self::UnaryOperator(_, _) => true,
			Self::Identifier(_, _) => true,
			Self::BinaryOperator(_, _, _) => true,
			Self::BuiltInFunction(_, _, _) => true,
			Self::UserDefinedFunctionOrArrayElement(_, _, _) => true,
			Self::Command(_, _) => false,
			Self::ExpressionSeparator(_) => false,
			Self::Assignment(_, _) => false,
		}
	}
}

/// Parse a BASIC line into trees of parse tree elements and extracts the labels and line comment.
/// `is_line_program` will not allow labels and comments if `true`.
#[inline(always)]
pub fn parse_tokens_to_parse_tree_elements(mut tokens: Vec<Token>, is_line_program: bool) -> Result<(Vec<ParseTreeElement>, Vec<String>, Option<String>), BasicError> {
	// Separate comment
	let mut comment = None;
	if let Some(Token::Comment(_)) = tokens.last() {
		// Should not be a line program
		if is_line_program {
			return Err(BasicError::CommentInLineProgram);
		}
		// Extract comment
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
	for token in tokens.iter() {
		assert!(!matches!(token, Token::Comment(_) | Token::Command(Command::Remark)))
	}
	// Parse each semicolon separated section
	let mut trees_out = Vec::new();
	let mut labels_out = Vec::new();
	let mut can_be_label = true;
	for mut statements_tokens in tokens.split(|token| *token == Token::Separator(Separator::Colon)) {
		// If we have a label
		if statements_tokens.len() == 1 && matches!(statements_tokens[0], Token::Identifier(..)) {
			// Labels should not be after statements
			if !can_be_label {
				return Err(BasicError::LabelNotAtLineStart);
			}
			// Should not be a line program
			if is_line_program {
				return Err(BasicError::LabelInLineProgram);
			}
			// Get label name
			let (label_name, type_restriction) = match statements_tokens[0].clone() {
				Token::Identifier(label_name, type_restriction) => (label_name, type_restriction),
				_ => unreachable!(),
			};
			if type_restriction != TypeRestriction::Any {
				return Err(BasicError::InvalidTypeRestriction(type_restriction.get_type_restriction_suffix_string().to_string()));
			}
			// Push label to label list
			labels_out.push(label_name);
			continue;
		}
		// Else parse each statement in the semicolon separated section
		while !statements_tokens.is_empty() {
			trees_out.push(parse_statement(&mut statements_tokens)?);
			// Labels should not be after statements
			can_be_label = false;
		}
	}
	// Return
	Ok((trees_out, labels_out, comment))
}

/// Parses and removes a single statement from `tokens`.
fn parse_statement(tokens: &mut &[Token]) -> Result<ParseTreeElement, BasicError> {
	// Extract the first token
	let first_token = tokens.get(0).ok_or(BasicError::ExpectedStatement)?;
	// Parse the statement
	match first_token {
		// Commands
		Token::Command(command) => {
			*tokens = &mut &tokens[1..];
			parse_command(*command, tokens)
		}

		// Assignments without a let keyword
		Token::Identifier(..) => parse_assignment(tokens),

		_ => return Err(BasicError::ExpectedStatement),
	}
}

/// Parses and removes an assignment to a variable or array at an index index.
fn parse_assignment(tokens: &mut &[Token]) -> Result<ParseTreeElement, BasicError> {
	// Get the length of the expressions (up to the next command token)
	let expression_index = tokens.iter()
	.position(|token| matches!(token, Token::Command(_)))
	.unwrap_or_else(|| tokens.len());
	// Get the expression tokens
	let mut expression_tokens;
	(expression_tokens, *tokens) = tokens.split_at(expression_index);
	// Parse l value
	let l_value = parse_l_value(&mut expression_tokens)?;
	match expression_tokens.get(0) {
		Some(Token::Operator(Operator::EqualToAssign)) => {}
		_ => return Err(BasicError::ExpectedEqualsChar),
	}
	expression_tokens = &expression_tokens[1..];
	// Parse r value
	let mut r_values = parse_expressions(&mut expression_tokens)?;
	if r_values.len() != 1 {
		return Err(BasicError::InvalidArgumentCount);
	}
	// Return
	Ok(ParseTreeElement::Assignment(Box::new(l_value), Box::new(r_values.pop().unwrap())))
}

/// Remove and parse a l-value variable or array element indexing that is at the start of the tokens.
fn parse_l_value(tokens: &mut &[Token]) -> Result<ParseTreeElement, BasicError> {
	// Extract name token
	let identifier_token = match tokens.get(0) {
		Some(token) => token,
		None => return Err(BasicError::ExpectedStatement),
	};
	*tokens = &mut &tokens[1..];
	let (name, type_restriction) = match identifier_token {
		Token::Identifier(name, type_restriction) => (name.clone(), *type_restriction),
		_ => return Err(BasicError::ExpectedLValue),
	};
	// If the l-value is just a simple variable
	if !matches!(tokens.get(0), Some(Token::Separator(Separator::OpeningBracket))) {
		return Ok(ParseTreeElement::Identifier(name, type_restriction));
	}
	// If the l-value is an array element
	// Get the length of the arrays bracketed area
	let bracketed_area_length = find_bracket_pair_length_in_tokens(tokens)?;
	// Remove bracketed area
	let bracketed_area;
	(bracketed_area, *tokens) = tokens.split_at(bracketed_area_length);
	// Parse bracketed area without brackets
	let bracketed_area_trees: Vec<ParseTreeElement> = bracketed_area[1..bracketed_area_length - 1].iter()
		.map(|token| ParseTreeElement::from_token(token.clone()))
		.collect();
	let bracketed_area_parsed = parse_function_or_array_expressions(&bracketed_area_trees)?;
	// Return
	Ok(ParseTreeElement::UserDefinedFunctionOrArrayElement(name, type_restriction, bracketed_area_parsed))
}

/// Parses and removes a single command or double commands "go to" and "go sub" from `tokens`.
fn parse_command(command: Command, tokens: &mut &[Token]) -> Result<ParseTreeElement, BasicError> {
	Ok(match command {
		// Commands that have a list of expressions and separators as sub-trees
		Command::Print | Command::Goto | Command::Run | Command::End | Command::GoSubroutine | Command::If | Command::To | Command::Step |
		Command::List | Command::On | Command::Return | Command::Stop | Command::Input => {
			// Get the length of the expressions (up to the next command token)
			let expression_index = tokens.iter()
				.position(|token| matches!(token, Token::Command(_)))
				.unwrap_or_else(|| tokens.len());
			// Get the expression tokens
			let mut extression_tokens;
			(extression_tokens, *tokens) = tokens.split_at(expression_index);
			// Parse expressions
			let expressions_parsed = parse_expressions(&mut extression_tokens)?;
			ParseTreeElement::Command(command, expressions_parsed)
		}
		// Commands that have another statement as a sub-tree
		Command::Then | Command::Else => {
			let statement = parse_statement(tokens)?;
			ParseTreeElement::Command(command, vec![statement])
		}
		// Let
		Command::Let => parse_assignment(tokens)?,
		// Merge "go" with "to" or "sub" to create one tree
		Command::Go => {
			// Extract the second command
			let next_token = tokens.get(0).ok_or(BasicError::ExpectedCommand)?;
			let next_command = match next_token {
				Token::Command(command) => *command,
				_ => return Err(BasicError::ExpectedCommand),
			};
			*tokens = &mut &tokens[1..];
			// Merge it with the "to" or "sub" command.
			let merged_commands = match next_command {
				Command::To => Command::Goto,
				Command::Subroutine => Command::GoSubroutine,
				_ => return Err(BasicError::InvalidMultiCommand(vec![command, next_command])),
			};
			// Parse the merged command
			parse_command(merged_commands, tokens)?
		}
		// Sub
		Command::Subroutine => return Err(BasicError::InvalidSingleCommand(Command::Subroutine)),
		// For
		Command::For => ParseTreeElement::Command(command, vec![parse_assignment(tokens)?]),
		// Commands with a single l-value
		Command::Next | Command::Dimension => {
			// Get the length of the expressions (up to the next command token)
			let expression_index = tokens.iter()
				.position(|token| matches!(token, Token::Command(_)))
				.unwrap_or_else(|| tokens.len());
			// Get the expression tokens
			let mut expression_tokens;
			(expression_tokens, *tokens) = tokens.split_at(expression_index);
			// Parse l-value
			let l_value = parse_l_value(&mut expression_tokens)?;
			ParseTreeElement::Command(command, vec![l_value])
		}

		_ => return Err(BasicError::FeatureNotYetSupported),
	})
}

/// Parse a list of expressions, commas and semicolons
fn parse_expressions(tokens: &mut &[Token]) -> Result<Vec<ParseTreeElement>, BasicError> {
	let mut parsed_trees_out = Vec::new();
	// Parse each expression untill there are none left
	while !tokens.is_empty() {
		// If we have a comma or semicolon then add that to the result
		match &tokens[0] {
			Token::Separator(separator) if matches!(separator, Separator::Comma | Separator::Semicolon) => {
				parsed_trees_out.push(ParseTreeElement::ExpressionSeparator(*separator));
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
				// If we have two expressions sitting next to one another without a separator
				Token::Identifier(..) | Token::NumericalLiteral(..) | Token::StringLiteral(..) | Token::BuiltInFunction(..) | Token::Separator(Separator::OpeningBracket)
					if bracket_depth == 0 && index != 0 &&
					matches!(tokens[index - 1], Token::Identifier(..) | Token::NumericalLiteral(..) | Token::StringLiteral(..) |
					Token::Separator(Separator::ClosingBracket) | Token::BuiltInFunction(..)) &&
					!(matches!(token, Token::Separator(Separator::OpeningBracket)) && matches!(tokens[index - 1], Token::Identifier(..) | Token::BuiltInFunction(..))) =>
				{
					expression_length = index;
					break;
				}
				// Brackets increase and decrease the bracket depth
				Token::Separator(Separator::OpeningBracket) => bracket_depth += 1,
				Token::Separator(Separator::ClosingBracket) => bracket_depth = bracket_depth.checked_sub(1)
					.ok_or(BasicError::TooManyClosingBrackets)?,
				// The expression stops at a comma or semicolon if the bracket depth is 0
				Token::Separator(Separator::Comma | Separator::Semicolon) if bracket_depth == 0 => {
					expression_length = index;
					break;
				}
				// Ignore other tokens
				_ => {}
			}
		}
		// Extract expression
		let expression_tokens;
		(expression_tokens, *tokens) = tokens.split_at(expression_length);
		let expression_parse_tree_elements: Vec<ParseTreeElement> = expression_tokens.into_iter()
			.map(|token| ParseTreeElement::from_token(token.clone()))
			.collect();
		parsed_trees_out.push(parse_expression(expression_parse_tree_elements)?);
	}
	// Return
	Ok(parsed_trees_out)
}

/// Takes a slice of parse tree elements and returns the length of the section from the opening bracket to it's matching closing bracket.
///
/// The section includes the opening bracket and the closing bracket and is not any larger.
///
/// The slice should start with an opening bracket.
fn find_bracket_pair_length(tokens: &[ParseTreeElement]) -> Result<usize, BasicError> {
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

/// Takes a slice of tokens and returns the length of the section from the opening bracket to it's matching closing bracket.
///
/// The section includes the opening bracket and the closing bracket and is not any larger.
///
/// The slice should start with an opening bracket.
fn find_bracket_pair_length_in_tokens(tokens: &[Token]) -> Result<usize, BasicError> {
	let mut bracket_depth = 0usize;
	// Go over each token
	for (index, token) in tokens.iter().enumerate() {
		// Opening and closing brackets increase and decrease the bracket depth
		match token {
			Token::Separator(separator) => {
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

/// Parse a single expression and returned the parsed tree
fn parse_expression(mut parse_tree_elements: Vec<ParseTreeElement>) -> Result<ParseTreeElement, BasicError> {
	// Parse code in brackets and parse functions
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
			// If we find an opening bracket, find the matching closing bracket and parse the bracketed area
			Token::Separator(Separator::OpeningBracket) => {
				// Extract bracketed area and discard the brackets arround said area
				let bracketed_area_length = find_bracket_pair_length(&parse_tree_elements[index..])?;
				let mut bracketed_area: Vec<ParseTreeElement> = parse_tree_elements.drain(index..index + bracketed_area_length)
					.skip(1)
					.collect();
				bracketed_area.pop();
				// Parse the bracketed area
				let bracketed_area_parsed = parse_expression(bracketed_area)?;
				parse_tree_elements.insert(index, bracketed_area_parsed);
			}
			// Other separators are invalid
			Token::Separator(seaprator) => return Err(BasicError::InvalidSeparator(*seaprator)),
			// Parse built-in functions
			Token::BuiltInFunction(function, type_restriction) => {
				let function = *function;
				let type_restriction = *type_restriction;
				// If there is no opening bracket after the function name then the function has no arguments
				let next_token = match parse_tree_elements.get(index + 1) {
					None => {
						let new_parse_tree_element = ParseTreeElement::BuiltInFunction(function, type_restriction, Vec::new());
						parse_tree_elements.remove(index);
						parse_tree_elements.insert(index, new_parse_tree_element);
						continue;
					}
					Some(token) => token,
				};
				if !matches!(next_token, ParseTreeElement::UnparsedToken(Token::Separator(Separator::OpeningBracket))) {
					let new_parse_tree_element = ParseTreeElement::BuiltInFunction(function, type_restriction, Vec::new());
					parse_tree_elements.remove(index);
					parse_tree_elements.insert(index, new_parse_tree_element);
					continue;
				}
				// Get the length of the functions bracketed area
				let bracketed_area_length = find_bracket_pair_length(&parse_tree_elements[index + 1..])?;
				// Get the bracketed area without the brackets and function name
				let mut bracketed_area: Vec<ParseTreeElement> = parse_tree_elements.drain(index..index + 1 + bracketed_area_length)
					.skip(2)
					.collect();
				bracketed_area.pop();
				// Parse the bracketed area
				let bracketed_area_parsed = parse_function_or_array_expressions(&bracketed_area)?;
				let new_parse_tree_element = ParseTreeElement::BuiltInFunction(function, type_restriction, bracketed_area_parsed);
				parse_tree_elements.insert(index, new_parse_tree_element);
			}
			// Parse user defined functions
			Token::Identifier(name, type_restriction) => {
				let name = name.clone();
				let type_restriction = *type_restriction;
				// If there is not an opening bracket after the function name then this is not a function
				if !matches!(parse_tree_elements.get(index + 1), Some(ParseTreeElement::UnparsedToken(Token::Separator(Separator::OpeningBracket)))) {
					continue;
				}
				// Get the length of the functions bracketed area
				let bracketed_area_length = find_bracket_pair_length(&parse_tree_elements[index + 1..])?;
				// Get the bracketed area without the brackets and function name
				let mut bracketed_area: Vec<ParseTreeElement> = parse_tree_elements.drain(index..index + 1 + bracketed_area_length)
					.skip(2)
					.collect();
				bracketed_area.pop();
				// Parse the bracketed area
				let bracketed_area_parsed = parse_function_or_array_expressions(&bracketed_area)?;
				let new_parse_tree_element = ParseTreeElement::UserDefinedFunctionOrArrayElement(name, type_restriction, bracketed_area_parsed);
				parse_tree_elements.insert(index, new_parse_tree_element);
			}
			// Ignore other tokens
			_ => {},
		}
	}
	// Parse single tokens that do not depend on other tokens
	for parse_tree_element in parse_tree_elements.iter_mut() {
		// Skip parsed tokens
		let token = match parse_tree_element {
			ParseTreeElement::UnparsedToken(token) => token,
			_ => continue,
		};
		// Parse
		match token {
			Token::NumericalLiteral(literal) => *parse_tree_element = ParseTreeElement::NumericalLiteral(mem::take(literal)),
			Token::StringLiteral(literal) => *parse_tree_element = ParseTreeElement::StringLiteral(mem::take(literal)),
			Token::Identifier(name, type_restriction) =>
				*parse_tree_element = ParseTreeElement::Identifier(mem::take(name), *type_restriction),
			_ => continue,
		}
	}
	// Parse unary operators
	// Go over all tokens from right to left
	for index in (0..parse_tree_elements.len()).rev() {
		// Get operator, skip non-operators
		let parse_tree_element = &parse_tree_elements[index];
		let operator = match parse_tree_element {
			ParseTreeElement::UnparsedToken(Token::Operator(operator)) => *operator,
			_ => continue,
		};
		// Skip if the element to the left is an expression (parse as a binary operator later)
		if index > 0 && parse_tree_elements[index - 1].is_expression() {
			continue;
		}
		// Extract the operand (to the right)
		if parse_tree_elements.len() <= index + 1 {
			return Err(BasicError::OperatorUsedOnNothing);
		}
		let operand = parse_tree_elements.remove(index + 1);
		// Parse the operator
		let new_parse_tree_element = ParseTreeElement::UnaryOperator(operator, Box::new(operand));
		parse_tree_elements[index] = new_parse_tree_element;
	}
	// Parse binary operators
	// For each priority of operators
	for operators in Operator::get_precedence_priority() {
		let operators = *operators;
		// Go over each element
		let mut index = 0;
		loop {
			// Get the element as an operator, skip non-operators
			let parse_tree_element = match parse_tree_elements.get(index) {
				Some(element) => element,
				None => break,
			};
			let operator = *match parse_tree_element {
				ParseTreeElement::UnparsedToken(Token::Operator(operator)) => operator,
				_ => {
					index += 1;
					continue;
				}
			};
			// Skip if the operator is not in this priority
			if !operators.contains(&operator) {
				index += 1;
				continue;
			}
			// It is an error to have the operator at the start of an expression
			if index == 0 {
				return Err(BasicError::OperatorUsedOnNothing);
			}
			// Extract left operand
			let left_operand = parse_tree_elements.remove(index - 1);
			if !left_operand.is_expression() {
				return Err(BasicError::OperatorUsedOnNothing);
			}
			parse_tree_elements.remove(index - 1);
			// It is an error to have the operator at the end of an expression
			if index > parse_tree_elements.len() {
				return Err(BasicError::OperatorUsedOnNothing);
			}
			// Extract right operand
			let right_operand = parse_tree_elements.remove(index - 1);
			// Parse operator
			let new_parse_tree_element = ParseTreeElement::BinaryOperator(operator, Box::new(left_operand), Box::new(right_operand));
			parse_tree_elements.insert(index - 1, new_parse_tree_element);
		}
	}
	// There should only be one element now so return it
	if parse_tree_elements.len() != 1 {
		return Err(BasicError::InvalidArgumentCount);
	}
	Ok(parse_tree_elements.pop().unwrap())
}

/// Parse a comma separated list of expressions for a function
fn parse_function_or_array_expressions(mut tokens: &[ParseTreeElement]) -> Result<Vec<ParseTreeElement>, BasicError> {
	let mut out = Vec::new();
	// Loop over each token
	let mut index = 0;
	while index < tokens.len() {
		match tokens[index] {
			// If we find an opening bracket then skip the bracketed area
			ParseTreeElement::UnparsedToken(Token::Separator(Separator::OpeningBracket)) => index += find_bracket_pair_length(&tokens[index..])?,
			// If we find a comma then extract and parse the area up to the comma as an expression and remove the comma
			ParseTreeElement::UnparsedToken(Token::Separator(Separator::Comma)) => {
				// Get and parse expression
				let section = tokens[..index].to_vec();
				out.push(parse_expression(section)?);
				// Set tokens to the rest of the tokens after the comma
				tokens = &tokens[index + 1..];
				index = 0;
			}
			// Else next token
			_ => index += 1,
		}
	}
	// Parse the last extression if it is not empty
	if !tokens.is_empty() {
		out.push(parse_expression(tokens.to_vec())?);
	}
	// Return ok
	Ok(out)
}

/// Deparse a parse tree element list into a list of tokens
pub fn deparse_line(line_parse_tree_elements: &[ParseTreeElement], labels: Vec<String>, comment: Option<String>) -> Result<Vec<Token>, BasicError> {
	let mut is_first_statement = true;
	let mut out = Vec::new();
	// Push labels
	for label in labels {
		out.push(Token::Identifier(label, TypeRestriction::Any));
		out.push(Token::Separator(Separator::Colon));
	}
	// Push tokens for each statement
	for parse_tree_element in line_parse_tree_elements {
		// Separate each statement with a semicolon
		if !is_first_statement {
			out.push(Token::Separator(Separator::Colon));
		}
		// Push deparsed statement
		out.extend(deparse(parse_tree_element)?);
		// Next statement is not the first
		is_first_statement = false
	}
	// Push comment
	match comment {
		None => {},
		Some(comment) => {
			if out.is_empty() {
				out.push(Token::Command(Command::Remark));
			}
			out.push(Token::Comment(comment));
		}
	}

	Ok(out)
}

/// Deparse a parse tree element into a list of tokens
fn deparse(parse_tree_element: &ParseTreeElement) -> Result<Vec<Token>, BasicError> {
	let mut out = Vec::new();
	// Convert the parse tree element
	match parse_tree_element {
		// Commands consist of the command name followed by the arguments
		ParseTreeElement::Command(command, arguments) => {
			out.push(Token::Command(*command));
			for argument in arguments {
				out.extend(deparse(argument)?);
			}
		}
		// Some parse tree elements get converted to a single token
		ParseTreeElement::NumericalLiteral(string) => out.push(Token::NumericalLiteral(string.clone())),
		ParseTreeElement::StringLiteral(string) => out.push(Token::StringLiteral(string.clone())),
		ParseTreeElement::ExpressionSeparator(separator) => out.push(Token::Separator(*separator)),
		ParseTreeElement::Identifier(name, type_restriction) => out.push(Token::Identifier(name.clone(), *type_restriction)),
		// Built in functions consist of a name followed by the arguments, arguments are enclosed in brackets if they exist.
		ParseTreeElement::BuiltInFunction(function, type_restriction, arguments) => {
			out.push(Token::BuiltInFunction(*function, *type_restriction));
			if !arguments.is_empty() {
				out.push(Token::Separator(Separator::OpeningBracket));
				let mut is_first_argument = true;
				for argument in arguments {
					if !is_first_argument {
						out.push(Token::Separator(Separator::Comma));
					}
					out.extend(deparse(argument)?);
					is_first_argument = false;
				}
				out.push(Token::Separator(Separator::ClosingBracket));
			}
		}
		// User defined functions consist of a name followed by the arguments, arguments are always enclosed in brackets, even if there are none.
		ParseTreeElement::UserDefinedFunctionOrArrayElement(name, type_restriction, arguments) => {
			out.push(Token::Identifier(name.clone(), *type_restriction));
			out.push(Token::Separator(Separator::OpeningBracket));
			let mut is_first_argument = true;
			for argument in arguments {
				if !is_first_argument {
					out.push(Token::Separator(Separator::Comma));
				}
				out.extend(deparse(argument)?);
				is_first_argument = false;
			}
			out.push(Token::Separator(Separator::ClosingBracket));
		}
		// Binary operators consist of the left operand, the operator and the right operand all enclosed in brackets.
		ParseTreeElement::BinaryOperator(operator, left_operand, right_operand) => {
			out.push(Token::Separator(Separator::OpeningBracket));
			out.extend(deparse(left_operand)?);
			out.push(Token::Operator(*operator));
			out.extend(deparse(right_operand)?);
			out.push(Token::Separator(Separator::ClosingBracket));
		}
		// Unary operators consist of the operator and the operand.
		ParseTreeElement::UnaryOperator(operator, operand) => {
			out.push(Token::Operator(*operator));
			out.extend(deparse(operand)?);
		}
		// Assignment operators consist of the left operand, the "=" operator and the right operand.
		ParseTreeElement::Assignment(l_value, r_value) => {
			out.extend(deparse(l_value)?);
			out.push(Token::Operator(Operator::EqualToAssign));
			out.extend(deparse(r_value)?);
		}
		// Unparsed tokens should never exist here
		ParseTreeElement::UnparsedToken(token) => out.push(token.clone()),
	}
	// Success
	Ok(out)
}