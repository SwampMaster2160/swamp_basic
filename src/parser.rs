use std::mem;

use crate::{lexer::{token::Token, separator::Separator, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, operator::Operator}, error::BasicError};

#[derive(Debug, Clone)]
pub enum ParseTreeElement {
	UnparsedToken(Token),
	NumericalLiteral(String),
	StringLiteral(String),
	UnaryOperator(Operator, Box<ParseTreeElement>),
}

impl ParseTreeElement {
	const fn from_token(token: Token) -> Self {
		Self::UnparsedToken(token)
	}

	const fn is_statment(&self) -> bool {
		match self {
			Self::UnparsedToken(_) => false,
			Self::NumericalLiteral(_) => false,
			Self::StringLiteral(_) => false,
			Self::UnaryOperator(_, _) => false,
		}
	}

	const fn is_expression(&self) -> bool {
		match self {
			Self::UnparsedToken(_) => false,
			Self::NumericalLiteral(_) => true,
			Self::StringLiteral(_) => true,
			Self::UnaryOperator(_, _) => true,
		}
	}
}

pub fn parse_tokens(tokens: Vec<Token>) -> Result<Vec<ParseTreeElement>, BasicError> {
	// Convert tokens to unparsed parse tree elements
	let out = tokens.into_iter()
		.map(|token| ParseTreeElement::from_token(token))
		.collect();
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

fn parse_expression(mut parse_tree_elements: Vec<ParseTreeElement>) -> Result<ParseTreeElement, BasicError> {
	// Parse bracketed pairs and functions
	for index in 0.. {
		// Get each unparsed token
		let parse_tree_element = match parse_tree_elements.get(index) {
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
				let bracketed_area_parsed = parse_function(function, type_restriction, bracketed_area)?;
				parse_tree_elements.insert(index, bracketed_area_parsed);
			}
			// TODO: User defined functions
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
			_ => continue,
		}
	}
	// Parse unary operators
	for index in (0..parse_tree_elements.len()).rev() {
		//TODO
	}
	// Return the first element of the parse tree elements
	if parse_tree_elements.len() != 1 {
		return Err(BasicError::TooManyExpressions);
	}
	Ok(parse_tree_elements.pop().unwrap())
}

fn parse_function(function_type: BuiltInFunction, type_restriction: TypeRestriction, tokens: Vec<ParseTreeElement>) -> Result<ParseTreeElement, BasicError> {
	todo!()
}