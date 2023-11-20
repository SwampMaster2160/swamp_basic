use crate::{lexer::{token::Token, separator::Separator}, error::BasicError};

pub enum ParseTreeElement {
	UnparsedToken(Token),
}

impl ParseTreeElement {
	fn from_token(token: Token) -> Self {
		Self::UnparsedToken(token)
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

fn parse_expression(mut tokens: Vec<ParseTreeElement>) -> Result<ParseTreeElement, BasicError> {
	// Evaluate bracketed pairs and functions
	for index in 0.. {
		// Get each unparsed token
		let parse_tree_element = match tokens.get(index) {
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
				let bracketed_area_length = find_bracket_pair_end(&tokens[index..])?;
				let mut bracketed_area: Vec<ParseTreeElement> = tokens.drain(index..index + bracketed_area_length)
					.skip(1)
					.collect();
				bracketed_area.pop();
				let bracketed_area_parsed = parse_expression(bracketed_area)?;
				tokens.insert(index, bracketed_area_parsed);
			}
			// Other separators are invalid
			Token::Separator(seaprator) => return Err(BasicError::InvalidSeparator(*seaprator)),
			// A function
			Token::BuiltInFunction(function, type_restriction) => {
				// TODO
			}
			_ => continue,
		}
	}
	todo!()
}

fn parse_function(tokens: Vec<ParseTreeElement>) -> Result<ParseTreeElement, BasicError> {
	todo!()
}