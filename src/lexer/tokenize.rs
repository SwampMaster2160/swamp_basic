use std::mem;

use crate::{error::BasicError, Main};

use super::{token::Token, operator::Operator, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, command::Command, separator::Separator};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
enum ParsingType {
	None,
	StringLiteral,
	NumericalLiteral,
	Comment,
	OperatorNonAlphabetic,
	IdentifierKeyword,
}

/// Converts a line of code without a line number into a vector of tokens
#[inline(always)]
pub fn tokenize_line(main_struct: &Main, line: &str) -> Result<Vec<Token>, BasicError> {
	let mut out = Vec::new();
	let mut current_token_string = String::new();
	let mut parsing_type = ParsingType::None;
	let mut is_string_char_escaped = false;
	let chars: Vec<char> = line.chars().collect();
	// For each char in the line
	for (index, this_char) in chars.iter().enumerate() {
		let this_char = *this_char;
		let mut end_token = false;
		// If we are not parsing a token (we should be at the start of a token or whitespace)
		if parsing_type == ParsingType::None {
			// Seperators are single-char so just convert them to a token and continue to the next char
			if let Some(separator) = Separator::from_char(main_struct, this_char) {
				out.push(Token::Separator(separator));
				continue;
			}
			// Determine what we are parsing
			match this_char {
				chr if chr.is_ascii_whitespace() => continue,
				chr if chr.is_ascii_digit() => parsing_type = ParsingType::NumericalLiteral,
				chr if Operator::is_char_in_non_alphabetic_character_set(main_struct, chr) => parsing_type = ParsingType::OperatorNonAlphabetic,
				'\'' => parsing_type = ParsingType::Comment,
				'"' => parsing_type = ParsingType::StringLiteral,
				_ => parsing_type = ParsingType::IdentifierKeyword,
			}
			// Add the first char to the parsed token unless we are parsing a comment or string which start with a starting char
			if !matches!(parsing_type, ParsingType::Comment | ParsingType::StringLiteral) {
				current_token_string.push(this_char);
			}
		}
		// For many tokens types just add the char to the parsing token string
		else if matches!(parsing_type, ParsingType::Comment | ParsingType::NumericalLiteral | ParsingType::OperatorNonAlphabetic | ParsingType::IdentifierKeyword) {
			current_token_string.push(this_char);
		}
		// Manage string literal parsing
		else if parsing_type == ParsingType::StringLiteral {
			// If the last char was a backslash then escape char
			if is_string_char_escaped {
				is_string_char_escaped = false;
				let char_replaced_with = match this_char {
					'\\' | '"' => this_char,
					'n' => '\n',
					't' => '\t',
					'r' => '\r',
					'0' => '\0',
					_ => return Err(BasicError::CharEscapeInvalidChar(this_char)),
				};
				current_token_string.push(char_replaced_with);
			}
			// Else
			else {
				// Look for an end of string double quote
				if this_char == '"' {
					end_token = true;
				}
				// A backslash will escape the next char
				else if this_char == '\\' {
					is_string_char_escaped = true;
				}
				// Else push the char to the token being tokenized
				else {
					current_token_string.push(this_char);
				}
			}
		}
		// Check if we should end the token here
		let next_char = chars.get(index + 1).copied();
		match next_char {
			None => end_token = true,
			Some(next_char) => {
				let is_non_alphabetic_operator_char = Operator::is_char_in_non_alphabetic_character_set(main_struct, next_char);
				match parsing_type {
					ParsingType::IdentifierKeyword => {
						let is_seperator = Separator::from_char(main_struct, next_char).is_some();
						let is_whitespace = next_char.is_ascii_whitespace();
						let is_string_start_char = next_char == '"';
						let is_comment_start = next_char == '\'';
						if is_non_alphabetic_operator_char || is_seperator || is_whitespace || is_string_start_char || is_comment_start {
							end_token = true;
						}
					}
					ParsingType::NumericalLiteral => {
						if !(next_char.is_ascii_alphanumeric() || next_char == '.' || next_char == '_') {
							end_token = true;
						}
					}
					ParsingType::OperatorNonAlphabetic => {
						if !is_non_alphabetic_operator_char {
							end_token = true;
						}
					}
					_ => {},
				}
			}
		}
		// Continue if the token does not end with this char
		if !end_token {
			continue;
		}
		// The line cannot end with an escape char
		if is_string_char_escaped {
			return Err(BasicError::CharEscapeAtLineEnd);
		}
		// Convert the token we are tokenizing now to a token
		match parsing_type {
			// For comments and literals just create the token from the current token string.
			ParsingType::Comment => out.push(Token::Comment(mem::take(&mut current_token_string))),
			ParsingType::NumericalLiteral => out.push(Token::NumericalLiteral(mem::take(&mut current_token_string))),
			ParsingType::StringLiteral => out.push(Token::StringLiteral(mem::take(&mut current_token_string))),
			// For non-alphabetic operators get the operater for the current token string.
			ParsingType::OperatorNonAlphabetic => {
				let operator = Operator::from_str(main_struct, &current_token_string);
				match operator {
					Some(operator) => out.push(Token::Operator(operator)),
					None => return Err(BasicError::InvalidNonAlphabeticOperator(current_token_string)),
				}
				current_token_string = String::new();
			}
			// For other token types
			ParsingType::IdentifierKeyword => 'end_parse_other: {
				// Try to convert the string to a command token
				let as_command = Command::from_str(main_struct, &current_token_string);
				if let Some(command) = as_command {
					out.push(Token::Command(command));
					current_token_string = String::new();
					break 'end_parse_other;
				}
				// Try to convert the string to an operator token
				let as_operator = Operator::from_str(main_struct, &current_token_string);
				if let Some(operator) = as_operator {
					out.push(Token::Operator(operator));
					current_token_string = String::new();
					break 'end_parse_other;
				}
				// Split type restriction suffix from string
				let (name_without_type_restriction, type_restriction) = TypeRestriction::from_string_with_suffix(main_struct, &current_token_string)?;
				// Try to convert the string to a built-in function token
				let as_built_in_function = BuiltInFunction::from_str(main_struct, &name_without_type_restriction);
				if let Some(built_in_function) = as_built_in_function {
					out.push(Token::BuiltInFunction(built_in_function, type_restriction));
					current_token_string = String::new();
					break 'end_parse_other;
				}
				// Convert the string to a identifier token
				out.push(Token::Identifier(name_without_type_restriction.to_string(), type_restriction));
				current_token_string = String::new();
			}
			ParsingType::None => {},
		}
		parsing_type = ParsingType::None;
		// If the last token was a "rem" token then start a comment
		if *out.last().unwrap() == Token::Command(Command::Remark) {
			parsing_type = ParsingType::Comment;
		}
	}
	// Parsing type should be None after parsing line
	#[cfg(debug_assertions)]
	if parsing_type != ParsingType::None {
		panic!("Parsing type should be None, is {:?}", parsing_type);
	}
	// Return parsed tokens
	Ok(out)
}