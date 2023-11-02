use crate::Main;

use super::{keyword::Keyword, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, separator::Separator, operator::Operator};

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Token {
	Identifier(String, TypeRestriction),
	Keyword(Keyword),
	BuiltInFunction(BuiltInFunction, TypeRestriction),
	Separator(Separator),
	Operator(Operator),
	NumericalLiteral(String),
	StringLiteral(String),
	Comment(String),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
enum ParsingType {
	None,
	StringLiteral,
	NumericalLiteral,
	Comment,
	OperatorNonAlphabetic,
	IdentifierKeywordOperatorAlphabeticBuiltInFunction,
}

/// Converts a line of code without a line number into a vector of tokens
pub fn tokenize_line(main_struct: &mut Main, line: &str) -> Vec<Token> {
	let mut out = Vec::new();
	let mut current_token_string = String::new();
	let mut parsing_type = ParsingType::None;
	// For each char in the line
	for (index, this_char) in line.chars().enumerate() {
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
				_ => parsing_type = ParsingType::IdentifierKeywordOperatorAlphabeticBuiltInFunction,
			}
			// Add the first char to the parsed token unless we are parsing a comment or string which start with a starting char
			if !matches!(parsing_type, ParsingType::Comment | ParsingType::StringLiteral) {
				current_token_string.push(this_char);
			}
		}
		// For many tokens types just add the char to the parsing token string
		else if matches!(parsing_type, ParsingType::Comment | ParsingType::NumericalLiteral | ParsingType::OperatorNonAlphabetic | ParsingType::IdentifierKeywordOperatorAlphabeticBuiltInFunction) {
			current_token_string.push(this_char);
		}
		else if parsing_type == ParsingType::StringLiteral {
			
		}
	}
	// Parsing type should be None after parsing line
	#[cfg(debug_assertions)]
	if parsing_type != ParsingType::None {
		panic!("Parsing type should be None, is {:?}", parsing_type);
	}
	// Return parsed tokens
	out
}