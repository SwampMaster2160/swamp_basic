pub mod lexer;
pub mod error;

use std::{io::stdin, collections::{HashMap, HashSet}};

use lexer::{token::tokenize_line, keyword::Keyword, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, separator::Separator, operator::Operator};
use num::BigInt;

fn main() {
	// Create main struct
	let mut main_struct = Main::new();
	// Starting message
	println!("--- Swamp BASIC, type \"exit\" to exit interpreter. ---");
	// Main loop untill exit typed
	'main_loop: loop {
		// Get lines
		let mut input_text = String::new();
		stdin().read_line(&mut input_text).unwrap();
		// For each line
		for line in input_text.lines() {
			// Interpret line
			let exit = interpret_line(&mut main_struct, line);
			// Exit if the function returned true
			if exit {
				break 'main_loop;
			}
		}
	}
}

/// Interpret a line that is a string slice.
/// Should not contain any newline chars.
/// Returns weather to exit the interpreter.
fn interpret_line(main_struct: &mut Main, line: &str) -> bool {
	// Remove starting whitespaces
	let line = line.trim_start();
	// Get first word of line
	let first_word_end = match line.find(|chr: char| chr.is_whitespace()) {
		None => line.len(),
		Some(line_number_substring_end) => line_number_substring_end,
	};
	let first_word = &line[..first_word_end];
	let line_without_first_word = &line[first_word_end..];

	// Get the line number if there is one, the main part of the line to be converted to tokens and weather to just print the tokens
	// The line is numbered if it's first char is a digit or negative sign
	let first_digit = first_word.chars().next().unwrap();
	let (line_number, line_body, print_tokens) = if first_digit.is_ascii_digit() || first_digit == '-' {
		// Get the line number
		let line_number = match first_word.parse::<BigInt>() {
			Err(_) => {
				println!("Error: Line number \"{first_word}\" is not a valid integer.");
				return false;
			},
			Ok(line_number) => line_number,
		};
		// Get rest of line
		(Some(line_number), line_without_first_word, false)
	}
	// If the line is "tokens" then we will print out the tokens that the lexer produces later
	else if first_word.eq_ignore_ascii_case("tokens") {
		(None, line_without_first_word, true)
	}
	// Exit the interpreter is exit is entered
	else if first_word.eq_ignore_ascii_case("exit") {
		return true;
	}
	// If there is no line number or special word then the line body is the entire line
	else {
		(None, line, false)
	};

	// Tokenize line body
	let tokens = tokenize_line(main_struct, line_body);
	let tokens = match tokens {
		Ok(tokens) => tokens,
		Err(error) => {
			println!("Error: {}", error);
			return false;
		}
	};
	// Print tokens if asked to and return
	if print_tokens {
		for token in tokens {
			println!("{:?}", token);
		}
		return false;
	}
	//
	println!("Line {:?}: \"{:?}\"", line_number, tokens);
	// Return false (do not exit interpreter)
	false
}

pub struct Main {
	string_to_keyword_mapping: HashMap<&'static str, Keyword>,
	string_to_built_in_keyword_mapping: HashMap<&'static str, BuiltInFunction>,
	string_to_operator_mapping: HashMap<&'static str, Operator>,
	char_to_type_restriction_mapping: HashMap<char, TypeRestriction>,
	char_to_separator_mapping: HashMap<char, Separator>,
	operator_character_set: HashSet<char>,
}

impl Main {
	#[inline(always)]
	pub fn new() -> Self {
		Self {
			string_to_keyword_mapping: Keyword::get_string_to_keyword_mapping(),
			string_to_built_in_keyword_mapping: BuiltInFunction::get_string_to_built_in_function_mapping(),
			char_to_type_restriction_mapping: TypeRestriction::get_char_to_type_restruction_mapping(),
			char_to_separator_mapping: Separator::get_char_to_separator_mapping(),
			string_to_operator_mapping: Operator::get_string_to_operator_mapping(),
			operator_character_set: Operator::get_character_set(),
		}
	}
}