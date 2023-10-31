pub mod lexer;

use std::io::stdin;

use lexer::token::tokenize_line;

fn main() {
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
			let exit = interpret_line(line);
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
fn interpret_line(line: &str) -> bool {
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
	// The line is numbered if it's first char is a digit
	let (line_number, line_body, print_tokens) = if first_word.chars().next().unwrap().is_ascii_digit() {
		// Get the line number
		let line_number = match first_word.parse::<i32>() {
			Err(_) => {
				println!("Error: Line number \"{}\" is not a valid signed 32-bit integer.", first_word);
				return false;
			},
			Ok(line_number) => line_number,
		};
		// Get rest of line
		(Some(line_number), line_without_first_word, false)
	}
	// If the line is "tokens" then we will print out the tokens that the lexer produces later
	else if first_word.eq_ignore_ascii_case("tokens") {
		(None, line, true)
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
	let tokens = tokenize_line(line_body);
	// Print tokens if asked to and return
	if print_tokens {
		for token in tokens {
			println!("{:?}", token);
		}
		return false;
	}
	//
	println!("Line {:?}: \"{}\"", line_number, line_body);
	// Return false (do not exit interpreter)
	false
}