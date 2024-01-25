pub mod lexer;
pub mod error;
pub mod scalar_value;
pub mod program;
pub mod bytecode;
pub mod compile;
pub mod program_executer;
pub mod parser;

use std::{io::{stdin, stdout, Write}, collections::{HashMap, HashSet}, rc::Rc};

use lexer::{command::Command, built_in_function::BuiltInFunction, type_restriction::TypeRestriction, separator::Separator, operator::Operator, tokenize::tokenize_line};
use num::BigInt;
use parser::parse_tokens_to_parse_tree_elements;
use program::Program;
use program_executer::ProgramExecuter;

use crate::compile::compile_parse_tree_elements_to_bytecode;

fn main() {
	//env::set_var("RUST_BACKTRACE", "1");
	// Create main struct
	let mut main_struct = Main::new();
	let mut program_executer = ProgramExecuter::new();
	// Starting message
	println!("--- Swamp BASIC {}, type \"exit\" to exit interpreter. ---", option_env!("CARGO_PKG_VERSION").unwrap());
	// Main loop untill exit typed
	'main_loop: loop {
		print!(">");
		stdout().flush().unwrap();
		// Get lines
		let mut input_text = String::new();
		stdin().read_line(&mut input_text).unwrap();
		// For each line
		for line in input_text.lines() {
			// Interpret line
			let exit = interpret_line(&mut main_struct, &mut program_executer, line);
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
#[inline(always)]
fn interpret_line(main_struct: &mut Main, program_executer: &mut ProgramExecuter, line: &str) -> bool {
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
	let first_digit = match first_word.chars().next() {
		Some(first_digit) => first_digit,
		None => return false,
	};
	let (line_number, line_body, print_tokens, print_bytecode, print_parse_tree_elements) = if first_digit.is_ascii_digit() || first_digit == '-' {
		// Get the line number
		let line_number = match first_word.parse::<BigInt>() {
			Err(_) => {
				println!("Error: Line number \"{first_word}\" is not a valid integer.");
				return false;
			},
			Ok(line_number) => line_number,
		};
		// Get rest of line
		(Some(line_number), line_without_first_word, false, false, false)
	}
	// If the line is "tokens" then we will print out the tokens that the lexer produces later
	else if first_word.eq_ignore_ascii_case("tokens") {
		(None, line_without_first_word, true, false, false)
	}
	// If the line is "bytecode" then we will print out the bytecode that the compiler produces later
	else if first_word.eq_ignore_ascii_case("bytecode") {
		(None, line_without_first_word, false, true, false)
	}
	// If the line is "parse" then we will print out the parse tree elements that the compiler produces later
	else if first_word.eq_ignore_ascii_case("parse") {
		(None, line_without_first_word, false, false, true)
	}
	// Exit the interpreter is exit is entered
	else if first_word.eq_ignore_ascii_case("exit") {
		return true;
	}
	// If there is no line number or special word then the line body is the entire line
	else {
		(None, line, false, false, false)
	};

	// Tokenize line body
	let tokens = tokenize_line(main_struct, line_body);
	let tokens = match tokens {
		Ok(tokens) => tokens,
		Err(error) => {
			println!("Lexer error: {error}");
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
	// Parse tokens
	let is_line_program = line_number.is_none();
	let (parse_tree_elements, labels, comment) = match parse_tokens_to_parse_tree_elements(tokens, is_line_program) {
		Ok(parse_tree_elements) => parse_tree_elements,
		Err(error) => {
			println!("Parse error: {error}");
			return false;
		}
	};
	let labels = labels.into_iter().map(|label| label.into_boxed_str()).collect();
	// Print parse tree elements if asked to and return
	if print_parse_tree_elements {
		for parse_tree_element in parse_tree_elements {
			println!("{:?}", parse_tree_element);
		}
		return false;
	}
	// Compile tree elements to bytecode
	let bytecode = match compile_parse_tree_elements_to_bytecode(&parse_tree_elements) {
		Ok(result) => result,
		Err(error) => {
			println!("Compile error: {error}");
			return false;
		}
	};
	// Print bytecode if asked to and return
	if print_bytecode {
		let mut print_comma = false;
		print!("[");
		for byte in bytecode {
			if print_comma {
				print!(", ")
			}
			print!("{byte:02X}");
			print_comma = true;
		}
		println!("]");
		return false;
	}
	// Add line to program if it has a line number
	if let Some(line_number) = line_number {
		let line_insert_result = main_struct.program.add_line(&line_number, &bytecode, labels, comment.as_deref());
		program_executer.invalidate_continue_counter();
		match line_insert_result {
			Err(error) => println!("Error while inserting line: {error}"),
			Ok(_) => {},
		}
		return false;
	}
	// Set the line's bytecode and execute line
	main_struct.program.set_line_program(bytecode);
	program_executer.execute_line(main_struct);
	// Return false (do not exit interpreter)
	false
}

pub struct Main {
	string_to_command_mapping: HashMap<&'static str, Command>,
	string_to_built_in_function_mapping: HashMap<&'static str, BuiltInFunction>,
	string_to_operator_mapping: HashMap<&'static str, Operator>,
	string_to_type_restriction_mapping: HashMap<&'static str, TypeRestriction>,
	char_to_separator_mapping: HashMap<char, Separator>,
	operator_character_set: HashSet<char>,
	
	program: Program,
}

impl Main {
	#[inline(always)]
	pub fn new() -> Self {
		Self {
			string_to_command_mapping: Command::get_string_to_command_mapping(),
			string_to_built_in_function_mapping: BuiltInFunction::get_string_to_built_in_function_mapping(),
			string_to_type_restriction_mapping: TypeRestriction::get_string_to_type_restruction_mapping(),
			char_to_separator_mapping: Separator::get_char_to_separator_mapping(),
			string_to_operator_mapping: Operator::get_string_to_operator_mapping(),
			operator_character_set: Operator::get_character_set(),
			program: Program::new(),
		}
	}
}

pub fn get_rc_only_or_clone<T: Clone>(value: Rc<T>) -> T {
	match Rc::try_unwrap(value) {
		Ok(unwrapped) => unwrapped,
		Err(wrapped) => wrapped.as_ref().clone(),
	}
}