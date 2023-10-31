use std::io::stdin;

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
			// Break loop and exit if exit typed
			if line == "exit" {
				break 'main_loop;
			}
			// Else interpret line
			interpret_line(line);
		}
	}
}

/// Interpret a line that is a string slice.
/// Should not contain any newline chars.
fn interpret_line(line: &str) {
	let first_char = match line.chars().next() {
		None => return,
		Some(first_char) => first_char,
	};
	if first_char.is_ascii_digit() {
		let line_number_substring_end = match line.find(|chr: char| chr.is_whitespace()) {
			None => line.len(),
			Some(line_number_substring_end) => line_number_substring_end,
		};
		let line_number_str = &line[..line_number_substring_end];
		let line_number = match line_number_str.parse::<i32>() {
			Err(_) => {
				println!("Error: Line number \"{}\" is not a valid signed 32-bit integer.", line_number_str);
				return;
			},
			Ok(line_number) => line_number,
		};
		let line_without_number = &line[line_number_substring_end..];
		println!("Line {}: \"{}\"", line_number, line_without_number);
		return;
	}
	println!("{}", line);
}