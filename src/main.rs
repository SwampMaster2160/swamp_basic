use std::io::stdin;

fn main() {
	println!("--- Swamp BASIC, type \"exit\" to exit interpreter. ---");
	'main_loop: loop {
		let mut input_text = String::new();
		stdin().read_line(&mut input_text).unwrap();
		for line in input_text.lines() {
			if line == "exit" {
				break 'main_loop;
			}
			interpret_line(line);
		}
	}
}

fn interpret_line(line: &str) {
	println!("{}", line)
}