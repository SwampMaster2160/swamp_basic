use std::str::from_utf8;

use num::BigInt;

use crate::error::BasicError;

/// A container for the loaded BASIC program containing the program bytecode and where line numbers and labels point to in the bytecode.
pub struct Program {
	/// The bytecode for the entire program.
	bytecode: Vec<u8>,
	/// A list of (line number, bytecode index) tuples that exist, MUST be ordered in accending order.
	line_numbers: Vec<(BigInt, usize)>,
	/// Bytecode for a line program without lines.
	line_bytecode: Vec<u8>,
}

impl Program {
	/// Construct a new blank program.
	pub fn new() -> Self {
		Self {
			bytecode: Vec::new(),
			line_numbers: Vec::new(),
			line_bytecode: Vec::new(),
		}
	}

	/// Returns the index into `line_numbers` that contains the line number or an error if the line number does not exist in the program.
	fn get_line_numbers_index_from_line_number(&self, target_line_number: &BigInt) -> Result<usize, BasicError> {
		match self.line_numbers.binary_search_by(|(line_number, _)| line_number.cmp(target_line_number)) {
			Ok(line_numbers_index) => Ok(line_numbers_index),
			Err(_) => Err(BasicError::LineNotFound(target_line_number.clone())),
		}
	}

	/// Returns the index into `line_numbers` where a line could be inserted.
	fn get_line_numbers_index_to_insert_line(&self, target_line_number: &BigInt) -> usize {
		match self.line_numbers.binary_search_by(|(line_number, _)| line_number.cmp(target_line_number)) {
			Ok(line_numbers_index) => line_numbers_index,
			Err(line_numbers_index) => line_numbers_index,
		}
	}

	/// Takes a line number and returns an index into the program bytecode.
	pub fn get_bytecode_index_from_line_number(&self, target_line_number: &BigInt) -> Result<usize, BasicError> {
		self.get_line_numbers_index_from_line_number(target_line_number)
			.map(|line_numbers_index| self.line_numbers[line_numbers_index].1)
	}

	/// Removes a line and it's bytecode. Lines afterwards will have their bytecode indicies adjusted.
	pub fn remove_line(&mut self, line_number: &BigInt) -> Result<(), BasicError> {
		// Get the index into the line numbers vector for the line
		let line_numbers_index = self.get_line_numbers_index_from_line_number(line_number)?;
		// Get where the line's bytecode starts and ends
		let bytecode_remove_start_index = self.line_numbers[line_numbers_index].1;
		let bytecode_remove_end_index = match self.line_numbers.get(line_numbers_index + 1) {
			Some((_, bytecode_remove_end_index)) => *bytecode_remove_end_index,
			None => self.bytecode.len(),
		};
		let length_of_bytecode_to_remove = bytecode_remove_end_index - bytecode_remove_start_index;
		// Remove the line's bytecode from the bytecode vector
		self.bytecode.drain(bytecode_remove_start_index..bytecode_remove_end_index);
		// Remove line number
		self.line_numbers.remove(line_numbers_index);
		// Repoint all the lines after the line removed
		for (_, bytecode_index) in &mut self.line_numbers.iter_mut().skip(line_numbers_index) {
			*bytecode_index -= length_of_bytecode_to_remove;
		}

		Ok(())
	}

	/// Inserts or updates a line and it's bytecode. Lines afterwards will have their bytecode indicies adjusted.
	/// If the line's bytecode is empty then will remove the line if it exists or else do nothing.
	pub fn add_line(&mut self, line_number: &BigInt, bytecode_to_insert: &[u8]) {
		// Remove the line if it exists ignoring an error that will be returned if the line does not exist
		self.remove_line(line_number).ok();
		// Return and do not insert the line if it is blank
		if bytecode_to_insert.is_empty() {
			return;
		}
		// Get the index to insert the line number and the bytecode
		let insert_index = self.get_line_numbers_index_to_insert_line(line_number);
		let bytecode_insert_index = match self.line_numbers.get(insert_index) {
			Some((_, bytecode_insert_index)) => *bytecode_insert_index,
			None => self.bytecode.len(),
		};
		let insert_length = bytecode_to_insert.len();
		// Insert the bytecode
		self.bytecode.splice(bytecode_insert_index..bytecode_insert_index, bytecode_to_insert.iter().map(|byte| *byte));
		// Insert the line
		self.line_numbers.insert(insert_index, (line_number.clone(), bytecode_insert_index));
		// Repoint all the lines after the line inserted
		for(_, bytecode_index) in &mut self.line_numbers.iter_mut().skip(insert_index + 1) {
			*bytecode_index += insert_length;
		}
	}

	#[inline(always)]
	pub fn set_line_program(&mut self, line_bytecode: Vec<u8>) {
		self.line_bytecode = line_bytecode;
	}

	#[inline(always)]
	fn get_bytecode(&self, is_line_program: bool) -> &[u8] {
		match is_line_program {
			false => self.bytecode.as_slice(),
			true => self.line_bytecode.as_slice(),
		}
	}

	/// Returns the next byte from the program then increments the program counter. Returns None if the end of the program has been reached.
	pub fn get_byte(&self, is_line_program: bool, program_counter: &mut usize) -> Option<u8> {
		let byte = self.get_bytecode(is_line_program)
			.get(*program_counter)
			.cloned();
		if byte.is_some() {
			*program_counter += 1;
		}
		byte
	}

	/// Returns the null-terminated utf-8 encoded string that is pointed to in the program by the program counter.
	///
	/// The program counter is incremented to point to the byte after the string's null byte.
	pub fn get_string(&self, is_line_program: bool, program_counter: &mut usize) -> Result<&str, BasicError> {
		let bytecode = self.get_bytecode(is_line_program);
		let null_byte_index = bytecode.iter()
			.skip(*program_counter)
			.position(|byte| *byte == 0)
			.ok_or(BasicError::UnterminatedString)?;
		let utf_8_byte_slice = &bytecode[*program_counter..*program_counter + null_byte_index];
		*program_counter += null_byte_index + 1;
		from_utf8(utf_8_byte_slice)
			.map_err(|_| BasicError::InvalidUtf8String)
	}
}