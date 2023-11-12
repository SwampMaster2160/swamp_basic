use num::BigInt;

use crate::error::BasicError;

/// A container for the loaded BASIC program containing the program bytecode and where line numbers and labels point to in the bytecode.
pub struct Program {
	/// The bytecode for the entire program.
	bytecode: Vec<u8>,
	/// A list of (line number, bytecode index) tuples that exist, MUST be ordered in accending order.
	line_numbers: Vec<(BigInt, usize)>,
}

impl Program {
	/// Returns the index into `line_numbers` that contains the line number or an error if the line number does not exist in the program.
	fn get_line_numbers_index_from_line_number(&self, target_line_number: &BigInt) -> Result<usize, BasicError> {
		match self.line_numbers.binary_search_by(|(line_number, _)| line_number.cmp(target_line_number)) {
			Ok(line_numbers_index) => Ok(line_numbers_index),
			Err(_) => Err(BasicError::LineNotFound(target_line_number.clone())),
		}
	}

	/// Takes a line number and returns an index into the program bytecode.
	pub fn get_bytecode_index_from_line_number(&self, target_line_number: &BigInt) -> Result<usize, BasicError> {
		self.get_line_numbers_index_from_line_number(target_line_number)
			.map(|line_numbers_index| self.line_numbers[line_numbers_index].1)
	}

	pub fn get_bytecode(&self) -> &[u8] {
		&self.bytecode
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
		// Repoint all the lines after the line removed
		for (_, bytecode_index) in &mut self.line_numbers.iter_mut().skip(line_numbers_index) {
			*bytecode_index -= length_of_bytecode_to_remove;
		}
		Ok(())
	}

	pub fn add_line(&mut self, line_number: &BigInt, bytecode: &[u8]) {
		// Remove the line if it exists
		self.remove_line(line_number).ok();
		
	}
}