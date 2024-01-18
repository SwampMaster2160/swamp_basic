use std::{str::from_utf8, rc::Rc, collections::HashMap};

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
	/// A map from each line that has labels to a list of its labels.
	labels_for_each_line: HashMap<BigInt, Box<[Box<str>]>>,
	/// A map from labels to their line number.
	labels: HashMap<Box<str>, BigInt>,
	/// A map from each line that has a comment to its comment.
	comments: HashMap<BigInt, Box<str>>,
}

impl Program {
	/// Construct a new blank program.
	pub fn new() -> Self {
		Self {
			bytecode: Vec::new(),
			line_numbers: Vec::new(),
			line_bytecode: Vec::new(),
			comments: HashMap::new(),
			labels_for_each_line: HashMap::new(),
			labels: HashMap::new(),
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
		// Remove labels
		if let Some(labels) = self.labels_for_each_line.remove(line_number) {
			for label in labels.into_iter() {
				self.labels.remove(label);
			}
		}
		// Remove comment
		self.comments.remove(line_number);

		Ok(())
	}

	/// Inserts or updates a line and it's bytecode. Lines afterwards will have their bytecode indicies adjusted.
	/// If the line's bytecode is empty then will remove the line if it exists or else do nothing.
	pub fn add_line(&mut self, line_number: &BigInt, bytecode_to_insert: &[u8], labels: Box<[Box<str>]>, comment: Option<&str>) -> Result<(), BasicError> {
		// Make sure none of the labels conflict with existing labels in the program
		for label in labels.iter() {
			if self.labels.contains_key(label) {
				return Err(BasicError::LabelConflict(label.to_string()));
			}
		}
		// Remove the line if it exists ignoring an error that will be returned if the line does not exist
		self.remove_line(line_number).ok();
		// Return and do not insert the line if it is blank
		if bytecode_to_insert.is_empty() && labels.is_empty() && comment.is_none() {
			return Ok(());
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
		// Set the labels for the line
		for label in labels.iter() {
			self.labels.insert(label.clone(), line_number.clone());
		}
		if !labels.is_empty() {
			self.labels_for_each_line.insert(line_number.clone(), labels);
		}
		// Set the comment for the line
		match comment {
			Some(comment) => {
				self.comments.insert(line_number.clone(), comment.into());
			},
			None => {}
		}

		Ok(())
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

	/// Returns the line that the bytecode at the index is on.
	pub fn get_line_number_bytecode_is_in(&self, bytecode_index: usize) -> &BigInt {
		let line_number_index = match self.line_numbers.binary_search_by(|(_, index)| index.cmp(&bytecode_index)) {
			Ok(line_number_index) => line_number_index,
			Err(line_number_index) => line_number_index - 1,
		};
		&self.line_numbers[line_number_index].0
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

	/// Skips the string that is pointed to in the program by the program counter.
	///
	/// The program counter is incremented to point to the byte after the string's null byte.
	pub fn skip_string(&self, is_line_program: bool, program_counter: &mut usize) -> Result<(), BasicError> {
		// Get bytecode
		let bytecode = self.get_bytecode(is_line_program);
		// Get null byte index or return an error if none exist
		let null_byte_index = bytecode.iter()
			.skip(*program_counter)
			.position(|byte| *byte == 0)
			.ok_or(BasicError::UnterminatedString)?;
		// Repoint program counter to point to the byte after the null byte
		*program_counter += null_byte_index + 1;
		Ok(())
	}

	/// Returns the null-terminated utf-8 encoded string that is pointed to in the program by the program counter.
	///
	/// The program counter is incremented to point to the byte after the string's null byte.
	pub fn get_string(&self, is_line_program: bool, program_counter: &mut usize) -> Result<&str, BasicError> {
		// Get bytecode
		let bytecode = self.get_bytecode(is_line_program);
		// Get null byte index or return an error if none exist
		let null_byte_index = bytecode.iter()
			.skip(*program_counter)
			.position(|byte| *byte == 0)
			.ok_or(BasicError::UnterminatedString)?;
		// Get the byte slice that contains the string without the null byte
		let utf_8_byte_slice = &bytecode[*program_counter..*program_counter + null_byte_index];
		// Repoint program counter to point to the byte after the null byte
		*program_counter += null_byte_index + 1;
		// Return string slice or an error if is not a valid utf-8 string
		from_utf8(utf_8_byte_slice)
			.map_err(|_| BasicError::InvalidUtf8String)
	}

	/// Get the index of the line number in the list of line numbers.
	/// `None` will return 0.
	/// If the line number is not in the program then returns the line index afterwards.
	pub fn get_lines_start_index(&self, line_number: Option<Rc<BigInt>>) -> usize {
		match line_number {
			Some(line_number) => {
				match self.line_numbers.binary_search_by(|(line_numbers_number, _)| line_numbers_number.cmp(&line_number)) {
					Ok(index) => index,
					Err(index) => index,
				}
			}
			None => 0
		}
	}

	/// Get the index of the line number in the list of line numbers.
	/// `None` will return the index of the last line.
	/// If the line number is not in the program then returns the line index beforehand.
	/// Returns `None` id the line number is less than the first line number in the program
	pub fn get_lines_end_index(&self, line_number: Option<Rc<BigInt>>) -> Option<usize> {
		match line_number {
			Some(line_number) => match self.line_numbers.binary_search_by(|(line_numbers_number, _)| line_numbers_number.cmp(&line_number)) {
				Ok(index) => Some(index),
				Err(index) => index.checked_sub(1),
			}
			None => self.line_numbers.len().checked_sub(1),
		}
	}

	/// Takes in the index of a line and returns the line number and bytecode.
	/// Returns `None` if the index is out of bounds.
	pub fn get_line_and_number_from_line_index(&self, index: usize) -> (&BigInt, &[u8]) {
		let (line_number, bytecode_start_index) = &self.line_numbers[index];
		let bytecode_start_index = *bytecode_start_index;
		let bytecode_end_index = match self.line_numbers.get(index + 1) {
			Some((_, bytecode_end_index)) => *bytecode_end_index,
			None => self.bytecode.len(),
		};
		let bytecode = &self.bytecode[bytecode_start_index..bytecode_end_index];
		(line_number, bytecode)
	}

	/// Gets the comment for a line. Returns `None` if there is no comment for the line.
	pub fn get_line_comment(&self, line: &BigInt) -> Option<&str> {
		self.comments.get(line)
			.map(|comment| &**comment)
	}

	/// Gets the comment for a line. Returns `None` if there is no comment for the line.
	pub fn get_line_labels(&self, line: &BigInt) -> Vec<String> {
		match self.labels_for_each_line.get(line) {
			Some(labels) => labels.iter()
				.map(|label| label.to_string())
				.collect(),
			None => Vec::new(),
		}
	}

	/// Takes in a label name and returns the line number it is on.
	pub fn get_labels_line(&self, label: &str) -> Option<&BigInt> {
		self.labels.get(label)
	}
}