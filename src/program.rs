use std::{collections::HashMap, fs::File, io::{Read, Write}, path::Path, rc::Rc, str::from_utf8};
use postcard::{from_bytes, to_allocvec};

use num::BigInt;
use serde::{de::Visitor, ser::SerializeStruct, Deserialize, Serialize};

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

	pub fn save(&self, mut file_path: String, format: &str) -> Result<(), BasicError> {
		// Get save format from name
		let format = match format {
			"" | "b" | "binary" => SaveFormat::Binary,
			"j" | "json" => SaveFormat::Json,
			_ => return Err(BasicError::InvalidSaveFormat(format.to_string())),
		};
		// Get filepath
		let file_path_path = Path::new(&file_path);
		if file_path_path.extension().is_none() {
			file_path.push_str(match format {
				SaveFormat::Binary => ".sbc",
				SaveFormat::Json => ".json",
			});
		}
		// Serialize program
		let serialized_program = match format {
			SaveFormat::Binary => to_allocvec(self).map_err(|_| BasicError::UnableToSerializeProgram)?,
			SaveFormat::Json => serde_json::to_string_pretty(self).map_err(|_| BasicError::UnableToSerializeProgram)?
				.bytes()
				.collect(),
		};
		// Write to file
		let mut file = File::create(&file_path).map_err(|_| BasicError::UnableToCreateFile(file_path.to_string()))?;
		file.write(&serialized_program).map_err(|_| BasicError::UnableToWriteToFile)?;

		Ok(())
	}

	pub fn load(mut file_path: String, format: &str) -> Result<Self, BasicError> {
		// Get save format from name
		let format = match format {
			"" | "b" | "binary" => SaveFormat::Binary,
			"j" | "json" => SaveFormat::Json,
			_ => return Err(BasicError::InvalidSaveFormat(format.to_string())),
		};
		// Get filepath
		let file_path_path = Path::new(&file_path);
		if file_path_path.extension().is_none() {
			file_path.push_str(match format {
				SaveFormat::Binary => ".sbc",
				SaveFormat::Json => ".json",
			});
		}
		// Read from file
		let mut file = File::open(&file_path).map_err(|_| BasicError::UnableToOpenFile(file_path.to_string()))?;
		let mut file_data = Vec::new();
		file.read_to_end(&mut file_data).map_err(|_| BasicError::UnableToReadFileBytes)?;
		// Deserialize program
		let result: ProgramResult = match format {
			SaveFormat::Binary => from_bytes(&file_data).map_err(|_| BasicError::UnableToDeserializeProgram)?,
			SaveFormat::Json => serde_json::from_slice(&file_data).map_err(|_| BasicError::UnableToDeserializeProgram)?,
		};
		match result {
			ProgramResult::Ok(program) => Ok(program),
			ProgramResult::Err(err) => Err(err),
		}
	}

	fn new_from_loaded_data(
		magic_bytes: [u8; 8], file_version: u64, major_version: u64, minor_version: u64, patch_version: u64,
		bytecode: Vec<u8>, line_numbers: Vec<(BigInt, usize)>, labels: Vec<(BigInt, String)>, comments: Vec<(BigInt, String)>,
	) -> Result<Self, BasicError>
	{
		// Make sure magic bytes match;
		if magic_bytes != MAGIC_BYTES {
			return Err(BasicError::MagicBytesDoNotMatch);
		};
		// Check if the file was saved in a later version of Swamp BASIC
		if file_version > FILE_VERSION {
			println!("Warning: This file was saved with a later file version. Saved in version {file_version}, loading in version {FILE_VERSION}.");
		}
		if is_version_later_than_this_version(patch_version, minor_version, major_version) {
			println!("Warning: This file was saved with a later version of Swamp BASIC. Saved in version {major_version}.{minor_version}.{patch_version}, loading in version {}.", env!("CARGO_PKG_VERSION"));
		}
		// Construct a list of labels for each line
		let mut labels_for_each_line: HashMap<BigInt, Vec<Box<str>>> = HashMap::new();
		for (line_number, label) in labels.iter() {
			match labels_for_each_line.get_mut(line_number) {
				Some(labels) => labels.push(label.clone().into_boxed_str()),
				None => {
					labels_for_each_line.insert(line_number.clone(), vec![label.clone().into_boxed_str()]);
				}
			}
		}
		// Construct struct
		Ok(Self {
			bytecode,
			line_bytecode: Vec::new(),
			line_numbers,
			labels: labels.into_iter().map(|(line_number, label)| (label.into_boxed_str(), line_number)).collect(),
			labels_for_each_line: labels_for_each_line.into_iter().map(|(line_number, labels)| (line_number, labels.into_boxed_slice())).collect(),
			comments: comments.into_iter().map(|(line_number, comment)| (line_number, comment.into_boxed_str())).collect(),
		})
	}
}

#[repr(u8)]
enum SaveFormat {
	Binary,
	Json,
}

const MAGIC_BYTES: [u8; 8] = [b'S', b'W', b'B', b'A', b'S', b'I', b'C', 0x69];
const FILE_VERSION: u64 = 0;

impl Serialize for Program {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: serde::Serializer {
		let mut serializer = serializer.serialize_struct("program", 3)?;

		serializer.serialize_field("magic", &MAGIC_BYTES)?;
		serializer.serialize_field("file_version", &FILE_VERSION)?;
		serializer.serialize_field("basic_version_major", &env!("CARGO_PKG_VERSION_MAJOR").parse::<u64>().unwrap())?;
		serializer.serialize_field("basic_version_minor", &env!("CARGO_PKG_VERSION_MINOR").parse::<u64>().unwrap())?;
		serializer.serialize_field("basic_version_patch", &env!("CARGO_PKG_VERSION_PATCH").parse::<u64>().unwrap())?;
		serializer.serialize_field("bytecode", &self.bytecode)?;
		serializer.serialize_field("line_numbers", &self.line_numbers)?;
		serializer.serialize_field("labels", &self.labels.iter().map(|(label, line_number)| (line_number, &**label)).collect::<Vec<(&BigInt, &str)>>())?;
		serializer.serialize_field("comments", &self.comments.iter().map(|(line_number, comment)| (line_number, &**comment)).collect::<Vec<(&BigInt, &str)>>())?;

		serializer.end()
	}
}

/// Returns true if a version of BASIC is later then this version.
fn is_version_later_than_this_version(patch: u64, minor: u64, major: u64) -> bool {
	let this_version_patch = env!("CARGO_PKG_VERSION_PATCH").parse::<u64>().unwrap();
	let this_version_minor = env!("CARGO_PKG_VERSION_MINOR").parse::<u64>().unwrap();
	let this_version_major = env!("CARGO_PKG_VERSION_MAJOR").parse::<u64>().unwrap();

	if major > this_version_major {
		return true;
	}
	else if major < this_version_major {
		return false;
	}

	if minor > this_version_minor {
		return true;
	}
	else if minor < this_version_minor {
		return false;
	}

	patch > this_version_patch
}

struct ProgramVisitor;

impl<'de> Visitor<'de> for ProgramVisitor {
	type Value = Result<Program, BasicError>;

	fn expecting(&self, _formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
		todo!()
	}

	fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: serde::de::SeqAccess<'de>, {
		// Make sure magic bytes match
		let magic_bytes = seq.next_element::<[u8; 8]>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		// Read file version
		let file_version = seq.next_element::<u64>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		// Read BASIC version
		let major_version = seq.next_element::<u64>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		let minor_version = seq.next_element::<u64>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		let patch_version = seq.next_element::<u64>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		// Read bytecode
		let bytecode = seq.next_element::<Vec<u8>>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		// Read line numbers
		let line_numbers = seq.next_element::<Vec<(BigInt, usize)>>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		// Read labels
		let labels = seq.next_element::<Vec<(BigInt, String)>>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		// Read comments
		let comments = seq.next_element::<Vec<(BigInt, String)>>()?.ok_or(serde::de::Error::custom("end of file reached"))?;
		// Construct program struct
		Ok(Program::new_from_loaded_data(magic_bytes, file_version, major_version, minor_version, patch_version, bytecode, line_numbers, labels, comments))
	}

	fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where A: serde::de::MapAccess<'de>, {
		let mut magic_bytes = None;
		let mut file_version = None;
		let mut major_version = None;
		let mut minor_version = None;
		let mut patch_version = None;
		let mut bytecode = None;
		let mut line_numbers = None;
		let mut labels = None;
		let mut comments = None;
		// Get values
		while let Some(key) = map.next_key::<String>()? {
			match key.as_str() {
				"magic" => magic_bytes = Some(map.next_value::<[u8; 8]>()?),
				"file_version" => file_version = Some(map.next_value::<u64>()?),
				"basic_version_major" => major_version = Some(map.next_value::<u64>()?),
				"basic_version_minor" => minor_version = Some(map.next_value::<u64>()?),
				"basic_version_patch" => patch_version = Some(map.next_value::<u64>()?),
				"bytecode" => bytecode = Some(map.next_value::<Vec<u8>>()?),
				"line_numbers" => line_numbers = Some(map.next_value::<Vec<(BigInt, usize)>>()?),
				"labels" => labels = Some(map.next_value::<Vec<(BigInt, String)>>()?),
				"comments" => comments = Some(map.next_value::<Vec<(BigInt, String)>>()?),
				_ => {
					map.next_value::<serde_json::Value>()?;
					println!("Warning: Invalid key \"{key}\".")
				}
			}
		}
		// Make sure all values exist or get defaults
		let magic_bytes = match magic_bytes {
			Some(magic_bytes) => magic_bytes,
			None => return Ok(Err(BasicError::MissingMagicBytes)),
		};
		let file_version = match file_version {
			Some(file_version) => file_version,
			None => {
				println!("Warning: File version not found, using default version.");
				0
			}
		};
		let major_version = match major_version {
			Some(major_version) => major_version,
			None => {
				println!("Warning: Major version not found, using default version.");
				0
			}
		};
		let minor_version = match minor_version {
			Some(minor_version) => minor_version,
			None => {
				println!("Warning: Minor version not found, using default version.");
				0
			}
		};
		let patch_version = match patch_version {
			Some(patch_version) => patch_version,
			None => {
				println!("Warning: Patch version not found, using default version.");
				0
			}
		};
		let bytecode = match bytecode {
			Some(bytecode) => bytecode,
			None => {
				println!("Warning: Bytecode version not found, using empty bytecode.");
				Vec::new()
			}
		};
		let line_numbers = match line_numbers {
			Some(line_numbers) => line_numbers,
			None => {
				println!("Warning: Line numbers not found, using no line numbers.");
				Vec::new()
			}
		};
		let labels = match labels {
			Some(labels) => labels,
			None => {
				println!("Warning: Labels not found, using no labels.");
				Vec::new()
			}
		};
		let comments = match comments {
			Some(comments) => comments,
			None => {
				println!("Warning: Comments not found, using no comments.");
				Vec::new()
			}
		};
		// Construct program struct
		Ok(Program::new_from_loaded_data(magic_bytes, file_version, major_version, minor_version, patch_version, bytecode, line_numbers, labels, comments))
	}
}

impl<'de> Deserialize<'de> for ProgramResult {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> {
		match deserializer.deserialize_struct("program", &[
			"magic", "file_version", "basic_version_major", "basic_version_minor", "basic_version_patch", "bytecode", "line_numbers", "labels", "comments"
		], ProgramVisitor)?
		{
			Ok(program) => Ok(ProgramResult::Ok(program)),
			Err(err) => Ok(ProgramResult::Err(err)),
		}
	}
}

pub enum ProgramResult {
	Ok(Program),
	Err(BasicError),
}