use std::io::{stdin, stdout, Write};
use std::mem;
use std::{rc::Rc, collections::HashMap};
use std::hash::Hash;

use num::{BigInt, complex::Complex64};
use num_traits::FromPrimitive;

use crate::compile::decompile_line;
use crate::lexer::tokenize::detokenize_line;
use crate::parser::deparse_line;
use crate::{Main, error::BasicError, bytecode::{statement_opcode::StatementOpcode, expression_opcode::ExpressionOpcode, l_value_opcode::LValueOpcode}, lexer::type_restriction::TypeRestriction, scalar_value::{scalar_value::ScalarValue, integer::BasicInteger, string::BasicString}};

/// Holds the execution state of an executing program. Holds stuff like variables, the program counter, ect.
pub struct ProgramExecuter {
	/// The opcode that will next be executed by the executer if executing the main program.
	program_counter: usize,
	/// The opcode that will next be executed by the executer if executing a line program.
	line_program_counter: usize,
	//continue_counter: Option<usize>,
	/// Is the program executing a line program?
	is_executing_line_program: bool,
	/// All the global scalar variables.
	scalar_variables: HashMap<(Box<str>, TypeRestriction), ScalarValue>,
	/// A map from (name, type restriction, dimension count) to (dimension lengths, all values in a 1D vector).
	arrays: HashMap<(Box<str>, TypeRestriction, usize), (Vec<usize>, Vec<ScalarValue>)>,
	/// When gosub is called, the current routine level info is pushed here.
	routine_stack: Vec<RoutineLevel>,
	/// The current routine level info.
	current_routine: RoutineLevel,
}

struct RoutineLevel {
	/// `None` if "if" has not been called since the subroutine start.
	/// The first bool hold weather the next "then" should be taken, the second holds weather the next "else" should be taken.
	if_condition: Option<(bool, bool)>,
	/// A list of l-values that for loops are using as counters. Maps said l-values to the index of the opcode that is at the start of the loop.
	for_loop_counters: HashMap<LValue, ForLoop>,
	/// The for loop l-value that "to" and "step" will change.
	/// `Err(true)` if we have looped back since a "for".
	/// `Err(false)` if the program started or a subroutine was entered since the last "for" keyword.
	current_for_loop_counter: Result<LValue, bool>,
	/// The location that should be returned to when a subroutine ends and we return to the subroutine of this struct.
	return_address: usize,
	/// When we return from a subroutine, we need to know if we are returning to a line program or a main program.
	return_is_line_address: bool,
}

impl RoutineLevel {
	pub fn new() -> Self {
		RoutineLevel {
			if_condition: None,
			for_loop_counters: HashMap::new(),
			current_for_loop_counter: Err(false),
			return_address: 0,
			return_is_line_address: false,
		}
	}
}

impl Default for RoutineLevel {
	fn default() -> Self {
		Self::new()
	}
}

struct ForLoop {
	start_bytecode_index: usize,
	is_in_line_program: bool,
	end_value: Option<ScalarValue>,
	step_value: Option<ScalarValue>,
}

impl ForLoop {
	pub const fn new(start_bytecode_index: usize, is_in_line_program: bool) -> Self {
		Self {
			start_bytecode_index,
			is_in_line_program,
			end_value: None,
			step_value: None,
		}
	}
}

#[derive(Clone, Copy)]
#[repr(u8)]
enum InstructionExecutionSuccessResult {
	ContinueToNextInstruction,
	ProgramEnd,
	ProgramStopped,
}

#[derive(Clone, PartialEq, Eq)]
struct LValue {
	name: Box<str>,
	type_restriction: TypeRestriction,
	arguments_or_indices: Option<Box<[ScalarValue]>>,
}

impl Hash for LValue {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.name.hash(state);
		self.type_restriction.hash(state);
		// Arguments are not hashed
	}
}

impl ProgramExecuter {
	/// Create a program executer in the deafult state.
	pub fn new() -> Self {
		ProgramExecuter {
			program_counter: 0,
			line_program_counter: 0,
			//continue_counter: None,
			is_executing_line_program: false,
			scalar_variables: HashMap::new(),
			current_routine: RoutineLevel::new(),
			routine_stack: Vec::new(),
			arrays: HashMap::new(),
		}
	}

	/// Gets a mutable reference to the current program counter (main or line).
	#[inline(always)]
	fn get_program_counter(&mut self) -> &mut usize {
		match self.is_executing_line_program {
			true => &mut self.line_program_counter,
			false => &mut self.program_counter,
		}
	}

	/// Clears everything about the program's execution state except the execution location including variables, arrays, functions, the stack, ect.
	fn clear(&mut self) {
		self.scalar_variables = HashMap::new();
		self.arrays = HashMap::new();
		self.current_routine = RoutineLevel::new();
		self.routine_stack = Vec::new();
	}

	/// Retrives a byte from the program and increments the current program counter. Or returns None if the end of the program has been reached.
	fn get_program_byte(&mut self, main_struct: &Main) -> Option<u8> {
		main_struct.program.get_byte(self.is_executing_line_program, self.get_program_counter())
	}

	/// Retrives a string from the program and increments the current program counter to the byte after the string's null byte.
	fn get_program_string<'a>(&'a mut self, main_struct: &'a Main) -> Result<&str, BasicError> {
		main_struct.program.get_string(self.is_executing_line_program, self.get_program_counter())
	}

	/// Skips a string from the program and increments the current program counter to the byte after the string's null byte.
	fn skip_program_string(&mut self, main_struct: &Main) -> Result<(), BasicError> {
		main_struct.program.skip_string(self.is_executing_line_program, self.get_program_counter())
	}

	/// Creates an array
	fn create_array(&mut self, name: Box<str>, type_restriction: TypeRestriction, dimension_lengths: Vec<usize>) -> Result<(), BasicError> {
		// Get the total array size
		let mut total_array_size = 1usize;
		for dimension_length in dimension_lengths.iter() {
			total_array_size = match total_array_size.checked_mul(*dimension_length) {
				Some(total_array_size) => total_array_size,
				None => return Err(BasicError::ArraySizeTooLarge),
			}
		}
		if dimension_lengths.len() == 0 {
			total_array_size = 0;
		}
		// Construct array
		let default_value = type_restriction.default_value();
		let array: Vec<ScalarValue> = (0..total_array_size).into_iter().map(|_| default_value.clone()).collect();
		// Add array to global arrays
		self.arrays.insert((name, type_restriction, dimension_lengths.len()), (dimension_lengths, array));

		Ok(())
	}

	fn get_input_value(&mut self, input: &str, type_restriction: TypeRestriction) -> Result<ScalarValue, BasicError> {
		// Parse
		let out = match type_restriction {
			TypeRestriction::Any | TypeRestriction::String => ScalarValue::String(BasicString::String(Rc::new(input.to_string())).compact()),
			TypeRestriction::Integer | TypeRestriction::ComplexFloat | TypeRestriction::Number | TypeRestriction::RealNumber | TypeRestriction::Float => {
				let input = input.trim();
				// Try to convert to complex number
				if input.ends_with('i') {
					let (string_without_i, _) = input.split_at(input.len() - 1);
					if let Ok(value) = string_without_i.parse() {
						return Ok(ScalarValue::ComplexFloat(Complex64::new(0., value)).compact());
					}
					return Err(BasicError::ParseError)
				}
				// Try to convert to integer
				if let Ok(value) = input.parse() {
					return Ok(ScalarValue::Integer(BasicInteger::SmallInteger(value)));
				}
				if let Ok(value) = input.parse() {
					return Ok(ScalarValue::Integer(BasicInteger::BigInteger(Rc::new(value))));
				}
				// Try to convert to float
				if let Ok(value) = input.parse() {
					return Ok(ScalarValue::Float(value));
				}
				// Else error
				return Err(BasicError::ParseError);
			}
			TypeRestriction::Boolean => {
				match input.trim() {
					"true" | "t" | "yes" | "y" | "1" => ScalarValue::Boolean(true),
					"false" | "f" | "no" | "n" | "0"  => ScalarValue::Boolean(false),
					_ => return Err(BasicError::ParseError),
				}
			}
		};
		// Make sure the value conforms to the type restriction
		match out.conforms_to_type_restriction(type_restriction) {
			true => Ok(out),
			false => Err(BasicError::TypeMismatch(out, type_restriction)),
		}
	}

	/// Executes a single statement.
	fn execute_statement(&mut self, main_struct: &mut Main, should_exist: bool) -> Result<InstructionExecutionSuccessResult, BasicError> {
		let mut out = InstructionExecutionSuccessResult::ContinueToNextInstruction;
		// Get opcode
		let opcode_id = match self.get_program_byte(main_struct) {
			Some(opcode_id) => opcode_id,
			None => match should_exist {
				false => return Ok(InstructionExecutionSuccessResult::ProgramEnd),
				true => return Err(BasicError::ExpectedStatementOpcodeButProgramEnd),
			},
		};
		let opcode: StatementOpcode = FromPrimitive::from_u8(opcode_id)
			.ok_or(BasicError::InvalidStatementOpcode(opcode_id))?;
		// Execute statment instruction
		match opcode {
			StatementOpcode::End => out = InstructionExecutionSuccessResult::ProgramStopped,
			StatementOpcode::Print => {
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					let result = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
					print!("{result}");
					stdout().flush().unwrap();
				}
			}
			StatementOpcode::Input => {
				// Print statements to be printed
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					let result = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
					print!("{result}");
					stdout().flush().unwrap();
				}
				// Get a list of input variables
				let mut l_values = Vec::new();
				loop {
					match self.execute_l_value(main_struct)? {
						Some(l_value) => l_values.push(l_value),
						None => break,
					}
				}
				// Get input
				let mut l_value_index = 0;
				'input_loop: loop {
					// Get input
					let mut input = String::new();
					stdin().read_line(&mut input).unwrap();
					input = input.chars().filter(|chr| !chr.is_ascii_control()).collect();
					let input = input.as_str();
					// Match each comma separated value to a l-value
					for input in input.split(',') {
						// Get l-value
						let l_value = match l_values.get(l_value_index) {
							Some(l_value) => l_value,
							None => {
								println!("Too many entries.");
								print!("Redo input: ");
								stdout().flush().unwrap();
								l_value_index = 0;
								continue 'input_loop;
							},
						};
						// Parse value
						let value = match self.get_input_value(input, l_value.type_restriction) {
							Ok(value) => value,
							Err(error) => {
								println!("Entry error: {error}");
								print!("Redo input: ");
								stdout().flush().unwrap();
								l_value_index = 0;
								continue 'input_loop;
							}
						};
						// Assign value
						self.assign_scalar_value(l_value.clone(), value)?;
						// Next value
						l_value_index += 1;
					}
					// If there are no more values to assign then break
					if l_value_index == l_values.len() {
						break;
					}
				}
			}
			StatementOpcode::Run | StatementOpcode::Goto | StatementOpcode::GoSubroutine => {
				// Get the opcode
				let expression_opcode = self.get_expression_opcode(main_struct)?;
				// Get new program counter
				let new_program_counter = match expression_opcode {
					None => 0,
					Some(opcode) => {
						let result = self.execute_expression(main_struct, opcode, TypeRestriction::Integer)?;
						let line_number: Rc<BigInt> = result.try_into()?;
						if self.get_expression_opcode(main_struct)? != None {
							return Err(BasicError::InvalidArgumentCount);
						}
						main_struct.program.get_bytecode_index_from_line_number(&line_number)?
					}
				};
				// The current (not new) program counter should now point to the next statement after this run/goto/gosub statement
				// So save the program counter if we are executing a gosub statement
				// Also push a new subroutine level to the subroutine stack so that loops and if conditions will be shadowed untill we return.
				if opcode == StatementOpcode::GoSubroutine {
					self.current_routine.return_address = self.program_counter;
					self.current_routine.return_is_line_address = self.is_executing_line_program;
					self.routine_stack.push(mem::take(&mut self.current_routine))
				}
				// Jump to new location
				self.program_counter = new_program_counter;
				// We are now executing the main program, not a line program
				self.is_executing_line_program = false;
				// Clear variables and some other stuff if the opcode is Run
				if opcode == StatementOpcode::Run {
					self.clear();
				}
			}
			StatementOpcode::Let => {
				// Get the l-value to assign to
				let l_value = self.execute_l_value(main_struct)?
					.ok_or(BasicError::UnexpectedLValueEndOpcode)?;
				// Get the scalar value to assign to the l-value
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				let scalar_value = self.execute_expression(main_struct, expression_opcode, l_value.type_restriction)?;
				// Assign the scalar value to the l-value
				self.assign_scalar_value(l_value, scalar_value)?;
			}
			StatementOpcode::If => {
				// Get the opcode
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				let expression_result: bool = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Boolean)?
					.try_into()?;
				self.current_routine.if_condition = Some((expression_result, !expression_result));
			}
			StatementOpcode::Then | StatementOpcode::Else => {
				// Get weather then and else statement's sub-statements should be executed or skipped.
				let (execute_then, execute_else) = match &mut self.current_routine.if_condition {
					Some(condition) => condition,
					None => return Err(BasicError::ThenWithoutIf),
				};
				// Get weather the current sub-statement statement should be executed
				let should_execute_sub_statement = match opcode {
					StatementOpcode::Then => *execute_then,
					StatementOpcode::Else => *execute_else,
					_ => unreachable!(),
				};
				// Dont execute the next then statement
				*execute_then = false;
				// Execute or skip the sub-statement
				match should_execute_sub_statement {
					true => out = self.execute_statement(main_struct, should_exist)?,
					false => self.skip_statement(main_struct)?,
				}
			}
			StatementOpcode::For => {
				// Get the l-value to assign to
				let l_value = self.execute_l_value(main_struct)?
					.ok_or(BasicError::UnexpectedLValueEndOpcode)?;
				// Get the scalar value to assign to the l-value
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				let scalar_value = self.execute_expression(main_struct, expression_opcode, l_value.type_restriction)?;
				// Assign the scalar value to the l-value
				self.assign_scalar_value(l_value.clone(), scalar_value)?;
				// Save info about the for loop
				let for_loop = ForLoop::new(*self.get_program_counter(), self.is_executing_line_program);
				self.current_routine.for_loop_counters.insert(l_value.clone(), for_loop);
				self.current_routine.current_for_loop_counter = Ok(l_value);
			}
			StatementOpcode::To | StatementOpcode::Step => {
				// Get the expression opcode
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				// See if there is a for loop to modify
				match self.current_routine.current_for_loop_counter.clone() {
					// If so then modify the for loop
					Ok(current_for_loop) => {
						// Execute the sub-expression to get the value to assign to the for loop
						let value = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
						// Get the loop to modify
						let for_loop = self.current_routine.for_loop_counters.get_mut(&current_for_loop).unwrap();
						// Assign value to the for loop end or step
						match opcode {
							StatementOpcode::To => for_loop.end_value = Some(value),
							StatementOpcode::Step => for_loop.step_value = Some(value),
							_ => unreachable!(),
						}
					}
					// Error if we have not executed a "for" statement since the program start or last gosub call
					Err(false) => return Err(BasicError::ToStepNoForLoop),
					// Skip the sub-expression if we have looped back since the last "for" statement
					Err(true) => self.skip_expression(main_struct, expression_opcode)?,
				}
			}
			StatementOpcode::Next => {
				// Get the l-value to increment
				let l_value = self.execute_l_value(main_struct)?
					.ok_or(BasicError::ExpectedLValue)?;
				let current_value = self.load_scalar_value(main_struct, l_value.clone())?;
				// Get the for loop to loop back on
				let for_loop = self.current_routine.for_loop_counters.get(&l_value)
					.ok_or(BasicError::NextOnLValueWithoutLoop)?;
				// Get the new value
				let increment = for_loop.step_value.clone()
					.unwrap_or(ScalarValue::ONE);
				let new_value = current_value.add_concatenate(increment.clone())?;
				// Get weather we should loop back
				let do_continue = match &for_loop.end_value {
					None => true,
					Some(end_value) => {
						// Get the direction the increment is going in
						let is_decrementing = increment.is_negative()?;
						// Have we reached the end value?
						let do_break = match is_decrementing {
							false => new_value.clone().greater_than(end_value.clone())?,
							true => new_value.clone().less_than(end_value.clone())?,
						};
						// Unwrap bool
						match do_break {
							ScalarValue::Boolean(do_break) => !do_break,
							_ => panic!(),
						}
					}
				};
				// If we should loop back to the start of the loop
				if do_continue {
					let is_in_line_program = for_loop.is_in_line_program;
					let start_bytecode_index = for_loop.start_bytecode_index;
					// Increment counter
					self.assign_scalar_value(l_value, new_value)?;
					// Jump to start of loop
					match is_in_line_program {
						true => self.line_program_counter = start_bytecode_index,
						false => self.program_counter = start_bytecode_index,
					}
				}
			}
			StatementOpcode::Dimension => {
				// Get the l-value with it's dimension lengths
				let l_value = self.execute_l_value(main_struct)?
					.ok_or(BasicError::ExpectedLValue)?;
				// Get dimension lengths
				let scalar_value_lengths = match l_value.arguments_or_indices {
					Some(lengths) => lengths,
					None => return Err(BasicError::ExpectedArrayLValue),
				};
				let mut dimension_lengths = Vec::with_capacity(scalar_value_lengths.len());
				for length in scalar_value_lengths.into_iter() {
					dimension_lengths.push(length.as_length()?);
				}
				// Create array
				self.create_array(l_value.name, l_value.type_restriction, dimension_lengths)?;
			}
			StatementOpcode::List => 'a: {
				// Get the start line index
				let start_expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				let start_expression_result: Option<Rc<BigInt>> = match start_expression_opcode {
					ExpressionOpcode::FromStartOrToEnd => None,
					_ => Some(self.execute_expression(main_struct, start_expression_opcode, TypeRestriction::Integer)?.try_into()?),
				};
				let start_line_number_index = main_struct.program.get_lines_start_index(start_expression_result.clone());
				// Get the end line index
				let end_expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				let end_expression_result: Option<Rc<BigInt>> = match end_expression_opcode {
					ExpressionOpcode::FromStartOrToEnd => None,
					ExpressionOpcode::OneElement => start_expression_result.clone(),
					_ => Some(self.execute_expression(main_struct, end_expression_opcode, TypeRestriction::Integer)?.try_into()?),
				};
				let end_line_number_index = match main_struct.program.get_lines_end_index(end_expression_result) {
					Some(end_line_number_index) => end_line_number_index,
					None => break 'a,
				};
				// List lines
				for line_number_index in start_line_number_index..=end_line_number_index {
					// Get bytecode for line
					let (line_number, line_bytecode) = main_struct.program.get_line_and_number_from_line_index(line_number_index);
					// Decompile line
					let line_parse_tree_elements = match decompile_line(line_bytecode) {
						Ok(line_parse_tree_elements) => line_parse_tree_elements,
						Err(error) => {
							println!("Decompile error on line {line_number}: {error}");
							continue;
						}
					};
					// Get line labels
					let line_labels = main_struct.program.get_line_labels(line_number);
					// Get line comment
					let line_comment = main_struct.program.get_line_comment(line_number)
						.map(|comment| comment.to_string());
					// Deparse line
					let line_tokens = match deparse_line(&line_parse_tree_elements, line_labels, line_comment) {
						Ok(line_parse_tree_elements) => line_parse_tree_elements,
						Err(error) => {
							println!("Deparse error on line {line_number}: {error}");
							continue;
						}
					};
					// Detokenize line
					let line_string = match detokenize_line(&line_tokens) {
						Ok(line_parse_tree_elements) => line_parse_tree_elements,
						Err(error) => {
							println!("Detokenize error on line {line_number}: {error}");
							continue;
						}
					};
					// Print line
					println!("{line_number} {line_string}");
				}
			}
			StatementOpcode::Return => {
				// Remove the current subroutine level and use the one below
				self.current_routine = match self.routine_stack.pop() {
					Some(routine_level) => routine_level,
					None => return Err(BasicError::ReturnWhenNotInSubroutine),
				};
				// Restore the program counter and weather we are in a line program or not from the new top subroutine level
				self.program_counter = self.current_routine.return_address;
				self.is_executing_line_program = self.current_routine.return_is_line_address;
			}
			_ => return Err(BasicError::FeatureNotYetSupported),
		}
		// Continue onto next instruction
		Ok(out)
	}

	fn skip_statement(&mut self, main_struct: &Main) -> Result<(), BasicError> {
		// Get opcode
		let opcode_id = match self.get_program_byte(main_struct) {
			Some(opcode_id) => opcode_id,
			None => return Err(BasicError::ExpectedStatementOpcodeButProgramEnd),
		};
		let opcode: StatementOpcode = FromPrimitive::from_u8(opcode_id)
			.ok_or(BasicError::InvalidStatementOpcode(opcode_id))?;
		// Skip statement arguments
		match opcode {
			// Skip opcodes with no arguments
			StatementOpcode::End | StatementOpcode::Return => {}
			// Skip expressions untill a null opcode is found
			StatementOpcode::Print | StatementOpcode::Run | StatementOpcode::Goto | StatementOpcode::GoSubroutine => loop {
				let expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => break,
				};
				self.skip_expression(main_struct, expression_opcode)?;
			}
			// Skip expressions untill a null opcode is found then skip l-values untill a null opcode is found
			StatementOpcode::Input => {
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					self.skip_expression(main_struct, expression_opcode)?;
				}
				loop {
					match self.skip_l_value(main_struct)? {
						true => {}
						false => break,
					}
				}
			}
			// Skip a l-value and an expression
			StatementOpcode::Let | StatementOpcode::For => {
				// Skip the l-value
				if !self.skip_l_value(main_struct)? {
					return Err(BasicError::UnexpectedLValueEndOpcode);
				}
				// Skip the expression
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				self.skip_expression(main_struct, expression_opcode)?;
			}
			// Skip an expression
			StatementOpcode::If | StatementOpcode::On | StatementOpcode::Step | StatementOpcode::To => {
				// Get the opcode
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				// Skip the expression
				self.skip_expression(main_struct, expression_opcode)?;
			}
			// Skip two expressions
			StatementOpcode::List => {
				// Get the opcode
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				// Skip the expression
				self.skip_expression(main_struct, expression_opcode)?;
				// Get the opcode
				let expression_opcode = self.get_expression_opcode(main_struct)?
					.ok_or(BasicError::InvalidNullStatementOpcode)?;
				// Skip the expression
				self.skip_expression(main_struct, expression_opcode)?;
			}
			// Skip a sub-statement
			StatementOpcode::Then | StatementOpcode::Else => self.skip_statement(main_struct)?,
			// Skip l-value
			StatementOpcode::Next | StatementOpcode::Dimension => {
				self.skip_l_value(main_struct)?;
			}
		}
		// Return that there where no errors
		Ok(())
	}

	/// Sets the value of a scalar variable or an element of an array.
	fn assign_scalar_value(&mut self, l_value: LValue, value: ScalarValue) -> Result<(), BasicError> {
		// Unpack
		let LValue {
			name,
			type_restriction,
			arguments_or_indices
		} = l_value;
		// Check if the l-value is of a array/function or not
		match arguments_or_indices {
			// Assign to scalar variable
			None => {
				self.scalar_variables.insert((name, type_restriction), value);
			}
			// Assign to array index
			Some(arguments_or_indices) => {
				// Get the array to assign to
				let array_identifier = (name, type_restriction, arguments_or_indices.len());
				let (dimension_lengths, elements) = match self.arrays.get_mut(&array_identifier) {
					// If the array exists then get it
					Some(elements) => elements,
					// Else create a default one with 11 elements and get the new array
					None => {
						if arguments_or_indices.len() != 1 {
							return Err(BasicError::ArrayOrFunctionDoesNotExist);
						}
						self.create_array(array_identifier.0.clone(), type_restriction, vec![11])?;
						self.arrays.get_mut(&array_identifier).unwrap()
					}
				};
				// Get indices
				let mut indices = Vec::with_capacity(arguments_or_indices.len());
				for (dimension_index, index) in arguments_or_indices.into_iter().enumerate() {
					indices.push(index.as_index(dimension_lengths[dimension_index])?);
				}
				// Get flat index for the 1D elements vector
				let mut flat_index = 0usize;
				let mut dimension_length = 1usize;
				for (dimension_index, index) in indices.iter().enumerate() {
					flat_index += dimension_length * index;
					dimension_length *= dimension_lengths[dimension_index];
				}
				// Set element
				elements[flat_index] = value;
			}
		}
		Ok(())
	}

	/// Gets the value of a scalar variable or the line number of a label or an element of an array.
	fn load_scalar_value(&mut self, main_struct: &mut Main, l_value: LValue) -> Result<ScalarValue, BasicError> {
		// Unpack
		let LValue {
			name,
			type_restriction,
			arguments_or_indices
		} = l_value;
		// Check if the l-value is of a array/function or not
		Ok(match arguments_or_indices {
			// Read from scalar variable or label or get the default value
			None => {
				let variable_identifier = (name, type_restriction);
				match self.scalar_variables.get(&variable_identifier) {
					// Get the value from the variable if it exists
					Some(value) => value.clone(),

					None => match main_struct.program.get_labels_line(&variable_identifier.0) {
						// Else get the line number of the label if it exists
						Some(value) => ScalarValue::Integer(BasicInteger::BigInteger(Rc::new(value.clone()))).compact(),
						// Else get the default value for the type restriction
						None => type_restriction.default_value(),
					}
				}
			}
			// Read from array index
			Some(arguments_or_indices) => {
				// Get the array to assign to
				let array_identifier = (name, type_restriction, arguments_or_indices.len());
				let (dimension_lengths, elements) = match self.arrays.get(&array_identifier) {
					// If the array exists then get it
					Some(elements) => elements,
					// Else create a default one with 11 elements and get the new array
					None => {
						// TODO: Add support for executing functions
						// Here
						if arguments_or_indices.len() != 1 {
							return Err(BasicError::ArrayOrFunctionDoesNotExist);
						}
						self.create_array(array_identifier.0.clone(), type_restriction, vec![11])?;
						self.arrays.get(&array_identifier).unwrap()
					}
				};
				// Get indices
				let mut indices = Vec::with_capacity(arguments_or_indices.len());
				for (dimension_index, index) in arguments_or_indices.into_iter().enumerate() {
					indices.push(index.as_index(dimension_lengths[dimension_index])?);
				}
				// Get flat index for the 1D elements vector
				let mut flat_index = 0usize;
				let mut dimension_length = 1usize;
				for (dimension_index, index) in indices.iter().enumerate() {
					flat_index += dimension_length * index;
					dimension_length *= dimension_lengths[dimension_index];
				}
				// Get element
				elements[flat_index].clone()
			}
		})
	}

	/// Retrives a l-value from the program and returns:
	/// * `Ok(Some(l-value))` if we get a valid l-value non-zero opcode from the program.
	/// * `Ok(None)` if we get a zero l-value opcode from the program.
	/// * `Err(error)` otherwise.
	fn execute_l_value<'a>(&'a mut self, main_struct: &'a mut Main) -> Result<Option<LValue>, BasicError> {
		// Get opcode
		let opcode_id = self.get_program_byte(main_struct)
			.ok_or(BasicError::ExpectedLValueOpcodeButProgramEnd)?;
		let opcode: LValueOpcode = FromPrimitive::from_u8(opcode_id)
			.ok_or(BasicError::InvalidLValueOpcode(opcode_id))?;
		// Execute opcode
		Ok(match opcode {
			// l-values that do not access an array
			LValueOpcode::ScalarAny | LValueOpcode::ScalarBoolean | LValueOpcode::ScalarComplexFloat | LValueOpcode::ScalarFloat | LValueOpcode::ScalarInteger |
			LValueOpcode::ScalarNumber | LValueOpcode::ScalarRealNumber | LValueOpcode::ScalarString => {
				// Get name
				let name = self.get_program_string(main_struct)?.into();
				// Get type restriction
				let type_restriction = match opcode {
					LValueOpcode::ScalarAny => TypeRestriction::Any,
					LValueOpcode::ScalarBoolean => TypeRestriction::Boolean,
					LValueOpcode::ScalarComplexFloat => TypeRestriction::ComplexFloat,
					LValueOpcode::ScalarFloat => TypeRestriction::Float,
					LValueOpcode::ScalarInteger => TypeRestriction::Integer,
					LValueOpcode::ScalarNumber => TypeRestriction::Number,
					LValueOpcode::ScalarRealNumber => TypeRestriction::RealNumber,
					LValueOpcode::ScalarString => TypeRestriction::String,
					_ => unreachable!(),
				};
				// Construct l-value
				Some(LValue {
					name,
					type_restriction,
					arguments_or_indices: None,
				})
			}
			// l-values that access an array
			LValueOpcode::ArrayElementAny | LValueOpcode::ArrayElementBoolean | LValueOpcode::ArrayElementComplexFloat | LValueOpcode::ArrayElementFloat | LValueOpcode::ArrayElementInteger |
			LValueOpcode::ArrayElementNumber | LValueOpcode::ArrayElementRealNumber | LValueOpcode::ArrayElementString => {
				// Get name
				let name = self.get_program_string(main_struct)?.into();
				// Get type restriction
				let type_restriction = match opcode {
					LValueOpcode::ArrayElementAny => TypeRestriction::Any,
					LValueOpcode::ArrayElementBoolean => TypeRestriction::Boolean,
					LValueOpcode::ArrayElementComplexFloat => TypeRestriction::ComplexFloat,
					LValueOpcode::ArrayElementFloat => TypeRestriction::Float,
					LValueOpcode::ArrayElementInteger => TypeRestriction::Integer,
					LValueOpcode::ArrayElementNumber => TypeRestriction::Number,
					LValueOpcode::ArrayElementRealNumber => TypeRestriction::RealNumber,
					LValueOpcode::ArrayElementString => TypeRestriction::String,
					_ => unreachable!(),
				};
				// Get arguments/indices
				let mut arguments_or_indices = Vec::new();
				loop {
					let argument_opcode = match self.get_expression_opcode(main_struct)? {
						Some(argument_opcode) => argument_opcode,
						None => break,
					};
					arguments_or_indices.push(self.execute_expression(main_struct, argument_opcode, TypeRestriction::Any)?);
				}
				// Construct l-value
				Some(LValue {
					name,
					type_restriction,
					arguments_or_indices: Some(arguments_or_indices.into_boxed_slice()),
				})
			}
			// The end of an l-value list
			LValueOpcode::End => None,
		})
	}

	fn skip_l_value(&mut self, main_struct: &Main) -> Result<bool, BasicError> {
		// Get opcode
		let opcode_id = self.get_program_byte(main_struct)
			.ok_or(BasicError::ExpectedLValueOpcodeButProgramEnd)?;
		let opcode: LValueOpcode = FromPrimitive::from_u8(opcode_id)
			.ok_or(BasicError::InvalidLValueOpcode(opcode_id))?;
		// Skip execution
		Ok(match opcode {
			// l-values that do not access an array
			LValueOpcode::ScalarAny | LValueOpcode::ScalarBoolean | LValueOpcode::ScalarComplexFloat | LValueOpcode::ScalarFloat | LValueOpcode::ScalarInteger |
			LValueOpcode::ScalarNumber | LValueOpcode::ScalarRealNumber | LValueOpcode::ScalarString => {
				// Skip name
				self.skip_program_string(main_struct)?;
				// Was not an end of an l-value list
				true
			},
			// l-values that access an array
			LValueOpcode::ArrayElementAny | LValueOpcode::ArrayElementBoolean | LValueOpcode::ArrayElementComplexFloat | LValueOpcode::ArrayElementFloat | LValueOpcode::ArrayElementInteger |
			LValueOpcode::ArrayElementNumber | LValueOpcode::ArrayElementRealNumber | LValueOpcode::ArrayElementString => {
				// Skip name
				self.skip_program_string(main_struct)?;
				// Skip arguments/indices
				loop {
					let argument_opcode = match self.get_expression_opcode(main_struct)? {
						Some(argument_opcode) => argument_opcode,
						None => break,
					};
					self.skip_expression(main_struct, argument_opcode)?;
				}
				// Was not an end of an l-value list
				true
			}
			// The end of an l-value list
			LValueOpcode::End => false,
		})
	}

	/// Retrives an expression opcode from the program and increments the current program counter. Returns:
	/// * `Ok(Some(opcode))` if we get a valid non-zero opcode from the program.
	/// * `Ok(None)` if we get a zero opcode from the program.
	/// * `Err(error)` otherwise.
	fn get_expression_opcode(&mut self, main_struct: &Main) -> Result<Option<ExpressionOpcode>, BasicError> {
		let opcode_id = self.get_program_byte(main_struct)
			.ok_or(BasicError::ExpectedExpressionOpcodeButProgramEnd)?;
		Ok(match opcode_id {
			0 => None,
			_ => Some(FromPrimitive::from_u8(opcode_id).ok_or(BasicError::InvalidExpressionOpcode(opcode_id))?)
		})
	}

	/// Executes an expression.
	fn execute_expression(&mut self, main_struct: &mut Main, opcode: ExpressionOpcode, return_type_restriction: TypeRestriction) -> Result<ScalarValue, BasicError> {
		// Execute expression
		Ok(match opcode {
			ExpressionOpcode::OneElement | ExpressionOpcode::FromStartOrToEnd => return Err(BasicError::InvalidExpressionOpcode(opcode as u8)),
			ExpressionOpcode::NumericalLiteral => {
				// Get string from program
				let string = self.get_program_string(main_struct)?;
				// Try to convert to complex number
				if string.ends_with('i') {
					let (string_without_i, _) = string.split_at(string.len() - 1);
					if let Ok(value) = string_without_i.parse() {
						return Ok(ScalarValue::ComplexFloat(Complex64::new(0., value)).compact());
					}
					return Err(BasicError::InvalidNumericalLiteral(string.to_string()))
				}
				// Try to convert to integer
				if let Ok(value) = string.parse() {
					return Ok(ScalarValue::Integer(BasicInteger::SmallInteger(value)));
				}
				if let Ok(value) = string.parse() {
					return Ok(ScalarValue::Integer(BasicInteger::BigInteger(Rc::new(value))));
				}
				// Try to convert to float
				if let Ok(value) = string.parse() {
					return Ok(ScalarValue::Float(value));
				}
				// Else error
				return Err(BasicError::InvalidNumericalLiteral(string.to_string()))
			},
			ExpressionOpcode::StringLiteral => {
				let string = self.get_program_string(main_struct)?
					.to_string();
				ScalarValue::String(BasicString::String(Rc::new(string)))
			}
			// Expressions that take in a null terminated list of values such as the sum and product instructions
			ExpressionOpcode::SumConcatenate | ExpressionOpcode::Product => {
				let mut result: Option<ScalarValue> = None;
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					let argument = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
					result = Some(match result {
						Some(some_value) => match opcode {
							ExpressionOpcode::SumConcatenate => some_value.add_concatenate(argument)?,
							ExpressionOpcode::Product => some_value.multiply(argument)?,
							_ => unreachable!(),
						},
						None => argument,
					});
				}
				match result {
					Some(result) => result,
					None => return_type_restriction.default_value(),
				}
			}
			// Expressions that take in 2 arguments
			ExpressionOpcode::EqualTo | ExpressionOpcode::LessThan | ExpressionOpcode::LessThanOrEqualTo | ExpressionOpcode::GreaterThan |
			ExpressionOpcode::GreaterThanOrEqualTo | ExpressionOpcode::NotEqualTo |
			ExpressionOpcode::Subtract | ExpressionOpcode::Divide | ExpressionOpcode::Exponent | ExpressionOpcode::Modulus | ExpressionOpcode::And |
			ExpressionOpcode::ExclusiveOr | ExpressionOpcode::Or | ExpressionOpcode::FlooredDivide => {
				let left_expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				let left_argument = self.execute_expression(main_struct, left_expression_opcode, TypeRestriction::Any)?;
				let right_expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				let right_argument = self.execute_expression(main_struct, right_expression_opcode, TypeRestriction::Any)?;
				match opcode {
					ExpressionOpcode::Subtract => left_argument.subtract(right_argument)?,
					ExpressionOpcode::Divide => left_argument.divide(right_argument)?,
					ExpressionOpcode::FlooredDivide => left_argument.floored_divide(right_argument)?,
					ExpressionOpcode::Exponent => left_argument.power(right_argument)?,
					ExpressionOpcode::And => left_argument.and(right_argument)?,
					ExpressionOpcode::ExclusiveOr => left_argument.xor(right_argument)?,
					ExpressionOpcode::Or => left_argument.or(right_argument)?,
					ExpressionOpcode::Modulus => left_argument.modulus(right_argument)?,
					ExpressionOpcode::EqualTo => ScalarValue::Boolean(left_argument == right_argument),
					ExpressionOpcode::NotEqualTo => ScalarValue::Boolean(left_argument != right_argument),
					ExpressionOpcode::LessThan => left_argument.less_than(right_argument)?,
					ExpressionOpcode::LessThanOrEqualTo => left_argument.less_than_or_equal_to(right_argument)?,
					ExpressionOpcode::GreaterThan => left_argument.greater_than(right_argument)?,
					ExpressionOpcode::GreaterThanOrEqualTo => left_argument.greater_than_or_equal_to(right_argument)?,
					_ => unreachable!(),
				}
			}
			// Expressions that take in 1 argument
			ExpressionOpcode::SquareRoot | ExpressionOpcode::AbsoluteValue | ExpressionOpcode::Arctangent | ExpressionOpcode::Cosine | ExpressionOpcode::Sine | ExpressionOpcode::Tangent |
			ExpressionOpcode::Integer | ExpressionOpcode::Negate | ExpressionOpcode::Not | ExpressionOpcode::Sign | ExpressionOpcode::Exponential => {
				let expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				let argument = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
				match opcode {
					ExpressionOpcode::AbsoluteValue => argument.absolute_value()?,
					ExpressionOpcode::Arctangent => argument.arctangent(return_type_restriction)?,
					ExpressionOpcode::Cosine => argument.cosine(return_type_restriction)?,
					ExpressionOpcode::Sine => argument.sine(return_type_restriction)?,
					ExpressionOpcode::Tangent => argument.tangent(return_type_restriction)?,
					ExpressionOpcode::Integer => argument.integer()?,
					ExpressionOpcode::Negate => argument.negate()?,
					ExpressionOpcode::Not => argument.not()?,
					ExpressionOpcode::SquareRoot => argument.square_root(return_type_restriction)?,
					ExpressionOpcode::Sign => argument.sign(return_type_restriction)?,
					ExpressionOpcode::Exponential => ScalarValue::EULERS_NUMBER.power(argument)?,
					_ => unreachable!(),
				}
			}
			ExpressionOpcode::Random => {
				let first_expression_opcode = self.get_expression_opcode(main_struct)?;
				match first_expression_opcode {
					Some(first_expression_opcode) => {
						let first_argument = self.execute_expression(main_struct, first_expression_opcode, TypeRestriction::Any)?;
						let second_expression_opcode = self.get_expression_opcode(main_struct)?;
						match second_expression_opcode {
							Some(second_expression_opcode) => {
								let second_argument = self.execute_expression(main_struct, second_expression_opcode, TypeRestriction::Any)?;
								if self.get_expression_opcode(main_struct)? != None {
									return Err(BasicError::InvalidArgumentCount);
								}
								ScalarValue::get_random(Some(first_argument), Some(second_argument), return_type_restriction)?
							}
							None => ScalarValue::get_random(None, Some(first_argument), return_type_restriction)?,
						}
					}
					None => ScalarValue::get_random(None, None, return_type_restriction)?,
				}
			}
			ExpressionOpcode::Logarithm => {
				let first_expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidArgumentCount),
				};
				let first_argument = self.execute_expression(main_struct, first_expression_opcode, return_type_restriction)?;
				match self.get_expression_opcode(main_struct)? {
					Some(second_expression_opcode) => {
						let second_argument = self.execute_expression(main_struct, second_expression_opcode, return_type_restriction)?;
						if self.get_expression_opcode(main_struct)? != None {
							return Err(BasicError::InvalidArgumentCount);
						}
						second_argument.logarithm(first_argument, return_type_restriction)?
					}
					None => first_argument.logarithm(ScalarValue::EULERS_NUMBER, return_type_restriction)?,
				}
			}
			// Constants
			ExpressionOpcode::True | ExpressionOpcode::False | ExpressionOpcode::Pi | ExpressionOpcode::EulersNumber | ExpressionOpcode::ImaginaryUnit |
			ExpressionOpcode::Space | ExpressionOpcode::NewLine => {
				match opcode {
					ExpressionOpcode::False => ScalarValue::FALSE,
					ExpressionOpcode::True => ScalarValue::TRUE,
					ExpressionOpcode::Pi => ScalarValue::PI,
					ExpressionOpcode::EulersNumber => ScalarValue::EULERS_NUMBER,
					ExpressionOpcode::ImaginaryUnit => ScalarValue::IMAGINARY_UNIT,
					ExpressionOpcode::Space => ScalarValue::SPACE,
					ExpressionOpcode::NewLine => ScalarValue::NEW_LINE,
					_ => unreachable!(),
				}
			}
			// Type restrictions
			ExpressionOpcode::GetBoolean | ExpressionOpcode::GetComplexFloat | ExpressionOpcode::GetFloat | ExpressionOpcode::GetInteger |
			ExpressionOpcode::GetNumber | ExpressionOpcode::GetRealNumber | ExpressionOpcode::GetString => {
				// Get type restriction
				let type_restriction_for_argument = match opcode {
					ExpressionOpcode::GetBoolean => TypeRestriction::Boolean,
					ExpressionOpcode::GetComplexFloat => TypeRestriction::ComplexFloat,
					ExpressionOpcode::GetFloat => TypeRestriction::Float,
					ExpressionOpcode::GetInteger => TypeRestriction::Integer,
					ExpressionOpcode::GetNumber => TypeRestriction::Number,
					ExpressionOpcode::GetRealNumber => TypeRestriction::RealNumber,
					ExpressionOpcode::GetString => TypeRestriction::String,
					_ => unreachable!(),
				};
				// Get opcode for the expression the type restriction is for
				let expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				// Execute the expression with the type restriction
				let out = self.execute_expression(main_struct, expression_opcode, type_restriction_for_argument)?;
				match out.conforms_to_type_restriction(type_restriction_for_argument) {
					true => out,
					false => return Err(BasicError::TypeMismatch(out, type_restriction_for_argument)),
				}
			}
			// Variable reads
			ExpressionOpcode::LoadScalarAny | ExpressionOpcode::LoadScalarBoolean | ExpressionOpcode::LoadScalarComplexFloat | ExpressionOpcode::LoadScalarFloat | ExpressionOpcode::LoadScalarInteger |
			ExpressionOpcode::LoadScalarNumber | ExpressionOpcode::LoadScalarRealNumber | ExpressionOpcode::LoadScalarString => {
				// Get type restriction
				let type_restriction = match opcode {
					ExpressionOpcode::LoadScalarAny => TypeRestriction::Any,
					ExpressionOpcode::LoadScalarBoolean => TypeRestriction::Boolean,
					ExpressionOpcode::LoadScalarComplexFloat => TypeRestriction::ComplexFloat,
					ExpressionOpcode::LoadScalarFloat => TypeRestriction::Float,
					ExpressionOpcode::LoadScalarInteger => TypeRestriction::Integer,
					ExpressionOpcode::LoadScalarNumber => TypeRestriction::Number,
					ExpressionOpcode::LoadScalarRealNumber => TypeRestriction::RealNumber,
					ExpressionOpcode::LoadScalarString => TypeRestriction::String,
					_ => unreachable!(),
				};
				// Get name
				let name = self.get_program_string(main_struct)?.into();
				// Load value
				self.load_scalar_value(main_struct, LValue {
					name,
					type_restriction,
					arguments_or_indices: None
				})?
			}
			ExpressionOpcode::CallUserFunctionOrGetArrayValueAny | ExpressionOpcode::CallUserFunctionOrGetArrayValueBoolean |
			ExpressionOpcode::CallUserFunctionOrGetArrayValueComplexFloat | ExpressionOpcode::CallUserFunctionOrGetArrayValueFloat |
			ExpressionOpcode::CallUserFunctionOrGetArrayValueInteger | ExpressionOpcode::CallUserFunctionOrGetArrayValueNumber |
			ExpressionOpcode::CallUserFunctionOrGetArrayValueRealNumber | ExpressionOpcode::CallUserFunctionOrGetArrayValueString => {
				// Get type restriction
				let type_restriction = match opcode {
					ExpressionOpcode::CallUserFunctionOrGetArrayValueAny => TypeRestriction::Any,
					ExpressionOpcode::CallUserFunctionOrGetArrayValueBoolean => TypeRestriction::Boolean,
					ExpressionOpcode::CallUserFunctionOrGetArrayValueComplexFloat => TypeRestriction::ComplexFloat,
					ExpressionOpcode::CallUserFunctionOrGetArrayValueFloat => TypeRestriction::Float,
					ExpressionOpcode::CallUserFunctionOrGetArrayValueInteger => TypeRestriction::Integer,
					ExpressionOpcode::CallUserFunctionOrGetArrayValueNumber => TypeRestriction::Number,
					ExpressionOpcode::CallUserFunctionOrGetArrayValueRealNumber => TypeRestriction::RealNumber,
					ExpressionOpcode::CallUserFunctionOrGetArrayValueString => TypeRestriction::String,
					_ => unreachable!(),
				};
				// Get name
				let name = self.get_program_string(main_struct)?.into();
				// Get arguments
				let mut arguments_or_indices = Vec::new();
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					let argument = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
					arguments_or_indices.push(argument);
				}
				// Load value
				self.load_scalar_value(main_struct, LValue {
					name,
					type_restriction,
					arguments_or_indices: Some(arguments_or_indices.into_boxed_slice()),
				})?
			}
		})
	}

	fn skip_expression(&mut self, main_struct: &Main, opcode: ExpressionOpcode) -> Result<(), BasicError> {
		// Execute expression
		match opcode {
			// Skip a string
			ExpressionOpcode::NumericalLiteral | ExpressionOpcode::StringLiteral |
			ExpressionOpcode::LoadScalarAny | ExpressionOpcode::LoadScalarBoolean | ExpressionOpcode::LoadScalarComplexFloat | ExpressionOpcode::LoadScalarFloat | ExpressionOpcode::LoadScalarInteger |
			ExpressionOpcode::LoadScalarNumber | ExpressionOpcode::LoadScalarRealNumber | ExpressionOpcode::LoadScalarString => self.skip_program_string(main_struct)?,
			// Skip expressions until a null opcode is found
			ExpressionOpcode::SumConcatenate | ExpressionOpcode::Product | ExpressionOpcode::Random | ExpressionOpcode::Logarithm => loop {
				let expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => break,
				};
				self.skip_expression(main_struct, expression_opcode)?;
			}
			// Skip 2 arguments
			ExpressionOpcode::EqualTo | ExpressionOpcode::LessThan | ExpressionOpcode::LessThanOrEqualTo | ExpressionOpcode::GreaterThan |
			ExpressionOpcode::GreaterThanOrEqualTo | ExpressionOpcode::NotEqualTo |
			ExpressionOpcode::Subtract | ExpressionOpcode::Divide | ExpressionOpcode::Exponent | ExpressionOpcode::Modulus | ExpressionOpcode::And |
			ExpressionOpcode::ExclusiveOr | ExpressionOpcode::Or | ExpressionOpcode::FlooredDivide => {
				let left_expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				self.skip_expression(main_struct, left_expression_opcode)?;
				let right_expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				self.skip_expression(main_struct, right_expression_opcode)?;
			}
			// Expressions that take in 1 argument
			ExpressionOpcode::AbsoluteValue | ExpressionOpcode::Arctangent | ExpressionOpcode::Cosine | ExpressionOpcode::Sine | ExpressionOpcode::Tangent |
			ExpressionOpcode::Integer | ExpressionOpcode::Negate | ExpressionOpcode::Not | ExpressionOpcode::SquareRoot | ExpressionOpcode::Sign |
			ExpressionOpcode::GetBoolean | ExpressionOpcode::GetComplexFloat | ExpressionOpcode::GetFloat | ExpressionOpcode::GetInteger |
			ExpressionOpcode::GetNumber | ExpressionOpcode::GetRealNumber | ExpressionOpcode::GetString | ExpressionOpcode::Exponential => {
				let expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				self.skip_expression(main_struct, expression_opcode)?;
			}
			// Skip nothing
			ExpressionOpcode::True | ExpressionOpcode::False | ExpressionOpcode::Pi | ExpressionOpcode::EulersNumber | ExpressionOpcode::OneElement |
			ExpressionOpcode::ImaginaryUnit | ExpressionOpcode::NewLine | ExpressionOpcode::Space | ExpressionOpcode::FromStartOrToEnd => {}

			// Skip string then null terminated expression list.
			ExpressionOpcode::CallUserFunctionOrGetArrayValueAny | ExpressionOpcode::CallUserFunctionOrGetArrayValueBoolean |
			ExpressionOpcode::CallUserFunctionOrGetArrayValueComplexFloat | ExpressionOpcode::CallUserFunctionOrGetArrayValueFloat | ExpressionOpcode::CallUserFunctionOrGetArrayValueInteger |
			ExpressionOpcode::CallUserFunctionOrGetArrayValueRealNumber | ExpressionOpcode::CallUserFunctionOrGetArrayValueNumber | ExpressionOpcode::CallUserFunctionOrGetArrayValueString => {
				self.skip_program_string(main_struct)?;
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					self.skip_expression(main_struct, expression_opcode)?;
				}
			}
		}
		Ok(())
	}

	/// Executes the program untill it stops.
	#[inline(always)]
	fn execute(&mut self, main_struct: &mut Main) {
		// Execute instructions
		loop {
			let instruction_result = self.execute_statement(main_struct, false);
			match instruction_result {
				Err(error) if !self.is_executing_line_program => {
					let line = main_struct.program.get_line_number_bytecode_is_in(self.program_counter);
					println!("Runtime error on line {line}: {error}");
					break;
				}
				Err(error) => {
					println!("Runtime error: {error}");
					break;
				}
				Ok(InstructionExecutionSuccessResult::ContinueToNextInstruction) => {}
				Ok(InstructionExecutionSuccessResult::ProgramEnd) => {
					println!("Program ended.");
					break;
				}
				Ok(InstructionExecutionSuccessResult::ProgramStopped) => {
					println!("Program stopped.");
					break;
				}
			}
		}
	}

	/// Executes the line program and jumps to the main program if a run, goto, ... is entered.
	#[inline(always)]
	pub fn execute_line(&mut self, main_struct: &mut Main) {
		self.is_executing_line_program = true;
		self.line_program_counter = 0;
		self.execute(main_struct);
	}
}