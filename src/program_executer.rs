use num::BigInt;
use num_traits::FromPrimitive;

use crate::{program::Program, Main, error::BasicError, bytecode::Bytecode};

pub struct ProgramExecuter {
	program_counter: usize,
	line_program_counter: usize,
	continue_line: Option<BigInt>,
	is_executing_line_program: bool,
}

#[derive(Clone, Copy)]
#[repr(u8)]
enum InstructionExecutionSuccessResult {
	ContinueToNextInstruction,
	ProgramEnd,
	ProgramStopped,
}

impl ProgramExecuter {
	/// Create a program executer in the deafult state.
	pub fn new() -> Self {
		ProgramExecuter {
			program_counter: 0,
			line_program_counter: 0,
			continue_line: None,
			is_executing_line_program: false,
		}
	}

	/// Gets a mutable reference to the current program counter (main or line).
	fn get_program_counter(&mut self) -> &mut usize {
		match self.is_executing_line_program {
			true => &mut self.line_program_counter,
			false => &mut self.program_counter,
		}
	}

	/// Retrives a byte from the program and increments the current program counter. Or returns None if the end of the program has been reached.
	fn get_program_byte(&mut self, main_struct: &mut Main) -> Option<u8> {
		main_struct.program.get_byte(self.is_executing_line_program, self.get_program_counter())
	}

	/// Retrives a string from the program and increments the current program counter.
	fn get_program_string<'a>(&'a mut self, main_struct: &'a mut Main) -> Result<&str, BasicError> {
		main_struct.program.get_string(self.is_executing_line_program, self.get_program_counter())
	}

	/// Executes a single instruction.
	fn execute_instruction(&mut self, main_struct: &mut Main) -> Result<InstructionExecutionSuccessResult, BasicError> {
		// Get opcode
		let opcode_id = match self.get_program_byte(main_struct) {
			Some(opcode_id) => opcode_id,
			None => return Ok(InstructionExecutionSuccessResult::ProgramEnd),
		};
		let opcode: Option<Bytecode> = FromPrimitive::from_u8(opcode_id);
		let opcode = match opcode {
			Some(opcode) => opcode,
			None => return Err(BasicError::InvalidOpcode(opcode_id)),
		};
		// Execute instruction
		match opcode {
			Bytecode::End => return Ok(InstructionExecutionSuccessResult::ProgramStopped),
			_ => todo!(),
		}
		// Continue onto next instruction
		Ok(InstructionExecutionSuccessResult::ContinueToNextInstruction)
	}

	/// Executes the program untill it stops.
	fn execute(&mut self, main_struct: &mut Main) {
		// Execute instructions
		loop {
			let instruction_result = self.execute_instruction(main_struct);
			match instruction_result {
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
	pub fn execute_line(&mut self, main_struct: &mut Main) {
		self.is_executing_line_program = true;
		self.line_program_counter = 0;
		self.execute(main_struct);
	}
}