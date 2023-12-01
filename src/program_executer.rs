use std::rc::Rc;

use num::BigInt;
use num_traits::FromPrimitive;

use crate::{Main, error::BasicError, bytecode::{statement_opcode::StatementOpcode, expression_opcode::ExpressionOpcode}, lexer::type_restriction::TypeRestriction, scalar_value::{scalar_value::ScalarValue, integer::BasicInteger, string::BasicString}};

pub struct ProgramExecuter {
	program_counter: usize,
	line_program_counter: usize,
	//continue_counter: Option<usize>,
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
			//continue_counter: None,
			is_executing_line_program: false,
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

	/// Retrives a byte from the program and increments the current program counter. Or returns None if the end of the program has been reached.
	fn get_program_byte(&mut self, main_struct: &mut Main) -> Option<u8> {
		main_struct.program.get_byte(self.is_executing_line_program, self.get_program_counter())
	}

	/// Retrives a string from the program and increments the current program counter.
	fn get_program_string<'a>(&'a mut self, main_struct: &'a mut Main) -> Result<&str, BasicError> {
		main_struct.program.get_string(self.is_executing_line_program, self.get_program_counter())
	}

	/// Executes a single statement.
	fn execute_statement(&mut self, main_struct: &mut Main) -> Result<InstructionExecutionSuccessResult, BasicError> {
		// Get opcode
		let opcode_id = match self.get_program_byte(main_struct) {
			Some(opcode_id) => opcode_id,
			None => {
				return Ok(InstructionExecutionSuccessResult::ProgramEnd)
			},
		};
		let opcode: StatementOpcode = FromPrimitive::from_u8(opcode_id)
			.ok_or(BasicError::InvalidStatementOpcode(opcode_id))?;
		// Execute statment instruction
		match opcode {
			StatementOpcode::End => return Ok(InstructionExecutionSuccessResult::ProgramStopped),
			StatementOpcode::Print => {
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					let result = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
					print!("{result}");
				}
				println!();
			}
			StatementOpcode::Run => {
				let expression_opcode = self.get_expression_opcode(main_struct)?;
				let new_program_counter = match expression_opcode {
					None => 0,
					Some(opcode) => {
						let result = self.execute_expression(main_struct, opcode, TypeRestriction::Integer)?;
						let line_number: Rc<BigInt> = result.try_into()?;
						main_struct.program.get_bytecode_index_from_line_number(&line_number)?
					}
				};
				self.program_counter = new_program_counter;
				self.is_executing_line_program = false;
			}
			StatementOpcode::Goto => {
				let expression_opcode = self.get_expression_opcode(main_struct)?;
				let new_program_counter = match expression_opcode {
					None => 0,
					Some(opcode) => {
						let result = self.execute_expression(main_struct, opcode, TypeRestriction::Integer)?;
						let line_number: Rc<BigInt> = result.try_into()?;
						main_struct.program.get_bytecode_index_from_line_number(&line_number)?
					}
				};
				self.program_counter = new_program_counter;
				self.is_executing_line_program = false;
			}
			_ => return Err(BasicError::FeatureNotYetSupported),
		}
		// Continue onto next instruction
		Ok(InstructionExecutionSuccessResult::ContinueToNextInstruction)
	}

	/// Retrives an expression opcode from the program and increments the current program counter. Returns:
	/// * `Ok(Some(opcode))` if we get a valid non-zero opcode from the program.
	/// * `Ok(None)` if we a zero opcode from the program.
	/// * `Err(error)` otherwise.
	fn get_expression_opcode(&mut self, main_struct: &mut Main) -> Result<Option<ExpressionOpcode>, BasicError> {
		let opcode_id = self.get_program_byte(main_struct)
			.ok_or(BasicError::ExpectedFunctionOpcodeButEnd)?;
		Ok(match opcode_id {
			0 => None,
			_ => Some(FromPrimitive::from_u8(opcode_id).ok_or(BasicError::InvalidExpressionOpcode(opcode_id))?)
		})
	}

	/// Executes an expression.
	fn execute_expression(&mut self, main_struct: &mut Main, opcode: ExpressionOpcode, return_type_restriction: TypeRestriction) -> Result<ScalarValue, BasicError> {
		// Execute function
		let out = match opcode {
			ExpressionOpcode::NumericalLiteral => {
				// TODO: Better number parsing
				let string = self.get_program_string(main_struct)?;
				let number = string.parse()
					.map_err(|_| BasicError::InvalidNumericalLiteral(string.to_string()))?;
				ScalarValue::Integer(BasicInteger::BigInteger(Rc::new(number)).compact())
			},
			ExpressionOpcode::StringLiteral => {
				let string = self.get_program_string(main_struct)?
					.to_string();
				ScalarValue::String(BasicString::String(Rc::new(string)))
			}
			ExpressionOpcode::SumConcatenate | ExpressionOpcode::Subtract | ExpressionOpcode::Product | ExpressionOpcode::Divide | ExpressionOpcode::Exponent | ExpressionOpcode::Modulus |
			ExpressionOpcode::And | ExpressionOpcode::ExclusiveOr | ExpressionOpcode::Or => {
				let mut result: Option<ScalarValue> = None;
				loop {
					let expression_opcode = match self.get_expression_opcode(main_struct)? {
						Some(expression_opcode) => expression_opcode,
						None => break,
					};
					let argument = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
					result = Some(match result {
						Some(sum_value) => match opcode {
							ExpressionOpcode::SumConcatenate => sum_value.add_concatenate(argument, return_type_restriction)?,
							ExpressionOpcode::Subtract => sum_value.sub(argument, return_type_restriction)?,
							ExpressionOpcode::Product => sum_value.mul(argument, return_type_restriction)?,
							ExpressionOpcode::Divide => sum_value.div(argument, return_type_restriction)?,
							ExpressionOpcode::Exponent => sum_value.pow(argument, return_type_restriction)?,
							ExpressionOpcode::And => sum_value.and(argument, return_type_restriction)?,
							ExpressionOpcode::ExclusiveOr => sum_value.xor(argument, return_type_restriction)?,
							ExpressionOpcode::Or => sum_value.or(argument, return_type_restriction)?,
							ExpressionOpcode::Modulus => sum_value.modulus(argument, return_type_restriction)?,
							_ => unreachable!(),
						},
						None => argument,
					});
				}
				match result {
					Some(sum_value) => sum_value,
					None => return_type_restriction.default_value(),
				}
			}
			ExpressionOpcode::AbsoluteValue | ExpressionOpcode::Arctangent | ExpressionOpcode::Cosine | ExpressionOpcode::Sine | ExpressionOpcode::Tangent | ExpressionOpcode::Random |
			ExpressionOpcode::Integer | ExpressionOpcode::Logarithm | ExpressionOpcode::Negate | ExpressionOpcode::Not | ExpressionOpcode::SquareRoot | ExpressionOpcode::Sign => {
				let expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				let argument = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
				match opcode {
					ExpressionOpcode::AbsoluteValue => argument.abs(return_type_restriction)?,
					ExpressionOpcode::Arctangent => argument.atan(return_type_restriction)?,
					ExpressionOpcode::Cosine => argument.cos(return_type_restriction)?,
					ExpressionOpcode::Sine => argument.sin(return_type_restriction)?,
					ExpressionOpcode::Tangent => argument.tan(return_type_restriction)?,
					ExpressionOpcode::Random => argument.random(return_type_restriction)?,
					ExpressionOpcode::Integer => argument.integer(return_type_restriction)?,
					ExpressionOpcode::Logarithm => argument.log(return_type_restriction)?,
					ExpressionOpcode::Negate => argument.neg(return_type_restriction)?,
					ExpressionOpcode::Not => argument.not(return_type_restriction)?,
					ExpressionOpcode::SquareRoot => argument.sqrt(return_type_restriction)?,
					ExpressionOpcode::Sign => argument.sign(return_type_restriction)?,
					_ => unreachable!(),
				}
			}
			_ => return Err(BasicError::FeatureNotYetSupported),
		};
		Ok(out)
	}

	/// Executes the program untill it stops.
	#[inline(always)]
	fn execute(&mut self, main_struct: &mut Main) {
		// Execute instructions
		loop {
			let instruction_result = self.execute_statement(main_struct);
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