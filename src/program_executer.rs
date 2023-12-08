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
		Ok(match opcode {
			ExpressionOpcode::NumericalLiteral => {
				// TODO: Better number parsing
				let string = self.get_program_string(main_struct)?;
				/*let number = string.parse()
					.map_err(|_| BasicError::InvalidNumericalLiteral(string.to_string()))?;
				ScalarValue::Integer(BasicInteger::BigInteger(Rc::new(number)).compact())*/
				if let Ok(value) = string.parse() {
					return Ok(ScalarValue::Integer(BasicInteger::SmallInteger(value)));
				}
				if let Ok(value) = string.parse() {
					return Ok(ScalarValue::Integer(BasicInteger::BigInteger(Rc::new(value))));
				}
				if let Ok(value) = string.parse() {
					return Ok(ScalarValue::Float(value));
				}
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
					ExpressionOpcode::EqualTo => left_argument.equal_to(right_argument)?,
					ExpressionOpcode::NotEqualTo => left_argument.not_equal_to(right_argument)?,
					ExpressionOpcode::LessThan => left_argument.less_than(right_argument)?,
					ExpressionOpcode::LessThanOrEqualTo => left_argument.less_than_or_equal_to(right_argument)?,
					ExpressionOpcode::GreaterThan => left_argument.greater_than(right_argument)?,
					ExpressionOpcode::GreaterThanOrEqualTo => left_argument.greater_than_or_equal_to(right_argument)?,
					_ => unreachable!(),
				}
			}
			// Expressions that take in 1 argument
			ExpressionOpcode::AbsoluteValue | ExpressionOpcode::Arctangent | ExpressionOpcode::Cosine | ExpressionOpcode::Sine | ExpressionOpcode::Tangent |
			ExpressionOpcode::Integer | ExpressionOpcode::Negate | ExpressionOpcode::Not | ExpressionOpcode::SquareRoot | ExpressionOpcode::Sign => {
				let expression_opcode = match self.get_expression_opcode(main_struct)? {
					Some(expression_opcode) => expression_opcode,
					None => return Err(BasicError::InvalidNullStatementOpcode),
				};
				let argument = self.execute_expression(main_struct, expression_opcode, TypeRestriction::Any)?;
				match opcode {
					ExpressionOpcode::AbsoluteValue => argument.absolute_value(return_type_restriction)?,
					ExpressionOpcode::Arctangent => argument.arctangent(return_type_restriction)?,
					ExpressionOpcode::Cosine => argument.cosine(return_type_restriction)?,
					ExpressionOpcode::Sine => argument.sine(return_type_restriction)?,
					ExpressionOpcode::Tangent => argument.tangent(return_type_restriction)?,
					ExpressionOpcode::Integer => argument.integer(return_type_restriction)?,
					ExpressionOpcode::Negate => argument.negate()?,
					ExpressionOpcode::Not => argument.not()?,
					ExpressionOpcode::SquareRoot => argument.square_root(return_type_restriction)?,
					ExpressionOpcode::Sign => argument.sign(return_type_restriction)?,
					_ => unreachable!(),
				}
			}
			ExpressionOpcode::Random => {
				let first_expression_opcode = self.get_expression_opcode(main_struct)?;
				match first_expression_opcode {
					Some(first_expression_opcode) => {
						let argument = self.execute_expression(main_struct, first_expression_opcode, TypeRestriction::Any)?;
						if self.get_expression_opcode(main_struct)? != None {
							return Err(BasicError::InvalidArgumentCount);
						}
						argument.random_1_argument(return_type_restriction)?
					}
					None => ScalarValue::get_random_no_arguments(return_type_restriction)?,
				}
			}
			ExpressionOpcode::Logarithm => {
				return Err(BasicError::FeatureNotYetSupported);
			}
			// Constants
			ExpressionOpcode::True | ExpressionOpcode::False | ExpressionOpcode::Pi | ExpressionOpcode::EulersNumber | ExpressionOpcode::ImaginaryUnit => {
				match opcode {
					ExpressionOpcode::False => ScalarValue::false_value(),
					ExpressionOpcode::True => ScalarValue::true_value(),
					ExpressionOpcode::Pi => ScalarValue::pi(),
					ExpressionOpcode::EulersNumber => ScalarValue::eulers_number(),
					ExpressionOpcode::ImaginaryUnit => ScalarValue::imaginary_unit(),
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
			_ => return Err(BasicError::FeatureNotYetSupported),
		})
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