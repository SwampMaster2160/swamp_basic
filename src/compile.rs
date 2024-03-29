use std::str::from_utf8;

use num_traits::FromPrimitive;

use crate::{
	bytecode::{expression_opcode::ExpressionOpcode, l_value_opcode::LValueOpcode, statement_opcode::StatementOpcode},
	error::BasicError,
	lexer::{built_in_function::BuiltInFunction, command::Command, operator::Operator, separator::Separator, type_restriction::TypeRestriction},
	parser::ParseTreeElement,
};

/// Compiles a list of parse tree elements to bytecode
#[inline(always)]
pub fn compile_parse_tree_elements_to_bytecode(parse_tree_elements: &[ParseTreeElement]) -> Result<Vec<u8>, BasicError> {
	let mut compiled_bytecode = Vec::new();
	// Compile each statement
	for parse_tree_element in parse_tree_elements {
		compiled_bytecode.extend(compile_statement(parse_tree_element)?);
	}
	// Return
	Ok(compiled_bytecode)
}

/// Compiles a single parse tree element that is a statement to bytecode
fn compile_statement(parse_tree_element: &ParseTreeElement) -> Result<Vec<u8>, BasicError> {
	let mut out = Vec::new();
	// Tree element must be a statement
	debug_assert!(parse_tree_element.is_statement());

	match parse_tree_element {
		ParseTreeElement::Command(command, arguments) => match command {
			// Commands that take in nothing
			Command::End | Command::Stop | Command::Return | Command::Continue | Command::Randomize => out.push(match command {
				Command::End => StatementOpcode::End,
				Command::Return => StatementOpcode::Return,
				Command::Stop => StatementOpcode::Stop,
				Command::Continue => StatementOpcode::Continue,
				Command::Randomize => StatementOpcode::Randomize,
				_ => unreachable!(),
			} as u8),
			// Print
			Command::Print => {
				// Push opcode
				out.push(StatementOpcode::Print as u8);
				// Push bytecode for arguments
				for argument in arguments {
					match argument {
						ParseTreeElement::ExpressionSeparator(separator) => match separator {
							Separator::Semicolon => {}
							Separator::Comma => out.push(ExpressionOpcode::Space as u8),
							_ => panic!(),
						}
						_ => out.extend(compile_expression(argument)?),
					}
				}
				// Push a newline opcode if the last argument is not a separator
				match arguments.last() {
					Some(ParseTreeElement::ExpressionSeparator(Separator::Semicolon | Separator::Comma)) => {}
					_ => out.push(ExpressionOpcode::NewLine as u8),
				}
				// Terminate
				out.push(StatementOpcode::End as u8);
			}
			// Input
			Command::Input => {
				// Get the index where we should split the arguments to print from thoes to be inputs
				let last_semicolon_index = arguments.iter()
					.enumerate()
					.rev()
					.find(|(_, tree)| matches!(tree, ParseTreeElement::ExpressionSeparator(Separator::Semicolon)))
					.map(|(index, _)| index);
				let input_arguments_start_index = match last_semicolon_index {
					Some(index) => index + 1,
					None => 0,
				};
				// Push opcode
				out.push(StatementOpcode::Input as u8);
				// Push bytecode for arguments that will be printed
				for argument in arguments.iter().take(input_arguments_start_index) {
					match argument {
						ParseTreeElement::ExpressionSeparator(separator) => match separator {
							Separator::Semicolon => {}
							Separator::Comma => out.push(ExpressionOpcode::Space as u8),
							_ => panic!(),
						}
						_ => out.extend(compile_expression(argument)?),
					}
				}
				// Terminate the arguments that will be printed
				out.push(StatementOpcode::End as u8);
				// Push bytecode for arguments that will be inputs
				for argument in arguments.iter().skip(input_arguments_start_index) {
					match argument {
						ParseTreeElement::ExpressionSeparator(separator) => match separator {
							Separator::Semicolon => panic!("Should not have semicolons here."),
							Separator::Comma => {},
							_ => panic!(),
						}
						_ => {
							out.extend(compile_l_value(argument)?)
						},
					}
				}
				// Terminate the arguments that will be inputs
				out.push(StatementOpcode::End as u8);
			}
			// Commands that take in a list of expressions and ignore commas and semicolons
			Command::Goto | Command::Run | Command::GoSubroutine | Command::Load | Command::Save | Command::Data | Command::Restore | Command::Base => {
				// Push the respective opcode
				out.push(match command {
					Command::Goto => StatementOpcode::Goto,
					Command::Run => StatementOpcode::Run,
					Command::GoSubroutine => StatementOpcode::GoSubroutine,
					Command::Load => StatementOpcode::Load,
					Command::Save => StatementOpcode::Save,
					Command::Data => StatementOpcode::Data,
					Command::Restore => StatementOpcode::Restore,
					Command::Base => StatementOpcode::OptionBase,
					_ => unreachable!(),
				} as u8);
				// Push arguments
				for argument in arguments {
					match argument {
						ParseTreeElement::ExpressionSeparator(separator) => {
							if *separator != Separator::Comma && *separator != Separator::Semicolon {
								return Err(BasicError::InvalidSeparator(*separator));
							}
						}
						_ => {
							debug_assert!(argument.is_expression());
							out.extend(compile_expression(argument)?);
						}
					}
				}
				// Null terminate
				out.push(StatementOpcode::End as u8);
			}
			// Commands that take in a list of l-values and ignore commas and semicolons
			Command::Read => {
				// Push the respective opcode
				out.push(match command {
					Command::Read => StatementOpcode::Read,
					_ => unreachable!(),
				} as u8);
				// Push arguments
				for argument in arguments {
					match argument {
						ParseTreeElement::ExpressionSeparator(separator) => {
							if *separator != Separator::Comma && *separator != Separator::Semicolon {
								return Err(BasicError::InvalidSeparator(*separator));
							}
						}
						_ => out.extend(compile_l_value(argument)?),
					}
				}
				// Null terminate
				out.push(StatementOpcode::End as u8);
			}
			// Commands that take in a sub-expression
			Command::Then | Command::Else => {
				// Push opcode
				out.push(match command {
					Command::Then => StatementOpcode::Then,
					Command::Else => StatementOpcode::Else,
					_ => unreachable!(),
				} as u8);
				// Push sub-statement
				let sub_statement = match arguments.as_slice() {
					[sub_statement] => sub_statement,
					_ => return Err(BasicError::InvalidArgumentCount),
				};
				out.extend(compile_statement(sub_statement)?);
			}
			// Commands that take in a single expression
			Command::If | Command::On | Command::Step | Command::To => {
				// Push opcode
				out.push(match command {
					Command::If => StatementOpcode::If,
					Command::On => StatementOpcode::On,
					Command::Step => StatementOpcode::Step,
					Command::To => StatementOpcode::To,
					_ => unreachable!(),
				} as u8);
				// Push expression bytecode
				let expression = match arguments.as_slice() {
					[sub_statement] => sub_statement,
					_ => return Err(BasicError::InvalidArgumentCount),
				};
				out.extend(compile_expression(expression)?);
			}
			// Commands that take in an assignment
			Command::For => {
				// Get values for assignment
				let (l_value, r_value) = match &arguments[0] {
					ParseTreeElement::Assignment(l_value, r_value) => (l_value, r_value),
					_ => panic!()
				};
				// Push expression bytecode
				out.push(StatementOpcode::For as u8);
				out.extend(compile_l_value(&l_value)?);
				out.extend(compile_expression(&r_value)?);
			}
			// Commands that take in a l-value
			Command::Next | Command::Dimension => {
				let statement_opcode = match command {
					Command::Next => StatementOpcode::Next,
					Command::Dimension => StatementOpcode::Dimension,
					_ => unreachable!(),
				};
				// Push bytecode
				out.push(statement_opcode as u8);
				out.extend(compile_l_value(&arguments[0])?);
			}
			Command::List => {
				// Look for invalid separators
				for argument in arguments {
					match argument {
						ParseTreeElement::ExpressionSeparator(Separator::Comma | Separator::Semicolon) => {}
						ParseTreeElement::ExpressionSeparator(separator) => return Err(BasicError::InvalidSeparator(*separator)),
						_ => {}
					}
				}
				// Push opcode
				out.push(StatementOpcode::List as u8);
				// Push arguments
				match arguments.as_slice() {
					// "list", "list ,"
					// List entire program
					[] | [ParseTreeElement::ExpressionSeparator(..)] => {
						out.push(ExpressionOpcode::FromStartOrToEnd as u8);
						out.push(ExpressionOpcode::FromStartOrToEnd as u8);
					},
					// "list x"
					// List one line
					[line_number] => {
						out.extend(compile_expression(line_number)?);
						out.push(ExpressionOpcode::OneElement as u8);
					}
					// "list x,"
					// A line onwards
					[start_line_number, ParseTreeElement::ExpressionSeparator(..)] => {
						out.extend(compile_expression(start_line_number)?);
						out.push(ExpressionOpcode::FromStartOrToEnd as u8);
					}
					// "list ,x"
					// From the program start up untill a line
					[ParseTreeElement::ExpressionSeparator(..), end_line_number] => {
						out.push(ExpressionOpcode::FromStartOrToEnd as u8);
						out.extend(compile_expression(end_line_number)?);
					}
					// "list x,y", "list x y"
					// Between lines
					[start_line_number, end_line_number] |
					[start_line_number, ParseTreeElement::ExpressionSeparator(..), end_line_number] => {
						out.extend(compile_expression(start_line_number)?);
						out.extend(compile_expression(end_line_number)?);
					}

					_ => return Err(BasicError::InvalidArgumentCount),
				}
			}
			Command::Let | Command::Go | Command::Subroutine | Command::Define | Command::Remark | Command::Option => panic!(),
		}
		ParseTreeElement::Assignment(l_value, r_value) => {
			out.push(StatementOpcode::Let as u8);
			out.extend(compile_l_value(&l_value)?);
			out.extend(compile_expression(&r_value)?);
		}
		ParseTreeElement::DefineFunction(name, type_restriction, arguments, function_body_expression) => {
			// Push opcode
			out.push(match type_restriction {
				TypeRestriction::Any => StatementOpcode::DefineAny,
				TypeRestriction::RealNumber => StatementOpcode::DefineRealNumber,
				TypeRestriction::Integer => StatementOpcode::DefineInteger,
				TypeRestriction::Float => StatementOpcode::DefineFloat,
				TypeRestriction::String => StatementOpcode::DefineString,
				TypeRestriction::Boolean => StatementOpcode::DefineBoolean,
				TypeRestriction::ComplexFloat => StatementOpcode::DefineFloat,
				TypeRestriction::Number => StatementOpcode::DefineNumber,
			} as u8);
			// Push name
			out.extend(name.as_bytes());
			out.push(0);
			// Push arguments
			for argument in arguments {
				out.extend(compile_l_value(argument)?);
			}
			out.push(0);
			// Push function body expression
			out.extend(compile_expression(function_body_expression)?);
		}
		_ => panic!(),
	}
	Ok(out)
}

/// Compile a l-value parse tree element to bytecode
fn compile_l_value(parse_tree_element: &ParseTreeElement) -> Result<Vec<u8>, BasicError> {
	Ok(match parse_tree_element {
		ParseTreeElement::Identifier(name, type_restriction) => {
			let mut out = Vec::new();
			// Push opcode
			out.push(match type_restriction {
				TypeRestriction::Any => LValueOpcode::ScalarAny,
				TypeRestriction::RealNumber => LValueOpcode::ScalarRealNumber,
				TypeRestriction::Integer => LValueOpcode::ScalarInteger,
				TypeRestriction::Float => LValueOpcode::ScalarFloat,
				TypeRestriction::String => LValueOpcode::ScalarString,
				TypeRestriction::Boolean => LValueOpcode::ScalarBoolean,
				TypeRestriction::ComplexFloat => LValueOpcode::ScalarFloat,
				TypeRestriction::Number => LValueOpcode::ScalarNumber,
			} as u8);
			// Push name
			out.extend(name.as_bytes());
			out.push(0);
			// Return
			out
		}
		ParseTreeElement::UserDefinedFunctionOrArrayElement(name, type_restriction, indices) => {
			let mut out = Vec::new();
			// Push opcode
			out.push(match type_restriction {
				TypeRestriction::Any => LValueOpcode::ArrayElementAny,
				TypeRestriction::RealNumber => LValueOpcode::ArrayElementRealNumber,
				TypeRestriction::Integer => LValueOpcode::ArrayElementInteger,
				TypeRestriction::Float => LValueOpcode::ArrayElementFloat,
				TypeRestriction::String => LValueOpcode::ArrayElementString,
				TypeRestriction::Boolean => LValueOpcode::ArrayElementBoolean,
				TypeRestriction::ComplexFloat => LValueOpcode::ArrayElementFloat,
				TypeRestriction::Number => LValueOpcode::ArrayElementNumber,
			} as u8);
			// Push name
			out.extend(name.as_bytes());
			out.push(0);
			// Push bytecode for index
			for index_expression in indices {
				out.extend(compile_expression(index_expression)?);
			}
			// Null terminate
			out.push(0);
			// Return
			out
		}
		_ => return Err(BasicError::ExpectedLValue),
	})
}

/// Compiles a single parse tree element that is an expression to bytecode
fn compile_expression(parse_tree_element: &ParseTreeElement) -> Result<Vec<u8>, BasicError> {
	let mut out = Vec::new();
	// Tree element must be an expression
	debug_assert!(parse_tree_element.is_expression());
	
	match parse_tree_element {
		ParseTreeElement::NumericalLiteral(number) => {
			out.push(ExpressionOpcode::NumericalLiteral as u8);
			out.extend(number.as_bytes());
			out.push(0);
		}
		ParseTreeElement::StringLiteral(number) => {
			out.push(ExpressionOpcode::StringLiteral as u8);
			out.extend(number.as_bytes());
			out.push(0);
		}
		ParseTreeElement::BinaryOperator(operator, left_operand, right_operand) => {
			match *operator {
				// Operators that end with a null byte
				Operator::AddConcatenate | Operator::Multiply => {
					// Push operator opcode
					out.push(match operator {
						Operator::AddConcatenate => ExpressionOpcode::SumConcatenate,
						Operator::Multiply => ExpressionOpcode::Product,
						_ => unreachable!(),
					} as u8);
					// Push bytecode for sub-expressions
					out.extend(compile_expression(left_operand)?);
					out.extend(compile_expression(right_operand)?);
					// Null terminate
					out.push(0);
				}
				// Operators that don't end in a null byte
				Operator::EqualTo | Operator::GreaterThan | Operator::GreaterThanOrEqualTo | Operator::LessThan | Operator::LessThanOrEqualTo | Operator::NotEqualTo |
				Operator::EqualToAssign |
				Operator::Divide | Operator::FlooredDivide | Operator::ExclusiveOr | Operator::Exponent | Operator::MinusNegate | Operator::Modulus | Operator::And | Operator::Or => {
					// Push operator opcode
					out.push(match operator {
						Operator::EqualTo => ExpressionOpcode::EqualTo,
						Operator::GreaterThan => ExpressionOpcode::GreaterThan,
						Operator::GreaterThanOrEqualTo => ExpressionOpcode::GreaterThanOrEqualTo,
						Operator::LessThan => ExpressionOpcode::LessThan,
						Operator::LessThanOrEqualTo => ExpressionOpcode::LessThanOrEqualTo,
						Operator::NotEqualTo => ExpressionOpcode::NotEqualTo,
						Operator::EqualToAssign => ExpressionOpcode::EqualTo,
						Operator::And => ExpressionOpcode::And,
						Operator::Divide => ExpressionOpcode::Divide,
						Operator::ExclusiveOr => ExpressionOpcode::ExclusiveOr,
						Operator::Exponent => ExpressionOpcode::Exponent,
						Operator::MinusNegate => ExpressionOpcode::Subtract,
						Operator::Modulus => ExpressionOpcode::Modulus,
						Operator::FlooredDivide => ExpressionOpcode::FlooredDivide,
						Operator::Or => ExpressionOpcode::Or,
						_ => unreachable!(),
					} as u8);
					// Push bytecode for sub-expressions
					out.extend(compile_expression(left_operand)?);
					out.extend(compile_expression(right_operand)?);
				}
				other => return Err(BasicError::InvalidBinaryOperatorSymbol(other)),
			}
		}
		ParseTreeElement::UnaryOperator(operator, operand) => {
			match *operator {
				Operator::MinusNegate | Operator::Not => {
					// Push operator opcode
					out.push(match operator {
						Operator::MinusNegate => ExpressionOpcode::Negate,
						Operator::Not => ExpressionOpcode::Not,
						_ => unreachable!(),
					} as u8);
					// Push bytecode for sub-expression
					out.extend(compile_expression(operand)?);
				}
				other => return Err(BasicError::InvalidUnaryOperatorSymbol(other)),
			}
		}
		ParseTreeElement::BuiltInFunction(function, type_restriction, argument_expressions) => {
			// Push type restriction if not any
			if *type_restriction != TypeRestriction::Any {
				out.push(match *type_restriction {
					TypeRestriction::Boolean => ExpressionOpcode::GetBoolean,
					TypeRestriction::Integer => ExpressionOpcode::GetInteger,
					TypeRestriction::String => ExpressionOpcode::GetString,
					TypeRestriction::ComplexFloat => ExpressionOpcode::GetComplexFloat,
					TypeRestriction::Number => ExpressionOpcode::GetNumber,
					TypeRestriction::RealNumber => ExpressionOpcode::GetRealNumber,
					TypeRestriction::Float => ExpressionOpcode::GetFloat,
					TypeRestriction::Any => unreachable!(),
				} as u8);
			}
			// Function
			match function {
				// Single argument functions
				BuiltInFunction::AbsoluteValue | BuiltInFunction::Arctangent | BuiltInFunction::Cosine | BuiltInFunction::Exponential | BuiltInFunction::Integer |
				BuiltInFunction::Sign | BuiltInFunction::Sine | BuiltInFunction::SquareRoot | BuiltInFunction::Tangent => {
					// Should only have one argument
					if argument_expressions.len() != 1 {
						return Err(BasicError::InvalidArgumentCount);
					}
					// Push the function opcode
					out.push(match function {
						BuiltInFunction::AbsoluteValue => ExpressionOpcode::AbsoluteValue,
						BuiltInFunction::Arctangent => ExpressionOpcode::Arctangent,
						BuiltInFunction::Cosine => ExpressionOpcode::Cosine,
						BuiltInFunction::Exponential => ExpressionOpcode::Exponential,
						BuiltInFunction::Integer => ExpressionOpcode::Integer,
						BuiltInFunction::Sign => ExpressionOpcode::Sign,
						BuiltInFunction::Sine => ExpressionOpcode::Sine,
						BuiltInFunction::SquareRoot => ExpressionOpcode::SquareRoot,
						BuiltInFunction::Tangent => ExpressionOpcode::Tangent,
						_ => unreachable!(),
					} as u8);
					// Push bytecode for sub-expression
					out.extend(compile_expression(&argument_expressions[0])?);
				}
				// Functions with any number of arguments that are null terminated
				BuiltInFunction::Random | BuiltInFunction::Logarithm | BuiltInFunction::Sum | BuiltInFunction::Product => {
					// Push the function opcode
					out.push(match function {
						BuiltInFunction::Random => ExpressionOpcode::Random,
						BuiltInFunction::Logarithm => ExpressionOpcode::Logarithm,
						BuiltInFunction::Sum => ExpressionOpcode::SumConcatenate,
						BuiltInFunction::Product => ExpressionOpcode::Product,
						_ => unreachable!(),
					} as u8);
					// Push bytecode for sub-expressions
					for argument_expression in argument_expressions {
						out.extend(compile_expression(argument_expression)?);
					}
					// Null terminate
					out.push(0);
				}
				// Constants
				BuiltInFunction::True | BuiltInFunction::False | BuiltInFunction::Pi | BuiltInFunction::EulersNumber | BuiltInFunction::ImaginaryUnit => {
					if argument_expressions.len() != 0 {
						return Err(BasicError::InvalidArgumentCount);
					}
					out.push(match function {
						BuiltInFunction::True => ExpressionOpcode::True,
						BuiltInFunction::False => ExpressionOpcode::False,
						BuiltInFunction::Pi => ExpressionOpcode::Pi,
						BuiltInFunction::EulersNumber => ExpressionOpcode::EulersNumber,
						BuiltInFunction::ImaginaryUnit => ExpressionOpcode::ImaginaryUnit,
						_ => unreachable!(),
					} as u8);
				}

				BuiltInFunction::Function => panic!(),
			}
		}
		ParseTreeElement::Identifier(name, type_restriction) => {
			// Push opcode
			out.push(match *type_restriction {
				TypeRestriction::Boolean => ExpressionOpcode::LoadScalarBoolean,
				TypeRestriction::Integer => ExpressionOpcode::LoadScalarInteger,
				TypeRestriction::String => ExpressionOpcode::LoadScalarString,
				TypeRestriction::ComplexFloat => ExpressionOpcode::LoadScalarComplexFloat,
				TypeRestriction::Number => ExpressionOpcode::LoadScalarNumber,
				TypeRestriction::RealNumber => ExpressionOpcode::LoadScalarRealNumber,
				TypeRestriction::Float => ExpressionOpcode::LoadScalarFloat,
				TypeRestriction::Any => ExpressionOpcode::LoadScalarAny,
			} as u8);
			// Push null terminated name
			out.extend(name.as_bytes());
			out.push(0);
		}
		ParseTreeElement::UserDefinedFunctionOrArrayElement(name, type_restriction, arguments) => {
			// Push opcode
			out.push(match *type_restriction {
				TypeRestriction::Boolean => ExpressionOpcode::GetArrayValueBoolean,
				TypeRestriction::Integer => ExpressionOpcode::GetArrayValueInteger,
				TypeRestriction::String => ExpressionOpcode::GetArrayValueString,
				TypeRestriction::ComplexFloat => ExpressionOpcode::GetArrayValueComplexFloat,
				TypeRestriction::Number => ExpressionOpcode::GetArrayValueNumber,
				TypeRestriction::RealNumber => ExpressionOpcode::GetArrayValueRealNumber,
				TypeRestriction::Float => ExpressionOpcode::GetArrayValueFloat,
				TypeRestriction::Any => ExpressionOpcode::GetArrayValueAny,
			} as u8);
			// Push null terminated name
			out.extend(name.as_bytes());
			out.push(0);
			// Push bytecode for sub-expressions
			for argument_expression in arguments {
				out.extend(compile_expression(argument_expression)?);
			}
			// Null terminate
			out.push(0);
		}
		ParseTreeElement::UserDefinedFunction(name, type_restriction, arguments) => {
			// Push opcode
			out.push(match *type_restriction {
				TypeRestriction::Boolean => ExpressionOpcode::CallUserFunctionBoolean,
				TypeRestriction::Integer => ExpressionOpcode::CallUserFunctionInteger,
				TypeRestriction::String => ExpressionOpcode::CallUserFunctionString,
				TypeRestriction::ComplexFloat => ExpressionOpcode::CallUserFunctionComplexFloat,
				TypeRestriction::Number => ExpressionOpcode::CallUserFunctionNumber,
				TypeRestriction::RealNumber => ExpressionOpcode::CallUserFunctionRealNumber,
				TypeRestriction::Float => ExpressionOpcode::CallUserFunctionFloat,
				TypeRestriction::Any => ExpressionOpcode::CallUserFunctionAny,
			} as u8);
			// Push null terminated name
			out.extend(name.as_bytes());
			out.push(0);
			// Push bytecode for sub-expressions
			for argument in arguments {
				out.extend(compile_expression(argument)?);
			}
			// Null terminate
			out.push(0);
		}
		_ => panic!(),
	}
	Ok(out)
}

/// Decompiles the bytecode for a line to list of statements.
pub fn decompile_line(bytecode: &[u8]) -> Result<Vec<ParseTreeElement>, BasicError> {
	let mut bytecode = bytecode;
	let mut out = Vec::new();
	while !bytecode.is_empty() {
		out.push(decompile_statement(&mut bytecode)?);
	}
	Ok(out)
}

/// Decompiles the bytecode for a statement to a parse tree element.
fn decompile_statement(statement_bytecode: &mut &[u8]) -> Result<ParseTreeElement, BasicError> {
	// Extract opcode
	let opcode_id = *statement_bytecode.get(0)
		.ok_or(BasicError::ExpectedStatementOpcodeButProgramEnd)?;
	let opcode: StatementOpcode = FromPrimitive::from_u8(opcode_id)
		.ok_or(BasicError::InvalidStatementOpcode(opcode_id))?;
	*statement_bytecode = &statement_bytecode[1..];
	// Decompile statement
	Ok(match opcode {
		// Statements that take in nothing
		StatementOpcode::End | StatementOpcode::Stop | StatementOpcode::Return | StatementOpcode::Continue | StatementOpcode::Randomize => ParseTreeElement::Command(match opcode {
			StatementOpcode::End => Command::End,
			StatementOpcode::Stop => Command::Stop,
			StatementOpcode::Return => Command::Return,
			StatementOpcode::Continue => Command::Continue,
			StatementOpcode::Randomize => Command::Randomize,
			_ => unreachable!(),
		}, Vec::new()),

		StatementOpcode::Print => {
			let mut sub_expressions = Vec::new();
			let mut should_print_semicolon = false;
			let mut had_newline = false;
			// For each expression in the print statement
			loop {
				// Extract opcode
				let opcode_id = statement_bytecode.get(0).cloned();
				*statement_bytecode = &statement_bytecode[1..];
				let expression_opcode = match ExpressionOpcode::from_option_u8(opcode_id)? {
					None => break,
					Some(expression_opcode) => expression_opcode,
				};
				// Decompile expression
				match expression_opcode {
					// A space opcode is caused by a comma
					ExpressionOpcode::Space => {
						sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Comma));
						should_print_semicolon = false;
					}
					// If there is a newline opcode at the end of the print statement then a semicolon was not used at the end
					ExpressionOpcode::NewLine => {
						if statement_bytecode.get(0) != Some(&0) {
							return Err(BasicError::InvalidNewline);
						}
						had_newline = true;
					}
					// Other expressions
					other => {
						// Add semicolon unless this is the first expression for the print statement or a comma was used since the last expression
						if should_print_semicolon {
							sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Semicolon));
						}
						// Push the decompiled expression
						sub_expressions.push(decompile_expression(statement_bytecode, other, TypeRestriction::Any)?);
						should_print_semicolon = true;
					}
				}
			}
			// End the print statement with a semicolon if there was no newline opcode at the end
			if !had_newline && should_print_semicolon {
				sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Semicolon));
			}
			// Construct the parse tree element
			ParseTreeElement::Command(Command::Print, sub_expressions)
		}
		StatementOpcode::Input => {
			let mut sub_expressions = Vec::new();
			// For each expression that will be printed
			let mut should_print_semicolon = false;
			loop {
				// Extract opcode
				let opcode_id = statement_bytecode.get(0).cloned();
				*statement_bytecode = &statement_bytecode[1..];
				let expression_opcode = match ExpressionOpcode::from_option_u8(opcode_id)? {
					None => break,
					Some(expression_opcode) => expression_opcode,
				};
				// Decompile expression
				match expression_opcode {
					// A space opcode is caused by a comma
					ExpressionOpcode::Space => {
						sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Comma));
						should_print_semicolon = false;
					}
					// Other expressions
					other => {
						// Add semicolon unless this is the first expression for the input statement or a comma was used since the last expression
						if should_print_semicolon {
							sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Semicolon));
						}
						// Push the decompiled expression
						sub_expressions.push(decompile_expression(statement_bytecode, other, TypeRestriction::Any)?);
						should_print_semicolon = true;
					}
				}
			}
			// End the printed expressions with a semicolon
			if !sub_expressions.is_empty() {
				sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Semicolon));
			}
			// For each l-value that will be an input
			let mut should_print_comma = false;
			loop {
				// Decompile l-value
				let decompiled_l_value = match decompile_l_value(statement_bytecode)? {
					Some(decompiled_l_value) => decompiled_l_value,
					None => break,
				};
				// Add comma unless this is the first l-value for the input statement
				if should_print_comma {
					sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Comma));
				}
				// Push the decompiled l-value
				sub_expressions.push(decompiled_l_value);
				// The next l-value is not the first l-value.
				should_print_comma = true;
			}
			// Construct the parse tree element
			ParseTreeElement::Command(Command::Input, sub_expressions)
		}
		// Decompile null terminated expression list to comma separated expression list
		StatementOpcode::Goto | StatementOpcode::Run | StatementOpcode::GoSubroutine | StatementOpcode::Load |
		StatementOpcode::Save | StatementOpcode::Data | StatementOpcode::Restore | StatementOpcode::OptionBase => {
			let mut is_first_expression = true;
			let mut sub_expressions = Vec::new();
			// For each sub-expression
			loop {
				// Extract the opcode or break if we get a null byte
				let opcode_id = statement_bytecode.get(0).cloned();
				*statement_bytecode = &statement_bytecode[1..];
				let expression_opcode = match ExpressionOpcode::from_option_u8(opcode_id)? {
					None => break,
					Some(expression_opcode) => expression_opcode,
				};
				// Push a comma if it is not the first expression.
				if !is_first_expression {
					sub_expressions.push(ParseTreeElement::ExpressionSeparator(Separator::Comma));
				}
				// Decompile the sub-expression
				sub_expressions.push(decompile_expression(statement_bytecode, expression_opcode, TypeRestriction::Any)?);
				// The next expression is not the first expression.
				is_first_expression = false;
			}
			// Create the parse tree element
			let command = match opcode {
				StatementOpcode::Goto => Command::Goto,
				StatementOpcode::Run => Command::Run,
				StatementOpcode::GoSubroutine => Command::GoSubroutine,
				StatementOpcode::Load => Command::Load,
				StatementOpcode::Save => Command::Save,
				StatementOpcode::Data => Command::Data,
				StatementOpcode::Restore => Command::Restore,
				StatementOpcode::OptionBase => Command::Base,
				_ => unreachable!(),
			};
			ParseTreeElement::Command(command, sub_expressions)
		}
		// Decompile null terminated l-value list to comma separated l-value list
		StatementOpcode::Read => {
			let mut is_first_l_value = true;
			let mut l_values = Vec::new();
			// For each l-value
			loop {
				// Decompile the l-value or break if it is he null l-value
				let l_value = match decompile_l_value(statement_bytecode)? {
					Some(decompiled_l_value) => decompiled_l_value,
					None => break,
				};
				// Push a comma if it is not the first l-value.
				if !is_first_l_value {
					l_values.push(ParseTreeElement::ExpressionSeparator(Separator::Comma));
				}
				// Push the l-value
				l_values.push(l_value);
				// The next l-value is not the first l-value.
				is_first_l_value = false;
			}
			// Create the parse tree element
			let command = match opcode {
				StatementOpcode::Read => Command::Read,
				_ => unreachable!(),
			};
			ParseTreeElement::Command(command, l_values)
		}
		// Statements that take in a single expression
		StatementOpcode::If | StatementOpcode::On | StatementOpcode::Step | StatementOpcode::To => {
			// Extract the opcode
			let expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile the sub-expression
			let sub_expressions = decompile_expression(statement_bytecode, expression_opcode, TypeRestriction::Any)?;
			// Construct parse tree element.
			let command = match opcode {
				StatementOpcode::If => Command::If,
				StatementOpcode::On => Command::On,
				StatementOpcode::Step => Command::Step,
				StatementOpcode::To => Command::To,
				_ => unreachable!(),
			};
			ParseTreeElement::Command(command, vec![sub_expressions])
		}
		// Statements that take in another statement.
		StatementOpcode::Then | StatementOpcode::Else => {
			// Get the command
			let command = match opcode {
				StatementOpcode::Then => Command::Then,
				StatementOpcode::Else => Command::Else,
				_ => unreachable!(),
			};
			// Construct the parse tree element with the decompiled sub-statement
			ParseTreeElement::Command(command, vec![decompile_statement(statement_bytecode)?])
		}
		StatementOpcode::List => {
			let mut out = Vec::new();
			// Extract the opcode for the first sub-expression.
			let first_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile range start
			match first_expression_opcode {
				ExpressionOpcode::FromStartOrToEnd => {}
				_ => {
					let decompiled_first_expression = decompile_expression(statement_bytecode, first_expression_opcode, TypeRestriction::Any)?;
					out.push(decompiled_first_expression);
				}
			}
			// Extract the opcode for the second sub-expression.
			let second_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile range end
			match second_expression_opcode {
				ExpressionOpcode::FromStartOrToEnd => {
					if first_expression_opcode != ExpressionOpcode::FromStartOrToEnd {
						out.push(ParseTreeElement::ExpressionSeparator(Separator::Comma));
					}
				}
				ExpressionOpcode::OneElement => {}
				_ => {
					out.push(ParseTreeElement::ExpressionSeparator(Separator::Comma));
					let decompiled_second_expression = decompile_expression(statement_bytecode, second_expression_opcode, TypeRestriction::Any)?;
					out.push(decompiled_second_expression);
				}
			}
			// Construct the parse tree element
			ParseTreeElement::Command(Command::List, out)
		}
		// Statements that take in an l-value and an expression and convert them to assignments.
		StatementOpcode::Let | StatementOpcode::For => {
			// Get the l-value
			let l_value = decompile_l_value(statement_bytecode)?
				.ok_or(BasicError::UnexpectedLValueEndOpcode)?;
			// Extract the expression opcode.
			let expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile the expression
			let decompiled_expression = decompile_expression(statement_bytecode, expression_opcode, TypeRestriction::Any);
			// Construct parse tree element
			let command = match opcode {
				StatementOpcode::Let => Command::Let,
				StatementOpcode::For => Command::For,
				_ => unreachable!(),
			};
			let assignment = ParseTreeElement::Assignment(Box::new(l_value), Box::new(decompiled_expression?));
			ParseTreeElement::Command(command, vec![assignment])
		}
		// Statements that take in a single l-value
		StatementOpcode::Next | StatementOpcode::Dimension => {
			// Decompile l-value
			let l_value = decompile_l_value(statement_bytecode)?
				.ok_or(BasicError::UnexpectedLValueEndOpcode)?;
			// Construct parse tree element
			let command = match opcode {
				StatementOpcode::Next => Command::Next,
				StatementOpcode::Dimension => Command::Dimension,
				_ => unreachable!(),
			};
			ParseTreeElement::Command(command, vec![l_value])
		}

		StatementOpcode::DefineAny | StatementOpcode::DefineBoolean | StatementOpcode::DefineComplexFloat | StatementOpcode::DefineFloat | StatementOpcode::DefineInteger |
		StatementOpcode::DefineNumber | StatementOpcode::DefineRealNumber | StatementOpcode::DefineString => {
			// Get the type restriction
			let type_restriction = match opcode {
				StatementOpcode::DefineAny => TypeRestriction::Any,
				StatementOpcode::DefineBoolean => TypeRestriction::Boolean,
				StatementOpcode::DefineComplexFloat => TypeRestriction::ComplexFloat,
				StatementOpcode::DefineFloat => TypeRestriction::Float,
				StatementOpcode::DefineInteger => TypeRestriction::Integer,
				StatementOpcode::DefineNumber => TypeRestriction::Number,
				StatementOpcode::DefineRealNumber => TypeRestriction::RealNumber,
				StatementOpcode::DefineString => TypeRestriction::String,
				_ => unreachable!(),
			};
			// Get name
			let name = decompile_string(statement_bytecode)?;
			// Get the arguments
			let mut arguments = Vec::new();
			loop {
				match decompile_l_value(statement_bytecode)? {
					Some(l_value) => arguments.push(l_value),
					None => break,
				}
			}
			// Extract the function body expression expression opcode.
			let function_body_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile function body expression
			let function_body_expression = decompile_expression(statement_bytecode, function_body_expression_opcode, type_restriction)?;
			// Construct the parse tree element
			ParseTreeElement::DefineFunction(name, type_restriction, arguments, Box::new(function_body_expression))
		}
	})
}

/// Decompiles the bytecode for an expression to a parse tree element.
/// `type_restriction` is the type restriction from a type restriction opciode.
fn decompile_expression(statement_bytecode: &mut &[u8], opcode: ExpressionOpcode, type_restriction: TypeRestriction) -> Result<ParseTreeElement, BasicError> {
	Ok(match opcode {
		// Literals
		ExpressionOpcode::NumericalLiteral | ExpressionOpcode::StringLiteral => {
			let string = decompile_string(statement_bytecode)?;
			match opcode {
				ExpressionOpcode::NumericalLiteral => ParseTreeElement::NumericalLiteral(string),
				ExpressionOpcode::StringLiteral => ParseTreeElement::StringLiteral(string),
				_ => unreachable!(),
			}
		}
		// No parameters
		ExpressionOpcode::EulersNumber | ExpressionOpcode::False | ExpressionOpcode::True | ExpressionOpcode::ImaginaryUnit | ExpressionOpcode::Pi => {
			let function = match opcode {
				ExpressionOpcode::EulersNumber => BuiltInFunction::EulersNumber,
				ExpressionOpcode::False => BuiltInFunction::False,
				ExpressionOpcode::True => BuiltInFunction::True,
				ExpressionOpcode::ImaginaryUnit => BuiltInFunction::ImaginaryUnit,
				ExpressionOpcode::Pi => BuiltInFunction::Pi,
				_ => unreachable!(),
			};
			ParseTreeElement::BuiltInFunction(function, type_restriction, Vec::new())
		}
		// Single argument functions
		ExpressionOpcode::AbsoluteValue | ExpressionOpcode::Arctangent | ExpressionOpcode::Cosine | ExpressionOpcode::Exponential | ExpressionOpcode::Integer |
		ExpressionOpcode::Sign | ExpressionOpcode::Sine | ExpressionOpcode::SquareRoot | ExpressionOpcode::Tangent => {
			// Get function
			let function = match opcode {
				ExpressionOpcode::AbsoluteValue => BuiltInFunction::AbsoluteValue,
				ExpressionOpcode::Arctangent => BuiltInFunction::AbsoluteValue,
				ExpressionOpcode::Cosine => BuiltInFunction::Cosine,
				ExpressionOpcode::Exponential => BuiltInFunction::Exponential,
				ExpressionOpcode::Integer => BuiltInFunction::Integer,
				ExpressionOpcode::Sign => BuiltInFunction::Sign,
				ExpressionOpcode::Sine => BuiltInFunction::Sine,
				ExpressionOpcode::SquareRoot => BuiltInFunction::SquareRoot,
				ExpressionOpcode::Tangent => BuiltInFunction::Tangent,
				_ => unreachable!(),
			};
			// Extract the sub-expression opcode.
			let sub_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile the sub-expression
			let decompiled_sub_expression = decompile_expression(statement_bytecode, sub_expression_opcode, TypeRestriction::Any)?;
			// Construct parse tree element
			ParseTreeElement::BuiltInFunction(function, type_restriction, vec![decompiled_sub_expression])
		}
		// Function with a null-terminated argument list
		ExpressionOpcode::Logarithm | ExpressionOpcode::Random => {
			// Get function
			let function = match opcode {
				ExpressionOpcode::Logarithm => BuiltInFunction::Logarithm,
				ExpressionOpcode::Random => BuiltInFunction::Random,
				_ => unreachable!(),
			};
			// Get each sub-expression
			let mut decompiled_sub_expressions = Vec::new();
			loop {
				// Extract the sub-expression opcode
				let sub_expression_opcode_id = statement_bytecode.get(0).cloned();
				*statement_bytecode = &statement_bytecode[1..];
				let sub_expression_opcode = match ExpressionOpcode::from_option_u8(sub_expression_opcode_id)? {
					Some(sub_expression_opcode) => sub_expression_opcode,
					None => break,
				};
				// Decompile the sub-expression
				decompiled_sub_expressions.push(decompile_expression(statement_bytecode, sub_expression_opcode, TypeRestriction::Any)?);
			}
			// Construct parse tree element
			ParseTreeElement::BuiltInFunction(function, TypeRestriction::Any, decompiled_sub_expressions)
		}
		// Unary operators
		ExpressionOpcode::Negate | ExpressionOpcode::Not => {
			// Get operator
			let operator = match opcode {
				ExpressionOpcode::Negate => Operator::MinusNegate,
				ExpressionOpcode::Not => Operator::Not,
				_ => unreachable!(),
			};
			// Extract the sub-expression opcode.
			let sub_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile the sub-expression
			let decompiled_sub_expression = decompile_expression(statement_bytecode, sub_expression_opcode, TypeRestriction::Any)?;
			// Construct parse tree element
			ParseTreeElement::UnaryOperator(operator, Box::new(decompiled_sub_expression))
		}
		// Binary operators
		ExpressionOpcode::And | ExpressionOpcode::Divide | ExpressionOpcode::EqualTo | ExpressionOpcode::ExclusiveOr | ExpressionOpcode::FlooredDivide |
		ExpressionOpcode::GreaterThan | ExpressionOpcode::GreaterThanOrEqualTo | ExpressionOpcode::LessThan | ExpressionOpcode::LessThanOrEqualTo |
		ExpressionOpcode::Modulus | ExpressionOpcode::Subtract | ExpressionOpcode::NotEqualTo | ExpressionOpcode::Exponent | ExpressionOpcode::Or => {
			// Get operator
			let operator = match opcode {
				ExpressionOpcode::And => Operator::And,
				ExpressionOpcode::Divide => Operator::Divide,
				ExpressionOpcode::EqualTo => Operator::EqualToAssign,
				ExpressionOpcode::NotEqualTo => Operator::NotEqualTo,
				ExpressionOpcode::ExclusiveOr => Operator::ExclusiveOr,
				ExpressionOpcode::FlooredDivide => Operator::FlooredDivide,
				ExpressionOpcode::GreaterThan => Operator::GreaterThan,
				ExpressionOpcode::GreaterThanOrEqualTo => Operator::GreaterThanOrEqualTo,
				ExpressionOpcode::LessThan => Operator::LessThan,
				ExpressionOpcode::LessThanOrEqualTo => Operator::LessThanOrEqualTo,
				ExpressionOpcode::Modulus => Operator::Modulus,
				ExpressionOpcode::Subtract => Operator::MinusNegate,
				ExpressionOpcode::Exponent => Operator::Exponent,
				ExpressionOpcode::Or => Operator::Or,
				_ => unreachable!(),
			};
			// Extract the first sub-expression opcode.
			let first_sub_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile the first sub-expression
			let decompiled_first_sub_expression = decompile_expression(statement_bytecode, first_sub_expression_opcode, TypeRestriction::Any)?;
			// Extract the second sub-expression opcode.
			let second_sub_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile the second sub-expression
			let decompiled_second_sub_expression = decompile_expression(statement_bytecode, second_sub_expression_opcode, TypeRestriction::Any)?;
			// Construct parse tree element
			ParseTreeElement::BinaryOperator(operator, Box::new(decompiled_first_sub_expression), Box::new(decompiled_second_sub_expression))
		}
		// Sum and product
		ExpressionOpcode::SumConcatenate | ExpressionOpcode::Product => {
			// Get each sub-expression
			let mut decompiled_sub_expressions = Vec::new();
			loop {
				// Extract the sub-expression opcode
				let sub_expression_opcode_id = statement_bytecode.get(0).cloned();
				*statement_bytecode = &statement_bytecode[1..];
				let sub_expression_opcode = match ExpressionOpcode::from_option_u8(sub_expression_opcode_id)? {
					Some(sub_expression_opcode) => sub_expression_opcode,
					None => break,
				};
				// Decompile the sub-expression
				decompiled_sub_expressions.push(decompile_expression(statement_bytecode, sub_expression_opcode, TypeRestriction::Any)?);
			}
			if decompiled_sub_expressions.len() == 2 {
				// Get operator
				let operator = match opcode {
					ExpressionOpcode::SumConcatenate => Operator::AddConcatenate,
					ExpressionOpcode::Product => Operator::Multiply,
					_ => unreachable!(),
				};
				// Unpack operand_list
				let (left_operand, right_operand) = match &decompiled_sub_expressions[..] {
					[left_operand, right_operand] => (left_operand.clone(), right_operand.clone()),
					_ => unreachable!(),
				};
				// Construct the parse tree element
				ParseTreeElement::BinaryOperator(operator, Box::new(left_operand), Box::new(right_operand))
			}
			else {
				// Get function
				let function = match opcode {
					ExpressionOpcode::SumConcatenate => BuiltInFunction::Sum,
					ExpressionOpcode::Product => BuiltInFunction::Product,
					_ => unreachable!(),
				};
				// Construct the parse tree element
				ParseTreeElement::BuiltInFunction(function, type_restriction, decompiled_sub_expressions)
			}
		}
		// Type restrictions
		ExpressionOpcode::GetBoolean | ExpressionOpcode::GetComplexFloat | ExpressionOpcode::GetFloat | ExpressionOpcode::GetInteger | ExpressionOpcode::GetNumber |
		ExpressionOpcode::GetRealNumber | ExpressionOpcode::GetString => {
			// Get the type restriction
			let type_restriction = match opcode {
				ExpressionOpcode::GetBoolean => TypeRestriction::Boolean,
				ExpressionOpcode::GetComplexFloat => TypeRestriction::ComplexFloat,
				ExpressionOpcode::GetFloat => TypeRestriction::Float,
				ExpressionOpcode::GetInteger => TypeRestriction::Integer,
				ExpressionOpcode::GetNumber => TypeRestriction::Number,
				ExpressionOpcode::GetRealNumber => TypeRestriction::RealNumber,
				ExpressionOpcode::GetString => TypeRestriction::String,
				_ => unreachable!(),
			};
			// Extract the sub-expression opcode.
			let sub_expression_opcode = ExpressionOpcode::from_non_zero_option_u8(statement_bytecode.get(0).cloned())?;
			*statement_bytecode = &statement_bytecode[1..];
			// Decompile the sub-expression
			decompile_expression(statement_bytecode, sub_expression_opcode, type_restriction)?
		}
		// A variable
		ExpressionOpcode::LoadScalarAny | ExpressionOpcode::LoadScalarBoolean | ExpressionOpcode::LoadScalarComplexFloat | ExpressionOpcode::LoadScalarFloat |
		ExpressionOpcode::LoadScalarInteger | ExpressionOpcode::LoadScalarNumber | ExpressionOpcode::LoadScalarRealNumber | ExpressionOpcode::LoadScalarString => {
			// Get name
			let name = decompile_string(statement_bytecode)?;
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
			// Construct parse tree element
			ParseTreeElement::Identifier(name, type_restriction)
		}
		// An array access or user defined function
		ExpressionOpcode::GetArrayValueAny | ExpressionOpcode::GetArrayValueBoolean |
		ExpressionOpcode::GetArrayValueComplexFloat | ExpressionOpcode::GetArrayValueFloat |
		ExpressionOpcode::GetArrayValueInteger | ExpressionOpcode::GetArrayValueNumber |
		ExpressionOpcode::GetArrayValueRealNumber | ExpressionOpcode::GetArrayValueString => {
			// Get name
			let name = decompile_string(statement_bytecode)?;
			// Get type restriction
			let type_restriction = match opcode {
				ExpressionOpcode::GetArrayValueAny => TypeRestriction::Any,
				ExpressionOpcode::GetArrayValueBoolean => TypeRestriction::Boolean,
				ExpressionOpcode::GetArrayValueComplexFloat => TypeRestriction::ComplexFloat,
				ExpressionOpcode::GetArrayValueFloat => TypeRestriction::Float,
				ExpressionOpcode::GetArrayValueInteger => TypeRestriction::Integer,
				ExpressionOpcode::GetArrayValueNumber => TypeRestriction::Number,
				ExpressionOpcode::GetArrayValueRealNumber => TypeRestriction::RealNumber,
				ExpressionOpcode::GetArrayValueString => TypeRestriction::String,
				_ => unreachable!(),
			};
			// Get each sub-expression
			let mut decompiled_sub_expressions = Vec::new();
			loop {
				// Extract the sub-expression opcode
				let sub_expression_opcode_id = statement_bytecode.get(0).cloned();
				*statement_bytecode = &statement_bytecode[1..];
				let sub_expression_opcode = match ExpressionOpcode::from_option_u8(sub_expression_opcode_id)? {
					Some(sub_expression_opcode) => sub_expression_opcode,
					None => break,
				};
				// Decompile the sub-expression
				decompiled_sub_expressions.push(decompile_expression(statement_bytecode, sub_expression_opcode, TypeRestriction::Any)?);
			}
			// Construct the parse tree element
			ParseTreeElement::UserDefinedFunctionOrArrayElement(name, type_restriction, decompiled_sub_expressions)
		}
		ExpressionOpcode::CallUserFunctionAny | ExpressionOpcode::CallUserFunctionBoolean | ExpressionOpcode::CallUserFunctionComplexFloat | ExpressionOpcode::CallUserFunctionFloat |
		ExpressionOpcode::CallUserFunctionInteger | ExpressionOpcode::CallUserFunctionNumber | ExpressionOpcode::CallUserFunctionRealNumber | ExpressionOpcode::CallUserFunctionString => {
			// Get type restriction
			let type_restriction = match opcode {
				ExpressionOpcode::CallUserFunctionAny => TypeRestriction::Any,
				ExpressionOpcode::CallUserFunctionBoolean => TypeRestriction::Boolean,
				ExpressionOpcode::CallUserFunctionComplexFloat => TypeRestriction::ComplexFloat,
				ExpressionOpcode::CallUserFunctionFloat => TypeRestriction::Float,
				ExpressionOpcode::CallUserFunctionInteger => TypeRestriction::Integer,
				ExpressionOpcode::CallUserFunctionNumber => TypeRestriction::Number,
				ExpressionOpcode::CallUserFunctionRealNumber => TypeRestriction::RealNumber,
				ExpressionOpcode::CallUserFunctionString => TypeRestriction::String,
				_ => unreachable!(),
			};
			// Decompile name
			let name = decompile_string(statement_bytecode)?;
			// Decompile each sub-expression
			let mut decompiled_sub_expressions = Vec::new();
			loop {
				// Extract the sub-expression opcode
				let sub_expression_opcode_id = statement_bytecode.get(0).cloned();
				*statement_bytecode = &statement_bytecode[1..];
				let sub_expression_opcode = match ExpressionOpcode::from_option_u8(sub_expression_opcode_id)? {
					Some(sub_expression_opcode) => sub_expression_opcode,
					None => break,
				};
				// Decompile the sub-expression
				decompiled_sub_expressions.push(decompile_expression(statement_bytecode, sub_expression_opcode, TypeRestriction::Any)?);
			}
			// Construct parse tree element
			ParseTreeElement::UserDefinedFunction(name, type_restriction, decompiled_sub_expressions)
		}
		// Opcodes that should not be here
		ExpressionOpcode::FromStartOrToEnd | ExpressionOpcode::NewLine | ExpressionOpcode::OneElement | ExpressionOpcode::Space =>
			return Err(BasicError::InvalidExpressionOpcode(opcode as u8)),
	})
}

/// Decompiles the bytecode for an l-value to a parse tree element.
/// Returns `Ok(None)` if a null opcode was encountered.
fn decompile_l_value(l_value_bytecode: &mut &[u8]) -> Result<Option<ParseTreeElement>, BasicError> {
	// Extract opcode
	let opcode_id = *l_value_bytecode.get(0)
		.ok_or(BasicError::ExpectedStatementOpcodeButProgramEnd)?;
	let opcode: LValueOpcode = FromPrimitive::from_u8(opcode_id)
		.ok_or(BasicError::InvalidStatementOpcode(opcode_id))?;
	*l_value_bytecode = &l_value_bytecode[1..];
	// Decompile l-value
	Ok(match opcode {
		// Simple indentifiers
		LValueOpcode::ScalarAny | LValueOpcode::ScalarBoolean | LValueOpcode::ScalarComplexFloat | LValueOpcode::ScalarFloat | LValueOpcode::ScalarInteger |
		LValueOpcode::ScalarNumber | LValueOpcode::ScalarString | LValueOpcode::ScalarRealNumber => Some({
			// Get name
			let name = decompile_string(l_value_bytecode)?;
			// Get type restriction
			let type_restriction = match opcode {
				LValueOpcode::ScalarAny => TypeRestriction::Any,
				LValueOpcode::ScalarBoolean => TypeRestriction::Boolean,
				LValueOpcode::ScalarComplexFloat => TypeRestriction::ComplexFloat,
				LValueOpcode::ScalarFloat => TypeRestriction::Float,
				LValueOpcode::ScalarInteger => TypeRestriction::Integer,
				LValueOpcode::ScalarNumber => TypeRestriction::Number,
				LValueOpcode::ScalarString => TypeRestriction::String,
				LValueOpcode::ScalarRealNumber => TypeRestriction::RealNumber,
				_ => unreachable!(),
			};
			// Construct parse tree element
			ParseTreeElement::Identifier(name, type_restriction)
		}),
		// Arrays
		LValueOpcode::ArrayElementAny | LValueOpcode::ArrayElementBoolean | LValueOpcode::ArrayElementComplexFloat | LValueOpcode::ArrayElementFloat |
		LValueOpcode::ArrayElementInteger | LValueOpcode::ArrayElementNumber | LValueOpcode::ArrayElementString | LValueOpcode::ArrayElementRealNumber => Some({
			// Get name
			let name = decompile_string(l_value_bytecode)?;
			// Get type restriction
			let type_restriction = match opcode {
				LValueOpcode::ArrayElementAny => TypeRestriction::Any,
				LValueOpcode::ArrayElementBoolean => TypeRestriction::Boolean,
				LValueOpcode::ArrayElementComplexFloat => TypeRestriction::ComplexFloat,
				LValueOpcode::ArrayElementFloat => TypeRestriction::Float,
				LValueOpcode::ArrayElementInteger => TypeRestriction::Integer,
				LValueOpcode::ArrayElementNumber => TypeRestriction::Number,
				LValueOpcode::ArrayElementString => TypeRestriction::String,
				LValueOpcode::ArrayElementRealNumber => TypeRestriction::RealNumber,
				_ => unreachable!(),
			};
			// Get indices
			let mut indices = Vec::new();
			loop {
				// Extract the expression opcode
				let expression_opcode_id = l_value_bytecode.get(0).cloned();
				*l_value_bytecode = &l_value_bytecode[1..];
				let expression_opcode = match ExpressionOpcode::from_option_u8(expression_opcode_id)? {
					Some(expression_opcode) => expression_opcode,
					None => break,
				};
				// Decompile the expression
				let decompiled_expression = decompile_expression(l_value_bytecode, expression_opcode, TypeRestriction::Any)?;
				// Add array element to output
				indices.push(decompiled_expression);
			}
			// Construct parse tree element
			ParseTreeElement::UserDefinedFunctionOrArrayElement(name, type_restriction, indices)
		}),
		// End of l-value list
		LValueOpcode::End => None,
	})
}

/// Decompiles the bytecode for a string to a string.
fn decompile_string(bytecode: &mut &[u8]) -> Result<String, BasicError> {
	// Get null byte index or return an error if none exist
	let null_byte_index = bytecode.iter()
		.position(|byte| *byte == 0)
		.ok_or(BasicError::UnterminatedString)?;
	// Get the byte slice that contains the string without the null byte
	let utf_8_byte_slice = &bytecode[..null_byte_index];
	// Repoint program counter to point to the byte after the null byte
	*bytecode = &bytecode[null_byte_index + 1..];
	// Return string slice or an error if is not a valid utf-8 string
	Ok(from_utf8(utf_8_byte_slice)
		.map_err(|_| BasicError::InvalidUtf8String)?
		.to_string())
}