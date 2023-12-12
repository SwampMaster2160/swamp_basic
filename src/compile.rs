use crate::{error::BasicError, bytecode::{statement_opcode::StatementOpcode, expression_opcode::ExpressionOpcode, l_value_opcode::LValueOpcode},
parser::ParseTreeElement, lexer::{command::Command, operator::Operator, built_in_function::BuiltInFunction, type_restriction::TypeRestriction}};

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
		ParseTreeElement::Command(command, sub_elements) => match command {
			Command::End => out.push(StatementOpcode::End as u8),
			Command::Print => {
				out.push(StatementOpcode::Print as u8);
				for element in sub_elements {
					match element {
						ParseTreeElement::ExpressionSeparator(..) => return Err(BasicError::FeatureNotYetSupported),
						_ => out.extend(compile_expression(element)?),
					}
				}
				out.push(StatementOpcode::End as u8);
			}
			Command::Goto | Command::Run => {
				// Push the respective opcode
				out.push(match command {
					Command::Goto => StatementOpcode::Goto,
					Command::Run => StatementOpcode::Run,
					_ => unreachable!(),
				} as u8);
				// Can only have 0 or 1 arguments
				// TODO: Many arguments for on statements
				match sub_elements.len() {
					0 => {},
					1 => match &sub_elements[0] {
						ParseTreeElement::ExpressionSeparator(separator) => return Err(BasicError::InvalidSeparator(*separator)),
						_ => out.extend(compile_expression(&sub_elements[0])?),
					}
					_ => return Err(BasicError::InvalidArgumentCount),
				}
				// Null terminate
				out.push(StatementOpcode::End as u8);
			}
			_ => return Err(BasicError::FeatureNotYetSupported),
		}
		ParseTreeElement::Assignment(l_value, r_value) => {
			out.push(StatementOpcode::Let as u8);
			out.extend(compile_l_value(&l_value)?);
			out.extend(compile_expression(&r_value)?);
		}
		_ => return Err(BasicError::FeatureNotYetSupported),
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
		_ => panic!()
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
				Operator::EqualTo | Operator::GreaterThan | Operator::GreaterThanOrEqualTo | Operator::LessThan | Operator::LessThanOrEqualTo | Operator::NotEqualTo | Operator::EqualToAssign |
				Operator::Divide | Operator::FlooredDivide | Operator::ExclusiveOr | Operator::Exponent | Operator::MinusNegate | Operator::Modulus | Operator::And => {
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
				TypeRestriction::Boolean => ExpressionOpcode::CallUserFunctionOrGetArrayValueBoolean,
				TypeRestriction::Integer => ExpressionOpcode::CallUserFunctionOrGetArrayValueInteger,
				TypeRestriction::String => ExpressionOpcode::CallUserFunctionOrGetArrayValueString,
				TypeRestriction::ComplexFloat => ExpressionOpcode::CallUserFunctionOrGetArrayValueComplexFloat,
				TypeRestriction::Number => ExpressionOpcode::CallUserFunctionOrGetArrayValueNumber,
				TypeRestriction::RealNumber => ExpressionOpcode::CallUserFunctionOrGetArrayValueRealNumber,
				TypeRestriction::Float => ExpressionOpcode::CallUserFunctionOrGetArrayValueFloat,
				TypeRestriction::Any => ExpressionOpcode::CallUserFunctionOrGetArrayValueAny,
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
		_ => return Err(BasicError::FeatureNotYetSupported),
	}
	Ok(out)
}