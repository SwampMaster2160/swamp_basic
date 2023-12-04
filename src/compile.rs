use crate::{error::BasicError, bytecode::{statement_opcode::StatementOpcode, expression_opcode::ExpressionOpcode}, parser::ParseTreeElement, lexer::{command::Command, operator::Operator, built_in_function::BuiltInFunction, type_restriction::TypeRestriction}};

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
				match sub_elements.len() {
					0 => {},
					1 => match &sub_elements[0] {
						ParseTreeElement::ExpressionSeparator(separator) => return Err(BasicError::InvalidSeparator(*separator)),
						_ => out.extend(compile_expression(&sub_elements[0])?),
					}
					_ => return Err(BasicError::InvalidArgumentCount),
				}
				out.push(StatementOpcode::End as u8);
			}
			_ => return Err(BasicError::FeatureNotYetSupported),
		}
		_ => return Err(BasicError::FeatureNotYetSupported),
	}
	Ok(out)
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
				Operator::AddConcatenate | Operator::And | Operator::Divide | Operator::ExclusiveOr | Operator::Exponent | Operator::MinusNegate | Operator::Modulus | Operator::Multiply/* |*/
				/*Operator::EqualTo | Operator::GreaterThan | Operator::GreaterThanOrEqualTo | Operator::LessThan | Operator::LessThanOrEqualTo | Operator::NotEqualTo | Operator::EqualToAssign*/ => {
					out.push(match operator {
						Operator::AddConcatenate => ExpressionOpcode::SumConcatenate,
						Operator::And => ExpressionOpcode::And,
						Operator::Divide => ExpressionOpcode::Divide,
						Operator::ExclusiveOr => ExpressionOpcode::ExclusiveOr,
						Operator::Exponent => ExpressionOpcode::Exponent,
						Operator::MinusNegate => ExpressionOpcode::Subtract,
						Operator::Modulus => ExpressionOpcode::Modulus,
						Operator::Multiply => ExpressionOpcode::Product,
						/*Operator::EqualTo => ExpressionOpcode::EqualTo,
						Operator::GreaterThan => ExpressionOpcode::GreaterThan,
						Operator::GreaterThanOrEqualTo => ExpressionOpcode::GreaterThanOrEqualTo,
						Operator::LessThan => ExpressionOpcode::LessThan,
						Operator::LessThanOrEqualTo => ExpressionOpcode::LessThanOrEqualTo,
						Operator::NotEqualTo => ExpressionOpcode::NotEqualTo,
						Operator::EqualToAssign => ExpressionOpcode::EqualToAssign,*/
						_ => unreachable!(),
					} as u8);
					out.extend(compile_expression(left_operand)?);
					out.extend(compile_expression(right_operand)?);
					out.push(0);
				}
				other => return Err(BasicError::InvalidBinaryOperatorSymbol(other)),
			}
		}
		ParseTreeElement::UnaryOperator(operator, operand) => {
			match *operator {
				Operator::MinusNegate | Operator::Not => {
					out.push(match operator {
						Operator::MinusNegate => ExpressionOpcode::Negate,
						Operator::Not => ExpressionOpcode::Not,
						_ => unreachable!(),
					} as u8);
					out.extend(compile_expression(operand)?);
				}
				other => return Err(BasicError::InvalidUnaryOperatorSymbol(other)),
			}
		}
		ParseTreeElement::BuiltInFunction(function, type_restriction, argument_expressions) => {
			if *type_restriction != TypeRestriction::Any {
				return Err(BasicError::FeatureNotYetSupported);
			}
			match function {
				BuiltInFunction::AbsoluteValue | BuiltInFunction::Arctangent | BuiltInFunction::Cosine | BuiltInFunction::Exponential | BuiltInFunction::Integer |
				BuiltInFunction::Logarithm | BuiltInFunction::Sign | BuiltInFunction::Sine | BuiltInFunction::SquareRoot | BuiltInFunction::Tangent => {
					if argument_expressions.len() != 1 {
						return Err(BasicError::InvalidArgumentCount);
					}
					out.push(match function {
						BuiltInFunction::AbsoluteValue => ExpressionOpcode::AbsoluteValue,
						BuiltInFunction::Arctangent => ExpressionOpcode::Arctangent,
						BuiltInFunction::Cosine => ExpressionOpcode::Cosine,
						BuiltInFunction::Exponential => ExpressionOpcode::Exponential,
						BuiltInFunction::Integer => ExpressionOpcode::Integer,
						BuiltInFunction::Logarithm => ExpressionOpcode::Logarithm,
						BuiltInFunction::Sign => ExpressionOpcode::Sign,
						BuiltInFunction::Sine => ExpressionOpcode::Sine,
						BuiltInFunction::SquareRoot => ExpressionOpcode::SquareRoot,
						BuiltInFunction::Tangent => ExpressionOpcode::Tangent,
						_ => unreachable!(),
					} as u8);
					out.extend(compile_expression(&argument_expressions[0])?);
				}
				BuiltInFunction::Random => {
					out.push(BuiltInFunction::Random as u8);
					for argument_expression in argument_expressions {
						out.extend(compile_expression(argument_expression)?);
					}
					out.push(0);
				}
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
		_ => return Err(BasicError::FeatureNotYetSupported),
	}
	Ok(out)
}