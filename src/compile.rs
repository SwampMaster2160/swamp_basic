use crate::{error::BasicError, bytecode::{statement_opcode::StatementOpcode, expression_opcode::ExpressionOpcode}, parser::ParseTreeElement, lexer::command::Command};

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
					_ => return Err(BasicError::TooManyExpressions),
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
		_ => return Err(BasicError::FeatureNotYetSupported),
	}
	Ok(out)
}