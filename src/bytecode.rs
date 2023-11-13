use crate::lexer::token::Token;

pub enum Bytecode {
	End = 0,
	NumericalLiteral,
	StringLiteral,
}

pub fn compile_tokens_to_bytecode(tokens: Vec<Token>) -> Vec<u8> {
	let mut out = Vec::new();
	//out.push(55);
	//out.push(55);
	//out.push(1);
	// Return
	out
}