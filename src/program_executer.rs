pub struct ProgramExecuter {
	program_counter: Option<usize>,
	is_executing_line_program: bool,
}

impl ProgramExecuter {
	/// Create a program executer in the deafult state.
	pub fn new() -> Self {
		ProgramExecuter {
			program_counter: None,
			is_executing_line_program: false,
		}
	}
}