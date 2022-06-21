use std::{fs::File, io::Read};
use std::io::{stdin, BufWriter, stdout};

fn main() {
	// let args: Vec<String> = env::args().collect();
	
	let program_path = "./prog";
	let mut program_file = File::open(program_path).unwrap();
	
	let mut code = String::new();
	program_file.read_to_string(&mut code).unwrap();
	
	let result = pseudo_interpretor::interpret(
		code.as_str(),
		&mut stdin().lock(),
		&mut BufWriter::new(stdout()),
	);
	match result {
		Ok(()) => {
			std::process::exit(0);
		}
		Err(err) => {
			eprintln!("{}", err.make_string());
			std::process::exit(1);
		}
	}
}
