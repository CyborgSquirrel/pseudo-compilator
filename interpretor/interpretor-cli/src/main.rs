use std::{fs::File, io::Read};
use std::io::{stdin, BufWriter, stdout};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
	#[arg(name = "SOURCE")]
	source_path: std::path::PathBuf,
}

fn main() {
	let args = Args::parse();
	
	let mut program_file = File::open(args.source_path).unwrap();
	
	let mut code = String::new();
	program_file.read_to_string(&mut code).unwrap();
	
	let result = interpretor_core::compile(
		code.as_str(),
	);

	// let result = interpretor_core::interpret(
	// 	code.as_str(),
	// 	&mut stdin().lock(),
	// 	&mut BufWriter::new(stdout()),
	// );
	// match result {
	// 	Ok(()) => {
	// 		std::process::exit(0);
	// 	}
	// 	Err(err) => {
	// 		eprintln!("{}", err.make_string());
	// 		std::process::exit(1);
	// 	}
	// }
}
