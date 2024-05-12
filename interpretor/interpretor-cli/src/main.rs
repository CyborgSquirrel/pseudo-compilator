use std::fmt::Display;
use std::process::ExitCode;
use std::{fs::File, io::Read};
use clap::{Parser, ValueEnum};
use clap::builder::PossibleValue;

#[derive(Debug, Clone)]
pub struct OptimizationLevel(interpretor_core::OptimizationLevel);

impl From<interpretor_core::OptimizationLevel> for OptimizationLevel {
	fn from(value: interpretor_core::OptimizationLevel) -> Self {
		OptimizationLevel(value)
  }
}

impl Into<interpretor_core::OptimizationLevel> for OptimizationLevel {
	fn into(self) -> interpretor_core::OptimizationLevel {
    self.0
  }
}

static OPTIMIZATION_LEVEL_VALUE_VARIANTS: &[OptimizationLevel] = &[
  OptimizationLevel(interpretor_core::OptimizationLevel::None),
  OptimizationLevel(interpretor_core::OptimizationLevel::Less),
  OptimizationLevel(interpretor_core::OptimizationLevel::Default),
  OptimizationLevel(interpretor_core::OptimizationLevel::Aggressive),
];

impl clap::ValueEnum for OptimizationLevel {
	fn value_variants<'a>() -> &'a [Self] {
		OPTIMIZATION_LEVEL_VALUE_VARIANTS
  }
  fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
    match self.0 {
	    interpretor_core::OptimizationLevel::None => Some(PossibleValue::new("none")),
	    interpretor_core::OptimizationLevel::Less => Some(PossibleValue::new("less")),
	    interpretor_core::OptimizationLevel::Default => Some(PossibleValue::new("default")),
	    interpretor_core::OptimizationLevel::Aggressive => Some(PossibleValue::new("aggressive")),
    }
  }
}

impl Display for OptimizationLevel {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self.0 {
	    interpretor_core::OptimizationLevel::None => f.write_str("none")?,
	    interpretor_core::OptimizationLevel::Less => f.write_str("less")?,
	    interpretor_core::OptimizationLevel::Default => f.write_str("default")?,
	    interpretor_core::OptimizationLevel::Aggressive => f.write_str("aggressive")?,
    }
		Ok(())
  }
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Output { Text, Json }

impl Display for Output {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
    	Output::Text => f.write_str("text")?,
    	Output::Json => f.write_str("json")?,
    }
    Ok(())
  }
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
	#[arg(name = "SOURCE")]
	source_path: std::path::PathBuf,

	#[arg(name = "DESTINATION")]
	destination_path: std::path::PathBuf,

	#[arg(long)]
	object: bool,

	#[arg(long, conflicts_with = "object")]
	executable: bool,

	#[arg(long, conflicts_with = "object", conflicts_with = "executable")]
	llvm_ir: bool,

	#[arg(long, default_value_t = OptimizationLevel(interpretor_core::OptimizationLevel::Default))]
	opt: OptimizationLevel,

	#[arg(long, default_value_t = Output::Text)]
	output: Output,
}

#[derive(serde::Serialize)]
struct ParserError {
	line: usize,
	column: usize,
	message: String,
}

fn main() -> ExitCode {
	let args = Args::parse();
	if (args.object as u32 + args.executable as u32 + args.llvm_ir as u32) != 1 {
		panic!("Need at least one of --object, --executable, or --llvm-ir.");
	}
	
	let mut program_file = File::open(&args.source_path).unwrap();
	let mut code = String::new();
	program_file.read_to_string(&mut code).unwrap();

	let result = (|| {
		let context = interpretor_core::Context::create();
		let compiler = interpretor_core::Compiler::compile(&context, &code, &args.source_path)?;

		if args.object {
			compiler.write_object(args.destination_path, args.opt.into())?;
		} else if args.executable {
			compiler.write_executable(args.destination_path, args.opt.into())?;
		} else if args.llvm_ir {
			compiler.write_llvm_ir(args.destination_path)?;
		} else {
			unreachable!()
		}
		Ok(())
	})();

	if let Err(interpretor_core::CompilerError::ParserError(parser_error)) = result {
		let parser_error = ParserError {
			line: parser_error.0,
			column: parser_error.1,
			message: parser_error.make_string(),
		};
		match args.output {
			Output::Text => {
				eprintln!("{}", parser_error.message);
			}
			Output::Json => {
				let serialized = serde_json::to_string(&parser_error).unwrap();
				eprintln!("{}", serialized);
			}
		}
		
		return ExitCode::FAILURE;
	} else {
		result.unwrap();
	}

	ExitCode::SUCCESS
}
