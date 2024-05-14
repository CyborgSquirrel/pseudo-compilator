pub use crate::compiler::error::CompilerError;
use std::{path::Path, process::Command};


use inkwell::{OptimizationLevel, targets::{InitializationConfig, Target, RelocMode, CodeModel, FileType}};

use crate::Compiler;

use super::error::ClangError;

impl<'src, 'ctx> Compiler<'src, 'ctx> {
	pub fn write_object<P: AsRef<Path>>(
		&self,
		path: P,
		optimization_level: OptimizationLevel,
	) -> Result<(), CompilerError> {
		let path = path.as_ref();
		
		let target_triple = inkwell::targets::TargetMachine::get_default_triple();
		Target::initialize_all(&InitializationConfig::default());
		let target = Target::from_triple(&target_triple)?;
		let target_machine = target.create_target_machine(
			&target_triple,
			"generic",
			"",
			optimization_level,
			RelocMode::PIC,
			CodeModel::Default,
		).unwrap();
		target_machine.write_to_file(&self.module, FileType::Object, path)?;

		Ok(())
	}

	pub fn write_executable<P1: AsRef<Path>, P2: AsRef<Path>>(
		&self,
		lib_path: P1,
		path: P2,
		optimization_level: OptimizationLevel,
	) -> Result<(), CompilerError> {
		let lib_path = lib_path.as_ref();
		let path = path.as_ref();

		let object_path = tempfile::NamedTempFile::new().unwrap().into_temp_path();
		let object_path = &object_path;
		self.write_object(object_path, optimization_level)?;

		let output = Command::new("clang")
			.arg("-lm")
			.arg(lib_path)
			.arg(object_path)
			.arg("-o").arg(path)
			.output().unwrap();
		if !output.status.success() {
			return Err(CompilerError::from(ClangError::from(output)));
		}

		Ok(())
	}

	pub fn write_llvm_ir<P: AsRef<Path>>(
		&self,
		path: P,
	) -> Result<(), CompilerError> {
		self.module.print_to_file(path)?;
		Ok(())
	}
}

