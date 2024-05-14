use inkwell::builder::BuilderError;

use crate::parse;

pub type CompilerResult<T> = Result<T, CompilerError>;

// errors
#[derive(Debug)]
pub enum CompilerError {
	BuilderError(BuilderError),
	VerificationError(VerificationError),
	LLVMError(inkwell::support::LLVMString),
	ClangError(ClangError),
	ParserError(parse::ParserError),
}

impl From<BuilderError> for CompilerError {
	fn from(err: BuilderError) -> Self {
		CompilerError::BuilderError(err)
  }
}

#[derive(Debug)]
pub struct VerificationError(inkwell::support::LLVMString);
impl From<VerificationError> for CompilerError {
	fn from(err: VerificationError) -> Self {
		CompilerError::VerificationError(err)
  }
}

impl From<inkwell::support::LLVMString> for VerificationError {
	fn from(err: inkwell::support::LLVMString) -> Self {
		VerificationError(err)
  }
}

impl From<inkwell::support::LLVMString> for CompilerError {
	fn from(err: inkwell::support::LLVMString) -> Self {
		CompilerError::LLVMError(err)
  }
}

#[derive(Debug)]
pub struct ClangError {
	pub stdout: String,
	pub stderr: String,
	pub status: std::process::ExitStatus,
}

impl From<std::process::Output> for ClangError {
	fn from(value: std::process::Output) -> Self {
    ClangError {
			stdout: String::from_utf8(value.stdout).unwrap(),
			stderr: String::from_utf8(value.stderr).unwrap(),
    	status: value.status,
	  }
  }
}

impl From<ClangError> for CompilerError {
	fn from(err: ClangError) -> Self {
		CompilerError::ClangError(err)
  }
}

impl From<parse::ParserError> for CompilerError {
	fn from(err: parse::ParserError) -> Self {
		CompilerError::ParserError(err)
  }
}
