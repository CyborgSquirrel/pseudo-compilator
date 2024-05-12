mod parse;
mod runtime;
mod ast;
mod compiler;

pub use compiler::{Compiler, CompilerError};
pub use inkwell::{context::Context, OptimizationLevel};

pub fn interpret<Reader: std::io::BufRead, Writer: std::io::Write>(
	code: &str,
	reader: &mut Reader,
	writer: &mut Writer,
) -> InterpretingResult {
	let program = parse::parse(code)?;
	
	let mut variables = runtime::Variables::new();
	let (mut input, mut output) = (String::new(), String::new());
	let mut runner = runtime::Runner::new(&program);
	
	loop {
		let state = runner.run(&mut variables, &mut input, &mut output);
		writer.write_all(output.as_bytes()).unwrap();
		writer.flush().unwrap();
		output.clear();
		
		let state = state?;
		match state {
			runtime::RuntimeState::Running => panic!(), // this shouldn't happen
			runtime::RuntimeState::WaitingForInput => reader.read_line(&mut input).unwrap(),
			runtime::RuntimeState::Finished => return Ok(()),
		};
	}
}

pub type InterpretingResult = Result<(), InterpretingError>;

#[derive(Debug, PartialEq, Eq)]
pub enum InterpretingError {
	ParsingError(parse::ParsingError),
	RuntimeError(runtime::RuntimeError),
}

impl InterpretingError {
	pub fn make_string(&self) -> String {
		match self {
			InterpretingError::ParsingError(err) => err.make_string(),
			InterpretingError::RuntimeError(err) => err.make_string(),
		}
	}
}

impl parse::ParsingError {
	pub fn make_string(&self) -> String {
		use parse::{
			ParsingErrorKind::*, LineParsingErrorKind::*,
		};
		let Self(line, grapheme, err) = self;
		format!("[{}:{}] Eroare la parsare: {}", line, grapheme, match err {
			DacaAlreadyHasAltfel =>
				format!("instrucțiunea „dacă” de dinaintea acestei instrucțiuni, are deja o instrucțiune „altfel”."),
			AltfelWithoutDaca =>
				format!("acestei instrucțiuni „altfel” nu îi corespunde o instrucțiune „dacă”."),
			InvalidIndent =>
				format!("indentarea este incorectă."),
			EmptyBlock =>
				format!("nu este permis ca o instrucțiune condițională sau de ciclare să nu aibă în corpul său nici o instrucțiune."),
			RepetaWithoutPanaCand =>
				format!("instrucțiunii „repetă” de mai înainte nu îi corespunde o instrucțiune „până când”."),
			LineParsingError(kind) => match kind {
				ExpectedStr(string) =>
					format!("în această poziție ar fi trebuit să apară „{}”.", string),
				ExpectedStrOptionalDiacritics(string) =>
					format!("în această poziție ar fi trebuit să apară „{}” (cu sau fără diacritice).", string),
				ExpectedAnyGrapheme =>
					format!("în această poziție ar fi trebuit să apară un caracter."),
				ExpectedIdent =>
					format!("aici ar fi trebuit să apară un nume de variabilă."),
				ExpectedScrieParam =>
					format!("aici ar fi trebuit să apară o expresie, un caracter ('.'), sau un șir de caractere (\"...\")."),
				ExpectedFloatRvalue =>
					format!("aici ar fi trebuit să apară o expresie."),
				ExpectedBoolRvalue =>
					format!("aici ar fi trebuit să apară o condiție."),
				ExpectedBlocklessInstruction =>
					format!("aici ar fi trebuit să apară o atribuire, interschimbare, sau o instrucțiune de tip scrie sau citește."),

				ExpectedEnd =>
					format!("până aici ar fi trebuit să se termine linia de cod."),
				ExpectedEndOfBlocklessInstruction =>
					format!("până aici ar fi trebuit să se termine instrucțiunea."),

				ExpectedSomethingElse(expecting) => {
					assert!(expecting.contains(parse::expression::Expecting::Rvalue));
					format!("aici ar fi trebuit să apară o valoare.")
				}
				MismatchedParens =>
					format!("parantezele nu corespund."),
				UnclosedLParen =>
					format!("paranteza stângă nu are paranteză dreaptă corespunzătoare."),
				UnclosedRParen =>
					format!("paranteza dreaptă nu are paranteză stângă corespunzătoare."),
				InvalidIdent(name) =>
					format!("nume de variabilă nevalid „{}”.", name),
				InvalidFloatLiteral => format!("număr nevalid."),
				InvalidFunction(name) =>
					format!("nume de funcție nevalid „{}”.", name),

				InvalidWord(value) =>
					format!("nume de variabilă sau cuvânt cheie nevalid „{}”.", value),
				
				InvalidFloatUnopOperands(op) =>
					format!("operația „{}” poate fi efectuată doar pe o expresie.", op.get_str()),
				InvalidFloatBinopOperands(op) =>
					format!("operația „{}” poate fi efectuată doar pe două expresii.", op.get_str()),
				InvalidBoolFloatBinopOperands(op) =>
					format!("operația „{}” poate fi efectuată doar pe două expresii.", op.get_str()),
				InvalidBoolBoolBinopOperands(op) =>
					format!("operația „{}” poate fi efectuată doar pe două condiții.", op.get_str()),
			},
		})
	}
}

impl runtime::RuntimeError {
	fn make_string(&self) -> String {
		use runtime::RuntimeError::*;
		format!("Eroare la rulare: {}", match self {
			UndefinedLvalue(lvalue)
				=> format!("variabilei „{}” nu i-a fost atribuită nicio valoare.", lvalue),
			InputParsingError(buf)
				=> format!("valoarea „{}” nu este un număr valid.", buf),
			InstructionLimitExceeded
				=> format!("programul nu se termină."),
		})
	}
}

impl From<parse::ParsingError> for InterpretingError {
	fn from(err: parse::ParsingError) -> Self {
		InterpretingError::ParsingError(err)
	}
}

impl From<runtime::RuntimeError> for InterpretingError {
	fn from(err: runtime::RuntimeError) -> Self {
		InterpretingError::RuntimeError(err)
	}
}

#[cfg(test)]
mod tests {
	use indoc::indoc;
	use super::{
		interpret,
		InterpretingError,
		runtime::RuntimeError,
	};
	
	macro_rules! assert_outputs {
		( $input:expr, $output:expr, $code:expr ) => {
			let mut input = std::io::BufReader::new($input.as_bytes());
			let mut output = Vec::new();
			interpret(indoc! { $code }, &mut input, &mut output).unwrap();
			assert_eq!(std::str::from_utf8(output.as_slice()).unwrap(), $output);
		}
	}
	
	macro_rules! test_output {
		{ $function:ident, $input:expr, $output:expr, $code:expr } => {
			#[test]
			fn $function() {
				assert_outputs!($input, $output, $code);
			}
		}
	}
	
	macro_rules! test_runtime_error {
		{ $function:ident, $input:expr, $err:expr, $code:expr } => {
			#[test]
			fn $function() {
				let mut input = std::io::BufReader::new($input.as_bytes());
				let mut output = Vec::new();
				let err = interpret(indoc! { $code }, &mut input, &mut output).unwrap_err();
				assert_eq!(err, InterpretingError::RuntimeError($err));
			}
		}
	}
	
	test_output! {
		pentru_executa,
		"",
		"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n",
		r#"
			pentru i <- 1,10 executa
				scrie i
		"#
	}
	test_output! {
		pentru_executa_backwards,
		"",
		"10\n9\n8\n7\n6\n5\n",
		r#"
			pentru i <- 10,5,-1 executa
				scrie i
		"#
	}
	test_output! {
		pentru_executa_once,
		"",
		"10\n",
		r#"
			pentru i <- 10,10 executa
				scrie i
		"#
	}
	test_output! {
		pentru_executa_nope,
		"",
		"",
		r#"
			pentru i <- 20,10 executa
				scrie i
		"#
	}
	test_output! {
		pentru_executa_big_increment,
		"",
		"1\n5\n9\n",
		r#"
			pentru i <- 1,10,4 executa
				scrie i
		"#
	}
	test_output! {
		pentru_executa_big_decrement,
		"",
		"15\n11\n7\n3\n",
		r#"
			pentru i <- 15,0,-4 executa
				scrie i
		"#
	}
	
	test_output! {
		scrie_arguments,
		"",
		"1 18very nice 42x\n",
		r#"
			a<-10
			scrie 1, ' ', 5+3+a, "", "very nice ",42, 'x'
		"#
	}
	
	test_output! {
		whole_part,
		"",
		"-3\n-2\n-2\n-1\n-1\n0\n0\n1\n1\n2\n2\n",
		r#"
			pentru i <- -5,5 executa
				scrie [i/2]
		"#
	}
	
	test_runtime_error! {
		too_many_instructions,
		"",
		RuntimeError::InstructionLimitExceeded,
		r#"
			i <- 0
			cat timp i = 0 executa
				i <- 0
		"#
	}
}
