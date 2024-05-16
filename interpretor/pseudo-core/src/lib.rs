mod parse;
mod ast;
mod compiler;
mod source;

pub use compiler::{Compiler, CompilerError};
pub use inkwell::{context::Context, OptimizationLevel};
use itertools::Itertools;

pub const EPSILON: f32 = 0.000001;

impl parse::ParserError {
	pub fn make_string(&self) -> String {
		use parse::ParserErrorKind::*;
		let Self(offset, err) = self;
		format!("[{}:{}] Eroare la parsare: {}", offset.line_one(), offset.column(), match err {
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
			ExpectedLvalue =>
				format!("aici ar fi trebuit să apară un nume de variabilă, sau un element al unei liste."),
			ExpectedBlocklessInstruction =>
				format!("această instrucțiune nu poate fi folosită după un „;”."),
			ExpectedValueType(type_flags) => {
				let expected_types = {
					type_flags.iter().map(|flag| match flag {
						parse::ValueType::Float => "o expresie",
						parse::ValueType::Bool => "o condiție",
						parse::ValueType::List => "o listă",
					}).join(", ")
				};
				format!("tipul acestei valori ar fi trebuit să fie unul dintre următoarele: {}.", expected_types)
			}

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
		})
	}
}

// TODO: Make tests use compiler.

// #[cfg(test)]
// mod tests {
// 	use indoc::indoc;
// 	use super::{
// 		interpret,
// 		InterpretingError,
// 		runtime::RuntimeError,
// 	};
	
// 	macro_rules! assert_outputs {
// 		( $input:expr, $output:expr, $code:expr ) => {
// 			let mut input = std::io::BufReader::new($input.as_bytes());
// 			let mut output = Vec::new();
// 			interpret(indoc! { $code }, &mut input, &mut output).unwrap();
// 			assert_eq!(std::str::from_utf8(output.as_slice()).unwrap(), $output);
// 		}
// 	}
	
// 	macro_rules! test_output {
// 		{ $function:ident, $input:expr, $output:expr, $code:expr } => {
// 			#[test]
// 			fn $function() {
// 				assert_outputs!($input, $output, $code);
// 			}
// 		}
// 	}
	
// 	macro_rules! test_runtime_error {
// 		{ $function:ident, $input:expr, $err:expr, $code:expr } => {
// 			#[test]
// 			fn $function() {
// 				let mut input = std::io::BufReader::new($input.as_bytes());
// 				let mut output = Vec::new();
// 				let err = interpret(indoc! { $code }, &mut input, &mut output).unwrap_err();
// 				assert_eq!(err, InterpretingError::RuntimeError($err));
// 			}
// 		}
// 	}
	
// 	test_output! {
// 		pentru_executa,
// 		"",
// 		"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n",
// 		r#"
// 			pentru i <- 1,10 executa
// 				scrie i
// 		"#
// 	}
// 	test_output! {
// 		pentru_executa_backwards,
// 		"",
// 		"10\n9\n8\n7\n6\n5\n",
// 		r#"
// 			pentru i <- 10,5,-1 executa
// 				scrie i
// 		"#
// 	}
// 	test_output! {
// 		pentru_executa_once,
// 		"",
// 		"10\n",
// 		r#"
// 			pentru i <- 10,10 executa
// 				scrie i
// 		"#
// 	}
// 	test_output! {
// 		pentru_executa_nope,
// 		"",
// 		"",
// 		r#"
// 			pentru i <- 20,10 executa
// 				scrie i
// 		"#
// 	}
// 	test_output! {
// 		pentru_executa_big_increment,
// 		"",
// 		"1\n5\n9\n",
// 		r#"
// 			pentru i <- 1,10,4 executa
// 				scrie i
// 		"#
// 	}
// 	test_output! {
// 		pentru_executa_big_decrement,
// 		"",
// 		"15\n11\n7\n3\n",
// 		r#"
// 			pentru i <- 15,0,-4 executa
// 				scrie i
// 		"#
// 	}
	
// 	test_output! {
// 		scrie_arguments,
// 		"",
// 		"1 18very nice 42x\n",
// 		r#"
// 			a<-10
// 			scrie 1, ' ', 5+3+a, "", "very nice ",42, 'x'
// 		"#
// 	}
	
// 	test_output! {
// 		whole_part,
// 		"",
// 		"-3\n-2\n-2\n-1\n-1\n0\n0\n1\n1\n2\n2\n",
// 		r#"
// 			pentru i <- -5,5 executa
// 				scrie [i/2]
// 		"#
// 	}
	
// 	test_runtime_error! {
// 		too_many_instructions,
// 		"",
// 		RuntimeError::InstructionLimitExceeded,
// 		r#"
// 			i <- 0
// 			cat timp i = 0 executa
// 				i <- 0
// 		"#
// 	}
// }