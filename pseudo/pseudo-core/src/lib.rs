mod parse;
mod ast;
mod compiler;
mod source;

pub use compiler::{Compiler, CompilerError};
pub use inkwell::{context::Context, OptimizationLevel};
use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
pub struct LanguageSettings {
	pub epsilon: f64,
	pub enable_list: bool,
}

impl LanguageSettings {
	pub const DEFAULT_EPSILON: f64 = 0.000001;
}

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
				assert!(expecting.contains(parse::expression::Expecting::Operand));
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
