mod parse;
mod runtime;
mod syntax;

use std::collections::HashMap;
use std::io::{stdin, stdout, BufReader};
use wasm_bindgen::prelude::wasm_bindgen;

impl Into<wasm_bindgen::JsValue> for InterpretingError {
	fn into(self) -> wasm_bindgen::JsValue {
		wasm_bindgen::JsValue::from_str(self.to_string().as_str())
	}
}

#[wasm_bindgen]
pub fn interpret(code: &str) -> InterpretingResult {
	let program = parse::parse(code)
		.map_err(|err| InterpretingError::ParsingError(err))?;
	
	let mut variables = HashMap::new();
	runtime::run(&mut variables, &mut BufReader::new(stdin()), &mut stdout(), &program)
		.map_err(|err| InterpretingError::RuntimeError(err))?;
	
	Ok(())
}

pub type InterpretingResult = Result<(), InterpretingError>;

#[derive(Debug)]
pub enum InterpretingError {
	ParsingError(parse::ParsingError),
	RuntimeError(runtime::RuntimeError),
}

impl InterpretingError {
	pub fn to_string(&self) -> String {
		use parse::{
			ParsingError, ParsingErrorKind::*, LineParsingErrorKind::*,
		};
		use runtime::RuntimeError::*;
		
		match self {
			InterpretingError::ParsingError(ParsingError(line, grapheme, err)) => {
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
						ExpectedGrapheme(grapheme) =>
							format!("în această poziție ar fi trebuit să apară „{}”.", grapheme),
						ExpectedAnyGrapheme =>
							format!("în această poziție ar fi trebuit să apară un caracter."),
						ExpectedEnd =>
							format!("până aici ar fi trebuit să se termine linia de cod."),
						ExpectedLvalue =>
							format!("aici ar fi trebuit să apară o variabilă."),
						ExpectedScrieParam =>
							format!("aici ar fi trebuit să apară o variabilă, o expresie, un caracter ('...'), sau un șir de caractere (\"...\")."),
						ExpectedFloatRvalue =>
							format!("aici ar fi trebuit să apară o expresie."),
						ExpectedBoolRvalue =>
							format!("aici ar fi trebuit să apară o condiție."),
						TokenParsingError(err) => {
							use parse::expression::TokenParsingError::*;
							match err {
								InvalidFloatLiteral(literal) =>
									format!("`{}` nu este un număr valid.", literal),
								InvalidOperator(operator) =>
									format!("`{}` nu este un operator valid.", operator),
							}
						}
						ExpressionConstructionError(err) => {
							use parse::expression::ExpressionConstructionError::*;
							match err {
								MismatchedParens =>
									format!("parantezele nu se potrivesc."),
								UnclosedRparen =>
									format!("parantezei închise nu i se potrivește o paranteză deschisă."),
								UnclosedLparen =>
									format!("parantezei deschise nu i se potrivește o paranteză închisă."),
								MissingOperand =>
									format!("aici ar trebui să apară un operand."),
								InvalidUnaryOpOperands(op) => {
									use parse::expression::BoolOrFloatUnaryOp;
									match op {
										BoolOrFloatUnaryOp::BoolUnaryOp(op) =>
											panic!(),
										BoolOrFloatUnaryOp::FloatUnaryOp(op) =>
											format!("operația `{}` poate fi efectuată doar pe o expresie.", op.get_str()),
									}
								}
								InvalidBinaryOpOperands(op) => {
									use parse::expression::BoolOrFloatBinaryOp;
									use syntax::BoolBinaryOp;
									match op {
										BoolOrFloatBinaryOp::BoolBinaryOp(op) =>
											match op {
												BoolBinaryOp::BoolBoolBinaryOp(op) =>
													format!("operația `{}` poate fi efectuată doar pe două condiții.", op.get_str()),
												BoolBinaryOp::BoolFloatBinaryOp(op) =>
													format!("operația `{}` poate fi efectuată doar pe două expresii.", op.get_str()),
											}
										BoolOrFloatBinaryOp::FloatBinaryOp(op) =>
											format!("operația `{}` poate fi efectuată doar pe două expresii.", op.get_str()),
									}
								}
								ExpectationError(expected) =>
									format!("{:?}", expected)
							}
						}
					},
				})
			}
			InterpretingError::RuntimeError(err) => format!("Eroare la rulare: {}", match err {
				UndefinedLvalue(lvalue)
					=> format!("variabilei „{}” nu i-a fost atribuită nicio valoare.", lvalue),
				InputParsingError(buf)
					=> format!("valoarea „{}” nu este un număr valid.", buf),
			})
		}
	}
}
