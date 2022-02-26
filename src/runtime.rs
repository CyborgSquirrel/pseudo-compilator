use std::collections::HashMap;
use std::io::{Write, BufRead};
use crate::syntax::{
	Instructiune,
	ScrieParam,
	FloatRvalue, BoolRvalue,
};

fn float_evaluate<'a>(
	variables: &HashMap<&'a str, f32>,
	rvalue: &FloatRvalue<'a>,
) -> RuntimeResult<f32> {
	match rvalue {
		FloatRvalue::Literal(x) => Ok(*x),
		FloatRvalue::Lvalue(x) => variables.get(x.0).cloned().ok_or(RuntimeError::UndefinedLvalue(x.0.into())),
		FloatRvalue::UnaryOp(op, x) => {
			let x = float_evaluate(variables, x)?;
			Ok(op.evaluate(x))
		}
		FloatRvalue::BinaryOp(op, x, y) => {
			let x = float_evaluate(variables, x)?;
			let y = float_evaluate(variables, y)?;
			Ok(op.evaluate(x, y))
		}
	}
}

fn bool_evaluate<'a>(
	variables: &HashMap<&'a str, f32>,
	rvalue: &BoolRvalue<'a>,
) -> RuntimeResult<bool> {
	match rvalue {
		BoolRvalue::BoolBoolBinaryOp(op, x, y) => {
			let x = bool_evaluate(variables, x)?;
			let y = bool_evaluate(variables, y)?;
			Ok(op.evaluate(x, y))
		}
		BoolRvalue::BoolFloatBinaryOp(op, x, y) => {
			let x = float_evaluate(variables, x)?;
			let y = float_evaluate(variables, y)?;
			Ok(op.evaluate(x, y))
		}
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	UndefinedLvalue(String),
	InputParsingError(String),
}
pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub fn run<'a, Input: BufRead, Output: Write>(
	variables: &mut HashMap<&'a str, f32>,
	input: &mut Input, output: &mut Output,
	instructions: &Vec<Instructiune<'a>>,
) -> RuntimeResult<()> {
	for instruction in instructions {
		match instruction {
			Instructiune::Atribuire(lvalue, rvalue) => {
				variables.insert(lvalue.0, float_evaluate(variables, rvalue)?);
			}
			Instructiune::Interschimbare(lt_lvalue, rt_lvalue) => {
				let lt_ptr = variables.get_mut(lt_lvalue.0).ok_or(RuntimeError::UndefinedLvalue(lt_lvalue.0.into()))? as *mut f32;
				let rt_ptr = variables.get_mut(rt_lvalue.0).ok_or(RuntimeError::UndefinedLvalue(lt_lvalue.0.into()))? as *mut f32;
				unsafe {
					std::ptr::swap(lt_ptr, rt_ptr);
				}
			}
			Instructiune::Scrie(params) => {
				for param in params {
					let value = match param {
						ScrieParam::Rvalue(rvalue) => float_evaluate(variables, rvalue)?.to_string(),
						ScrieParam::CharacterLiteral(chr) => chr.to_string(),
						ScrieParam::StringLiteral(string) => string.to_string(),
					};
					output.write(value.as_bytes()).unwrap();
				}
				output.write("\n".as_bytes()).unwrap();
			}
			Instructiune::Citeste(lvalues) => {
				let mut buf = String::new();
				for lvalue in lvalues {
					input.read_line(&mut buf).unwrap();
					let value = buf.trim().parse().map_err(|_| RuntimeError::InputParsingError(buf.clone()))?;
					variables.insert(lvalue.0, value);
					buf.clear();
				}
			}
			Instructiune::DacaAtunciAltfel(conditie, atunci, altfel) => {
				if bool_evaluate(variables, conditie)? {
					run(variables, input, output, &atunci)?;
				} else {
					if let Some(altfel) = altfel {
						run(variables, input, output, &altfel)?;
					}
				}
			}
			Instructiune::CatTimpExecuta(conditie, executa) => {
				while bool_evaluate(variables, conditie)? {
					run(variables, input, output, executa)?;
				}
			}
			Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
				variables.insert(contor.0, float_evaluate(variables, start)?);
				let stop = float_evaluate(variables, stop)?;
				let increment = if let Some(increment) = increment {
					float_evaluate(variables, increment)?
				} else { 1f32 };
				while variables.get(contor.0).cloned().ok_or(RuntimeError::UndefinedLvalue(contor.0.into()))? != stop {
					run(variables, input, output, executa)?;
					*variables.get_mut(contor.0).ok_or(RuntimeError::UndefinedLvalue(contor.0.into()))? += increment;
				}
				if *variables.get(contor.0).ok_or(RuntimeError::UndefinedLvalue(contor.0.into()))? == stop {
					run(variables, input, output, executa)?;
				}
			}
			Instructiune::RepetaPanaCand(repeta, conditie) => {
				run(variables, input, output, repeta)?;
				while !bool_evaluate(variables, conditie)? {
					run(variables, input, output, repeta)?;
				}
			}
		}
	}
	Ok(())
}

