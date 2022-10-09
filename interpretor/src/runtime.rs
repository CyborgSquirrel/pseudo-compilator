use std::collections::HashMap;
use wasm_bindgen::prelude::wasm_bindgen;
use unicode_segmentation::UnicodeSegmentation;

use crate::syntax::{
	Instructiune,
	ScrieParam,
	FloatRvalue, BoolRvalue,
};

pub const EPSILON: f32 = 0.001;
pub const INSTRUCTIONS_EXECUTED_LIMIT: usize = 1_000_000;

fn float_evaluate<'a>(
	variables: &Variables,
	rvalue: &FloatRvalue<'a>,
) -> RuntimeResult<f32> {
	match rvalue {
		FloatRvalue::Literal(x) => Ok(*x),
		FloatRvalue::Lvalue(x) => variables.get(x.0).cloned(),
		FloatRvalue::Unop(op, x) => {
			let x = float_evaluate(variables, x)?;
			Ok(op.evaluate(x))
		}
		FloatRvalue::Binop(op, x, y) => {
			let x = float_evaluate(variables, x)?;
			let y = float_evaluate(variables, y)?;
			Ok(op.evaluate(x, y))
		}
	}
}

fn bool_evaluate<'a>(
	variables: &Variables,
	rvalue: &BoolRvalue<'a>,
) -> RuntimeResult<bool> {
	match rvalue {
		BoolRvalue::BoolBoolBinop(op, x, y) => {
			let x = bool_evaluate(variables, x)?;
			let y = bool_evaluate(variables, y)?;
			Ok(op.evaluate(x, y))
		}
		BoolRvalue::BoolFloatBinop(op, x, y) => {
			let x = float_evaluate(variables, x)?;
			let y = float_evaluate(variables, y)?;
			Ok(op.evaluate(x, y))
		}
	}
}

use std::iter::Peekable;
pub struct Runner<'a, 'b> {
	stack: Vec<(StackData, Peekable<std::slice::Iter<'a, Instructiune<'b>>>)>,
	instructions_executed: usize,
}

pub struct Variables<'a> {
	variables: HashMap<&'a str, f32>,
}

impl<'a> Variables<'a> {
	pub fn new() -> Self {
		Variables { variables: HashMap::new() }
	}
	fn assign(&mut self, name: &'a str, value: f32) {
		self.variables.insert(name, value);
	}
	fn get(&self, name: &str) -> RuntimeResult<&f32> {
		self.variables.get(name).ok_or(RuntimeError::UndefinedLvalue(name.into()))
	}
	fn get_mut(&mut self, name: &str) -> RuntimeResult<&mut f32> {
		self.variables.get_mut(name).ok_or(RuntimeError::UndefinedLvalue(name.into()))
	}
	fn swap(&mut self, lhs: &str, rhs: &str) -> RuntimeResult<()> {
		let lhs = self.variables.get_mut(lhs).ok_or(RuntimeError::UndefinedLvalue(lhs.into()))? as *mut f32;
		let rhs = self.variables.get_mut(rhs).ok_or(RuntimeError::UndefinedLvalue(rhs.into()))? as *mut f32;
		unsafe { std::ptr::swap(lhs, rhs) }
		Ok(())
	}
}

#[derive(Debug)]
enum StackData {
	Nothing,
	PentruExecutaVariableWasSet,
	RepetaPanaCandWasExecutedOnce,
	CitesteParamIndex(usize),
}

#[wasm_bindgen]
#[derive(Debug,PartialEq, Eq)]
pub enum RuntimeState {
	Running,
	WaitingForInput,
	Finished,
}

impl<'a, 'b> Runner<'a, 'b> {
	pub fn new(instructions: &'a Vec<Instructiune<'b>>) -> Self {
		Self {
			stack: vec![ (StackData::Nothing, instructions.iter().peekable()) ],
			instructions_executed: 0,
		}
	}
	
	pub fn run_once(
		&mut self,
		variables: &mut Variables<'b>,
		input: &mut String, output: &mut String,
	) -> RuntimeResult<RuntimeState> {
		if self.instructions_executed > INSTRUCTIONS_EXECUTED_LIMIT {
			Err(RuntimeError::InstructionLimitExceeded)
		} else {
			while let Some(None) = self.stack.last_mut().map(|a| a.1.peek_mut()) {
				self.stack.pop();
			}
			if let Some((data, last)) = self.stack.last_mut() {
				let instruction = last.peek().unwrap();
				match instruction {
					Instructiune::Atribuire(lvalue, rvalue) => {
						variables.assign(lvalue.0, float_evaluate(variables, rvalue)?);
						last.next();
						self.instructions_executed += 1;
						Ok(RuntimeState::Running)
					}
					Instructiune::Interschimbare(lt_lvalue, rt_lvalue) => {
						variables.swap(lt_lvalue.0, rt_lvalue.0)?;
						last.next();
						self.instructions_executed += 1;
						Ok(RuntimeState::Running)
					}
					Instructiune::Scrie(params) => {
						for param in params {
							match param {
								ScrieParam::Rvalue(rvalue) =>
									output.push_str(float_evaluate(variables, rvalue)?.to_string().as_str()),
								ScrieParam::CharacterLiteral(chr) =>
									output.push_str(chr),
								ScrieParam::StringLiteral(string) =>
									output.push_str(string),
							}
						}
						output.push_str("\n");
						last.next();
						self.instructions_executed += 1;
						Ok(RuntimeState::Running)
					}
					Instructiune::Citeste(lvalues) => {
						if !matches!(data, StackData::CitesteParamIndex(..)) {
							*data = StackData::CitesteParamIndex(0);
						}
						if let StackData::CitesteParamIndex(index) = data {
							while *index < lvalues.len() {
								let mut graphemes = input.grapheme_indices(true)
									.skip_while(|(_, grapheme)| matches!(*grapheme, "\n"|" "));
								if let Some((index, _)) = graphemes.next() {
									*input = input.split_off(index);
								} else {
									input.clear();
									return Ok(RuntimeState::WaitingForInput);
								}
								
								let mut graphemes = input.grapheme_indices(true)
									.skip_while(|(_, grapheme)| !matches!(*grapheme, "\n"|" "));
								let value = if let Some((index, _)) = graphemes.next() {
									let mut value = input.split_off(index);
									std::mem::swap(&mut value, input);
									value
								} else { return Ok(RuntimeState::WaitingForInput) };
								
								let value = value.parse().map_err(|_| RuntimeError::InputParsingError(value.clone()))?;
								variables.assign(lvalues[*index].0, value);
								
								*index += 1;
							}
						}
						
						// we only ever get here if all the lvalues have been read
						*data = StackData::Nothing;
						last.next();
						self.instructions_executed += 1;
						Ok(RuntimeState::Running)
					}
					Instructiune::DacaAtunciAltfel(conditie, atunci, altfel) => {
						last.next();
						if bool_evaluate(variables, conditie)? {
							self.stack.push((StackData::Nothing, atunci.iter().peekable()));
						} else {
							if let Some(altfel) = altfel {
								self.stack.push((StackData::Nothing, altfel.iter().peekable()));
							}
						}
						Ok(RuntimeState::Running)
					}
					Instructiune::CatTimpExecuta(conditie, executa) => {
						if bool_evaluate(variables, conditie)? {
							self.stack.push((StackData::Nothing, executa.iter().peekable()));
						} else {
							last.next();
						}
						Ok(RuntimeState::Running)
					}
					Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
						let increment = if let Some(increment) = increment {
							float_evaluate(variables, increment)?
						} else { 1f32 };
						
						if !matches!(*data, StackData::PentruExecutaVariableWasSet) {
							*data = StackData::PentruExecutaVariableWasSet;
							variables.assign(contor.0, float_evaluate(variables, start)?);
						} else {
							*variables.get_mut(contor.0)? += increment;
						}
						
						let stop = float_evaluate(variables, stop)?;
						let contor = *variables.get(contor.0)?;
						if (increment > 0f32 && contor < stop) || (increment < 0f32 && contor > stop) {
							self.stack.push((StackData::Nothing, executa.iter().peekable()));
						} else {
							*data = StackData::Nothing;
							last.next();
							if (contor - stop).abs() <= EPSILON {
								self.stack.push((StackData::Nothing, executa.iter().peekable()));
							}
						}
						Ok(RuntimeState::Running)
					}
					Instructiune::RepetaPanaCand(repeta, conditie) => {
						if !matches!(*data, StackData::RepetaPanaCandWasExecutedOnce) {
							*data= StackData::RepetaPanaCandWasExecutedOnce;
							self.stack.push((StackData::Nothing, repeta.iter().peekable()));
						} else if !bool_evaluate(variables, conditie)? {
							self.stack.push((StackData::Nothing, repeta.iter().peekable()));
						} else {
							*data = StackData::Nothing;
							last.next();
						}
						Ok(RuntimeState::Running)
					}
				}
			} else {
				Ok(RuntimeState::Finished)
			}
		}
	}
	
	pub fn run(
		&mut self,
		variables: &mut Variables<'b>,
		input: &mut String, output: &mut String,
	) -> RuntimeResult<RuntimeState> {
		loop {
			let state = self.run_once(variables, input, output)?;
			if state != RuntimeState::Running {
				return Ok(state);
			}
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
	UndefinedLvalue(String),
	InputParsingError(String),
	InstructionLimitExceeded,
}
pub type RuntimeResult<T> = Result<T, RuntimeError>;
