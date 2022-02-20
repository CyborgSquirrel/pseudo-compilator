mod expression;
use expression::*;

// use std::env;

use std::{fs::File, io::{Read, Write, BufRead, stdin, stdout, BufReader}};
use unicode_segmentation::UnicodeSegmentation;
use std::collections::HashMap;

#[derive(Debug)]
pub enum GraphemeKind { Reserved, Ignored, Other }
pub fn get_grapheme_kind(grapheme: &str) -> Option<GraphemeKind> {
	match grapheme {
		"+"|"-"|"*"|"/"|"%"|
		"="|"!"|"<"|">"|"|"|
		"("|")"|"["|"]"
			=> Some(GraphemeKind::Reserved),
		" "
			=> Some(GraphemeKind::Ignored),
		","
			=> None,
		_
			=> Some(GraphemeKind::Other),
	}
}

#[derive(Debug)]
enum LineParsingErrorKind {
	ExpectedStr(&'static str),
	ExpectedGrapheme(&'static str),
	ExpectedAnyGrapheme,
	ExpectedEnd,
	ExpectedLvalue,
	ExpectedScrieParam,
	ExpectedBoolRvalue,
	ExpectedFloatRvalue,
	ExpressionParsingError(ExpressionConstructionError),
	ExpectationError,
}

#[derive(Debug)]
struct LineParsingError(usize, LineParsingErrorKind);
type LineParsingIntermediateResult<'a, T> = Result<(LineCursor<'a>, T), LineParsingError>;
type LineParsingResult<T> = Result<T, LineParsingError>;

#[derive(Debug, Clone, Copy)]
struct LineCursor<'a> {
	pub code: &'a str,
	pub index: usize,
	pub grapheme: usize,
}
impl<'a> LineCursor<'a> {
	fn new(code: &'a str) -> Self {
		Self { code, index: 0, grapheme: 0 }
	}
	fn make_error(&self, kind: LineParsingErrorKind) -> LineParsingError {
		LineParsingError(self.grapheme, kind)
	}
	fn code(&self) -> &'a str {
		&self.code[self.index..]
	}
	fn code_until(&self, until: usize) -> &'a str {
		&self.code[self.index..self.index+until]
	}
	fn advance_by(&mut self, amount: usize) {
		self.grapheme += self.code_until(amount).graphemes(true).count();
		self.index += amount;
	}
	fn expect_str(mut self, expected: &'static str) -> LineParsingIntermediateResult<'a, ()> {
		if self.code().starts_with(expected) {
			self.advance_by(expected.len());
			Ok((self, ()))
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedStr(expected))) }
	}
	fn skip_spaces(mut self) -> Self {
		let mut graphemes = 0;
		let offset = {
			let code = self.code();
			code.grapheme_indices(true)
				.find(|(_, x)| { graphemes += 1; *x != " "})
				.map(|(i, _)| i)
				.unwrap_or(code.len())
		};
		self.grapheme += graphemes;
		self.index += offset;
		self
	}
	fn read_while<P: FnMut(&str) -> bool>(mut self, mut predicate: P) -> (Self, &'a str) {
		let mut graphemes = 0;
		let offset = self.code().grapheme_indices(true)
			.skip_while(|(_, x)| { graphemes += 1; predicate(x)})
			.next()
			.map(|(i, _)| i)
			.unwrap_or(self.code().len());
		let result = self.code_until(offset);
		self.grapheme += graphemes;
		self.index += offset;
		(self, result)
	}
	fn parse_lvalue(self) -> LineParsingIntermediateResult<'a, Lvalue<'a>> {
		let (new_self, name) = self.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
		if !name.is_empty() {
			Ok((new_self, Lvalue(name)))
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedLvalue)) }
	}
	fn expect_end(self) -> LineParsingResult<()> {
		if self.code.len() == self.index {
			Ok(())
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedEnd)) }
	}
	fn parse_second_step_citeste(mut self) -> LineParsingResult<Instructiune<'a>> {
		let mut lvalues = Vec::new();
		let mut done = false;
		while !done {
			let (new_self, lvalue) = self.parse_lvalue()?;
			self = new_self;
			lvalues.push(lvalue);
			self = self.skip_spaces();
			match self.expect_grapheme(",") {
				Ok((new_self, _)) => self = new_self,
				Err(..) => done = true,
			}
		}
		self.skip_spaces().expect_end()?;
		Ok(Instructiune::Citeste(lvalues))
	}
	fn next_grapheme(mut self) -> LineParsingIntermediateResult<'a, &'a str> {
		let grapheme = self.code()
			.graphemes(true)
			.next();
		if let Some(grapheme) = grapheme {
			self.index += grapheme.len();
			self.grapheme += 1;
			Ok((self, grapheme))
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedAnyGrapheme)) }
	}
	fn expect_grapheme(self, expected_grapheme: &'static str) -> LineParsingIntermediateResult<'a, ()> {
		let err = LineParsingErrorKind::ExpectedGrapheme(expected_grapheme);
		if let Ok((new_self, grapheme)) = self.next_grapheme() {
			if grapheme == expected_grapheme { Ok((new_self, ())) }
			else { Err(self.make_error(err)) }
		} else { Err(self.make_error(err)) }
	}
	fn parse_scrie_param(self) -> LineParsingIntermediateResult<'a, ScrieParam<'a>> {
		match self.next_grapheme().map_err(|_| self.make_error(LineParsingErrorKind::ExpectedScrieParam))? {
			(new_self, "'") => {
				let (new_self, character) = new_self.next_grapheme()?;
				let (new_self, _) = new_self.expect_grapheme("'")?;
				Ok((new_self, ScrieParam::CharLiteral(character)))
			}
			(new_self, "\"") => {
				let (new_self, string) = new_self.read_while(|grapheme| grapheme != "\"");
				let (new_self, _) = new_self.expect_grapheme("\"")?;
				Ok((new_self, ScrieParam::StringLiteral(string)))
			}
			_ => {
				let (new_self, rvalue) = self.parse_float_rvalue()?;
				Ok((new_self, ScrieParam::Rvalue(rvalue)))
			}
		}
	}
	fn parse_second_step_scrie(mut self) -> LineParsingResult<Instructiune<'a>> {
		let mut params = Vec::new();
		let mut done = false;
		while !done {
			let (new_self, param) = self.skip_spaces().parse_scrie_param()?;
			params.push(param);
			self = new_self.skip_spaces();
			match self.expect_grapheme(",") {
				Ok((new_self, _)) => self = new_self,
				Err(..) => done = true,
			}
		}
		self.skip_spaces().expect_end()?;
		Ok(Instructiune::Scrie(params))
	}
	
	fn parse_second_step_lvalue(self, lvalue: Lvalue<'a>) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, _) = self.skip_spaces().expect_str("<-")?;
		if let Ok((new_self, _)) = new_self.expect_grapheme(">") {
			let (new_self, other_lvalue) = new_self.skip_spaces().parse_lvalue()?;
			new_self.skip_spaces().expect_end()?;
			Ok(Instructiune::Interschimbare(lvalue, other_lvalue))
		} else {
			let (new_self, rvalue) = new_self.skip_spaces().parse_float_rvalue()?;
			new_self.skip_spaces().expect_end()?;
			Ok(Instructiune::Atribuire(lvalue, rvalue))
		}
	}
	
	fn parse_second_step_pentru(self) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, lvalue) =  self.skip_spaces().parse_lvalue()?;
		let (new_self, _) = new_self.skip_spaces().expect_str("<-")?;
		let (new_self, start) = new_self.skip_spaces().parse_float_rvalue()?;
		
		let (new_self, _) = new_self.skip_spaces().expect_str(",")?;
		let (new_self, end) = new_self.skip_spaces().parse_float_rvalue()?;
		
		let (new_self, increment) = if let Ok((new_self, _)) = new_self.skip_spaces().expect_str(",") {
			new_self.parse_float_rvalue().map(|(new_self, increment)| (new_self, Some(increment)))?
		} else { (new_self, None) };
		
		let (new_self, _) = new_self.skip_spaces().expect_str("executa")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::PentruExecuta(lvalue, start, end, increment, Vec::new()))
	}
	
	fn parse_second_step_daca(self) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, rvalue) = self.skip_spaces().parse_bool_rvalue()?;
		let (new_self, _) = new_self.skip_spaces().expect_str("atunci")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::DacaAtunciAltfel(rvalue, Vec::new(), None))
	}
	
	fn parse_second_step_cat_timp(self) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, _) = self.expect_str(" timp ")?;
		let (new_self, rvalue) = new_self.skip_spaces().parse_bool_rvalue()?;
		let (new_self, _) = new_self.skip_spaces().expect_str("executa")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::CatTimpExecuta(rvalue, Vec::new()))
	}
	
	fn parse_pana_cand(self, instructions: Vec<Instructiune<'a>>) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, _) = self.expect_str("pana cand ")?;
		let (new_self, rvalue) = new_self.skip_spaces().parse_bool_rvalue()?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::RepetaPanaCand(instructions, rvalue))
	}
	
	fn parse_altfel(self) -> LineParsingResult<()> {
		let (new_self, _) = self.expect_str("altfel")?;
		new_self.skip_spaces().expect_end()?;
		Ok(())
	}
	
	fn parse_repeta(self) -> LineParsingResult<()> {
		let (new_self, _) = self.expect_str("repeta")?;
		new_self.skip_spaces().expect_end()?;
		Ok(())
	}
	
	fn parse_first_step(self) -> LineParsingResult<Instructiune<'a>> {
		let (cursor, name) = self.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
		match dbg!(name) {
			"daca" =>
				cursor.parse_second_step_daca(),
			"cat" =>
				cursor.parse_second_step_cat_timp(),
			"pentru" =>
				cursor.parse_second_step_pentru(),
			"scrie" =>
				cursor.parse_second_step_scrie(),
			"citeste" =>
				cursor.parse_second_step_citeste(),
			x =>
				cursor.parse_second_step_lvalue(Lvalue(x)),
		}
	}
}

#[derive(Debug)]
enum ParsingErrorKind {
	LineParsingError(LineParsingErrorKind),
	AltfelWithoutDaca,
	DacaAlreadyHasAltfel,
}
#[derive(Debug)]
struct ParsingError(usize, usize, ParsingErrorKind);
type IntermediateParsingResult<'a> = Result<Cursor<'a>, ParsingError>;
type ParsingResult<T> = Result<T, ParsingError>;

#[derive(Debug, Clone, Copy)]
struct Cursor<'a> {
	code: &'a str,
	line: usize,
	index: usize,
}

impl<'a> Cursor<'a> {
	fn new(code: &'a str) -> Self {
		Self { code, line: 0, index: 0 }
	}
	fn next_line(mut self) -> Option<(Self, &'a str)> {
		if let Some(offset) = self.code[self.index..].find("\n") {
			let line = &self.code[self.index..self.index+offset];
			self.index += offset + "\n".len();
			self.line += 1;
			Some((self, line))
		} else if self.index < self.code.len() {
			let line = &self.code[self.index..];
			self.index = self.code.len();
			self.line += 1;
			Some((self, line))
		} else { None }
	}
	fn make_error_from_line(&self, line_parsing_error: LineParsingError) -> ParsingError {
		ParsingError(
			self.line,
			line_parsing_error.0,
			ParsingErrorKind::LineParsingError(line_parsing_error.1)
		)
	}
	fn make_error(&self, kind: ParsingErrorKind) -> ParsingError {
		ParsingError(self.line, 0, kind)
	}
	fn parse(mut self, instructions: &mut Vec<Instructiune<'a>>, indent: usize) -> IntermediateParsingResult<'a> {
		enum Expecting<'a> { Anything, PanaCand(Vec<Instructiune<'a>>) }
		let mut expecting = Expecting::Anything;
		while let Some((new_self, line)) = self.next_line() {
			if line != "" {
				let current_indent = line
					.graphemes(true)
					.take_while(|x| *x == "\t")
					.count();
				// TODO: take care of case where current_indent is bigger by more than one than indent
				// TODO: user-specified separator for scrie
				// TODO: unary operators
				// TODO: paranthese unary operators
				if current_indent != indent {
					break;
				} else {
					self = new_self;
					let line = &line[current_indent*"\t".len()..];
					let line_cursor = LineCursor::new(line);
					println!("{:?}", line);
					expecting = match expecting {
						Expecting::Anything => {
							if line_cursor.parse_altfel().is_ok() {
								if let Some(Instructiune::DacaAtunciAltfel(_, _, instructions)) = instructions.last_mut() {
									if instructions.is_some() { return Err(self.make_error(ParsingErrorKind::DacaAlreadyHasAltfel)) }
									*instructions = Some({
										let mut instructions = Vec::new();
										self = self.parse(&mut instructions, indent+1)?;
										instructions
									});
								} else { return Err(self.make_error(ParsingErrorKind::AltfelWithoutDaca)) }
								Expecting::Anything
							} else if line_cursor.parse_repeta().is_ok() {
								let mut instructions = Vec::new();
								self = self.parse(&mut instructions, indent+1)?;
								Expecting::PanaCand(instructions)
							} else {
								let mut instruction = line_cursor.parse_first_step()
									.map_err(|err| self.make_error_from_line(err))?;
								match &mut instruction {
									Instructiune::DacaAtunciAltfel(_, instructions, _)
									| Instructiune::CatTimpExecuta(_, instructions)
									| Instructiune::PentruExecuta(_, _, _, _, instructions)
									| Instructiune::RepetaPanaCand(instructions, _)
										=> self = self.parse(instructions, indent+1)?,
									Instructiune::Atribuire(..)
									| Instructiune::Interschimbare(..)
									| Instructiune::Scrie(..)
									| Instructiune::Citeste(..)
										=> (), // do nothing 
								}
								instructions.push(instruction);
								Expecting::Anything
							}
						}
						Expecting::PanaCand(pana_cand_instructions) => {
							instructions.push(line_cursor.parse_pana_cand(pana_cand_instructions).unwrap());
							Expecting::Anything
						}
					}
				}
			}
		}
		Ok(self)
	}
}

#[derive(Debug)]
enum ScrieParam<'a> {
	Rvalue(FloatRvalue<'a>),
	StringLiteral(&'a str),
	CharLiteral(&'a str),
}
#[derive(Debug)]
enum Instructiune<'a> {
	Atribuire(Lvalue<'a>, FloatRvalue<'a>),
	Interschimbare(Lvalue<'a>, Lvalue<'a>),
	Scrie(Vec<ScrieParam<'a>>),
	Citeste(Vec<Lvalue<'a>>),
	DacaAtunciAltfel(BoolRvalue<'a>, Vec<Instructiune<'a>>, Option<Vec<Instructiune<'a>>>),
	CatTimpExecuta(BoolRvalue<'a>, Vec<Instructiune<'a>>),
	PentruExecuta(Lvalue<'a>, FloatRvalue<'a>, FloatRvalue<'a>, Option<FloatRvalue<'a>>, Vec<Instructiune<'a>>),
	RepetaPanaCand(Vec<Instructiune<'a>>, BoolRvalue<'a>),
}

fn float_evaluate<'a>(
	variables: &HashMap<&'a str, f32>,
	rvalue: &FloatRvalue<'a>,
) -> RuntimeResult<'a, f32> {
	match rvalue {
		FloatRvalue::Literal(x) => Ok(*x),
		FloatRvalue::Lvalue(x) => variables.get(x.0).cloned().ok_or(RuntimeError::UndefinedLvalue(x.0)),
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
) -> RuntimeResult<'a, bool> {
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
enum RuntimeError<'a> {
	UndefinedLvalue(&'a str),
	InputParsingError(String),
}
type RuntimeResult<'a, T> = Result<T, RuntimeError<'a>>;

fn execute<'a, Input: BufRead, Output: Write>(
	variables: &mut HashMap<&'a str, f32>,
	input: &mut Input, output: &mut Output,
	instructions: &Vec<Instructiune<'a>>,
) -> RuntimeResult<'a, ()> {
	for instruction in instructions {
		match instruction {
			Instructiune::Atribuire(lvalue, rvalue) => {
				variables.insert(lvalue.0, float_evaluate(variables, rvalue)?);
			}
			Instructiune::Interschimbare(lt_lvalue, rt_lvalue) => {
				let lt_ptr = variables.get_mut(lt_lvalue.0).ok_or(RuntimeError::UndefinedLvalue(lt_lvalue.0))? as *mut f32;
				let rt_ptr = variables.get_mut(rt_lvalue.0).ok_or(RuntimeError::UndefinedLvalue(lt_lvalue.0))? as *mut f32;
				unsafe {
					std::ptr::swap(lt_ptr, rt_ptr);
				}
			}
			Instructiune::Scrie(params) => {
				for param in params {
					let value = match param {
						ScrieParam::Rvalue(rvalue) => float_evaluate(variables, rvalue)?.to_string(),
						ScrieParam::CharLiteral(chr) => chr.to_string(),
						ScrieParam::StringLiteral(string) => string.to_string(),
					};
					output.write(value.as_bytes()).unwrap();
				}
				output.write("\n".as_bytes()).unwrap();
			}
			Instructiune::Citeste(lvalues) => {
				let mut buf  = String::new();
				for lvalue in lvalues {
					input.read_line(&mut buf).unwrap();
					let value = buf.trim().parse().map_err(|_| RuntimeError::InputParsingError(buf.clone()))?;
					variables.insert(lvalue.0, value);
					buf.clear();
				}
			}
			Instructiune::DacaAtunciAltfel(conditie, atunci, altfel) => {
				if bool_evaluate(variables, conditie)? {
					execute(variables, input, output, &atunci)?;
				} else {
					if let Some(altfel) = altfel {
						execute(variables, input, output, &altfel)?;
					}
				}
			}
			Instructiune::CatTimpExecuta(conditie, executa) => {
				while bool_evaluate(variables, conditie)? {
					execute(variables, input, output, executa)?;
				}
			}
			Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
				variables.insert(contor.0, float_evaluate(variables, start)?);
				let stop = float_evaluate(variables, stop)?;
				let increment = if let Some(increment) = increment {
					float_evaluate(variables, increment)?
				} else { 1f32 };
				while variables.get(contor.0).cloned().ok_or(RuntimeError::UndefinedLvalue(contor.0))? != stop {
					execute(variables, input, output, executa)?;
					*variables.get_mut(contor.0).ok_or(RuntimeError::UndefinedLvalue(contor.0))? += increment;
				}
				if *variables.get(contor.0).ok_or(RuntimeError::UndefinedLvalue(contor.0))? == stop {
					execute(variables, input, output, executa)?;
				}
			}
			Instructiune::RepetaPanaCand(repeta, conditie) => {
				execute(variables, input, output, repeta)?;
				while !bool_evaluate(variables, conditie)? {
					execute(variables, input, output, repeta)?;
				}
			}
		}
	}
	Ok(())
}

// fn test_execute() {
// 	let instructions = vec![
// 		Instructiune::Atribuire(Lvalue("a"), FloatRvalue::Literal(10f32)),
// 		Instructiune::Atribuire(Lvalue("b"), FloatRvalue::Literal(5f32)),
// 		Instructiune::Interschimbare(Lvalue("a"), Lvalue("b")),
// 		Instructiune::Scrie(
// 			vec![
// 				ScrieParam::Rvalue(
// 					FloatRvalue::Binary(
// 						FloatBinaryOp::Add,
// 						Box::new(FloatRvalue::Binary(
// 							FloatBinaryOp::Add,
// 							Box::new(FloatRvalue::Lvalue(Lvalue("a"))),
// 							Box::new(FloatRvalue::Lvalue(Lvalue("b"))),
// 						)),
// 						Box::new(FloatRvalue::Literal(3f32)),
// 					)
// 				)
// 			]
// 		)
// 	];

// 	let mut variables = HashMap::new();
// 	execute(&mut variables, &instructions);
// }

fn parse<'a>(code: &'a str) -> ParsingResult<Vec<Instructiune<'a>>> {
	let mut program = Vec::new();
	let cursor = Cursor::new(&code);
	cursor.parse(&mut program, 0).map(|_| program)
}

fn main() {
	// let args: Vec<String> = env::args().collect();
	
	let program_path = "./prog";
	let mut program_file = File::open(program_path).unwrap();
	
	let mut code = String::new();
	program_file.read_to_string(&mut code).unwrap();
	
	let program = parse(code.as_str());
	match program {
		Err(ParsingError(line, grapheme, err)) => println!("[{:?}:{:?}] Eroare la parsare: {:?}", line, grapheme, err),
		Ok(program) => {
			let mut variables = HashMap::new();
			let result = execute(&mut variables, &mut BufReader::new(stdin()), &mut stdout(), &program);
			match result {
				Err(err) => println!("Eroare la rulare: {:?}", err),
				Ok(..) => (),
			}
		}
	}
}
