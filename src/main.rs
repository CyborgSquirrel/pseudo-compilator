mod expression;
use expression::*;

// use std::env;

use std::{fs::File, io::Read, iter};
use itertools::izip;
use unicode_segmentation::UnicodeSegmentation;
use std::collections::HashMap;

#[derive(Clone, Copy)]
// We have one cursor per line.
struct Cursor<'a> {
	code: &'a str,
	index: usize,
}
impl<'a> Cursor<'a> {
	fn new(code: &'a str) -> Self {
		Self { code, index: 0 }
	}
	fn code(&self) -> &'a str {
		&self.code[self.index..]
	}
	fn expect_str(mut self, expected: &str) -> CompilationResult<Self> {
		if self.code().starts_with(expected) {
			self.index += expected.len();
			Ok(self)
		} else { Err(CompilationError::ExpectationError) }
	}
	fn next_grapheme_matches<P: FnMut(&str) -> bool>(mut self, mut predicate: P) -> CompilationResult<Self> {
		let next = self.code().grapheme_indices(true).next();
			
		if let Some(next) = next {
			self.index += next.1.len();
			if predicate(next.1) { Ok(self) }
			else { Err(CompilationError::ExpectationError) }
		} else { Err(CompilationError::ExpectationError) }
	}
	fn skip_spaces(mut self) -> Self {
		let offset = {
			let code = self.code();
			code.grapheme_indices(true)
				.find(|(_, x)| *x != " ")
				.map(|(i, _)| i)
				.unwrap_or(code.len())
		};
		self.index += offset;
		self
	}
	fn read_until<P: FnMut(&str) -> bool>(mut self, mut predicate: P) -> (Self, &'a str) {
		let code = self.code();
		let end = code.grapheme_indices(true)
			.find(|(_, x)| predicate(x))
			.map(|(i, _)| i)
			.unwrap_or(code.len());
		self.index += end;
		(self, &code[..end])
	}
	fn parse_lvalue(self) -> CompilationResult<(Self, Lvalue<'a>)> {
		let (new_self, name) = self.skip_spaces().read_until(|x| not_variable(x));
		if !name.is_empty() {
			Ok((new_self, Lvalue(name)))
		} else { Err(CompilationError::ExpectationError) }
	}
	fn expect_end(self) -> CompilationResult<()> {
		if self.code.len() == self.index {
			Ok(())
		} else { Err(CompilationError::ExpectationError) }
	}
}

#[derive(Debug)]
enum CompilationError {
	ExpectationError,
}
type CompilationResult<T> = Result<T, CompilationError>;

fn is_whitespace(x: &(usize, &str)) -> bool { x.1 == " "  }

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

fn float_evaluate(
	variables: &HashMap<&str, f32>,
	rvalue: &FloatRvalue,
) -> f32 {
	match rvalue {
		FloatRvalue::Literal(x) => *x,
		FloatRvalue::Lvalue(x) => *variables.get(x.0).unwrap(),
		FloatRvalue::Unary(op, x) => {
			let x = float_evaluate(variables, x);
			match *op {
				FloatUnaryOp::Ident => x,
				FloatUnaryOp::Neg => -x,
				FloatUnaryOp::Whole => x.floor(),
			}
		}
		FloatRvalue::Binary(op, x, y) => {
			let x = float_evaluate(variables, x);
			let y = float_evaluate(variables, y);
			match *op {
				FloatBinaryOp::Add => x+y,
				FloatBinaryOp::Sub => x-y,
				FloatBinaryOp::Mul => x*y,
				FloatBinaryOp::Div => x/y,
				FloatBinaryOp::Rem => ((x as i32) % (y as i32)) as f32,
			}
		}
	}
}

fn bool_evaluate<'a>(
	variables: &HashMap<&str, f32>,
	rvalue: &BoolRvalue,
) -> bool {
	match rvalue {
		BoolRvalue::BoolBoolBinaryOp(op, lhs, rhs) => {
			let lhs = bool_evaluate(variables, lhs);
			let rhs = bool_evaluate(variables, rhs);
			match op {
				BoolBoolBinaryOp::And => lhs && rhs,
				BoolBoolBinaryOp::Or => lhs || rhs,
			}
		}
		BoolRvalue::BoolFloatBinaryOp(op, lhs, rhs) => {
			let lhs = float_evaluate(variables, lhs);
			let rhs = float_evaluate(variables, rhs);
			match op {
				BoolFloatBinaryOp::Equ => lhs == rhs,
				BoolFloatBinaryOp::Nequ => lhs != rhs,
				BoolFloatBinaryOp::Lt => lhs < rhs,
				BoolFloatBinaryOp::Gt => lhs > rhs,
				BoolFloatBinaryOp::Lte => lhs <= rhs,
				BoolFloatBinaryOp::Gte => lhs >= rhs,
				BoolFloatBinaryOp::Divides => (lhs as i32) % (rhs as i32) == 0,
			}
		}
	}
}

fn execute<'a>(
	variables: &mut HashMap<&'a str, f32>,
	instructions: &Vec<Instructiune<'a>>,
) {
	use std::io::stdin;
	for instruction in instructions {
		match instruction {
			Instructiune::Atribuire(lvalue, rvalue) => {
				variables.insert(lvalue.0, float_evaluate(variables, rvalue));
			}
			Instructiune::Interschimbare(lt_lvalue, rt_lvalue) => {
				let lt_ptr = variables.get_mut(lt_lvalue.0).unwrap() as *mut f32;
				let rt_ptr = variables.get_mut(rt_lvalue.0).unwrap() as *mut f32;
				unsafe {
					std::ptr::swap(lt_ptr, rt_ptr);
				}
			}
			Instructiune::Scrie(params) => {
				for param in params {
					let value = match param {
						ScrieParam::Rvalue(rvalue) => float_evaluate(variables, rvalue).to_string(),
						ScrieParam::CharLiteral(chr) => chr.to_string(),
						ScrieParam::StringLiteral(string) => string.to_string(),
					};
					print!("{}", value);
				}
				print!("\n");
			}
			Instructiune::Citeste(lvalues) => {
				let mut input = String::new();
				for lvalue in lvalues {
					stdin().read_line(&mut input).unwrap();
					variables.insert(lvalue.0, input.trim().parse().unwrap());
					input.clear();
				}
			}
			Instructiune::DacaAtunciAltfel(conditie, atunci, altfel) => {
				if bool_evaluate(variables, conditie) {
					execute(variables, &atunci);
				} else {
					if let Some(altfel) = altfel {
						execute(variables, &altfel);
					}
				}
			}
			Instructiune::CatTimpExecuta(conditie, executa) => {
				while bool_evaluate(variables, conditie) {
					execute(variables, executa);
				}
			}
			Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
				variables.insert(contor.0, float_evaluate(variables, start));
				let stop = float_evaluate(variables, stop);
				let increment = increment
					.as_ref()
					.map(|x| float_evaluate(variables, x))
					.unwrap_or(1f32);
				while *variables.get(contor.0).unwrap() != stop {
					execute(variables, executa);
					*variables.get_mut(contor.0).unwrap() += increment;
				}
				if *variables.get(contor.0).unwrap() == stop {
					execute(variables, executa);
				}
			}
			Instructiune::RepetaPanaCand(repeta, conditie) => {
				execute(variables, repeta);
				while !bool_evaluate(variables, conditie) {
					execute(variables, repeta);
				}
			}
		}
	}
}

fn not_variable(grapheme: &str) -> bool {
	return matches!(
		grapheme,
		" "
		|"<"|">"|"="|"!"
		|"-"|"+"|"*"|"/"|"%"
		|"("|")"
		|","|";"
		|"\""|"'"
	);
}



fn parse_scrie_param<'a>(code: &'a str) -> (ScrieParam<'a>, &'a str) {
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	match graphemes.next() {
		Some((_, "'")) => {
			let chr = graphemes.next().unwrap().1;
			assert!(matches!(graphemes.next(), Some((_, "'"))));
			let code =
				if let Some((i, _)) = graphemes.next() { &code[i..] }
				else { &code[code.len()..] };
			(ScrieParam::CharLiteral(chr), code)
		}
		Some((start, "\"")) => {
			let start = start + "\"".len();
			let mut graphemes = graphemes.skip_while(|x| x.1 != "\"");
			let next = graphemes.next().unwrap();
			assert!(next.1 == "\"");
			let end = next.0;
			(ScrieParam::StringLiteral(&code[start..end]), &code[end+"\"".len()..])
		}
		Some(_) => {
			let (rvalue, code) = parse_float_rvalue(code);
			(ScrieParam::Rvalue(rvalue), code)
		}
		None => panic!(),
	}
}

fn parse_second_step_scrie<'a>(mut code: &'a str) -> Instructiune<'a> {
	let mut params = Vec::new();
	let mut done = false;
	while !done {
		let (param, new_code) = parse_scrie_param(code);
		params.push(param);
		code = new_code;
		let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
		match graphemes.next() {
			Some((i, ",")) => code = &code[i+",".len()..],
			None => done = true,
			_ => panic!(),
		}
	}
	Instructiune::Scrie(params)
}

fn parse_lvalue<'a>(code: &'a str) -> (Lvalue, &'a str) {
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	let next = graphemes.next().unwrap();
	
	let cursor = next.0;
	let graphemes = iter::once(next).chain(graphemes);
	let mut graphemes = graphemes.skip_while(|x| !not_variable(x.1));
	
	let end = 
		if let Some((end, _)) = graphemes.next() { end }
		else { code.len() };
	
	(Lvalue(&code[cursor..end]), &code[end..])
}

fn parse_second_step_citeste<'a>(mut code: &'a str) -> Instructiune<'a> {
	let mut lvalues = Vec::new();
	let mut done = false;
	while !done {
		let (lvalue, new_code) = parse_lvalue(code);
		lvalues.push(lvalue);
		code = new_code;
		let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
		match graphemes.next() {
			Some((i, ",")) => code = &code[i+",".len()..],
			None => done = true,
			_ => panic!(),
		}
	}
	Instructiune::Citeste(lvalues)
}

fn parse_second_step_lvalue<'a>(mut cursor: Cursor<'a>, lvalue: Lvalue<'a>) -> Instructiune<'a> {
	let cursor = cursor
		.skip_spaces()
		.expect_str("<-").unwrap();
	if let Ok(cursor) = cursor.next_grapheme_matches(|x| x == ">") {
		let other_lvalue = cursor.parse_lvalue().unwrap().1;
		Instructiune::Interschimbare(lvalue, other_lvalue)
	} else {
		let rvalue = parse_float_rvalue(cursor.code()).0;
		Instructiune::Atribuire(lvalue, rvalue)
	}
}

fn parse_second_step_pentru<'a>(code: &'a str) -> Instructiune<'a> {
	let (lvalue, code) = parse_lvalue(code);
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	assert!(matches!(graphemes.next(), Some((_, "<"))));
	assert!(matches!(graphemes.next(), Some((_, "-"))));
	let next = graphemes.next().unwrap();
	let code = &code[next.0..];
	let (start, code) = parse_float_rvalue(code);
	
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	let next = graphemes.next().unwrap();
	assert!(next.1 == ",");
	
	let code = &code[next.0 + next.1.len()..];
	let (end, code) = parse_float_rvalue(code);
	
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	
	let mut next = graphemes.next().unwrap();
	let (increment, mut graphemes) = if next.1 == "," {
		let code = &code[next.0 + next.1.len()..];
		let (increment, code) = parse_float_rvalue(code);
		let mut graphemes = code.grapheme_indices(true)
			.skip_while(is_whitespace);
		next = graphemes.next().unwrap();
		(Some(increment), graphemes)
	} else { (None, graphemes) };
	
	assert!(next.1 == "e");
	assert!(matches!(graphemes.next(), Some((_, "x"))));
	assert!(matches!(graphemes.next(), Some((_, "e"))));
	assert!(matches!(graphemes.next(), Some((_, "c"))));
	assert!(matches!(graphemes.next(), Some((_, "u"))));
	assert!(matches!(graphemes.next(), Some((_, "t"))));
	assert!(matches!(graphemes.next(), Some((_, "a"))));
	
	let mut graphemes = graphemes.skip_while(is_whitespace);
	assert!(graphemes.next().is_none());
	
	Instructiune::PentruExecuta(lvalue, start, end, increment, Vec::new())
}

fn parse_second_step_daca(code: &str) -> Instructiune {
	let (rvalue, code) = parse_bool_rvalue(code);
	
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	
	assert!(matches!(graphemes.next(), Some((_, "a"))));
	assert!(matches!(graphemes.next(), Some((_, "t"))));
	assert!(matches!(graphemes.next(), Some((_, "u"))));
	assert!(matches!(graphemes.next(), Some((_, "n"))));
	assert!(matches!(graphemes.next(), Some((_, "c"))));
	assert!(matches!(graphemes.next(), Some((_, "i"))));
	
	let mut graphemes = graphemes.skip_while(is_whitespace);
	
	assert!(graphemes.next().is_none());

	Instructiune::DacaAtunciAltfel(rvalue, Vec::new(), None)
}

fn parse_second_step_cat_timp<'a>(code: &'a str) -> Instructiune<'a> {
	let mut graphemes = code.grapheme_indices(true);
	assert!(matches!(graphemes.next(), Some((_, " "))));
	assert!(matches!(graphemes.next(), Some((_, "t"))));
	assert!(matches!(graphemes.next(), Some((_, "i"))));
	assert!(matches!(graphemes.next(), Some((_, "m"))));
	assert!(matches!(graphemes.next(), Some((_, "p"))));
	let next = graphemes.next().unwrap();
	assert!(next.1 == " ");
	
	let (condition, code) = parse_bool_rvalue(&code[next.0..]);
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	assert!(matches!(graphemes.next(), Some((_, "e"))));
	assert!(matches!(graphemes.next(), Some((_, "x"))));
	assert!(matches!(graphemes.next(), Some((_, "e"))));
	assert!(matches!(graphemes.next(), Some((_, "c"))));
	assert!(matches!(graphemes.next(), Some((_, "u"))));
	assert!(matches!(graphemes.next(), Some((_, "t"))));
	assert!(matches!(graphemes.next(), Some((_, "a"))));
	
	let mut graphemes = graphemes.skip_while(is_whitespace);
	assert!(graphemes.next().is_none());
	
	Instructiune::CatTimpExecuta(condition, Vec::new())
}

fn parse_altfel(code: &str) -> bool {
	let mut graphemes = code.grapheme_indices(true);
	let mut answer = true;
	answer &= matches!(graphemes.next(), Some((_, "a")));
	answer &= matches!(graphemes.next(), Some((_, "l")));
	answer &= matches!(graphemes.next(), Some((_, "t")));
	answer &= matches!(graphemes.next(), Some((_, "f")));
	answer &= matches!(graphemes.next(), Some((_, "e")));
	answer &= matches!(graphemes.next(), Some((_, "l")));
	
	let mut graphemes = graphemes.skip_while(is_whitespace);
	answer &= graphemes.next().is_none();
	
	answer
}

fn parse_repeta(code: &str) -> bool {
	let mut graphemes = code.grapheme_indices(true);
	let mut answer = true;
	answer &= matches!(graphemes.next(), Some((_, "r")));
	answer &= matches!(graphemes.next(), Some((_, "e")));
	answer &= matches!(graphemes.next(), Some((_, "p")));
	answer &= matches!(graphemes.next(), Some((_, "e")));
	answer &= matches!(graphemes.next(), Some((_, "t")));
	answer &= matches!(graphemes.next(), Some((_, "a")));
	
	let mut graphemes = graphemes.skip_while(is_whitespace);
	answer &= graphemes.next().is_none();
	
	answer
}

fn parse_pana_cand<'a>(code: &'a str, instructions: Vec<Instructiune<'a>>) -> Instructiune<'a> {
	let mut graphemes = code.grapheme_indices(true);
	assert!(matches!(graphemes.next(), Some((_, "p"))));
	assert!(matches!(graphemes.next(), Some((_, "a"))));
	assert!(matches!(graphemes.next(), Some((_, "n"))));
	assert!(matches!(graphemes.next(), Some((_, "a"))));
	assert!(matches!(graphemes.next(), Some((_, " "))));
	assert!(matches!(graphemes.next(), Some((_, "c"))));
	assert!(matches!(graphemes.next(), Some((_, "a"))));
	assert!(matches!(graphemes.next(), Some((_, "n"))));
	assert!(matches!(graphemes.next(), Some((_, "d"))));
	let next = graphemes.next().unwrap();
	
	let code = &code[next.0..];
	let (condition, code) = parse_bool_rvalue(code);
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	assert!(graphemes.next().is_none());
	
	Instructiune::RepetaPanaCand(instructions, condition)
}

fn parse_first_step<'a>(cursor: Cursor<'a>) -> Instructiune<'a> {
	let (cursor, name) = cursor.read_until(not_variable);
	dbg!(cursor.code());
	match dbg!(name) {
		"daca" =>
			parse_second_step_daca(cursor.code()),
		"cat" =>
			parse_second_step_cat_timp(cursor.code()),
		"pentru" =>
			parse_second_step_pentru(cursor.code()),
		"scrie" =>
			parse_second_step_scrie(cursor.code()),
		"citeste" =>
			parse_second_step_citeste(cursor.code()),
		x =>
			parse_second_step_lvalue(cursor, Lvalue(x)),
	}
}

fn parse<'a>(mut code: &'a str, instructions: &mut Vec<Instructiune<'a>>, indent: usize) -> &'a str {
	enum Expecting<'a> { Anything, PanaCand(Vec<Instructiune<'a>>) }
	let mut expecting = Expecting::Anything;
	let mut cursor = 0;
	let mut lines = code.split("\n");
	while let Some(line) = lines.next() {
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
				cursor += line.len() + "\n".len();
				let line = &line[current_indent*"\t".len()..];
				println!("{:?}", line);
				expecting = match expecting {
					Expecting::Anything => {
						if parse_altfel(line) {
							if let Some(Instructiune::DacaAtunciAltfel(_, _, instructions)) = instructions.last_mut() {
								assert!(instructions.is_none());
								*instructions = Some({
									let mut instructions = Vec::new();
									code = parse(&code[cursor..], &mut instructions, indent+1);
									cursor = 0;
									lines = code.split("\n");
									instructions
								});
							} else { panic!() }
							Expecting::Anything
						} else if parse_repeta(line) {
							let mut instructions = Vec::new();
							code = parse(&code[cursor..], &mut instructions, indent+1);
							cursor = 0;
							lines = code.split("\n");
							Expecting::PanaCand(instructions)
						} else {
							let other_cursor = Cursor::new(line);
							let mut instruction = dbg!(parse_first_step(other_cursor));
							match &mut instruction {
								Instructiune::DacaAtunciAltfel(_, instructions, _)
								| Instructiune::CatTimpExecuta(_, instructions)
								| Instructiune::PentruExecuta(_, _, _, _, instructions)
								| Instructiune::RepetaPanaCand(instructions, _)
								=> {
									code = parse(&code[cursor..], instructions, indent+1);
									cursor = 0;
									lines = code.split("\n");
								}
								Instructiune::Atribuire(..)
								| Instructiune::Interschimbare(..)
								| Instructiune::Scrie(..)
								| Instructiune::Citeste(..)
								=> {} // do nothing 
							}
							instructions.push(instruction);
							Expecting::Anything
						}
					}
					Expecting::PanaCand(pana_cand_instructions) => {
						instructions.push(parse_pana_cand(line, pana_cand_instructions));
						Expecting::Anything
					}
				}
			}
		}
	}
	&code[cursor..]
}

fn test_execute() {
	let instructions = vec![
		Instructiune::Atribuire(Lvalue("a"), FloatRvalue::Literal(10f32)),
		Instructiune::Atribuire(Lvalue("b"), FloatRvalue::Literal(5f32)),
		Instructiune::Interschimbare(Lvalue("a"), Lvalue("b")),
		Instructiune::Scrie(
			vec![
				ScrieParam::Rvalue(
					FloatRvalue::Binary(
						FloatBinaryOp::Add,
						Box::new(FloatRvalue::Binary(
							FloatBinaryOp::Add,
							Box::new(FloatRvalue::Lvalue(Lvalue("a"))),
							Box::new(FloatRvalue::Lvalue(Lvalue("b"))),
						)),
						Box::new(FloatRvalue::Literal(3f32)),
					)
				)
			]
		)
	];

	let mut variables = HashMap::new();
	execute(&mut variables, &instructions);
}

fn main() {
	// let args: Vec<String> = env::args().collect();
	
	let program_path = "./prog";
	let mut program_file = File::open(program_path).unwrap();
	
	let mut code = String::new();
	program_file.read_to_string(&mut code).unwrap();

	let mut program = Vec::new();
	parse(&code, &mut program, 0);
	dbg!(&program);
	let mut variables = HashMap::new();
	execute(&mut variables, &program);
}
