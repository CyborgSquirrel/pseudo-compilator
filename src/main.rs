// use std::env;

use std::{fs::File, io::Read, iter};
use itertools::izip;
use unicode_segmentation::UnicodeSegmentation;
use std::collections::HashMap;

fn is_whitespace(x: &(usize, &str)) -> bool { x.1 == " "  }

#[derive(Debug)]
struct Lvalue<'a> (&'a str);

#[derive(Debug)]
enum FloatUnaryOp { Pos, Neg, Whole }

#[derive(Debug)]
enum FloatBinaryOp { Add, Sub, Mul, Div, Rem }

#[derive(Debug)]
enum FloatRvalue<'a> {
	Literal(f32),
	Lvalue(Lvalue<'a>),
	Unary(FloatUnaryOp, Box<FloatRvalue<'a>>),
	Binary(FloatBinaryOp, Box<FloatRvalue<'a>>, Box<FloatRvalue<'a>>),
}

#[derive(Debug)]
enum ScrieParam<'a> {
	Rvalue(FloatRvalue<'a>),
	StringLiteral(&'a str),
	CharLiteral(&'a str),
}

#[derive(Debug)]
enum BoolFloatBinaryOp {
	Equ, Lt, Gt, Lte, Gte,
	Divides,
}

#[derive(Debug)]
enum BoolBoolBinaryOp {
	And, Or,
}

#[derive(Debug)]
enum BoolRvalue<'a> {
	BoolFloatBinaryOp(BoolFloatBinaryOp, FloatRvalue<'a>, FloatRvalue<'a>),
	BoolBoolBinaryOp(BoolBoolBinaryOp, Box<BoolRvalue<'a>>, Box<BoolRvalue<'a>>),
}

#[derive(Debug)]
enum BoolUnaryOp {}

#[derive(Debug)]
enum BoolBinaryOp {
	BoolFloatBinaryOp(BoolFloatBinaryOp),
	BoolBoolBinaryOp(BoolBoolBinaryOp),
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
				FloatUnaryOp::Pos => x,
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
		|"<"|">"|"="
		|"-"|"+"|"*"|"/"|"%"
		|"("|")"
		|","|";"
		|"\""|"'"
	);
}

#[derive(Debug)]
enum Token <UnaryOp, BinaryOp> { Lparen, Rparen, UnaryOp(UnaryOp), BinaryOp(BinaryOp) }
fn float_get_priority(token: &Token<FloatUnaryOp, FloatBinaryOp>) -> u32 {
	match token {
		Token::Lparen => 0,
		Token::Rparen => panic!(),
		Token::UnaryOp(op) => todo!(),
		Token::BinaryOp(op) => match op {
			FloatBinaryOp::Add => 1,
			FloatBinaryOp::Sub => 1,
			FloatBinaryOp::Mul => 2,
			FloatBinaryOp::Div => 2,
			FloatBinaryOp::Rem => 2,
		}
	}
}
fn float_eval<'a>(token: Token<FloatUnaryOp, FloatBinaryOp>, lhs: FloatRvalue<'a>, rhs: FloatRvalue<'a>) -> FloatRvalue<'a> {
	match token {
		Token::Lparen => panic!(),
		Token::Rparen => panic!(),
		Token::BinaryOp(op) => {
			FloatRvalue::Binary(op, Box::new(lhs), Box::new(rhs))
		}
		Token::UnaryOp(..) => todo!()
	}
}

struct ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn> {
	tokens: Vec<Token<UnaryOp, BinaryOp>>,
	operands: Vec<Operand>,
	expecting: Expecting,
	get_priority: GetPriorityFn,
	eval: EvalFn,
}

enum Expecting {
	Operand, Operator
}

impl<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn>
ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn>
where
	GetPriorityFn: Fn(&Token<UnaryOp, BinaryOp>) -> u32,
	EvalFn: Fn(Token<UnaryOp, BinaryOp>, Operand, Operand) -> Operand,
	Operand: std::fmt::Debug,
	UnaryOp: std::fmt::Debug,
	BinaryOp: std::fmt::Debug,
{
	fn new(get_priority: GetPriorityFn, eval: EvalFn) -> Self {
		Self {
			tokens: Vec::new(),
			operands: Vec::new(),
			expecting: Expecting::Operand,
			get_priority,
			eval,
		}
	}
	fn eval_binary_op(&mut self) {
		let rhs = dbg!(&mut self.operands).pop().unwrap();
		let lhs = self.operands.pop().unwrap();
		let last = self.tokens.pop().unwrap();
		self.operands.push((self.eval)(last, lhs, rhs));
	}
	fn push_token(&mut self, token: Token<UnaryOp, BinaryOp>) -> bool {
		match dbg!(&token) {
			Token::Lparen => { }
			Token::Rparen => {
				while let Some(last) = dbg!(&self.tokens).last() {
					if !matches!(last, Token::Lparen) {
						self.eval_binary_op();
					} else {
						self.tokens.pop();
						break;
					}
				}
			}
			Token::BinaryOp(..) => {
				if !matches!(self.expecting, Expecting::Operator) { return false }
				self.expecting = Expecting::Operand;
				while let Some(last) = self.tokens.last() {
					if (self.get_priority)(&token) <= (self.get_priority)(last) {
						self.eval_binary_op();
					} else {
						break;
					}
				}
			}
			Token::UnaryOp(..) => {
				// assert!(matches!(self.expecting, Expecting::Operator));
				todo!();
			}
		}
		if !matches!(token, Token::Rparen) {
			self.tokens.push(token);
		}
		true
	}
	fn push_operand(&mut self, operand: Operand) -> bool {
		dbg!(&operand);
		if !matches!(self.expecting, Expecting::Operand) { return false }
		self.expecting = Expecting::Operator;
		self.operands.push(operand);
		true
	}
	fn finish(mut self) -> Operand {
		while !self.tokens.is_empty() {
			self.eval_binary_op();
		}
		assert!(self.operands.len() == 1);
		self.operands.pop().unwrap()
	}
}

fn parse_float_rvalue<'a>(code: &'a str) -> (FloatRvalue, &'a str) {
	let mut expression_constructor = ExpressionConstructor::new(
		float_get_priority,
		float_eval,
	);
	enum State {
		Unsure,
		ParsingLvalue,
		ParsingFloatLiteral,
	}
	let try_push_operand = |expression_constructor: &mut ExpressionConstructor<_,_,_,_,_>, state: &mut State, cursor: &mut usize, end: usize| {
		match state {
			State::ParsingLvalue => {
				*state = State::Unsure;
				let result = expression_constructor.push_operand(FloatRvalue::Lvalue(Lvalue(&code[*cursor..end])));
				if result { *cursor = end }
				result
			}
			State::ParsingFloatLiteral => {
				*state = State::Unsure;
				let result = expression_constructor.push_operand(FloatRvalue::Literal(code[*cursor..end].parse().unwrap()));
				if result { *cursor = end }
				result
			}
			_ => true,
		}
	};
	let mut state = State::Unsure;
	let mut cursor = 0;
	let mut graphemes = code.grapheme_indices(true);
	while let Some((i, grapheme)) = graphemes.next() {
		if not_variable(grapheme) {
			if !try_push_operand(&mut expression_constructor, &mut state, &mut cursor, i) { break }
		}
		if let State::Unsure = state {
			cursor = i;
			let mut token = None;
			match grapheme {
				" " => {}
				"," => break,
				"(" => token = Some(Token::Lparen),
				")" => token = Some(Token::Rparen),
				
				"+" => token = Some(Token::BinaryOp(FloatBinaryOp::Add)),
				"-" => token = Some(Token::BinaryOp(FloatBinaryOp::Sub)),
				"*" => token = Some(Token::BinaryOp(FloatBinaryOp::Mul)),
				"/" => token = Some(Token::BinaryOp(FloatBinaryOp::Div)),
				"%" => token = Some(Token::BinaryOp(FloatBinaryOp::Rem)),
				
				"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
				=> state = State::ParsingFloatLiteral,
				_ => state = State::ParsingLvalue,
			}
			if let Some(token) = token {
				if !expression_constructor.push_token(token) { break }
			}
		}
	}
	try_push_operand(&mut expression_constructor, &mut state, &mut cursor, code.len());
	dbg!((expression_constructor.finish(), &code[cursor..]))
}

#[derive(Debug)]
enum BoolOrFloatBinaryOp {
	BoolBinaryOp(BoolBinaryOp),
	FloatBinaryOp(FloatBinaryOp),
}

fn bool_get_priority(token: &Token<BoolUnaryOp, BoolOrFloatBinaryOp>) -> u32 {
	match token {
		Token::Lparen => 0,
		Token::Rparen => panic!(),
		Token::UnaryOp(op) => todo!(),
		Token::BinaryOp(op) => match op {
			BoolOrFloatBinaryOp::BoolBinaryOp(op) => match op {
				BoolBinaryOp::BoolBoolBinaryOp(op) => match op {
					BoolBoolBinaryOp::Or => 1,
					BoolBoolBinaryOp::And => 1,
				}
				BoolBinaryOp::BoolFloatBinaryOp(op) => match op {
					BoolFloatBinaryOp::Equ => 2,
					BoolFloatBinaryOp::Lt => 2,
					BoolFloatBinaryOp::Gt => 2,
					_ => todo!(),
				}
			}
			BoolOrFloatBinaryOp::FloatBinaryOp(op) => match op {
				FloatBinaryOp::Add => 3,
				FloatBinaryOp::Sub => 3,
				FloatBinaryOp::Mul => 4,
				FloatBinaryOp::Div => 4,
				FloatBinaryOp::Rem => 4,
			}
		}
	}
}

#[derive(Debug)]
enum BoolOrFloatRvalue<'a> {
	BoolRvalue(BoolRvalue<'a>),
	FloatRvalue(FloatRvalue<'a>),
}

fn bool_eval<'a>(token: Token<BoolUnaryOp, BoolOrFloatBinaryOp>, lhs: BoolOrFloatRvalue<'a>, rhs: BoolOrFloatRvalue<'a>) -> BoolOrFloatRvalue<'a> {
	match token {
		Token::Lparen => panic!(),
		Token::Rparen => panic!(),
		Token::BinaryOp(op) => {
			match op {
				BoolOrFloatBinaryOp::BoolBinaryOp(op) => {
					match op {
						BoolBinaryOp::BoolBoolBinaryOp(op) => {
							if let (BoolOrFloatRvalue::BoolRvalue(lhs), BoolOrFloatRvalue::BoolRvalue(rhs)) = (lhs, rhs) {
								BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolBoolBinaryOp(op, Box::new(lhs), Box::new(rhs)))
							} else { panic!() }
						}
						BoolBinaryOp::BoolFloatBinaryOp(op) => {
							if let (BoolOrFloatRvalue::FloatRvalue(lhs), BoolOrFloatRvalue::FloatRvalue(rhs)) = (lhs, rhs) {
								BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolFloatBinaryOp(op, lhs, rhs))
							} else { panic!() }
						}
					}
				}
				BoolOrFloatBinaryOp::FloatBinaryOp(op) => {
					if let (BoolOrFloatRvalue::FloatRvalue(lhs), BoolOrFloatRvalue::FloatRvalue(rhs)) = (lhs, rhs) {
						BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Binary(op, Box::new(lhs), Box::new(rhs)))
					} else { panic!() }
				}
			}
		}
		Token::UnaryOp(..) => todo!()
	}
}

fn parse_bool_rvalue<'a>(code: &'a str) -> (BoolRvalue, &'a str) {
	let mut expression_constructor = ExpressionConstructor::new(
		bool_get_priority,
		bool_eval,
	);
	enum State {
		Unsure,
		ParsingLvalueOrSauOrSi,
		ParsingFloatLiteral,
		ParsingToken,
	}
	let try_push_operand = |expression_constructor: &mut ExpressionConstructor<_,_,_,_,_>, state: &mut State, cursor: &mut usize, end: usize| {
		match state {
			State::ParsingLvalueOrSauOrSi => {
				*state = State::Unsure;
				let name = &code[*cursor..end];
				let result = match (&expression_constructor.expecting, name) {
					(Expecting::Operator, "si") => {
						*cursor = end;
						expression_constructor.push_token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolBoolBinaryOp(BoolBoolBinaryOp::And))))
					}
					(Expecting::Operator, "sau") => {
						*cursor = end;
						expression_constructor.push_token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolBoolBinaryOp(BoolBoolBinaryOp::Or))))
					}
					(Expecting::Operand, name) => {
						*cursor = end;
						expression_constructor.push_operand(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Lvalue(Lvalue(name))))
					}
					_ => false,
				};
				result
			}
			State::ParsingFloatLiteral => {
				*state = State::Unsure;
				let result = expression_constructor.push_operand(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Literal(code[*cursor..end].parse().unwrap())));
				*cursor = end;
				result
			}
			_ => true,
		}
	};
	let mut state = State::Unsure;
	let mut cursor = 0;
	let mut graphemes = code.grapheme_indices(true);
	while let Some((i, grapheme)) = graphemes.next() {
		if not_variable(grapheme) {
			if !try_push_operand(&mut expression_constructor, &mut state, &mut cursor, i) { break }
		}
		if let State::Unsure = state {
			cursor = i;
			let mut token = None;
			match grapheme {
				" " => {}
				"," => break,
				"(" => token = Some(Token::Lparen),
				")" => token = Some(Token::Rparen),
				
				"+" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(FloatBinaryOp::Add))),
				"-" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(FloatBinaryOp::Sub))),
				"*" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(FloatBinaryOp::Mul))),
				"/" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(FloatBinaryOp::Div))),
				"%" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(FloatBinaryOp::Rem))),
				
				"=" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(BoolFloatBinaryOp::Equ)))),
				"<" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(BoolFloatBinaryOp::Lt)))),
				">" => token = Some(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(BoolFloatBinaryOp::Gt)))),
				
				"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
				=> state = State::ParsingFloatLiteral,
				_ => state = State::ParsingLvalueOrSauOrSi,
			}
			if let Some(token) = token {
				if !expression_constructor.push_token(token) { break }
			}
		}
	}
	try_push_operand(&mut expression_constructor, &mut state, &mut cursor, code.len());
	if let BoolOrFloatRvalue::BoolRvalue(rvalue) = expression_constructor.finish() {
		dbg!((rvalue, &code[cursor..]))
	} else { panic!() }
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

fn parse_second_step_lvalue<'a>(code: &'a str, lvalue: Lvalue<'a>) -> Instructiune<'a> {
	let mut graphemes = code.grapheme_indices(true).skip_while(is_whitespace);
	assert!(matches!(graphemes.next(), Some((_, "<"))));
	assert!(matches!(graphemes.next(), Some((_, "-"))));
	let next = graphemes.next().unwrap();
	if next.1 == ">" {
		let other_lvalue = parse_lvalue(&code[next.0+next.1.len()..]).0;
		Instructiune::Interschimbare(lvalue, other_lvalue)
	} else {
		let rvalue = parse_float_rvalue(&code[next.0..]).0;
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

fn parse_first_step<'a>(code: &'a str) -> Instructiune<'a> {
	let mut cursor = 0;
	let mut last_grapheme_len = 0;
	let mut read_first_step = || {
		for (u, grapheme) in code.grapheme_indices(true) {
			if not_variable(grapheme) {
				cursor = u;
				last_grapheme_len = grapheme.len();
				return &code[0..u];
			}
		}
		panic!()
	};
	match dbg!(read_first_step()) {
		"daca" =>
			parse_second_step_daca(&code[cursor..]),
		"cat" =>
			parse_second_step_cat_timp(&code[cursor..]),
		"pentru" =>
			parse_second_step_pentru(&code[cursor..]),
		"scrie" =>
			parse_second_step_scrie(&code[cursor..]),
		"citeste" =>
			parse_second_step_citeste(&code[cursor..]),
		x =>
			parse_second_step_lvalue(&code[cursor..], Lvalue(x)),
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
							let mut instruction = dbg!(parse_first_step(line));
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
