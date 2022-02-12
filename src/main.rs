// use std::env;

use std::{fs::File, io::Read, iter};
use itertools::izip;
use unicode_segmentation::UnicodeSegmentation;
use std::collections::HashMap;

#[derive(Debug)]
struct Lvalue<'a> (&'a str);

#[derive(Debug)]
enum UnaryOp { Pos, Neg, Whole }

#[derive(Debug)]
enum BinaryOp {
	Add, Sub, Mul, Div, Rem,
	Equ, Lt, Gt, Lte, Gte,
	Or, And,
	Divides,
}

#[derive(Debug)]
enum Rvalue<'a> {
	Literal(f32),
	Lvalue(Lvalue<'a>),
	Unary(UnaryOp, Box<Rvalue<'a>>),
	Binary(BinaryOp, Box<Rvalue<'a>>, Box<Rvalue<'a>>),
}

#[derive(Debug)]
enum Instructiune<'a> {
	Atribuire(Lvalue<'a>, Rvalue<'a>),
	Interschimbare(Lvalue<'a>, Lvalue<'a>),
	Scrie(Vec<Rvalue<'a>>),
	Citeste(Vec<Lvalue<'a>>),
	DacaAtunciAltfel(Rvalue<'a>, Vec<Instructiune<'a>>, Option<Vec<Instructiune<'a>>>),
	CatTimpExecuta(Rvalue<'a>, Vec<Instructiune<'a>>),
	PentruExecuta(Lvalue<'a>, Rvalue<'a>, Rvalue<'a>, Option<Rvalue<'a>>, Vec<Instructiune<'a>>),
	RepetaPanaCand(Vec<Instructiune<'a>>, Rvalue<'a>),
}

fn evaluate(
	variables: &HashMap<&str, f32>,
	rvalue: &Rvalue,
) -> f32 {
	fn bool_to_float(x: bool) -> f32 { if x { 1f32 } else { 0f32 } }
	fn float_to_bool(x: f32) -> bool { x == 1f32 }
	match rvalue {
		Rvalue::Literal(x) => *x,
		Rvalue::Lvalue(x) => *variables.get(x.0).unwrap(),
		Rvalue::Unary(op, x) => {
			let x = evaluate(variables, x);
			match *op {
				UnaryOp::Pos => x,
				UnaryOp::Neg => -x,
				UnaryOp::Whole => x.floor(),
			}
		}
		Rvalue::Binary(op, x, y) => {
			let x = evaluate(variables, x);
			let y = evaluate(variables, y);
			match *op {
				BinaryOp::Add => x+y,
				BinaryOp::Sub => x-y,
				BinaryOp::Mul => x*y,
				BinaryOp::Div => x/y,
				BinaryOp::Rem => ((x as i32) % (y as i32)) as f32,
				BinaryOp::Equ => bool_to_float(x == y),
				BinaryOp::Lt => bool_to_float(x < y),
				BinaryOp::Gt => bool_to_float(x > y),
				BinaryOp::Lte => bool_to_float(x <= y),
				BinaryOp::Gte => bool_to_float(x >= y),
				BinaryOp::And => bool_to_float(float_to_bool(x) && float_to_bool(y)),
				BinaryOp::Or => bool_to_float(float_to_bool(x) || float_to_bool(y)),
				BinaryOp::Divides => bool_to_float((y as i32) % (x as i32) == 0),
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
				variables.insert(lvalue.0, evaluate(variables, rvalue));
			}
			Instructiune::Interschimbare(lt_lvalue, rt_lvalue) => {
				let lt_ptr = variables.get_mut(lt_lvalue.0).unwrap() as *mut f32;
				let rt_ptr = variables.get_mut(rt_lvalue.0).unwrap() as *mut f32;
				unsafe {
					std::ptr::swap(lt_ptr, rt_ptr);
				}
			}
			Instructiune::Scrie(rvalues) => {
				for rvalue in rvalues {
					print!("{} ", evaluate(variables, rvalue));
				}
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
				if evaluate(variables, conditie) == 1f32 {
					execute(variables, &atunci);
				} else {
					if let Some(altfel) = altfel {
						execute(variables, &altfel);
					}
				}
			}
			Instructiune::CatTimpExecuta(conditie, executa) => {
				while evaluate(variables, conditie) == 1f32 {
					execute(variables, executa);
				}
			}
			Instructiune::PentruExecuta(contor, start, stop, increment, executa) => {
				variables.insert(contor.0, evaluate(variables, start));
				let stop = evaluate(variables, stop);
				let increment = increment
					.as_ref()
					.map(|x| evaluate(variables, x))
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
				while evaluate(variables, conditie) != 1f32 {
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
	);
}


fn parse_expression<'a>(code: &'a str) -> (Rvalue<'a>, &'a str) {
	#[derive(Debug)]
	enum Operator { Lparen, Rparen, UnaryOp(UnaryOp), BinaryOp(BinaryOp) }
	impl Operator {
		fn get_priority(&self) -> u32 {
			match self {
				Operator::Lparen => 0,
				Operator::Rparen => panic!(),
				Operator::UnaryOp(op) => todo!(),
				Operator::BinaryOp(op) => match op {
					BinaryOp::Lt => 1,
					BinaryOp::Gt => 1,
					BinaryOp::Equ => 1,
					BinaryOp::Add => 2,
					BinaryOp::Sub => 2,
					BinaryOp::Mul => 3,
					BinaryOp::Div => 3,
					_ => todo!(),
				}
			}
		}
	}
	
	#[derive(Debug)]
	enum State { ParsingExpression, ParsingFloatLiteral, ParsingLvalue }

	#[derive(Debug)]
	enum Expecting { Operator, Operand }
	let mut expecting = Expecting::Operand;

	let try_push_operand = |state: &mut State, expecting: &mut Expecting, operands: &mut Vec<Rvalue<'a>>, cursor: &mut usize, u: usize, should_push: bool| {
		match state {
			State::ParsingFloatLiteral => {
				if should_push {
					*state = State::ParsingExpression;
					operands.push(Rvalue::Literal(code[*cursor..u].parse().unwrap()));
					*cursor = u;
					*expecting = Expecting::Operator;
				}
			}
			State::ParsingLvalue => {
				if should_push {
					*state = State::ParsingExpression;
					operands.push(Rvalue::Lvalue(Lvalue(&code[*cursor..u])));
					*cursor = u;
					*expecting = Expecting::Operator;
				}
			}
			State::ParsingExpression => {}
		}
	};

	fn evaluate_last_operator(operators: &mut Vec<Operator>, operands: &mut Vec<Rvalue>) {
		let last = operators.pop().unwrap();
		match dbg!(last) {
			Operator::BinaryOp(op) => {
				let rhs = operands.pop().unwrap();
				let lhs = operands.pop().unwrap();
				operands.push(Rvalue::Binary(op, Box::new(lhs), Box::new(rhs)));
			}
			_ => todo!(),
		}
	}
	
	let mut state = State::ParsingExpression;
	let mut operators = Vec::<Operator>::new();
	let mut operands = Vec::new();
	let mut cursor = 0;
	for (u, grapheme) in code.grapheme_indices(true) {
		try_push_operand(&mut state, &mut expecting, &mut operands, &mut cursor, u, not_variable(grapheme));
		if let State::ParsingExpression = state {
			cursor = u;
			let mut operator = None;
			match grapheme {
				" " => {}
				// "," => { break }
				"(" => operator = Some(Operator::Lparen),
				")" => operator = Some(Operator::Rparen),
				
				"+" => operator = Some(Operator::BinaryOp(BinaryOp::Add)),
				"-" => operator = Some(Operator::BinaryOp(BinaryOp::Sub)),
				"*" => operator = Some(Operator::BinaryOp(BinaryOp::Mul)),
				"/" => operator = Some(Operator::BinaryOp(BinaryOp::Div)),
				
				"=" => operator = Some(Operator::BinaryOp(BinaryOp::Equ)),
				"<" => operator = Some(Operator::BinaryOp(BinaryOp::Lt)),
				">" => operator = Some(Operator::BinaryOp(BinaryOp::Gt)),
				
				"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
				=> state = State::ParsingFloatLiteral,
				_ => state = State::ParsingLvalue,
			}
			if matches!(state, State::ParsingFloatLiteral|State::ParsingLvalue) {
				if !matches!(expecting, Expecting::Operand) { break }
			}
			if let Some(operator) = operator {
				if !matches!(operator, Operator::Lparen|Operator::Rparen) {
					if !matches!(expecting, Expecting::Operator) { break }
					expecting = Expecting::Operand;
				}
				if !matches!(operator, Operator::Lparen) {
					while let Some(last) = operators.last() {
						if matches!(operator, Operator::Rparen) {
							if !matches!(last, Operator::Lparen) {
								evaluate_last_operator(&mut operators, &mut operands);
							} else {
								operators.pop();
								break;
							}
						} else {
							if operator.get_priority() <= last.get_priority() {
								evaluate_last_operator(&mut operators, &mut operands);
							} else {
								break
							}
						}
					}
				}
				if !matches!(operator, Operator::Rparen) {
					operators.push(operator);
				}
			}
		}
	}
	if matches!(expecting, Expecting::Operand) {
		try_push_operand(&mut state, &mut expecting, &mut operands, &mut cursor, code.len(), true);
	}
	while !operators.is_empty() {
		evaluate_last_operator(&mut operators, &mut operands);
	}
	dbg!(&operands);
	assert!(operands.len() == 1);
	(operands.pop().unwrap(), &code[cursor..])
}

fn parse_second_step_scrie<'a>(code: &'a str) -> Instructiune<'a> {
	Instructiune::Scrie(
		code.split(",").map(|code| parse_expression(code).0).collect(),
	)
}

fn parse_lvalue<'a>(code: &'a str) -> (Lvalue, &'a str) {
	enum State {
		SkippingWhitespace,
		ReadingLvalue(usize),
	}
	let mut state = State::SkippingWhitespace;
	for (u, grapheme) in code.grapheme_indices(true) {
		if let State::SkippingWhitespace = state {
			if grapheme != " " {
				state = State::ReadingLvalue(u);
			}
		}
		if let State::ReadingLvalue(start) = state {
			if not_variable(grapheme) {
				return (Lvalue(&code[start..u-grapheme.len()]), &code[u-grapheme.len()..]);
			}
		}
	}
	if let State::ReadingLvalue(start) = state {
		(Lvalue(&code[start..]), &code[start..start])
	} else {
		panic!()
	}
}

fn parse_second_step_citeste<'a>(code: &'a str) -> Instructiune<'a> {
	Instructiune::Citeste(
		code.split(",").map(|code| parse_lvalue(code).0).collect(),
	)
}

fn parse_second_step_lvalue<'a>(code: &'a str, lvalue: Lvalue<'a>) -> Instructiune<'a> {
	enum State {
		SkippingWhitespace,
		ReadingInstruction(u32),
	}
	let mut state = State::SkippingWhitespace;
	let mut last_grapheme = "";
	let mut cursor = 0;
	for (u, grapheme) in code.grapheme_indices(true) {
		if let State::SkippingWhitespace = state {
			if grapheme != " " {
				state = State::ReadingInstruction(0);
			}
		}
		if let State::ReadingInstruction(i) = &mut state {
			match i {
				0 => assert!(grapheme == "<"),
				1 => assert!(grapheme == "-"),
				2 => {
					cursor = u;
					last_grapheme = grapheme;
					break;
				}
				_ => panic!(),
			}
			*i += 1;
		}
	}
	if last_grapheme == ">" {
		cursor += last_grapheme.len();
		let other_lvalue = parse_lvalue(&code[cursor..]).0;
		Instructiune::Interschimbare(lvalue, other_lvalue)
	} else {
		let rvalue = parse_expression(&code[cursor..]).0;
		Instructiune::Atribuire(lvalue, rvalue)
	}
}

fn parse_second_step_pentru<'a>(code: &'a str) -> Instructiune<'a> {
	enum State {
		SkippingWhitespace,
		Reading(usize),
	}
	let (lvalue, code) = parse_lvalue(code);
	let mut state = State::SkippingWhitespace;
	let mut cursor = 0;
	let mut last_grapheme_len = 0;
	for (u, grapheme) in code.grapheme_indices(true) {
		if let State::SkippingWhitespace = state {
			if grapheme != " " { state = State::Reading(0) }
		}
		match &mut state {
			State::Reading(i) => {
				match i {
					0 => assert!(grapheme == "<"),
					1 => {
						assert!(grapheme == "-");
						cursor = u;
						last_grapheme_len = grapheme.len();
						break;
					}
					_ => panic!(),
				}
				*i += 1;
			}
			_ => {}
		}
	}
	let code = &code[cursor+last_grapheme_len..];
	let (start, code) = parse_expression(code);
	
	let mut graphemes = code.grapheme_indices(true)
		.skip_while(|x| x.1 == " ");
	let next = graphemes.next().unwrap();
	assert!(next.1 == ",");
	
	cursor = next.0 + next.1.len();
	let code = &code[cursor..];
	let (end, code) = parse_expression(code);

	let graphemes = code.grapheme_indices(true)
		.skip_while(|x| x.1 == " ");

	for (i, (u, grapheme)) in izip!(0.., graphemes) {
		match i {
			0 => assert!(grapheme == "e"),
			1 => assert!(grapheme == "x"),
			2 => assert!(grapheme == "e"),
			3 => assert!(grapheme == "c"),
			4 => assert!(grapheme == "u"),
			5 => assert!(grapheme == "t"),
			6 => assert!(grapheme == "a"),
			_ => assert!(grapheme == " "),
		}
	}

	Instructiune::PentruExecuta(lvalue, start, end, None, Vec::new())
}

fn parse_second_step_daca(code: &str) -> Instructiune {
	let (rvalue, code) = parse_expression(code);
	
	let graphemes = code.grapheme_indices(true)
		.skip_while(|x| x.1 == " ");

	for (i, (u, grapheme)) in izip!(0.., graphemes) {
		match i {
			0 => assert!(grapheme == "a"),
			1 => assert!(grapheme == "t"),
			2 => assert!(grapheme == "u"),
			3 => assert!(grapheme == "n"),
			4 => assert!(grapheme == "c"),
			5 => assert!(grapheme == "i"),
			_ => assert!(grapheme == " "),
		}
	}

	Instructiune::DacaAtunciAltfel(rvalue, Vec::new(), None)
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
		"pentru" =>
			parse_second_step_pentru(&code[cursor..]),
		"daca" =>
			parse_second_step_daca(&code[cursor..]),
		"scrie" =>
			parse_second_step_scrie(&code[cursor..]),
		"citeste" =>
			parse_second_step_citeste(&code[cursor..]),
		x =>
			parse_second_step_lvalue(&code[cursor..], Lvalue(x)),
	}
}

fn parse<'a>(mut code: &'a str, instructions: &mut Vec<Instructiune<'a>>, indent: usize) -> &'a str {
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
					=> {
						// do nothing
					}
				}
				instructions.push(instruction);
			}
		}
	}
	// if lines.next().is_none() { cursor -= "\n".len() }
	&code[cursor..]
}

fn test_execute() {
	let instructions = vec![
		Instructiune::Atribuire(Lvalue("a"), Rvalue::Literal(10f32)),
		Instructiune::Atribuire(Lvalue("b"), Rvalue::Literal(5f32)),
		Instructiune::Interschimbare(Lvalue("a"), Lvalue("b")),
		Instructiune::Scrie(
			vec![
				Rvalue::Binary(
					BinaryOp::Add,
					Box::new(Rvalue::Binary(
						BinaryOp::Add,
						Box::new(Rvalue::Lvalue(Lvalue("a"))),
						Box::new(Rvalue::Lvalue(Lvalue("b"))),
					)),
					Box::new(Rvalue::Literal(3f32)),
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
