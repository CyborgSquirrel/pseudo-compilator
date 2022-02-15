use crate::not_variable;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug)]
pub struct Lvalue<'a> (pub &'a str);

#[derive(Debug)]
pub enum FloatUnaryOp { Ident, Neg, Whole }

#[derive(Debug)]
pub enum FloatBinaryOp { Add, Sub, Mul, Div, Rem }

#[derive(Debug)]
pub enum FloatRvalue<'a> {
	Literal(f32),
	Lvalue(Lvalue<'a>),
	Unary(FloatUnaryOp, Box<FloatRvalue<'a>>),
	Binary(FloatBinaryOp, Box<FloatRvalue<'a>>, Box<FloatRvalue<'a>>),
}

#[derive(Debug)]
pub enum BoolFloatBinaryOp {
	Equ, Nequ, Lt, Gt, Lte, Gte,
	Divides,
}

#[derive(Debug)]
pub enum BoolBoolBinaryOp {
	And, Or,
}

#[derive(Debug)]
pub enum BoolRvalue<'a> {
	BoolFloatBinaryOp(BoolFloatBinaryOp, FloatRvalue<'a>, FloatRvalue<'a>),
	BoolBoolBinaryOp(BoolBoolBinaryOp, Box<BoolRvalue<'a>>, Box<BoolRvalue<'a>>),
}

#[derive(Debug)]
pub enum BoolUnaryOp {}

#[derive(Debug)]
pub enum BoolBinaryOp {
	BoolFloatBinaryOp(BoolFloatBinaryOp),
	BoolBoolBinaryOp(BoolBoolBinaryOp),
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
		let rhs = self.operands.pop().unwrap();
		let lhs = self.operands.pop().unwrap();
		let last = self.tokens.pop().unwrap();
		self.operands.push((self.eval)(last, lhs, rhs));
	}
	fn push_token(&mut self, token: Token<UnaryOp, BinaryOp>) -> bool {
		match &token {
			Token::Lparen => { }
			Token::Rparen => {
				while let Some(last) = self.tokens.last() {
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

pub fn parse_float_rvalue<'a>(code: &'a str) -> (FloatRvalue, &'a str) {
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
					BoolBoolBinaryOp::And => 2,
				}
				BoolBinaryOp::BoolFloatBinaryOp(op) => match op {
					BoolFloatBinaryOp::Equ => 3,
					BoolFloatBinaryOp::Lt => 3,
					BoolFloatBinaryOp::Lte => 3,
					BoolFloatBinaryOp::Gt => 3,
					BoolFloatBinaryOp::Gte => 3,
					_ => todo!(),
				}
			}
			BoolOrFloatBinaryOp::FloatBinaryOp(op) => match op {
				FloatBinaryOp::Add => 4,
				FloatBinaryOp::Sub => 4,
				FloatBinaryOp::Mul => 5,
				FloatBinaryOp::Div => 5,
				FloatBinaryOp::Rem => 5,
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

#[derive(Debug)]
enum GraphemeKind { Reserved, Ignored, Other }
fn get_grapheme_kind(grapheme: &str) -> Option<GraphemeKind> {
	match grapheme {
		"+"|"-"|"*"|"/"|"%"|
		"="|"!"|"<"|">"
			=> Some(GraphemeKind::Reserved),
		" "
			=> Some(GraphemeKind::Ignored),
		","
			=> None,
		_
			=> Some(GraphemeKind::Other),
	}
}


pub fn parse_bool_rvalue<'a>(code: &'a str) -> (BoolRvalue, &'a str) {
	let mut expression_constructor = ExpressionConstructor::new(
		bool_get_priority,
		bool_eval,
	);
	#[derive(Debug)]
	enum State {
		Unsure,
		ParsingReserved,
		ParsingOther(HelpIsNeeded),
	}
	#[derive(Debug)]
	enum HelpIsNeeded {
		LvalueOrSauOrSi,
		FloatLiteral,
	}
	#[derive(Debug)]
	enum Catastrophe<'a> {
		Parsed(What<'a>),
		Skipped,
	}
	#[derive(Debug)]
	enum What<'a> {
		Operand(BoolOrFloatRvalue<'a>),
		Token(Token<BoolUnaryOp, BoolOrFloatBinaryOp>),
	}
	let catastrophe = |expecting: &Expecting, state: &State, cursor: usize, end: usize, last: Option<usize>| {
		let name = &code[cursor..end];
		fn bool_bool_binary_op_token<'a>(op: BoolBoolBinaryOp) -> What<'a> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolBoolBinaryOp(op)))) }
		fn bool_float_binary_op_token<'a>(op: BoolFloatBinaryOp) -> What<'a> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(op)))) }
		fn float_binary_op_token<'a>(op: FloatBinaryOp) -> What<'a> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(op))) }
		match state {
			State::ParsingOther(help) => {
				if last.is_none() {
					match help {
						HelpIsNeeded::LvalueOrSauOrSi => {
							match (expecting, name) {
								(Expecting::Operator, "si") =>
									Catastrophe::Parsed(bool_bool_binary_op_token(BoolBoolBinaryOp::And)),
								(Expecting::Operator, "sau") =>
									Catastrophe::Parsed(bool_bool_binary_op_token(BoolBoolBinaryOp::Or)),
								(_, name) =>
									Catastrophe::Parsed(What::Operand(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Lvalue(Lvalue(name))))),
							}
						}
						HelpIsNeeded::FloatLiteral =>
							Catastrophe::Parsed(What::Operand(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Literal(name.parse().unwrap())))),
					}
				} else { Catastrophe::Skipped }
			}
			State::ParsingReserved => {
				fn parse_reserved(name: &str) -> Option<What> {
					match name {
						"(" => Some(What::Token(Token::Lparen)),
						")" => Some(What::Token(Token::Rparen)),
						
						"+" => Some(float_binary_op_token(FloatBinaryOp::Add)),
						"-" => Some(float_binary_op_token(FloatBinaryOp::Sub)),
						"*" => Some(float_binary_op_token(FloatBinaryOp::Mul)),
						"/" => Some(float_binary_op_token(FloatBinaryOp::Div)),
						"%" => Some(float_binary_op_token(FloatBinaryOp::Rem)),
						
						"=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Equ)),
						"!=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Nequ)),
						"<" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Lt)),
						"<=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Lte)),
						">" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Gt)),
						">=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Gte)),
						
						_ => None,
					}
				}
				let token = if let Some(last) = last {
					let help = &code[cursor..last];
					if parse_reserved(help).is_none() {
						parse_reserved(name)
					} else {
						None
					}
				} else {
					parse_reserved(name)
				};
				if let Some(token) = token {
					Catastrophe::Parsed(token)
				} else {
					Catastrophe::Skipped
				}
			}
			State::Unsure => Catastrophe::Skipped
		}
	};
	let mut state = State::Unsure;
	let mut cursor = 0;
	let mut graphemes = code.grapheme_indices(true);
	while let Some((i, grapheme)) = graphemes.next() {
		if let Some(grapheme_kind) = get_grapheme_kind(grapheme) {
			let last = match (&grapheme_kind, &state) {
				(GraphemeKind::Reserved, State::ParsingReserved) |
				(GraphemeKind::Other, State::ParsingOther(..))
					=> Some(i+grapheme.len()),
				_ => None,
			};
			let result = catastrophe(&expression_constructor.expecting, &state, cursor, i, last);
			match result {
				Catastrophe::Skipped => { }
				Catastrophe::Parsed(what) => {
					if match what {
						What::Operand(operand) => expression_constructor.push_operand(operand),
						What::Token(token) => expression_constructor.push_token(token),
					} {
						cursor = i;
						state = State::Unsure;
					} else { break }
				}
			}
			if let State::Unsure = &state {
				cursor = i;
				state = match grapheme_kind {
					GraphemeKind::Ignored => State::Unsure,
					GraphemeKind::Reserved => State::ParsingReserved,
					GraphemeKind::Other => State::ParsingOther(
						match grapheme {
							"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
								=> HelpIsNeeded::FloatLiteral,
							_
								=> HelpIsNeeded::LvalueOrSauOrSi,
						}
					),
				};
			}
		} else { break }
	}
	println!("{:?}", &code[cursor..]);
	let i = graphemes.next().map(|(i, _)| i).unwrap_or(code.len());
	let result = catastrophe(&expression_constructor.expecting, &state, cursor, i, None);
	match result {
		Catastrophe::Skipped => { }
		Catastrophe::Parsed(what) => {
			if match what {
				What::Operand(operand) => expression_constructor.push_operand(operand),
				What::Token(token) => expression_constructor.push_token(token),
			} {
				cursor = i;
			}
		}
	}
	if let BoolOrFloatRvalue::BoolRvalue(rvalue) = expression_constructor.finish() {
		dbg!((rvalue, &code[cursor..]))
	} else { panic!() }
}
