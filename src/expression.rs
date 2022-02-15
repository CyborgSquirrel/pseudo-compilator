use unicode_segmentation::{UnicodeSegmentation, GraphemeIndices};
use crate::{LineCursor, LineParsingIntermediateResult, LineParsingErrorKind, get_grapheme_kind, GraphemeKind};

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

#[derive(Debug)]
struct ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn> {
	tokens: Vec<Token<UnaryOp, BinaryOp>>,
	operands: Vec<Operand>,
	expecting: Expecting,
	get_priority: GetPriorityFn,
	eval: EvalFn,
}

#[derive(Debug)]
enum Expecting {
	Operand, Operator
}

#[derive(Debug)]
enum ExpressionConstructionError {
	TooManyOperands,
	TooFewOperands,
	TooFewOperators,
}

type ExpressionConstructorResult<T> = Result<T, ExpressionConstructionError>;

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
	fn eval_binary_op(&mut self) -> ExpressionConstructorResult<()> {
		dbg!(&self.tokens);
		dbg!(&self.operands);
		let rhs = self.operands.pop().ok_or(ExpressionConstructionError::TooFewOperands)?;
		let lhs = self.operands.pop().ok_or(ExpressionConstructionError::TooFewOperands)?;
		let last = self.tokens.pop().ok_or(ExpressionConstructionError::TooFewOperators)?;
		self.operands.push((self.eval)(last, lhs, rhs));
		Ok(())
	}
	fn push_token(&mut self, token: Token<UnaryOp, BinaryOp>) -> ExpressionConstructorResult<bool> {
		match &token {
			Token::Lparen => { }
			Token::Rparen => {
				while let Some(last) = self.tokens.last() {
					if !matches!(last, Token::Lparen) {
						self.eval_binary_op()?;
					} else {
						self.tokens.pop();
						break;
					}
				}
			}
			Token::BinaryOp(..) => {
				if !matches!(self.expecting, Expecting::Operator) { return Ok(false) }
				self.expecting = Expecting::Operand;
				while let Some(last) = self.tokens.last() {
					if (self.get_priority)(&token) <= (self.get_priority)(last) {
						self.eval_binary_op()?;
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
		Ok(true)
	}
	fn push_operand(&mut self, operand: Operand) -> bool {
		if !matches!(self.expecting, Expecting::Operand) { return false }
		self.expecting = Expecting::Operator;
		self.operands.push(operand);
		true
	}
	fn finish(mut self) -> ExpressionConstructorResult<Operand> {
		while !self.tokens.is_empty() {
			self.eval_binary_op()?;
		}
		match self.operands.len().cmp(&1) {
			std::cmp::Ordering::Less => Err(ExpressionConstructionError::TooFewOperands),
			std::cmp::Ordering::Greater => Err(ExpressionConstructionError::TooManyOperands),
			std::cmp::Ordering::Equal => Ok(self.operands.pop().unwrap()),
		}
	}
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
enum Catastrophe<Operand, UnaryOp, BinaryOp> {
	Parsed(What<Operand, UnaryOp, BinaryOp>),
	Skipped,
}

#[derive(Debug)]
enum What<Operand, UnaryOp, BinaryOp> {
	Operand(Operand),
	Token(Token<UnaryOp, BinaryOp>),
}

fn parsinate<'a, F, GetPriorityFn, EvalFn, Operand, UnaryOp, BinaryOp>(
	mut cursor: LineCursor<'a>, mut catastrophe: F,
	mut expression_constructor: ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn>,
)
-> LineParsingIntermediateResult<Operand>
where
	F: FnMut(&Expecting, &State, &'a str, Option<&'a str>) -> Catastrophe<Operand, UnaryOp, BinaryOp>,
	GetPriorityFn: Fn(&Token<UnaryOp, BinaryOp>) -> u32,
	EvalFn: Fn(Token<UnaryOp, BinaryOp>, Operand, Operand) -> Operand,
	Operand: std::fmt::Debug,
	UnaryOp: std::fmt::Debug,
	BinaryOp: std::fmt::Debug,
{
	let mut state = State::Unsure;
	let mut graphemes = cursor.code().grapheme_indices(true);
	let mut do_the_thing = |cursor: &mut LineCursor<'a>, grapheme, current, graphemes: &mut GraphemeIndices<'a>| {
		let mut keep_going = true;
		let trickery = if let Some(grapheme) = grapheme {
			if let Some(grapheme_kind) = get_grapheme_kind(grapheme) {
				Some((grapheme, grapheme_kind))
			} else { keep_going = false; None }
		} else { None };
		let next = if let Some((grapheme, grapheme_kind)) = &trickery {
			match (grapheme_kind, &state) {
				(GraphemeKind::Reserved, State::ParsingReserved) |
				(GraphemeKind::Other, State::ParsingOther(..))
					=> Some(cursor.code_until(current+grapheme.len())),
				_
					=> None,
			}
		} else { None };
		dbg!(current);
		let result = catastrophe(&expression_constructor.expecting, &state, cursor.code_until(current), next);
		match result {
			Catastrophe::Skipped => { }
			Catastrophe::Parsed(what) => {
				if match what {
					What::Operand(operand) => expression_constructor.push_operand(operand),
					What::Token(token) => expression_constructor.push_token(token).unwrap(),
				} {
					cursor.advance_by(current);
					*graphemes = cursor.code().grapheme_indices(true);
					state = State::Unsure;
				} else { keep_going = false }
			}
		}
		dbg!(&trickery);
		if let Some((grapheme, grapheme_kind)) = &trickery {
			if let State::Unsure = &state {
				state = match grapheme_kind {
					GraphemeKind::Ignored => {
						cursor.advance_by(grapheme.len());
						*graphemes = cursor.code().grapheme_indices(true);
						State::Unsure
					},
					GraphemeKind::Reserved => State::ParsingReserved,
					GraphemeKind::Other => State::ParsingOther(
						match *grapheme {
							"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
								=> HelpIsNeeded::FloatLiteral,
							_
								=> HelpIsNeeded::LvalueOrSauOrSi,
						}
					),
				};
			}
		}
		keep_going
	};
	dbg!(cursor.code());
	while let Some((current, grapheme)) = graphemes.next() {
		dbg!((current, grapheme));
		if !dbg!(do_the_thing(&mut cursor, Some(grapheme), current, &mut graphemes)) { break }
	}
	if graphemes.next().is_none() {
		let current = cursor.code().len();
		do_the_thing(&mut cursor, None, current, &mut graphemes);
	}
	expression_constructor.finish()
		.map(|rvalue| (cursor, rvalue))
		.map_err(|_| cursor.make_error(LineParsingErrorKind::ExpectationError))
}

fn global_catastrophe<'a>(expecting: &Expecting, state: &State, current: &'a str, next: Option<&'a str>) -> Catastrophe<BoolOrFloatRvalue<'a>, BoolUnaryOp, BoolOrFloatBinaryOp> {
	fn bool_bool_binary_op_token<'a>(op: BoolBoolBinaryOp) -> What<BoolOrFloatRvalue<'a>, BoolUnaryOp, BoolOrFloatBinaryOp> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolBoolBinaryOp(op)))) }
	fn bool_float_binary_op_token<'a>(op: BoolFloatBinaryOp) -> What<BoolOrFloatRvalue<'a>, BoolUnaryOp, BoolOrFloatBinaryOp> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(op)))) }
	fn pass_to_float_catastrophe<'a>(expecting: &Expecting, state: &State, current: &'a str, next: Option<&'a str>) -> Catastrophe<BoolOrFloatRvalue<'a>, BoolUnaryOp, BoolOrFloatBinaryOp> {
		match float_catastrophe(expecting, state, current, next) {
			Catastrophe::Skipped => Catastrophe::Skipped,
			Catastrophe::Parsed(what) => Catastrophe::Parsed({
				match what {
					What::Operand(operand) => What::Operand(BoolOrFloatRvalue::FloatRvalue(operand)),
					What::Token(token) => What::Token(match token {
						Token::Lparen => Token::Lparen,
						Token::Rparen => Token::Rparen,
						Token::UnaryOp(op) => todo!(),
						Token::BinaryOp(op) => Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(op)),
					}),
				}
			}),
		}
	}
	match state {
		State::ParsingOther(help) => {
			match (next, help, expecting, current) {
				(None, HelpIsNeeded::LvalueOrSauOrSi, Expecting::Operator, "si") =>
					Catastrophe::Parsed(bool_bool_binary_op_token(BoolBoolBinaryOp::And)),
				(None, HelpIsNeeded::LvalueOrSauOrSi, Expecting::Operator, "sau") =>
					Catastrophe::Parsed(bool_bool_binary_op_token(BoolBoolBinaryOp::Or)),
				_ =>
					pass_to_float_catastrophe(expecting, state, current, next),
			}
		}
		State::ParsingReserved => {
			let parse_reserved = |name: &'a str| -> Option<What<BoolOrFloatRvalue<'a>, BoolUnaryOp, BoolOrFloatBinaryOp>> {
				match name {
					"=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Equ)),
					"!=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Nequ)),
					"<" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Lt)),
					"<=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Lte)),
					">" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Gt)),
					">=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Gte)),
					
					_ => match pass_to_float_catastrophe(expecting, state, current, next) {
						Catastrophe::Skipped => None,
						Catastrophe::Parsed(what) => Some(what),
					}
				}
			};
			let what = if let Some(next) = next {
				if parse_reserved(next).is_none() {
					parse_reserved(current)
				} else {
					None
				}
			} else {
				parse_reserved(current)
			};
			if let Some(what) = what {
				Catastrophe::Parsed(what)
			} else {
				Catastrophe::Skipped
			}
		}
		State::Unsure => Catastrophe::Skipped
	}
}

fn float_catastrophe<'a>(expecting: &Expecting, state: &State, current: &'a str, next: Option<&'a str>) -> Catastrophe<FloatRvalue<'a>, FloatUnaryOp, FloatBinaryOp> {
	fn float_binary_op_token<'a>(op: FloatBinaryOp) -> What<FloatRvalue<'a>, FloatUnaryOp, FloatBinaryOp> { What::Token(Token::BinaryOp(op)) }
	match state {
		State::ParsingOther(help) => {
			if next.is_none() {
				match help {
					HelpIsNeeded::LvalueOrSauOrSi =>
						Catastrophe::Parsed(What::Operand(FloatRvalue::Lvalue(Lvalue(current)))),
					HelpIsNeeded::FloatLiteral =>
						Catastrophe::Parsed(What::Operand(FloatRvalue::Literal(dbg!(current).parse().unwrap()))),
				}
			} else { Catastrophe::Skipped }
		}
		State::ParsingReserved => {
			fn parse_reserved<'a>(name: &'a str) -> Option<What<FloatRvalue<'a>, FloatUnaryOp, FloatBinaryOp>> {
				match name {
					"(" => Some(What::Token(Token::Lparen)),
					")" => Some(What::Token(Token::Rparen)),
					
					"+" => Some(float_binary_op_token(FloatBinaryOp::Add)),
					"-" => Some(float_binary_op_token(FloatBinaryOp::Sub)),
					"*" => Some(float_binary_op_token(FloatBinaryOp::Mul)),
					"/" => Some(float_binary_op_token(FloatBinaryOp::Div)),
					"%" => Some(float_binary_op_token(FloatBinaryOp::Rem)),
					
					_ => None,
				}
			}
			let token = if let Some(next) = next {
				if parse_reserved(next).is_none() {
					parse_reserved(current)
				} else {
					None
				}
			} else {
				parse_reserved(current)
			};
			if let Some(token) = token {
				Catastrophe::Parsed(token)
			} else {
				Catastrophe::Skipped
			}
		}
		State::Unsure => Catastrophe::Skipped
	}
}

impl<'a> LineCursor<'a> {
	pub fn parse_float_rvalue(self) -> LineParsingIntermediateResult<'a, FloatRvalue<'a>> {
		parsinate(
			self, float_catastrophe,
			ExpressionConstructor::new(float_get_priority, float_eval),
		)
	}
	pub fn parse_bool_rvalue(self) -> LineParsingIntermediateResult<'a, BoolRvalue<'a>> {
		let result = parsinate(
			self, global_catastrophe,
			ExpressionConstructor::new(bool_get_priority, bool_eval),
		);
		match result {
			Ok((new_self, BoolOrFloatRvalue::BoolRvalue(rvalue))) => Ok((new_self, rvalue)),
			_ => Err(self.make_error(LineParsingErrorKind::ExpectedBoolRvalue)),
		}
	}
}
