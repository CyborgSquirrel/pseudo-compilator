use unicode_segmentation::{UnicodeSegmentation, GraphemeIndices};
use crate::{LineCursor, LineParsingIntermediateResult, LineParsingErrorKind, get_grapheme_kind, GraphemeKind};

// float stuff
#[derive(Debug)]
pub struct Lvalue<'a> (pub &'a str);

#[derive(Debug)]
pub enum FloatRvalue<'a> {
	Literal(f32),
	Lvalue(Lvalue<'a>),
	UnaryOp(FloatUnaryOp, Box<FloatRvalue<'a>>),
	BinaryOp(FloatBinaryOp, Box<FloatRvalue<'a>>, Box<FloatRvalue<'a>>),
}

#[derive(Debug)]
pub enum FloatUnaryOp { Ident, Neg, Whole }
impl FloatUnaryOp {
	pub fn evaluate(&self, x: f32) -> f32 {
		match self {
			FloatUnaryOp::Ident => x,
			FloatUnaryOp::Neg => -x,
			FloatUnaryOp::Whole => x.floor(),
		}
	}
}

#[derive(Debug)]
pub enum FloatBinaryOp { Add, Sub, Mul, Div, Rem }
impl FloatBinaryOp {
	pub fn evaluate(&self, x: f32, y: f32) -> f32 {
		match &self {
			FloatBinaryOp::Add => x+y,
			FloatBinaryOp::Sub => x-y,
			FloatBinaryOp::Mul => x*y,
			FloatBinaryOp::Div => x/y,
			FloatBinaryOp::Rem => ((x as i32) % (y as i32)) as f32,
		}
	}
}

// bool stuff
#[derive(Debug)]
pub enum BoolRvalue<'a> {
	BoolFloatBinaryOp(BoolFloatBinaryOp, FloatRvalue<'a>, FloatRvalue<'a>),
	BoolBoolBinaryOp(BoolBoolBinaryOp, Box<BoolRvalue<'a>>, Box<BoolRvalue<'a>>),
}

#[derive(Debug)]
pub enum BoolUnaryOp { }

#[derive(Debug)]
pub enum BoolFloatBinaryOp {
	Equ, Nequ, Lt, Gt, Lte, Gte,
	Divides,
}
impl BoolFloatBinaryOp {
	pub fn evaluate(&self, x: f32, y: f32) -> bool {
		match self {
			BoolFloatBinaryOp::Equ => x == y,
			BoolFloatBinaryOp::Nequ => x != y,
			BoolFloatBinaryOp::Lt => x < y,
			BoolFloatBinaryOp::Gt => x > y,
			BoolFloatBinaryOp::Lte => x <= y,
			BoolFloatBinaryOp::Gte => x >= y,
			BoolFloatBinaryOp::Divides => (y as i32) % (x as i32) == 0,
		}
	}
}

#[derive(Debug)]
pub enum BoolBoolBinaryOp {
	And, Or,
}
impl BoolBoolBinaryOp{
	pub fn evaluate(&self, x: bool, y: bool) -> bool {
		match self {
			BoolBoolBinaryOp::And => x && y,
			BoolBoolBinaryOp::Or => x || y,
		}
	}
}

#[derive(Debug)]
pub enum BoolBinaryOp {
	BoolFloatBinaryOp(BoolFloatBinaryOp),
	BoolBoolBinaryOp(BoolBoolBinaryOp),
}

#[derive(Debug)]
enum Token <UnaryOp, BinaryOp> { Lparen, Rparen, UnaryOp(UnaryOp), BinaryOp(BinaryOp) }

#[derive(Debug)]
struct ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn> {
	tokens: Vec<Token<UnaryOp, BinaryOp>>,
	operands: Vec<Operand>,
	expecting: Expecting,
	get_priority: GetPriorityFn,
	eval: EvalFn,
}

#[derive(Debug, Clone, Copy)]
pub enum Expecting {
	Operand, OperandOrUnaryOperator, BinaryOperator
}

#[derive(Debug)]
pub struct ExpressionConstructionError(pub Expecting);

type ExpressionConstructorResult<T> = Result<T, ExpressionConstructionError>;

impl<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn>
ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn>
where
	GetPriorityFn: Fn(&Token<UnaryOp, BinaryOp>) -> u32,
	EvalFn: Fn(Token<UnaryOp, BinaryOp>, &mut Vec<Operand>) -> Option<Operand>,
	Operand: std::fmt::Debug,
	UnaryOp: std::fmt::Debug,
	BinaryOp: std::fmt::Debug,
{
	fn new(get_priority: GetPriorityFn, eval: EvalFn) -> Self {
		Self {
			tokens: Vec::new(),
			operands: Vec::new(),
			expecting: Expecting::OperandOrUnaryOperator,
			get_priority,
			eval,
		}
	}
	fn make_error(&self) -> ExpressionConstructionError {
		ExpressionConstructionError(self.expecting)
	}
	fn eval_top_op(&mut self) -> ExpressionConstructorResult<()> {
		dbg!(&self.tokens);
		dbg!(&self.operands);
		let last = self.tokens.pop().ok_or(self.make_error())?;
		let operand = (self.eval)(last, &mut self.operands).ok_or(self.make_error())?;
		self.operands.push(operand);
		Ok(())
	}
	fn push_token(&mut self, token: Token<UnaryOp, BinaryOp>) -> ExpressionConstructorResult<bool> {
		match &token {
			Token::Lparen => { }
			Token::Rparen => {
				while let Some(last) = self.tokens.last() {
					if !matches!(last, Token::Lparen) {
						self.eval_top_op()?;
					} else {
						self.tokens.pop();
						break;
					}
				}
			}
			Token::BinaryOp(..) => {
				if !matches!(self.expecting, Expecting::BinaryOperator) { return Ok(false) }
				self.expecting = Expecting::OperandOrUnaryOperator;
				while let Some(last) = self.tokens.last() {
					if (self.get_priority)(&token) <= (self.get_priority)(last) {
						self.eval_top_op()?;
					} else {
						break;
					}
				}
			}
			Token::UnaryOp(..) => {
				if !matches!(self.expecting, Expecting::OperandOrUnaryOperator) { return Ok(false) }
				self.expecting = Expecting::Operand;
			}
		}
		if !matches!(token, Token::Rparen) {
			self.tokens.push(token);
		}
		Ok(true)
	}
	fn push_operand(&mut self, operand: Operand) -> bool {
		if !matches!(self.expecting, Expecting::Operand|Expecting::OperandOrUnaryOperator) { return false }
		self.expecting = Expecting::BinaryOperator;
		self.operands.push(operand);
		true
	}
	fn finish(mut self) -> ExpressionConstructorResult<Operand> {
		while !self.tokens.is_empty() {
			self.eval_top_op()?;
		}
		match self.operands.len().cmp(&1) {
			std::cmp::Ordering::Less => Err(self.make_error()),
			std::cmp::Ordering::Greater => Err(self.make_error()),
			std::cmp::Ordering::Equal => Ok(self.operands.pop().unwrap()),
		}
	}
}

#[derive(Debug)]
enum BoolOrFloatBinaryOp {
	BoolBinaryOp(BoolBinaryOp),
	FloatBinaryOp(FloatBinaryOp),
}

#[derive(Debug)]
enum BoolOrFloatUnaryOp {
	BoolUnaryOp(BoolUnaryOp),
	FloatUnaryOp(FloatUnaryOp),
}

fn bool_get_priority(token: &Token<BoolOrFloatUnaryOp, BoolOrFloatBinaryOp>) -> u32 {
	match token {
		Token::Lparen => 0,
		Token::Rparen => panic!(),
		Token::UnaryOp(..) => 0,
		Token::BinaryOp(op) => match op {
			BoolOrFloatBinaryOp::BoolBinaryOp(op) => match op {
				BoolBinaryOp::BoolBoolBinaryOp(op) => match op {
					BoolBoolBinaryOp::Or => 1,
					BoolBoolBinaryOp::And => 2,
				}
				BoolBinaryOp::BoolFloatBinaryOp(op) => match op {
					BoolFloatBinaryOp::Equ => 3,
					BoolFloatBinaryOp::Nequ => 3,
					BoolFloatBinaryOp::Lt => 3,
					BoolFloatBinaryOp::Lte => 3,
					BoolFloatBinaryOp::Gt => 3,
					BoolFloatBinaryOp::Gte => 3,
					BoolFloatBinaryOp::Divides => 3,
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

fn bool_eval<'a>(token: Token<BoolOrFloatUnaryOp, BoolOrFloatBinaryOp>, stack: &mut Vec<BoolOrFloatRvalue<'a>>) -> Option<BoolOrFloatRvalue<'a>> {
	match token {
		Token::Lparen => panic!(),
		Token::Rparen => panic!(),
		Token::BinaryOp(op) => {
			let x = stack.pop()?;
			let y = stack.pop()?;
			match op {
				BoolOrFloatBinaryOp::BoolBinaryOp(op) => {
					match op {
						BoolBinaryOp::BoolBoolBinaryOp(op) => {
							if let (BoolOrFloatRvalue::BoolRvalue(y), BoolOrFloatRvalue::BoolRvalue(x)) = (y, x) {
								Some(BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolBoolBinaryOp(op, Box::new(y), Box::new(x))))
							} else { None }
						}
						BoolBinaryOp::BoolFloatBinaryOp(op) => {
							if let (BoolOrFloatRvalue::FloatRvalue(y), BoolOrFloatRvalue::FloatRvalue(x)) = (y, x) {
								Some(BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolFloatBinaryOp(op, y, x)))
							} else { None }
						}
					}
				}
				BoolOrFloatBinaryOp::FloatBinaryOp(op) => {
					if let (BoolOrFloatRvalue::FloatRvalue(y), BoolOrFloatRvalue::FloatRvalue(x)) = (y, x) {
						Some(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::BinaryOp(op, Box::new(y), Box::new(x))))
					} else { None }
				}
			}
		}
		Token::UnaryOp(op) => {
			let x = stack.pop()?;
			match (op, x) {
				(BoolOrFloatUnaryOp::FloatUnaryOp(op), BoolOrFloatRvalue::FloatRvalue(x)) =>
					Some(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::UnaryOp(op, Box::new(x)))),
				_ =>
					None,
			}
		}
	}
}

#[derive(Debug)]
enum State {
	Unsure,
	ParsingReserved,
	ParsingOther(OtherKind),
}

#[derive(Debug)]
enum OtherKind {
	LvalueOrSauOrSi,
	FloatLiteral,
}

#[derive(Debug)]
enum What<'a> {
	Operand(BoolOrFloatRvalue<'a>),
	Token(Token<BoolOrFloatUnaryOp, BoolOrFloatBinaryOp>),
}

#[derive(Debug)]
pub enum CatastropheError {
	InvalidFloatLiteral,
	InvalidOperator,
}

fn global_catastrophe<'a>(
	expecting: &Expecting,
	state: &State,
	current: &'a str,
	next: Option<&'a str>
) -> Option<Result<What<'a>, CatastropheError>> {
	fn bool_bool_binary_op_token<'a>(op: BoolBoolBinaryOp) -> What<'a> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolBoolBinaryOp(op)))) }
	fn bool_float_binary_op_token<'a>(op: BoolFloatBinaryOp) -> What<'a> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(op)))) }
	fn float_rvalue<'a>(rvalue: FloatRvalue<'a>) -> What<'a> { What::Operand(BoolOrFloatRvalue::FloatRvalue(rvalue)) }
	fn float_binary_op_token<'a>(op: FloatBinaryOp) -> What<'a> { What::Token(Token::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(op))) }
	fn float_unary_op_token<'a>(op: FloatUnaryOp) -> What<'a> { What::Token(Token::UnaryOp(BoolOrFloatUnaryOp::FloatUnaryOp(op))) }
	match state {
		State::ParsingOther(help) => {
			if next.is_none() {
				Some( match help {
					OtherKind::LvalueOrSauOrSi => Ok( match (expecting, current) {
						(Expecting::BinaryOperator, "si") =>
							bool_bool_binary_op_token(BoolBoolBinaryOp::And),
						(Expecting::BinaryOperator, "sau") =>
							bool_bool_binary_op_token(BoolBoolBinaryOp::Or),
						(_, _) =>
							float_rvalue(FloatRvalue::Lvalue(Lvalue(current))),
					}),
					OtherKind::FloatLiteral => {
						if let Ok(literal) = current.parse() {
							Ok(float_rvalue(FloatRvalue::Literal(literal)))
						} else { Err(CatastropheError::InvalidFloatLiteral) }
					}
				})
			} else { None }
		}
		State::ParsingReserved => {
			let parse_reserved = |name: &'a str| -> Option<What<'a>> {
				match name {
					"(" => Some(What::Token(Token::Lparen)),
					")" => Some(What::Token(Token::Rparen)),
					
					"=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Equ)),
					"!=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Nequ)),
					"<" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Lt)),
					"<=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Lte)),
					">" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Gt)),
					">=" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Gte)),
					"|" => Some(bool_float_binary_op_token(BoolFloatBinaryOp::Divides)),
					
					"+" => match expecting {
						Expecting::BinaryOperator => Some(float_binary_op_token(FloatBinaryOp::Add)),
						Expecting::OperandOrUnaryOperator => Some(float_unary_op_token(FloatUnaryOp::Ident)),
						_ => None,
					}
					"-" => match expecting {
						Expecting::BinaryOperator => Some(float_binary_op_token(FloatBinaryOp::Sub)),
						Expecting::OperandOrUnaryOperator => Some(float_unary_op_token(FloatUnaryOp::Neg)),
						_ => None,
					}
					
					"*" => Some(float_binary_op_token(FloatBinaryOp::Mul)),
					"/" => Some(float_binary_op_token(FloatBinaryOp::Div)),
					"%" => Some(float_binary_op_token(FloatBinaryOp::Rem)),
					
					_ => None,
				}
			};
			let next = next.and_then(parse_reserved);
			if next.is_none() {
				Some(
					parse_reserved(current)
						.ok_or(CatastropheError::InvalidOperator)
				)
			} else { None }
		}
		State::Unsure => None,
	}
}

impl<'a> LineCursor<'a> {
	fn parse_bool_or_float_rvalue(mut self) -> LineParsingIntermediateResult<'a, BoolOrFloatRvalue<'a>> {
		let mut expression_constructor = ExpressionConstructor::new(bool_get_priority, bool_eval);
		let mut state = State::Unsure;
		let mut graphemes = self.code().grapheme_indices(true);
		let mut do_the_thing = |cursor: &mut LineCursor<'a>, grapheme, current, graphemes: &mut GraphemeIndices<'a>| {
			let mut keep_going = true;
			let grapheme_and_its_kind = if let Some(grapheme) = grapheme {
				if let Some(grapheme_kind) = get_grapheme_kind(grapheme) {
					Some((grapheme, grapheme_kind))
				} else { keep_going = false; None }
			} else { None };
			let next = if let Some((grapheme, grapheme_kind)) = &grapheme_and_its_kind {
				match (grapheme_kind, &state) {
					(GraphemeKind::Reserved, State::ParsingReserved) |
					(GraphemeKind::Other, State::ParsingOther(..))
						=> Some(cursor.code_until(current+grapheme.len())),
					_
						=> None,
				}
			} else { None };
			let result = global_catastrophe(&expression_constructor.expecting, &state, cursor.code_until(current), next);
			match result {
				None => { }
				Some(what) => {
					match what {
						Err(err) => return Err(cursor.make_error(LineParsingErrorKind::CatastropheError(err))),
						Ok(what) => {
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
				}
			}
			if let Some((grapheme, grapheme_kind)) = &grapheme_and_its_kind {
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
									=> OtherKind::FloatLiteral,
								_
									=> OtherKind::LvalueOrSauOrSi,
							}
						),
					};
				}
			}
			Ok(keep_going)
		};
		while let Some((current, grapheme)) = graphemes.next() {
			(current, grapheme);
			if !do_the_thing(&mut self, Some(grapheme), current, &mut graphemes)? { break }
		}
		if graphemes.next().is_none() {
			let current = self.code().len();
			do_the_thing(&mut self, None, current, &mut graphemes)?;
		}
		expression_constructor.finish()
			.map(|rvalue| (self, rvalue))
			.map_err(|err| self.make_error(LineParsingErrorKind::ExpressionParsingError(err)))
	}
	pub fn parse_float_rvalue(self) -> LineParsingIntermediateResult<'a, FloatRvalue<'a>> {
		let rvalue = self.parse_bool_or_float_rvalue()?;
		match rvalue {
			(_, BoolOrFloatRvalue::BoolRvalue(..)) => Err(self.make_error(LineParsingErrorKind::ExpectedFloatRvalue)),
			(new_self, BoolOrFloatRvalue::FloatRvalue(rvalue)) => Ok((new_self, rvalue)),
		}
	}
	pub fn parse_bool_rvalue(self) -> LineParsingIntermediateResult<'a, BoolRvalue<'a>> {
		let rvalue = self.parse_bool_or_float_rvalue()?;
		match rvalue {
			(_, BoolOrFloatRvalue::FloatRvalue(..)) => Err(self.make_error(LineParsingErrorKind::ExpectedBoolRvalue)),
			(new_self, BoolOrFloatRvalue::BoolRvalue(rvalue)) => Ok((new_self, rvalue)),
		}
	}
}
