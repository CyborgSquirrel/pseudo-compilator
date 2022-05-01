use trace::trace;
trace::init_depth_var!();

use unicode_segmentation::{UnicodeSegmentation, GraphemeIndices};
use super::{LineCursor, LineParsingIntermediateResult, LineParsingErrorKind, get_grapheme_kind, GraphemeKind};
use crate::syntax::{
	Lvalue,
	FloatUnaryOp, BoolUnaryOp,
	FloatBinaryOp, BoolBinaryOp, BoolFloatBinaryOp, BoolBoolBinaryOp,
	FloatRvalue, BoolRvalue,
};

// bool and float stuff
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolOrFloatUnaryOp {
	BoolUnaryOp(BoolUnaryOp),
	FloatUnaryOp(FloatUnaryOp),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolOrFloatBinaryOp {
	BoolBinaryOp(BoolBinaryOp),
	FloatBinaryOp(FloatBinaryOp),
}

#[derive(Debug)]
enum BoolOrFloatRvalue<'a> {
	BoolRvalue(BoolRvalue<'a>),
	FloatRvalue(FloatRvalue<'a>),
}

fn get_priority(operator: &Operator<BoolOrFloatUnaryOp, BoolOrFloatBinaryOp>) -> u32 {
	match operator {
		Operator::ParenOp(_, kind) => {
			match kind {
				ParenKind::Lparen => 0,
				ParenKind::Rparen => panic!(),
			}
		}
		Operator::UnaryOp(..) => 0,
		Operator::BinaryOp(op) => match op {
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

fn evaluate_top_operation<'a>(
	operator: Operator<BoolOrFloatUnaryOp,
	BoolOrFloatBinaryOp>,
	stack: &mut Vec<BoolOrFloatRvalue<'a>>
) -> ExpressionConstructorResult<BoolOrFloatRvalue<'a>, BoolOrFloatUnaryOp, BoolOrFloatBinaryOp> {
	use ExpressionConstructionError::*;
	match operator {
		Operator::ParenOp(_, kind) => match kind {
			ParenKind::Lparen => Err(UnclosedLparen),
			ParenKind::Rparen => panic!(),
		}
		Operator::BinaryOp(op) => {
			let x = stack.pop().ok_or(MissingOperand)?;
			let y = stack.pop().ok_or(MissingOperand)?;
			match op {
				BoolOrFloatBinaryOp::BoolBinaryOp(op) => {
					match op {
						BoolBinaryOp::BoolBoolBinaryOp(op) => {
							if let (BoolOrFloatRvalue::BoolRvalue(y), BoolOrFloatRvalue::BoolRvalue(x)) = (y, x) {
								return Ok(BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolBoolBinaryOp(op, Box::new(y), Box::new(x))));
							}
						}
						BoolBinaryOp::BoolFloatBinaryOp(op) => {
							if let (BoolOrFloatRvalue::FloatRvalue(y), BoolOrFloatRvalue::FloatRvalue(x)) = (y, x) {
								return Ok(BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolFloatBinaryOp(op, y, x)));
							}
						}
					}
				}
				BoolOrFloatBinaryOp::FloatBinaryOp(op) => {
					if let (BoolOrFloatRvalue::FloatRvalue(y), BoolOrFloatRvalue::FloatRvalue(x)) = (y, x) {
						return Ok(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::BinaryOp(op, Box::new(y), Box::new(x))));
					}
				}
			}
			
			Err(InvalidBinaryOpOperands(op))
		}
		Operator::UnaryOp(op) => {
			let x = stack.pop().ok_or(MissingOperand)?;
			match (op, x) {
				(BoolOrFloatUnaryOp::FloatUnaryOp(op), BoolOrFloatRvalue::FloatRvalue(x)) =>
					Ok(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::UnaryOp(op, Box::new(x)))),
				_ =>
					Err(InvalidUnaryOpOperands(op)),
			}
		}
	}
}

#[derive(Debug)]
struct ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvaluateTopOperationFn> {
	operators: Vec<Operator<UnaryOp, BinaryOp>>,
	operands: Vec<Operand>,
	expecting: Expecting,
	get_priority: GetPriorityFn,
	evaluate_top_operation: EvaluateTopOperationFn,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Expecting {
	Operand, OperandOrUnaryOperator, BinaryOperator
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionConstructionError<UnaryOp, BinaryOp> {
	ExpectationError(Expecting),
	MismatchedParens, UnclosedRparen, UnclosedLparen,
	InvalidUnaryOpOperands(UnaryOp),
	InvalidBinaryOpOperands(BinaryOp),
	MissingOperand,
}

impl From<ExpressionConstructionError<BoolOrFloatUnaryOp, BoolOrFloatBinaryOp>> for LineParsingErrorKind {
	fn from(err: ExpressionConstructionError<BoolOrFloatUnaryOp, BoolOrFloatBinaryOp>) -> Self {
		LineParsingErrorKind::ExpressionConstructionError(err)
	}
}

type ExpressionConstructorResult<T, UnaryOp, BinaryOp> = Result<T, ExpressionConstructionError<UnaryOp, BinaryOp>>;

impl<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn>
ExpressionConstructor<UnaryOp, BinaryOp, Operand, GetPriorityFn, EvalFn>
where
	GetPriorityFn: Fn(&Operator<UnaryOp, BinaryOp>) -> u32,
	EvalFn: Fn(Operator<UnaryOp, BinaryOp>, &mut Vec<Operand>) -> ExpressionConstructorResult<Operand, UnaryOp, BinaryOp>,
	Operand: std::fmt::Debug,
	UnaryOp: std::fmt::Debug + Eq,
	BinaryOp: std::fmt::Debug,
{
	fn new(get_priority: GetPriorityFn, evaluate_top_operation: EvalFn) -> Self {
		Self {
			operators: Vec::new(),
			operands: Vec::new(),
			expecting: Expecting::OperandOrUnaryOperator,
			get_priority,
			evaluate_top_operation,
		}
	}
	fn make_expectation_error(&self) -> ExpressionConstructionError<UnaryOp, BinaryOp> {
		ExpressionConstructionError::ExpectationError(self.expecting)
	}
	fn eval_top_op(&mut self) -> ExpressionConstructorResult<(), UnaryOp, BinaryOp> {
		let last = self.operators.pop().ok_or(self.make_expectation_error())?;
		let operand = (self.evaluate_top_operation)(last, &mut self.operands)?;
		self.operands.push(operand);
		Ok(())
	}
	fn push_operator(&mut self, operator: Operator<UnaryOp, BinaryOp>) -> ExpressionConstructorResult<bool, UnaryOp, BinaryOp> {
		match &operator {
			Operator::ParenOp(rop, rkind) => {
				match rkind {
					ParenKind::Lparen => { }
					ParenKind::Rparen => {
						let mut found_lparen = false;
						while let Some(last) = self.operators.last() {
							if let Operator::ParenOp(lop, lkind) = last {
								assert!(matches!(lkind, ParenKind::Lparen));
								if rop != lop {
									return Err(ExpressionConstructionError::MismatchedParens);
								}
								if let Operator::ParenOp(lop, ParenKind::Lparen) = self.operators.pop().unwrap() {
									self.operators.push(Operator::UnaryOp(lop));
								} else { panic!() }
								found_lparen = true;
								break;
							} else {
								self.eval_top_op()?;
							}
						}
						
						if !found_lparen {
							return Err(ExpressionConstructionError::UnclosedRparen);
						}
					}
				}
			}
			Operator::BinaryOp(..) => {
				if !matches!(self.expecting, Expecting::BinaryOperator) { return Ok(false) }
				self.expecting = Expecting::OperandOrUnaryOperator;
				while let Some(last) = self.operators.last() {
					if (self.get_priority)(&operator) <= (self.get_priority)(last) {
						self.eval_top_op()?;
					} else {
						break;
					}
				}
			}
			Operator::UnaryOp(..) => {
				if !matches!(self.expecting, Expecting::OperandOrUnaryOperator) { return Ok(false) }
				self.expecting = Expecting::Operand;
			}
		}
		if !matches!(operator, Operator::ParenOp(_, ParenKind::Rparen)) {
			self.operators.push(operator);
		}
		Ok(true)
	}
	fn push_operand(&mut self, operand: Operand) -> bool {
		if !matches!(self.expecting, Expecting::Operand|Expecting::OperandOrUnaryOperator) { return false }
		self.expecting = Expecting::BinaryOperator;
		self.operands.push(operand);
		true
	}
	fn finish(mut self) -> ExpressionConstructorResult<Operand, UnaryOp, BinaryOp> {
		while !self.operators.is_empty() {
			self.eval_top_op()?;
		}
		match self.operands.len().cmp(&1) {
			std::cmp::Ordering::Less => Err(self.make_expectation_error()),
			std::cmp::Ordering::Greater => Err(self.make_expectation_error()),
			std::cmp::Ordering::Equal => Ok(self.operands.pop().unwrap()),
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
enum ParenKind { Lparen, Rparen }

#[derive(Debug)]
enum Operator<UnaryOp, BinaryOp> { ParenOp(UnaryOp, ParenKind), UnaryOp(UnaryOp), BinaryOp(BinaryOp) }

#[derive(Debug)]
enum Token<'a> {
	Operand(BoolOrFloatRvalue<'a>),
	Operator(Operator<BoolOrFloatUnaryOp, BoolOrFloatBinaryOp>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenParsingError {
	InvalidFloatLiteral(String),
	InvalidOperator(String),
}

impl From<TokenParsingError> for LineParsingErrorKind {
	fn from(err: TokenParsingError) -> Self {
		LineParsingErrorKind::TokenParsingError(err)
	}
}

fn try_parse_token<'a>(
	expecting: &Expecting,
	state: &State,
	current: &'a str,
	next: Option<&'a str>
) -> Option<Result<Token<'a>, TokenParsingError>> {
	fn bool_bool_binary_op_operand<'a>(op: BoolBoolBinaryOp) -> Token<'a> { Token::Operator(Operator::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolBoolBinaryOp(op)))) }
	fn bool_float_binary_op_operand<'a>(op: BoolFloatBinaryOp) -> Token<'a> { Token::Operator(Operator::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(op)))) }
	fn float_rvalue<'a>(rvalue: FloatRvalue<'a>) -> Token<'a> { Token::Operand(BoolOrFloatRvalue::FloatRvalue(rvalue)) }
	fn float_binary_op_operand<'a>(op: FloatBinaryOp) -> Token<'a> { Token::Operator(Operator::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(op))) }
	fn float_unary_op_operand<'a>(op: FloatUnaryOp) -> Token<'a> { Token::Operator(Operator::UnaryOp(BoolOrFloatUnaryOp::FloatUnaryOp(op))) }
	fn float_paren_op_operand<'a>(op: FloatUnaryOp, kind: ParenKind) -> Token<'a> { Token::Operator(Operator::ParenOp(BoolOrFloatUnaryOp::FloatUnaryOp(op), kind)) }
	match state {
		State::ParsingOther(other_kind) => {
			if next.is_none() {
				Some( match other_kind {
					OtherKind::LvalueOrSauOrSi => Ok( match (expecting, current) {
						(Expecting::BinaryOperator, "si"|"și") =>
							bool_bool_binary_op_operand(BoolBoolBinaryOp::And),
						(Expecting::BinaryOperator, "sau") =>
							bool_bool_binary_op_operand(BoolBoolBinaryOp::Or),
						(_, _) =>
							float_rvalue(FloatRvalue::Lvalue(Lvalue(current))),
					}),
					OtherKind::FloatLiteral => {
						if let Ok(literal) = current.parse() {
							Ok(float_rvalue(FloatRvalue::Literal(literal)))
						} else { Err(TokenParsingError::InvalidFloatLiteral(String::from(current))) }
					}
				})
			} else { None }
		}
		State::ParsingReserved => {
			let parse_reserved = |name: &'a str| -> Option<Option<Token<'a>>> {
				match name {
					""   => Some(None),
					
					"("  => Some(Some(float_paren_op_operand(FloatUnaryOp::Ident, ParenKind::Lparen))),
					")"  => Some(Some(float_paren_op_operand(FloatUnaryOp::Ident, ParenKind::Rparen))),
					
					"["  => Some(Some(float_paren_op_operand(FloatUnaryOp::Whole, ParenKind::Lparen))),
					"]"  => Some(Some(float_paren_op_operand(FloatUnaryOp::Whole, ParenKind::Rparen))),
					
					"="  => Some(Some(bool_float_binary_op_operand(BoolFloatBinaryOp::Equ))),
					"!"  => Some(None),
					"!=" => Some(Some(bool_float_binary_op_operand(BoolFloatBinaryOp::Nequ))),
					"<"  => Some(Some(bool_float_binary_op_operand(BoolFloatBinaryOp::Lt))),
					"<=" => Some(Some(bool_float_binary_op_operand(BoolFloatBinaryOp::Lte))),
					">"  => Some(Some(bool_float_binary_op_operand(BoolFloatBinaryOp::Gt))),
					">=" => Some(Some(bool_float_binary_op_operand(BoolFloatBinaryOp::Gte))),
					"|"  => Some(Some(bool_float_binary_op_operand(BoolFloatBinaryOp::Divides))),
					
					"+" => match expecting {
						Expecting::BinaryOperator => Some(Some(float_binary_op_operand(FloatBinaryOp::Add))),
						Expecting::OperandOrUnaryOperator => Some(Some(float_unary_op_operand(FloatUnaryOp::Ident))),
						_ => None,
					}
					"-" => match expecting {
						Expecting::BinaryOperator => Some(Some(float_binary_op_operand(FloatBinaryOp::Sub))),
						Expecting::OperandOrUnaryOperator => Some(Some(float_unary_op_operand(FloatUnaryOp::Neg))),
						_ => None,
					}
					
					"*" => Some(Some(float_binary_op_operand(FloatBinaryOp::Mul))),
					"/" => Some(Some(float_binary_op_operand(FloatBinaryOp::Div))),
					"%" => Some(Some(float_binary_op_operand(FloatBinaryOp::Rem))),
					
					_ => None,
				}
			};
			let must_push =
				if let Some(next) = next { parse_reserved(next).is_none() }
				else { matches!(expecting, Expecting::BinaryOperator|Expecting::OperandOrUnaryOperator) };
			if must_push {
				Some(
					parse_reserved(current)
						.flatten()
						.ok_or(TokenParsingError::InvalidOperator(String::from(current)))
				)
			} else { None }
		}
		State::Unsure => None,
	}
}

impl<'a> LineCursor<'a> {
	fn parse_bool_or_float_rvalue(mut self) -> LineParsingIntermediateResult<'a, BoolOrFloatRvalue<'a>> {
		let mut expression_constructor = ExpressionConstructor::new(get_priority, evaluate_top_operation);
		let mut state = State::Unsure;
		let mut graphemes = self.code().grapheme_indices(true);
		
		let mut try_push_token = |cursor: &mut LineCursor<'a>, grapheme, current, graphemes: &mut GraphemeIndices<'a>| {
			let mut keep_going = true;
			
			let grapheme_and_its_kind =
				if let Some(grapheme) = grapheme {
					if let Some(grapheme_kind) = get_grapheme_kind(grapheme) {
						Some((grapheme, grapheme_kind))
					} else { keep_going = false; None }
				} else { None };
			
			let next =
				if let Some((grapheme, grapheme_kind)) = &grapheme_and_its_kind {
					match (grapheme_kind, &state) {
						(GraphemeKind::Reserved, State::ParsingReserved) |
						(GraphemeKind::Other, State::ParsingOther(..))
							=> Some(cursor.code_until(current+grapheme.len())),
						_
							=> None,
					}
				} else { None };
			
			let token = try_parse_token(
				&expression_constructor.expecting, &state,
				cursor.code_until(current), next
			);
			match token {
				None => { }
				Some(token) => {
					let token = token.map_err(|err| match (expression_constructor.expecting, &err) {
						(Expecting::OperandOrUnaryOperator|Expecting::Operand, TokenParsingError::InvalidFloatLiteral(..))|
						(Expecting::OperandOrUnaryOperator|Expecting::BinaryOperator, TokenParsingError::InvalidOperator(..))
							=> cursor.make_error(err),
						_
							=> cursor.make_error(ExpressionConstructionError::ExpectationError(expression_constructor.expecting)),
					})?;
					
					match token {
						Token::Operand(BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Lvalue(Lvalue(
							"atunci"|"executa"|"execută"
						)))) => {
							keep_going = false;
						}
						token => {
							if match token {
								Token::Operand(operand) => expression_constructor.push_operand(operand),
								Token::Operator(operand) => expression_constructor.push_operator(operand).map_err(|err| cursor.make_error(err))?,
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
			if !try_push_token(&mut self, Some(grapheme), current, &mut graphemes)? { break };
		}
		if graphemes.next().is_none() {
			let current = self.code().len();
			try_push_token(&mut self, None, current, &mut graphemes)?;
		}
		expression_constructor.finish()
			.map(|rvalue| (self, rvalue))
			.map_err(|err| self.make_error(err))
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
