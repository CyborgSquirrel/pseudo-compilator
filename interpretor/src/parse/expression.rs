use enumflags2::{bitflags, BitFlags, make_bitflags};

use trace::trace;
trace::init_depth_var!();

use unicode_segmentation::{UnicodeSegmentation, GraphemeIndices};
use super::{LineCursor, LineParsingIntermediateResult, LineParsingErrorKind, get_grapheme_kind, GraphemeKind};
use crate::{syntax::{
	Lvalue,
	FloatUnaryOp, BoolUnaryOp,
	FloatBinaryOp, BoolBinaryOp, BoolFloatBinaryOp, BoolBoolBinaryOp,
	FloatRvalue, BoolRvalue,
}, parse::LineParsingError};

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

macro_rules! unwrap_or_return {
	( $option:expr, $none:expr ) => {
		match $option {
			Some(some) => some,
			None => return $none,
		}
	}
}

#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expecting {
	PrefixUnaryOp,
	
	// This one also includes LParens.
	Rvalue,
	
	// This one also include RParens and is a marker for the end.
	Operator,
}
pub type ExpectingFlags = BitFlags<Expecting>;

#[derive(Debug)]
enum Operator<'a> {
	ParenUnaryOp(&'a str),
	PrefixUnaryOp(BoolOrFloatUnaryOp),
	BinaryOp(BoolOrFloatBinaryOp),
}

fn get_priority(operator: &Operator) -> u32 {
	match operator {
		Operator::ParenUnaryOp(..) => 0,
		Operator::PrefixUnaryOp(..) => 1,
		Operator::BinaryOp(op) => match op {
			BoolOrFloatBinaryOp::BoolBinaryOp(op) => match op {
				BoolBinaryOp::BoolBoolBinaryOp(op) => match op {
					BoolBoolBinaryOp::Or => 2,
					BoolBoolBinaryOp::And => 3,
				}
				BoolBinaryOp::BoolFloatBinaryOp(op) => match op {
					BoolFloatBinaryOp::Eq => 4,
					BoolFloatBinaryOp::Neq => 4,
					BoolFloatBinaryOp::Lt => 4,
					BoolFloatBinaryOp::Lte => 4,
					BoolFloatBinaryOp::Gt => 4,
					BoolFloatBinaryOp::Gte => 4,
					BoolFloatBinaryOp::Divides => 4,
				}
			}
			BoolOrFloatBinaryOp::FloatBinaryOp(op) => match op {
				FloatBinaryOp::Add => 5,
				FloatBinaryOp::Sub => 5,
				FloatBinaryOp::Mul => 6,
				FloatBinaryOp::Div => 6,
				FloatBinaryOp::Rem => 6,
			}
		}
	}
}

struct Parser<'a> {
	cursor: LineCursor<'a>,
	expecting: ExpectingFlags,
	operands: Vec<BoolOrFloatRvalue<'a>>,
	operators: Vec<Operator<'a>>,
}

type Result<T> = std::result::Result<T, LineParsingError>;

impl<'a> Parser<'a> {
	fn new(cursor: LineCursor<'a>) -> Self {
		Self {
			cursor,
			expecting: make_bitflags!(Expecting::{PrefixUnaryOp | Rvalue}),
			operands: Vec::new(),
			operators: Vec::new(),
		}
	}
	
	#[trace]
	fn try_prefix_float_unary_op(&mut self) -> bool {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = match grapheme_0 {
			"+" => FloatUnaryOp::Ident,
			"-" => FloatUnaryOp::Neg,
			_ => return false,
		};
		
		let op = Operator::PrefixUnaryOp(BoolOrFloatUnaryOp::FloatUnaryOp(op));
		self.expecting = make_bitflags!(Expecting::{Rvalue});
		self.cursor = new_cursor;
		self.operators.push(op);
		true
	}
	
	#[trace]
	fn try_lparen_unary_op(&mut self) -> bool {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = match grapheme_0 {
			"(" => "(",
			"[" => "[",
			_ => return false,
		};
		
		self.operators.push(Operator::ParenUnaryOp(op));
		self.expecting = make_bitflags!(Expecting::{PrefixUnaryOp | Rvalue});
		self.cursor = new_cursor;
		true
	}
	
	#[trace]
	fn try_rparen_unary_op(&mut self) -> Result<bool> {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let op = match grapheme_0 {
			")" => ")",
			"]" => "]",
			_ => return Ok(false),
		};
		
		let rparen_op = op;
		while !self.operators.is_empty()
			&& !matches!(self.operators.last(), Some(Operator::ParenUnaryOp(..))) {
			self.eval_top_operation()?;
		}
		
		if let Some(Operator::ParenUnaryOp(lparen_op)) = self.operators.pop() {
			let x = self.operands.pop().unwrap();
			let result = match x {
				BoolOrFloatRvalue::BoolRvalue(..) => {
					let _op = match (lparen_op, rparen_op) {
						("(", ")") => BoolUnaryOp::Ident,
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::MismatchedParens)),
					};
					x
				}
				BoolOrFloatRvalue::FloatRvalue(x) => {
					let op = match (lparen_op, rparen_op) {
						("(", ")") => FloatUnaryOp::Ident,
						("[", "]") => FloatUnaryOp::Whole,
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::MismatchedParens)),
					};
					BoolOrFloatRvalue::FloatRvalue(FloatRvalue::UnaryOp(op, Box::new(x)))
				}
			};
			self.operands.push(result);
			self.expecting = make_bitflags!(Expecting::{Operator});
			self.cursor = new_cursor;
			Ok(true)
		} else { Ok(false) }
	}
	
	#[trace]
	fn try_float_rvalue(&mut self) -> Result<bool> {
		let (new_cursor, name) = self.cursor.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
		let rvalue = match unwrap_or_return!(name.graphemes(true).next(), Ok(false)) {
			"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" => match name.parse() {
				Ok(literal) => FloatRvalue::Literal(literal),
				Err(..) => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatLiteral)),
			}
			_ => match name {
				"atunci"|"executa"|"execută"
					=> return Err(self.cursor.make_error(LineParsingErrorKind::InvalidLvalueName)),
				_ => FloatRvalue::Lvalue(Lvalue(name)),
			}
		};
		
		self.operands.push(BoolOrFloatRvalue::FloatRvalue(rvalue));
		self.expecting = make_bitflags!(Expecting::{Operator});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn eval_top_operation(&mut self) -> Result<()> {
		let op = self.operators.pop().unwrap();
		let result = match op {
			Operator::ParenUnaryOp(..) => panic!(),
			Operator::PrefixUnaryOp(op) => {
				let x = self.operands.pop().unwrap();
				match op {
					// the operation will always be Ident, so we just ignore it
					BoolOrFloatUnaryOp::BoolUnaryOp(..) => x,
					BoolOrFloatUnaryOp::FloatUnaryOp(op) => match x {
						BoolOrFloatRvalue::FloatRvalue(x)
							=> BoolOrFloatRvalue::FloatRvalue(FloatRvalue::UnaryOp(op, Box::new(x))),
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatUnaryOpOperands(op))),
					}
				}
			}
			Operator::BinaryOp(op) => {
				let y = self.operands.pop().unwrap();
				let x = self.operands.pop().unwrap();
				match op {
					BoolOrFloatBinaryOp::FloatBinaryOp(op) => match (x, y) {
						(BoolOrFloatRvalue::FloatRvalue(x), BoolOrFloatRvalue::FloatRvalue(y))
							=> BoolOrFloatRvalue::FloatRvalue(FloatRvalue::BinaryOp(op, Box::new(x), Box::new(y))),
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatBinaryOpOperands(op))),
					}
					BoolOrFloatBinaryOp::BoolBinaryOp(op) => match op {
						BoolBinaryOp::BoolFloatBinaryOp(op) => match (x, y) {
							(BoolOrFloatRvalue::FloatRvalue(x), BoolOrFloatRvalue::FloatRvalue(y))
								=> BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolFloatBinaryOp(op, x, y)),
							_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolFloatBinaryOpOperands(op))),
						}
						BoolBinaryOp::BoolBoolBinaryOp(op) => match (x, y) {
							(BoolOrFloatRvalue::BoolRvalue(x), BoolOrFloatRvalue::BoolRvalue(y))
								=> BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolBoolBinaryOp(op, Box::new(x), Box::new(y))),
							_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolBoolBinaryOpOperands(op))),
						}
					}
				}
			}
		};
		self.operands.push(result);
		Ok(())
	}
	
	#[trace]
	fn eval_while_priority_greater_or_equal(&mut self, priority: u32) -> Result<()> {
		while self.operators.last().map_or(false, |last| priority <= get_priority(last)) {
			self.eval_top_operation()?;
		}
		Ok(())
	}
	
	#[trace]
	fn try_float_binary_op(&mut self) -> Result<bool> {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let op = match grapheme_0 {
			"+" => FloatBinaryOp::Add,
			"-" => FloatBinaryOp::Sub,
			"*" => FloatBinaryOp::Mul,
			"/" => FloatBinaryOp::Div,
			"%" => FloatBinaryOp::Rem,
			_ => return Ok(false),
		};
		
		let op = Operator::BinaryOp(BoolOrFloatBinaryOp::FloatBinaryOp(op));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;
		self.operators.push(op);
		self.expecting = make_bitflags!(Expecting::{PrefixUnaryOp | Rvalue});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn try_bool_float_binary_op(&mut self) -> Result<bool> {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let (new_cursor, op) = match grapheme_0 {
			"=" => (new_cursor, BoolFloatBinaryOp::Eq),
			
			"!" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinaryOp::Neq)
			} else { return Ok(false) }
			
			"<" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinaryOp::Gte)
			} else { (new_cursor, BoolFloatBinaryOp::Gt) }
			
			">" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinaryOp::Lte)
			} else { (new_cursor, BoolFloatBinaryOp::Lt) }
			
			"|" => (new_cursor, BoolFloatBinaryOp::Divides),
			
			_ => return Ok(false),
		};
		
		let op = Operator::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolFloatBinaryOp(op)));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;
		self.operators.push(op);
		self.expecting = make_bitflags!(Expecting::{PrefixUnaryOp | Rvalue});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn try_bool_bool_binary_op(&mut self) -> Result<bool> {
		let (new_cursor, name) = self.cursor.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
		let op = match name {
			"sau" => BoolBoolBinaryOp::And,
			"si"|"și" => BoolBoolBinaryOp::Or,
			_ => return Ok(false),
		};
		
		let op = Operator::BinaryOp(BoolOrFloatBinaryOp::BoolBinaryOp(BoolBinaryOp::BoolBoolBinaryOp(op)));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;
		self.operators.push(op);
		self.expecting = make_bitflags!(Expecting::{PrefixUnaryOp | Rvalue});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn parse_expecting(&mut self) -> Result<bool> {
		self.cursor = self.cursor.skip_spaces();
		dbg!(self.cursor.code());
		
		Ok(    (self.expecting.contains(Expecting::PrefixUnaryOp) && self.try_prefix_float_unary_op())
			|| (self.expecting.contains(Expecting::Rvalue) && self.try_lparen_unary_op())
			|| (self.expecting.contains(Expecting::Operator) && self.try_rparen_unary_op()?)
			|| (self.expecting.contains(Expecting::Operator) && (
				   self.try_float_binary_op()?
				|| self.try_bool_float_binary_op()?
				|| self.try_bool_bool_binary_op()? ))
			|| (self.expecting.contains(Expecting::Rvalue) && self.try_float_rvalue()?)
		)
	}
	
	#[trace]
	fn parse(mut self) -> LineParsingIntermediateResult<'a, BoolOrFloatRvalue<'a>> {
		while self.parse_expecting()? {
			dbg!(&self.operators);
		}
		if !self.expecting.contains(Expecting::Operator) {
			Err(self.cursor.make_error(LineParsingErrorKind::ExpectedMegatron(self.expecting)))
		} else {
			while !self.operators.is_empty() {
				self.eval_top_operation()?;
			}
			let result = self.operands.pop();
			Ok((self.cursor, result.unwrap()))
		}
	}
}

impl<'a> LineCursor<'a> {
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
	
	fn parse_bool_or_float_rvalue(self) -> LineParsingIntermediateResult<'a, BoolOrFloatRvalue<'a>> {
		let parser = Parser::new(self);
		parser.parse()
	}
}
