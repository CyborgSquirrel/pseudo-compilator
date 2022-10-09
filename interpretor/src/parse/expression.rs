use enumflags2::{bitflags, BitFlags, make_bitflags};

use trace::trace;
trace::init_depth_var!();

use unicode_segmentation::UnicodeSegmentation;
use super::{LineCursor, LineParsingIntermediateResult, LineParsingErrorKind, get_grapheme_kind, GraphemeKind};
use crate::{syntax::{
	Lvalue,
	FloatUnop, FloatBinop, BoolBinop, FloatRvalue, BoolRvalue, BoolUnop, BoolBoolBinop, BoolFloatBinop
}, parse::LineParsingError};

// bool and float stuff
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolOrFloatUnop {
	BoolUnop(BoolUnop),
	FloatUnop(FloatUnop),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolOrFloatBinop {
	BoolBinop(BoolBinop),
	FloatBinop(FloatBinop),
}

#[derive(Debug)]
enum BoolOrFloatRvalue<'a> {
	BoolRvalue(BoolRvalue<'a>),
	FloatRvalue(FloatRvalue<'a>),
}

#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expecting {
	PrefixUnop,
	
	// This also includes LParens.
	Rvalue,
	
	// This also includes RParens, and is a marker for the end.
	Operator,
}
pub type ExpectingFlags = BitFlags<Expecting>;

#[derive(Debug)]
enum Operator<'a> {
	ParenUnop(&'a str),
	PrefixUnop(BoolOrFloatUnop),
	Binop(BoolOrFloatBinop),
}

fn get_priority(operator: &Operator) -> u32 {
	match operator {
		Operator::ParenUnop(..) => 0,
		Operator::PrefixUnop(..) => 1,
		Operator::Binop(op) => match op {
			BoolOrFloatBinop::BoolBinop(op) => match op {
				BoolBinop::BoolBoolBinop(op) => match op {
					BoolBoolBinop::Or => 2,
					BoolBoolBinop::And => 3,
				}
				BoolBinop::BoolFloatBinop(op) => match op {
					BoolFloatBinop::Eq => 4,
					BoolFloatBinop::Neq => 4,
					BoolFloatBinop::Lt => 4,
					BoolFloatBinop::Lte => 4,
					BoolFloatBinop::Gt => 4,
					BoolFloatBinop::Gte => 4,
					BoolFloatBinop::Divides => 4,
				}
			}
			BoolOrFloatBinop::FloatBinop(op) => match op {
				FloatBinop::Add => 5,
				FloatBinop::Sub => 5,
				FloatBinop::Mul => 6,
				FloatBinop::Div => 6,
				FloatBinop::Rem => 6,
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

macro_rules! unwrap_or_return {
	( $option:expr, $none:expr ) => {
		match $option {
			Some(some) => some,
			None => return $none,
		}
	}
}

impl<'a> Parser<'a> {
	fn new(cursor: LineCursor<'a>) -> Self {
		Self {
			cursor,
			expecting: make_bitflags!(Expecting::{PrefixUnop | Rvalue}),
			operands: Vec::new(),
			operators: Vec::new(),
		}
	}
	
	#[trace]
	fn try_prefix_float_unop(&mut self) -> bool {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = match grapheme_0 {
			"+" => FloatUnop::Ident,
			"-" => FloatUnop::Neg,
			_ => return false,
		};
		
		let op = Operator::PrefixUnop(BoolOrFloatUnop::FloatUnop(op));
		self.expecting = make_bitflags!(Expecting::{Rvalue});
		self.cursor = new_cursor;
		self.operators.push(op);
		true
	}
	
	#[trace]
	fn try_lparen_unop(&mut self) -> bool {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = match grapheme_0 {
			"(" => "(",
			"[" => "[",
			_ => return false,
		};
		
		self.operators.push(Operator::ParenUnop(op));
		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});
		self.cursor = new_cursor;
		true
	}
	
	#[trace]
	fn try_rparen_unop(&mut self) -> Result<bool> {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let op = match grapheme_0 {
			")" => ")",
			"]" => "]",
			_ => return Ok(false),
		};
		
		let rparen_op = op;
		while !self.operators.is_empty()
			&& !matches!(self.operators.last(), Some(Operator::ParenUnop(..))) {
			self.eval_top_operation()?;
		}
		
		if let Some(Operator::ParenUnop(lparen_op)) = self.operators.pop() {
			let x = self.operands.pop().unwrap();
			let result = match x {
				BoolOrFloatRvalue::BoolRvalue(..) => {
					let _op = match (lparen_op, rparen_op) {
						("(", ")") => BoolUnop::Ident,
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::MismatchedParens)),
					};
					x
				}
				BoolOrFloatRvalue::FloatRvalue(x) => {
					let op = match (lparen_op, rparen_op) {
						("(", ")") => FloatUnop::Ident,
						("[", "]") => FloatUnop::Whole,
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::MismatchedParens)),
					};
					BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Unop(op, Box::new(x)))
				}
			};
			self.operands.push(result);
			self.expecting = make_bitflags!(Expecting::{Operator});
			self.cursor = new_cursor;
			Ok(true)
		} else { Err(self.cursor.make_error(LineParsingErrorKind::UnclosedRParen)) }
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
			Operator::ParenUnop(..) => return Err(self.cursor.make_error(LineParsingErrorKind::UnclosedLParen)),
			Operator::PrefixUnop(op) => {
				let x = self.operands.pop().unwrap();
				match op {
					// the operation will always be Ident, so we just ignore it
					BoolOrFloatUnop::BoolUnop(..) => x,
					BoolOrFloatUnop::FloatUnop(op) => match x {
						BoolOrFloatRvalue::FloatRvalue(x)
							=> BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Unop(op, Box::new(x))),
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatUnopOperands(op))),
					}
				}
			}
			Operator::Binop(op) => {
				let y = self.operands.pop().unwrap();
				let x = self.operands.pop().unwrap();
				match op {
					BoolOrFloatBinop::FloatBinop(op) => match (x, y) {
						(BoolOrFloatRvalue::FloatRvalue(x), BoolOrFloatRvalue::FloatRvalue(y))
							=> BoolOrFloatRvalue::FloatRvalue(FloatRvalue::Binop(op, Box::new(x), Box::new(y))),
						_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatBinopOperands(op))),
					}
					BoolOrFloatBinop::BoolBinop(op) => match op {
						BoolBinop::BoolFloatBinop(op) => match (x, y) {
							(BoolOrFloatRvalue::FloatRvalue(x), BoolOrFloatRvalue::FloatRvalue(y))
								=> BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolFloatBinop(op, x, y)),
							_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolFloatBinopOperands(op))),
						}
						BoolBinop::BoolBoolBinop(op) => match (x, y) {
							(BoolOrFloatRvalue::BoolRvalue(x), BoolOrFloatRvalue::BoolRvalue(y))
								=> BoolOrFloatRvalue::BoolRvalue(BoolRvalue::BoolBoolBinop(op, Box::new(x), Box::new(y))),
							_ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolBoolBinopOperands(op))),
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
	fn try_float_binop(&mut self) -> Result<bool> {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let op = match grapheme_0 {
			"+" => FloatBinop::Add,
			"-" => FloatBinop::Sub,
			"*" => FloatBinop::Mul,
			"/" => FloatBinop::Div,
			"%" => FloatBinop::Rem,
			_ => return Ok(false),
		};
		
		let op = Operator::Binop(BoolOrFloatBinop::FloatBinop(op));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;
		self.operators.push(op);
		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn try_bool_float_binop(&mut self) -> Result<bool> {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let (new_cursor, op) = match grapheme_0 {
			"=" => (new_cursor, BoolFloatBinop::Eq),
			
			"!" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinop::Neq)
			} else { return Ok(false) }
			
			"<" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinop::Gte)
			} else { (new_cursor, BoolFloatBinop::Gt) }
			
			">" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinop::Lte)
			} else { (new_cursor, BoolFloatBinop::Lt) }
			
			"|" => (new_cursor, BoolFloatBinop::Divides),
			
			_ => return Ok(false),
		};
		
		let op = Operator::Binop(BoolOrFloatBinop::BoolBinop(BoolBinop::BoolFloatBinop(op)));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;
		self.operators.push(op);
		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn try_bool_bool_binop(&mut self) -> Result<bool> {
		let (new_cursor, name) = self.cursor.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
		let op = match name {
			"sau" => BoolBoolBinop::And,
			"si"|"și" => BoolBoolBinop::Or,
			_ => return Ok(false),
		};
		
		let op = Operator::Binop(BoolOrFloatBinop::BoolBinop(BoolBinop::BoolBoolBinop(op)));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;
		self.operators.push(op);
		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn parse_expecting(&mut self) -> Result<bool> {
		self.cursor = self.cursor.skip_spaces();
		dbg!(self.cursor.code());
		
		// NOTE: I took advantage of short-circuiting here to make
		// the code a bit terser (and arguably more understandable).
		Ok(    (self.expecting.contains(Expecting::PrefixUnop) && self.try_prefix_float_unop())
			|| (self.expecting.contains(Expecting::Rvalue) && self.try_lparen_unop())
			|| (self.expecting.contains(Expecting::Operator) && self.try_rparen_unop()?)
			|| (self.expecting.contains(Expecting::Operator) && (
				   self.try_float_binop()?
				|| self.try_bool_float_binop()?
				|| self.try_bool_bool_binop()? ))
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
			let result = self.operands.pop().unwrap();
			Ok((self.cursor, result))
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
