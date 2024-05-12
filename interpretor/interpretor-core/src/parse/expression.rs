use enumflags2::{bitflags, BitFlags, make_bitflags};

use trace::trace;
trace::init_depth_var!();

use unicode_segmentation::UnicodeSegmentation;
use super::{LineCursor, LineParsingIntermediateResult, LineParsingErrorKind, get_grapheme_kind, GraphemeKind};
use crate::{ast::{
	Ident,
	FloatUnop, FloatBinop, BoolBinop, FloatRvalue, BoolRvalue, BoolUnop, BoolBoolBinop, BoolFloatBinop, ListRvalue, FloatLvalue, ListLvalue, Lvalue
}, parse::{LineParsingError, Word}};

macro_rules! any {
	($value:expr $(,)?) => { $value };
	($value:expr, $($rest:expr),+ $(,)?) => {
		$value || any!{$($rest),+}
	}
}

macro_rules! all {
	($value:expr $(,)?) => { $value };
	($value:expr, $($rest:expr),+ $(,)?) => {
		$value && all!{$($rest),+}
	}
}

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
pub enum Operand<'a> {
	Ident(Ident<'a>),
	BoolRvalue(BoolRvalue<'a>),
	FloatRvalue(FloatRvalue<'a>),
}

impl<'a> Operand<'a> {
	pub fn into_bool(self) -> Option<BoolRvalue<'a>> {
		Some(
			match self {
				Operand::BoolRvalue(value) => value,
				_ => return None,
			}
		)
	}

	pub fn into_float(self) -> Option<FloatRvalue<'a>> {
		Some(
			match self {
				Operand::FloatRvalue(value) => value,
				Operand::Ident(value) => FloatRvalue::Lvalue(FloatLvalue::Variable(value)),
				_ => return None,
			}
		)
	}

	pub fn into_list(self) -> Option<ListRvalue<'a>> {
		Some(
			match self {
				Operand::Ident(value) => ListRvalue::Lvalue(ListLvalue::Variable(value)),
				_ => return None,
			}
		)
	}
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
enum ParenKind {
	Ident,
	IntegralPart,
	Index,
}

impl ParenKind {
	fn get(expecting: ExpectingFlags, left: &str) -> Option<Self> {
		Some(
			match left {
				"(" => {
					if expecting.contains(Expecting::Rvalue) {
						ParenKind::Ident
					} else {
						return None
					}
				}
				"[" => {
					if expecting.contains(Expecting::Rvalue) {
						ParenKind::IntegralPart
					} else if expecting.contains(Expecting::Operator) {
						ParenKind::Index
					} else {
						return None
					}
				}
				_ => return None
			}
		)
	}
	
	fn match_right(&self, right: &str) -> bool {
		match (self, right) {
			(ParenKind::Ident, ")") => true,
			(ParenKind::IntegralPart, "]") => true,
			(ParenKind::Index, "]") => true,
			_ => false,
		}
	}
}

#[derive(Debug)]
enum Operator {
	ParenOp(ParenKind),
	PrefixUnop(BoolOrFloatUnop),
	Binop(BoolOrFloatBinop),
}

fn get_priority(operator: &Operator) -> u32 {
	match operator {
		Operator::ParenOp(..) => 0,
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
	parse_bool: bool,
	
	cursor: LineCursor<'a>,
	expecting: ExpectingFlags,
	operands: Vec<Operand<'a>>,
	operators: Vec<Operator>,
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
	fn new(cursor: LineCursor<'a>, parse_bool: bool) -> Self {
		Self {
			parse_bool,
			
			cursor,
			expecting: make_bitflags!(Expecting::{PrefixUnop | Rvalue}),
			operands: Vec::new(),
			operators: Vec::new(),
		}
	}
	
	// #[trace]
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
		let (new_cursor, lparen_grapheme) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = unwrap_or_return!(ParenKind::get(self.expecting, lparen_grapheme), false);
		
		self.operators.push(Operator::ParenOp(op));
		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});
		self.cursor = new_cursor;
		true
	}
	
	// #[trace]
	fn try_rparen_unop(&mut self) -> Result<bool> {
		let (new_cursor, rparen_grapheme) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let (")"|"]") = rparen_grapheme else {
			return Ok(false);
		};
		
		while all!(
			!self.operators.is_empty(),
			!matches!(self.operators.last(), Some(Operator::ParenOp(..))),
		) {
			self.eval_top_operation()?;
		}

		let Some(Operator::ParenOp(op)) = self.operators.pop() else {
			return Err(self.cursor.make_error(LineParsingErrorKind::UnclosedRParen));
		};

		if !op.match_right(rparen_grapheme) {
			return Err(self.cursor.make_error(LineParsingErrorKind::MismatchedParens));
		}

		let result = match op {
			// if it's an Ident, simply ignore it
			ParenKind::Ident => {
				let x = self.operands.pop().unwrap();
				x
			}
			ParenKind::IntegralPart => {
				let x = self.operands.pop().unwrap().into_float().unwrap(); // TODO: type error
				Operand::FloatRvalue(FloatRvalue::Unop(FloatUnop::IntegralPart, Box::new(x)))
			}
			ParenKind::Index => {
				let y = self.operands.pop().unwrap().into_float().unwrap(); // TODO: type error
				let x = self.operands.pop().unwrap().into_list().unwrap(); // TODO: type error
				Operand::FloatRvalue(FloatRvalue::Lvalue(FloatLvalue::ListElement(x, Box::new(y))))
			}
		};
		
		self.operands.push(result);
		self.expecting = make_bitflags!(Expecting::{Operator});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	#[trace]
	fn try_other(&mut self) -> Result<bool> {
		let (new_cursor, name) = self.cursor.read_while(|x| matches!(get_grapheme_kind(x), GraphemeKind::Other));
		let rvalue = match unwrap_or_return!(name.graphemes(true).next(), Ok(false)) {
			"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" => match name.parse() {
				Ok(literal) => Operand::FloatRvalue(FloatRvalue::Literal(literal)),
				Err(..) => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatLiteral)),
			}
			_ => {
				let word = Word::from_name(name);
				if word.is_some() {
					return Err(self.cursor.make_error(LineParsingErrorKind::InvalidIdent(String::from(name))));
				} else {
					Operand::Ident(Ident(name))
				}
			}
		};
		
		self.operands.push(rvalue);
		self.expecting = make_bitflags!(Expecting::{Operator});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	// #[trace]
	fn eval_top_operation(&mut self) -> Result<()> {
		let op = self.operators.pop().unwrap();
		let result = match op {
			Operator::ParenOp(..) => return Err(self.cursor.make_error(LineParsingErrorKind::UnclosedLParen)),
			Operator::PrefixUnop(op) => {
				let x = self.operands.pop().unwrap();
				match op {
					// the operation will always be Ident, so we just ignore it
					BoolOrFloatUnop::BoolUnop(..) => x,
					BoolOrFloatUnop::FloatUnop(op) => {
						let x = x.into_float().unwrap(); // TODO: type error
						// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatUnopOperands(op))),
						Operand::FloatRvalue(FloatRvalue::Unop(op, Box::new(x)))
					}
				}
			}
			Operator::Binop(op) => {
				let y = self.operands.pop().unwrap();
				let x = self.operands.pop().unwrap();
				match op {
					BoolOrFloatBinop::FloatBinop(op) => {
						let y = y.into_float().unwrap(); // TODO: type error
						let x = x.into_float().unwrap(); // TODO: type error
						// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatBinopOperands(op))),
						Operand::FloatRvalue(FloatRvalue::Binop(op, Box::new(x), Box::new(y)))
					}
					BoolOrFloatBinop::BoolBinop(op) => match op {
						BoolBinop::BoolFloatBinop(op) => {
							let y = y.into_float().unwrap(); // TODO: type error
							let x = x.into_float().unwrap(); // TODO: type error
							// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolFloatBinopOperands(op))),
							Operand::BoolRvalue(BoolRvalue::BoolFloatBinop(op, x, y))
						}
						BoolBinop::BoolBoolBinop(op) => {
							let y = y.into_bool().unwrap(); // TODO: type error
							let x = x.into_bool().unwrap(); // TODO: type error
							// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolBoolBinopOperands(op))),
							Operand::BoolRvalue(BoolRvalue::BoolBoolBinop(op, Box::new(x), Box::new(y)))
						}
					}
				}
			}
		};
		self.operands.push(result);
		Ok(())
	}
	
	// #[trace]
	fn eval_while_priority_greater_or_equal(&mut self, priority: u32) -> Result<()> {
		while self.operators.last().map_or(false, |last| priority <= get_priority(last)) {
			self.eval_top_operation()?;
		}
		Ok(())
	}
	
	// #[trace]
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
	
	// #[trace]
	fn try_bool_float_binop(&mut self) -> Result<bool> {
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let (new_cursor, op) = match grapheme_0 {
			"=" => (new_cursor, BoolFloatBinop::Eq),
			
			"!" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinop::Neq)
			} else { return Ok(false) }
			
			"<" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinop::Lte)
			} else { (new_cursor, BoolFloatBinop::Lt) }
			
			">" => if let Some((new_cursor, "=")) = new_cursor.read_one() {
				(new_cursor, BoolFloatBinop::Gte)
			} else { (new_cursor, BoolFloatBinop::Gt) }
			
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
	
	// #[trace]
	fn try_bool_bool_binop(&mut self) -> Result<bool> {
		let (new_cursor, name) = self.cursor.read_while(|x| matches!(get_grapheme_kind(x), GraphemeKind::Other));
		let op = match name {
			"sau" => BoolBoolBinop::Or,
			"si"|"și" => BoolBoolBinop::And,
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
		// NOTE: I took advantage of short-circuiting here to make the code a bit
		// terser (and arguably more understandable).
		Ok(
			any!(
			  all!(self.expecting.contains(Expecting::PrefixUnop), self.try_prefix_float_unop()),
	 			all!(
	 				self.expecting.intersects(make_bitflags!(Expecting::{Rvalue | Operator})),
	 				self.try_lparen_unop(),
	 			),
				all!(self.expecting.contains(Expecting::Operator), self.try_rparen_unop()?),
				all!(
					self.expecting.contains(Expecting::Operator),
					any!(
						self.try_float_binop()?,
						(self.parse_bool && self.try_bool_float_binop()?),
						(self.parse_bool && self.try_bool_bool_binop()?),
					),
				),
				all!(self.expecting.contains(Expecting::Rvalue), self.try_other()?),
			)
		)
	}
	
	#[trace]
	fn parse(mut self) -> LineParsingIntermediateResult<'a, Operand<'a>> {
		while self.parse_expecting()? { }
		if !self.expecting.contains(Expecting::Operator) {
			Err(self.cursor.make_error(LineParsingErrorKind::ExpectedSomethingElse(self.expecting)))
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
		Ok({
			let rvalue = self.parse_rvalue()?;
			let (new_self, Operand::FloatRvalue(rvalue)) = rvalue else {
				return Err(self.make_error(LineParsingErrorKind::ExpectedFloatRvalue));
			};
			(new_self, rvalue)
		})
	}
	
	pub fn parse_bool_rvalue(self) -> LineParsingIntermediateResult<'a, BoolRvalue<'a>> {
		Ok({
			let rvalue = self.parse_rvalue()?;
			let (new_self, Operand::BoolRvalue(rvalue)) = rvalue else {
				return Err(self.make_error(LineParsingErrorKind::ExpectedBoolRvalue));
			};
			(new_self, rvalue)
		})
	}

	pub fn parse_lvalue(self) -> LineParsingIntermediateResult<'a, Lvalue<'a>> {
		Ok({
			let parser = Parser::new(self, false);
			let (new_self, operand) = parser.parse()?;
			let lvalue = match operand {
				Operand::Ident(value) => Lvalue::Unknown(value),
				Operand::FloatRvalue(value) => match value {
					FloatRvalue::Lvalue(value) => Lvalue::Float(value),
					// TODO: Type error, expected lvalue
					_ => todo!(),
				}
				_ => todo!(),
			};
			(new_self, lvalue)
		})
	}
	
	pub fn parse_rvalue(self) -> LineParsingIntermediateResult<'a, Operand<'a>> {
		let parser = Parser::new(self, true);
		parser.parse()
	}
}
