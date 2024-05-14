use enumflags2::{bitflags, BitFlags, make_bitflags};

trace::init_depth_var!();

use unicode_segmentation::UnicodeSegmentation;
use super::{get_grapheme_kind, GraphemeKind, ValueTypeFlags, ValueType, line::LineCursor, ParserErrorKind, ParserError};
use crate::{ast::{
	Ident,
	FloatUnop, FloatBinop, BoolBinop, FloatRvalue, BoolRvalue, BoolUnop, BoolBoolBinop, BoolFloatBinop, ListRvalue, FloatLvalue, ListLvalue, Lvalue
}, parse::Word, source::Node};
use super::line::ParserIntermediateResult;

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
pub struct TypeError(ValueTypeFlags);

impl From<TypeError> for ParserErrorKind {
	fn from(value: TypeError) -> Self {
		let TypeError(flags) = value;
		ParserErrorKind::ExpectedValueType(flags)
  }
}

#[derive(Debug)]
pub struct IdentError;

impl From<IdentError> for ParserErrorKind {
	fn from(_value: IdentError) -> Self {
		ParserErrorKind::ExpectedIdent
	}
}

#[derive(Debug)]
pub struct LvalueError;
impl From<LvalueError> for ParserErrorKind {
	fn from(_value: LvalueError) -> Self {
		ParserErrorKind::ExpectedLvalue
	}
}

pub fn convert_node<'src, F, Out, Err>(
	f: F,
	node: Node<Operand<'src>>,
) -> std::result::Result<Node<Out>, ParserError>
where
	F: FnOnce(Operand<'src>) -> std::result::Result<Out, Err>,
	Err: Into<ParserErrorKind>,
{
	let Node(span, inner) = node;
	let result = f(inner).map_err(|err| span.0.make_err(err)).map(|inner| Node(span, inner));
	result
}

#[derive(Debug)]
pub enum Operand<'src> {
	Ident(Ident<'src>),
	BoolRvalue(BoolRvalue<'src>),
	FloatRvalue(FloatRvalue<'src>),
}

impl<'src> Operand<'src> {
	pub fn into_bool(self) -> std::result::Result<BoolRvalue<'src>, TypeError> {
		Ok(
			match self {
				Operand::BoolRvalue(value) => value,
				_ => return Err(TypeError(make_bitflags!(ValueType::{Bool}))),
			}
		)
	}

	pub fn into_float(self) -> std::result::Result<FloatRvalue<'src>, TypeError> {
		Ok(
			match self {
				Operand::FloatRvalue(value) => value,
				Operand::Ident(value) => FloatRvalue::Lvalue(FloatLvalue::Variable(value)),
				_ => return Err(TypeError(make_bitflags!(ValueType::{Float}))),
			}
		)
	}

	pub fn into_list(self) -> std::result::Result<ListRvalue<'src>, TypeError> {
		Ok(
			match self {
				Operand::Ident(value) => ListRvalue::Lvalue(ListLvalue::Variable(value)),
				_ => return Err(TypeError(make_bitflags!(ValueType::{List}))),
			}
		)
	}

	pub fn into_ident(self) -> std::result::Result<Ident<'src>, IdentError> {
		Ok(
			match self {
				Operand::Ident(value) => value,
				_ => return Err(IdentError),
			}
		)
	}

	pub fn into_lvalue(self) -> std::result::Result<Lvalue<'src>, LvalueError> {
		Ok(
			match self {
				Operand::Ident(value) => Lvalue::Unknown(value),
				Operand::FloatRvalue(value) => match value {
					FloatRvalue::Lvalue(value) => Lvalue::Float(value),
					_ => return Err(LvalueError),
				}
				_ => return Err(LvalueError),
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
	FunctionCall,
}

impl ParenKind {
	fn get(expecting: ExpectingFlags, left: &str) -> Option<Self> {
		Some(
			match left {
				"(" => {
					if expecting.contains(Expecting::Rvalue) {
						ParenKind::Ident
					} else if expecting.contains(Expecting::Operator) {
						ParenKind::FunctionCall
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
			(ParenKind::FunctionCall, ")") => true,
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

struct Parser<'src> {
	parse_bool: bool,
	
	cursor: LineCursor<'src>,
	expecting: ExpectingFlags,
	operands: Vec<Node<Operand<'src>>>,
	operators: Vec<Node<Operator>>,
}

type Result<T> = std::result::Result<T, ParserError>;

macro_rules! unwrap_or_return {
	( $option:expr, $none:expr ) => {
		match $option {
			Some(some) => some,
			None => return $none,
		}
	}
}

impl<'src> Parser<'src> {
	fn new(cursor: LineCursor<'src>, parse_bool: bool) -> Self {
		Self {
			parse_bool,
			
			cursor,
			expecting: make_bitflags!(Expecting::{PrefixUnop | Rvalue}),
			operands: Vec::new(),
			operators: Vec::new(),
		}
	}
	
	fn try_prefix_float_unop(&mut self) -> bool {
		let start_offset = self.cursor.offset;
		
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = match grapheme_0 {
			"+" => FloatUnop::Ident,
			"-" => FloatUnop::Neg,
			_ => return false,
		};
		
		let op = Operator::PrefixUnop(BoolOrFloatUnop::FloatUnop(op));
		self.expecting = make_bitflags!(Expecting::{Rvalue});
		self.cursor = new_cursor;

		self.operators.push(start_offset.span(&self.cursor.offset).node(op));

		true
	}
	
	fn try_lparen_unop(&mut self) -> bool {
		let start_offset = self.cursor.offset;
		
		let (new_cursor, lparen_grapheme) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = unwrap_or_return!(ParenKind::get(self.expecting, lparen_grapheme), false);
		
		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});
		self.cursor = new_cursor;

		self.operators.push(start_offset.span(&self.cursor.offset).node(Operator::ParenOp(op)));

		true
	}
	
	fn try_rparen_unop(&mut self) -> Result<bool> {
		let start_offset = self.cursor.offset;
		let (new_cursor, rparen_grapheme) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let (")"|"]") = rparen_grapheme else {
			return Ok(false);
		};
		let rparen_span = start_offset.span(&self.cursor.offset);

		while all!(
			!self.operators.is_empty(),
			!matches!(
				self.operators.last().map(Node::inner),
				Some(Operator::ParenOp(..)),
			),
		) {
			self.eval_top_operation()?;
		}

		let Some(Node(lparen_span, Operator::ParenOp(op))) = self.operators.pop() else {
			return Err(start_offset.make_err(ParserErrorKind::UnclosedRParen));
		};

		if !op.match_right(rparen_grapheme) {
			return Err(self.cursor.offset.make_err(ParserErrorKind::MismatchedParens));
		}

		let parens_span = lparen_span.merge(&rparen_span);

		let result = match op {
			// if it's an Ident, simply ignore it
			ParenKind::Ident => {
				let x = self.operands.pop().unwrap().into_inner();
				parens_span.node(x)
			}
			ParenKind::IntegralPart => {
				let Node(_, x) = convert_node(Operand::into_float, self.operands.pop().unwrap())?;
				let operand = Operand::FloatRvalue(FloatRvalue::Unop(FloatUnop::IntegralPart, Box::new(x)));
				parens_span.node(operand)
			}
			ParenKind::Index => {
				let Node(_, y) = convert_node(Operand::into_float, self.operands.pop().unwrap())?;
				let Node(x_span, x) = convert_node(Operand::into_list, self.operands.pop().unwrap())?;
				let span = x_span.merge(&parens_span);
				let operand = Operand::FloatRvalue(FloatRvalue::Lvalue(FloatLvalue::ListElement(x, Box::new(y))));
				span.node(operand)
			}
			ParenKind::FunctionCall => {
				let y = self.operands.pop().unwrap();
				let Node(x_span, x) = convert_node(Operand::into_ident, self.operands.pop().unwrap())?;
				match x.0 {
					"lungime" => {
						let Node(_, y) = convert_node(Operand::into_list, y)?;
						let operand = Operand::FloatRvalue(FloatRvalue::ListLength(y));
						let span = x_span.merge(&parens_span);
						span.node(operand)
					}
					_ => {
						return Err(self.cursor.offset.make_err(ParserErrorKind::InvalidFunction(String::from(x.0))));
					}
				}
			}
		};
		
		self.operands.push(result);
		self.expecting = make_bitflags!(Expecting::{Operator});
		self.cursor = new_cursor;
		Ok(true)
	}
	
	fn try_other(&mut self) -> Result<bool> {
		let start_offset = self.cursor.offset;
		let (new_cursor, name) = self.cursor.read_while(|x| matches!(get_grapheme_kind(x), GraphemeKind::Other));
		let span = start_offset.span(&new_cursor.offset);

		let rvalue = match unwrap_or_return!(name.graphemes(true).next(), Ok(false)) {
			"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" => match name.parse() {
				Ok(literal) => Operand::FloatRvalue(FloatRvalue::Literal(literal)),
				Err(..) => return Err(self.cursor.offset.make_err(ParserErrorKind::InvalidFloatLiteral)),
			}
			_ => {
				let word = Word::from_name(name);
				if word.as_ref().map(Word::can_be_ident).unwrap_or(true) {
					Operand::Ident(Ident(name))
				} else {
					return Err(self.cursor.offset.make_err(ParserErrorKind::InvalidIdent(String::from(name))));
				}
			}
		};
		
		self.operands.push(span.node(rvalue));
		self.expecting = make_bitflags!(Expecting::{Operator});
		self.cursor = new_cursor;

		Ok(true)
	}
	
	fn eval_top_operation(&mut self) -> Result<()> {
		let op = self.operators.pop().unwrap();
		let Node(op_span, op) = op;
		let result = match op {
			Operator::ParenOp(..) => return Err(op_span.0.make_err(ParserErrorKind::UnclosedLParen)),
			Operator::PrefixUnop(op) => {
				let x = self.operands.pop().unwrap();
				match op {
					BoolOrFloatUnop::BoolUnop(_) => unreachable!(),
					BoolOrFloatUnop::FloatUnop(op) => {
						let Node(x_span, x) = convert_node(Operand::into_float, x)?;
						let operand = Operand::FloatRvalue(FloatRvalue::Unop(op, Box::new(x)));
						Node(
							op_span.merge(&x_span),
							operand,
						)
						// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatUnopOperands(op))),
					}
				}
			}
			Operator::Binop(op) => {
				let y = self.operands.pop().unwrap();
				let x = self.operands.pop().unwrap();
				match op {
					BoolOrFloatBinop::FloatBinop(op) => {
						let Node(y_span, y) = convert_node(Operand::into_float, y)?;
						let Node(x_span, x) = convert_node(Operand::into_float, x)?;
						let operand = Operand::FloatRvalue(FloatRvalue::Binop(op, Box::new(x), Box::new(y)));
						Node(
							x_span.merge(&op_span).merge(&y_span),
							operand,
						)
						// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidFloatBinopOperands(op))),
					}
					BoolOrFloatBinop::BoolBinop(op) => match op {
						BoolBinop::BoolFloatBinop(op) => {
							let Node(y_span, y) = convert_node(Operand::into_float, y)?;
							let Node(x_span, x) = convert_node(Operand::into_float, x)?;
							let operand = Operand::BoolRvalue(BoolRvalue::BoolFloatBinop(op, x, y));
							Node(
								x_span.merge(&op_span).merge(&y_span),
								operand,
							)
							// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolFloatBinopOperands(op))),
						}
						BoolBinop::BoolBoolBinop(op) => {
							let Node(y_span, y) = convert_node(Operand::into_bool, y)?;
							let Node(x_span, x) = convert_node(Operand::into_bool, x)?;
							let operand = Operand::BoolRvalue(BoolRvalue::BoolBoolBinop(op, Box::new(x), Box::new(y)));
							Node(
								x_span.merge(&op_span).merge(&y_span),
								operand,
							)
							// _ => return Err(self.cursor.make_error(LineParsingErrorKind::InvalidBoolBoolBinopOperands(op))),
						}
					}
				}
			}
		};
		self.operands.push(result);
		Ok(())
	}
	
	fn eval_while_priority_greater_or_equal(&mut self, priority: u32) -> Result<()> {
		while self.operators.last().map_or(false, |last| priority <= get_priority(&last.1)) {
			self.eval_top_operation()?;
		}
		Ok(())
	}
	
	fn try_float_binop(&mut self) -> Result<bool> {
		let start_offset = self.cursor.offset;
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), Ok(false));
		let op = match grapheme_0 {
			"+" => FloatBinop::Add,
			"-" => FloatBinop::Sub,
			"*" => FloatBinop::Mul,
			"/" => FloatBinop::Div,
			"%" => FloatBinop::Rem,
			_ => return Ok(false),
		};
		self.cursor = new_cursor;

		let span = start_offset.span(&self.cursor.offset);
		
		let op = Operator::Binop(BoolOrFloatBinop::FloatBinop(op));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;

		let node = span.node(op);
		self.operators.push(node);

		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});

		Ok(true)
	}
	
	fn try_bool_float_binop(&mut self) -> Result<bool> {
		let start_offset = self.cursor.offset;
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
		self.cursor = new_cursor;

		let span = start_offset.span(&self.cursor.offset);
		
		let op = Operator::Binop(BoolOrFloatBinop::BoolBinop(BoolBinop::BoolFloatBinop(op)));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;

		let node = span.node(op);
		self.operators.push(node);

		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});

		Ok(true)
	}
	
	fn try_bool_bool_binop(&mut self) -> Result<bool> {
		let start_offset = self.cursor.offset;
		let (new_cursor, name) = self.cursor.read_while(|x| matches!(get_grapheme_kind(x), GraphemeKind::Other));
		let op = match name {
			"sau" => BoolBoolBinop::Or,
			"si"|"și" => BoolBoolBinop::And,
			_ => return Ok(false),
		};
		self.cursor = new_cursor;

		let span = start_offset.span(&self.cursor.offset);
		
		let op = Operator::Binop(BoolOrFloatBinop::BoolBinop(BoolBinop::BoolBoolBinop(op)));
		self.eval_while_priority_greater_or_equal(get_priority(&op))?;

		let node = span.node(op);
		self.operators.push(node);

		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Rvalue});

		Ok(true)
	}
	
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
	
	fn parse(mut self) -> ParserIntermediateResult<'src, Node<Operand<'src>>> {
		while self.parse_expecting()? { }
		if !self.expecting.contains(Expecting::Operator) {
			Err(self.cursor.offset.make_err(ParserErrorKind::ExpectedSomethingElse(self.expecting)))
		} else {
			while !self.operators.is_empty() {
				self.eval_top_operation()?;
			}
			let result = self.operands.pop().unwrap();
			Ok((self.cursor, result))
		}
	}
}

impl<'src> LineCursor<'src> {
	pub fn parse_float_rvalue(self) -> ParserIntermediateResult<'src, FloatRvalue<'src>> {
		Ok({
			let (new_self, operand) = self.parse_operand()?;
			let rvalue = convert_node(Operand::into_float, operand)?.into_inner();
			(new_self, rvalue)
		})
	}
	
	pub fn parse_bool_rvalue(self) -> ParserIntermediateResult<'src, BoolRvalue<'src>> {
		Ok({
			let (new_self, operand) = self.parse_operand()?;
			let rvalue = convert_node(Operand::into_bool, operand)?.into_inner();
			(new_self, rvalue)
		})
	}

	pub fn parse_lvalue(self) -> ParserIntermediateResult<'src, Lvalue<'src>> {
		Ok({
			let parser = Parser::new(self, false);
			let (new_self, operand) = parser.parse()?;
			let lvalue = convert_node(Operand::into_lvalue, operand)?.into_inner();
			(new_self, lvalue)
		})
	}
	
	pub fn parse_operand(self) -> ParserIntermediateResult<'src, Node<Operand<'src>>> {
		Ok({
			let parser = Parser::new(self, true);
			let (new_self, operand) = parser.parse()?;
			(new_self, operand)
		})
	}
}
