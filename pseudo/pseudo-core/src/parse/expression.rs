use enumflags2::{bitflags, BitFlags, make_bitflags};

trace::init_depth_var!();

use unicode_segmentation::UnicodeSegmentation;
use super::{get_grapheme_kind, GraphemeKind, ValueTypeFlags, ValueType, line::LineCursor, ParserErrorKind, ParserError};
use crate::{ast::{
	Ident,
	FloatUnop, FloatBinop, BoolBinop, FloatRvalue, BoolRvalue, BoolUnop, BoolBoolBinop, BoolFloatBinop, ListRvalue, FloatLvalue, ListLvalue, Lvalue, IdentNode, BoolRvalueNode, FloatRvalueNode, ListRvalueNode, UnknownRvalue, UnknownRvalueNode
}, parse::Word, source::{Node, Span}, LanguageSettings};
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

#[derive(Debug)]
pub enum Operand<'src> {
	Ident(IdentNode<'src>),
	UnknownRvalue(UnknownRvalueNode<'src>),
	BoolRvalue(BoolRvalueNode<'src>),
	FloatRvalue(FloatRvalueNode<'src>),
}

impl<'src> Operand<'src> {
	pub fn span(&self) -> Span {
		match self {
			Operand::Ident(Node(span, _)) => *span,
			Operand::UnknownRvalue(Node(span, _)) => *span,
			Operand::BoolRvalue(Node(span, _)) => *span,
			Operand::FloatRvalue(Node(span, _)) => *span,
		}
	}
	
	pub fn into_bool(self) -> std::result::Result<BoolRvalueNode<'src>, ParserError> {
		Ok(
			match self {
				Operand::BoolRvalue(value) => value,
				_ => {
					return Err(self.span().0.make_err(ParserErrorKind::ExpectedValueType(make_bitflags!(ValueType::{Bool}))));
				}
			}
		)
	}

	pub fn into_float(self) -> std::result::Result<FloatRvalueNode<'src>, ParserError> {
		Ok(
			match self {
				Operand::FloatRvalue(value) => value,
				Operand::Ident(value) => {
					let span = *value.span();
					let lvalue = span.node(FloatLvalue::Variable(value));
					let rvalue = span.node(FloatRvalue::Lvalue(lvalue));
					rvalue
				}
				_ => {
					return Err(self.span().0.make_err(ParserErrorKind::ExpectedValueType(make_bitflags!(ValueType::{Float}))));
				}
			}
		)
	}

	pub fn into_list(self) -> std::result::Result<ListRvalueNode<'src>, ParserError> {
		Ok(
			match self {
				Operand::Ident(value) => {
					let span = *value.span();
					span.node(ListRvalue::Lvalue(ListLvalue::Variable(value)))
				}
				_ => {
					return Err(self.span().0.make_err(ParserErrorKind::ExpectedValueType(make_bitflags!(ValueType::{List}))));
				}
			}
		)
	}

	pub fn into_ident(self) -> std::result::Result<IdentNode<'src>, ParserError> {
		Ok(
			match self {
				Operand::Ident(value) => value,
				_ => {
					return Err(self.span().0.make_err(ParserErrorKind::ExpectedIdent));
				}
			}
		)
	}

	pub fn into_lvalue(self) -> std::result::Result<Lvalue<'src>, ParserError> {
		Ok({
			let span = self.span();
			match self {
				Operand::Ident(value) => Lvalue::Unknown(value),
				Operand::FloatRvalue(value) => match value.into_inner() {
					FloatRvalue::Lvalue(value) => Lvalue::Float(value),
					_ => {
						return Err(span.0.make_err(ParserErrorKind::ExpectedLvalue));
					}
				}
				_ => {
					return Err(self.span().0.make_err(ParserErrorKind::ExpectedLvalue));
				}
			}
		})
	}
}

#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expecting {
	PrefixUnop,
	
	// This also includes LParens.
	Operand,
	
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
	fn get(
		language_settings: &LanguageSettings,
		expecting: ExpectingFlags,
		left: &str,
	) -> Option<Self> {
		Some(
			match left {
				"(" => {
					if expecting.contains(Expecting::Operand) {
						ParenKind::Ident
					} else if expecting.contains(Expecting::Operator) {
						ParenKind::FunctionCall
					} else {
						return None
					}
				}
				"[" => {
					if expecting.contains(Expecting::Operand) {
						ParenKind::IntegralPart
					} else if expecting.contains(Expecting::Operator) {
						if language_settings.enable_list {
							ParenKind::Index
						} else {
							return None;
						}
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
	operands: Vec<Operand<'src>>,
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
			expecting: make_bitflags!(Expecting::{PrefixUnop | Operand}),
			operands: Vec::new(),
			operators: Vec::new(),
		}
	}
	
	fn try_prefix_float_unop(&mut self) -> bool {
		let start_offset = self.cursor.offset;
		
		let (new_cursor, grapheme_0) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = match grapheme_0 {
			"+" => FloatUnop::Identity,
			"-" => FloatUnop::Neg,
			_ => return false,
		};
		
		let op = Operator::PrefixUnop(BoolOrFloatUnop::FloatUnop(op));
		self.expecting = make_bitflags!(Expecting::{Operand});
		self.cursor = new_cursor;

		self.operators.push(start_offset.span(&self.cursor.offset).node(op));

		true
	}
	
	fn try_lparen_unop(&mut self) -> bool {
		let start_offset = self.cursor.offset;
		
		let (new_cursor, lparen_grapheme) = unwrap_or_return!(self.cursor.read_one(), false);
		let op = unwrap_or_return!(ParenKind::get(self.cursor.language_settings, self.expecting, lparen_grapheme), false);
		
		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Operand});
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
			ParenKind::Ident => {
				let x = self.operands.pop().unwrap();
				match x {
					Operand::Ident(x) => {
						let span = *x.span();
						let rvalue = span.node(UnknownRvalue::Ident(x));
						let rvalue = parens_span.node(UnknownRvalue::Identity(Box::new(rvalue)));
						Operand::UnknownRvalue(rvalue)
					}
					Operand::UnknownRvalue(x) => {
						let rvalue = parens_span.node(UnknownRvalue::Identity(Box::new(x)));
						Operand::UnknownRvalue(rvalue)
					}
					Operand::BoolRvalue(x) => {
						let rvalue = BoolRvalue::BoolUnop(BoolUnop::Identity, Box::new(x));
						Operand::BoolRvalue(parens_span.node(rvalue))
					}
					Operand::FloatRvalue(x) => {
						let rvalue = FloatRvalue::Unop(FloatUnop::Identity, Box::new(x));
						Operand::FloatRvalue(parens_span.node(rvalue))
					}
				}
			}
			ParenKind::IntegralPart => {
				let x = self.operands.pop().unwrap().into_float()?;
				let rvalue = FloatRvalue::Unop(FloatUnop::IntegralPart, Box::new(x));
				Operand::FloatRvalue(parens_span.node(rvalue))
			}
			ParenKind::Index => {
				let y = self.operands.pop().unwrap().into_float()?;
				let x = self.operands.pop().unwrap().into_list()?;
				let span = x.span().merge(&parens_span);
				let lvalue = span.node(FloatLvalue::ListElement(x, Box::new(y)));
				let rvalue = span.node(FloatRvalue::Lvalue(lvalue));
				Operand::FloatRvalue(rvalue)
			}
			ParenKind::FunctionCall => {
				let y = self.operands.pop().unwrap();
				let x = self.operands.pop().unwrap().into_ident()?;
				match x.inner().0 {
					"lungime" if self.cursor.language_settings.enable_list => {
						let y = y.into_list()?;
						let rvalue = FloatRvalue::ListLength(y);
						let span = x.span().merge(&parens_span);
						Operand::FloatRvalue(span.node(rvalue))
					}
					value => {
						return Err(self.cursor.offset.make_err(ParserErrorKind::InvalidFunction(String::from(value))));
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
				Ok(literal) => Operand::FloatRvalue(span.node(FloatRvalue::Literal(literal))),
				Err(..) => return Err(self.cursor.offset.make_err(ParserErrorKind::InvalidFloatLiteral)),
			}
			_ => {
				let word = Word::from_name(self.cursor.language_settings, name);
				if word.as_ref().map(Word::can_be_ident).unwrap_or(true) {
					Operand::Ident(span.node(Ident(name)))
				} else {
					return Err(self.cursor.offset.make_err(ParserErrorKind::InvalidIdent(String::from(name))));
				}
			}
		};
		
		self.operands.push(rvalue);
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
						let x = x.into_float()?;
						let span = op_span.merge(x.span());
						let rvalue = FloatRvalue::Unop(op, Box::new(x));
						Operand::FloatRvalue(span.node(rvalue))
					}
				}
			}
			Operator::Binop(op) => {
				let y = self.operands.pop().unwrap();
				let x = self.operands.pop().unwrap();
				match op {
					BoolOrFloatBinop::FloatBinop(op) => {
						let y = y.into_float()?;
						let x = x.into_float()?;
						let span = x.span().merge(&op_span).merge(y.span());
						let rvalue = FloatRvalue::Binop(op, Box::new(x), Box::new(y));
						Operand::FloatRvalue(span.node(rvalue))
					}
					BoolOrFloatBinop::BoolBinop(op) => match op {
						BoolBinop::BoolFloatBinop(op) => {
							let y = y.into_float()?;
							let x = x.into_float()?;
							let span = x.span().merge(&op_span).merge(y.span());
							let rvalue = BoolRvalue::BoolFloatBinop(op, x, y);
							Operand::BoolRvalue(span.node(rvalue))
						}
						BoolBinop::BoolBoolBinop(op) => {
							let y = y.into_bool()?;
							let x = x.into_bool()?;
							let span = x.span().merge(&op_span).merge(y.span());
							let rvalue = BoolRvalue::BoolBoolBinop(op, Box::new(x), Box::new(y));
							Operand::BoolRvalue(span.node(rvalue))
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

		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Operand});

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

		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Operand});

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

		self.expecting = make_bitflags!(Expecting::{PrefixUnop | Operand});

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
	 				self.expecting.intersects(make_bitflags!(Expecting::{Operand | Operator})),
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
				all!(self.expecting.contains(Expecting::Operand), self.try_other()?),
			)
		)
	}
	
	fn parse(mut self) -> ParserIntermediateResult<'src, Operand<'src>> {
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
	pub fn parse_float_rvalue(self) -> ParserIntermediateResult<'src, FloatRvalueNode<'src>> {
		Ok({
			let (new_self, operand) = self.parse_operand()?;
			let rvalue = operand.into_float()?;
			(new_self, rvalue)
		})
	}
	
	pub fn parse_bool_rvalue(self) -> ParserIntermediateResult<'src, BoolRvalueNode<'src>> {
		Ok({
			let (new_self, operand) = self.parse_operand()?;
			let rvalue = operand.into_bool()?;
			(new_self, rvalue)
		})
	}

	pub fn parse_lvalue(self) -> ParserIntermediateResult<'src, Lvalue<'src>> {
		Ok({
			let parser = Parser::new(self, false);
			let (new_self, operand) = parser.parse()?;
			let lvalue = operand.into_lvalue()?;
			(new_self, lvalue)
		})
	}
	
	pub fn parse_operand(self) -> ParserIntermediateResult<'src, Operand<'src>> {
		Ok({
			let parser = Parser::new(self, true);
			let (new_self, operand) = parser.parse()?;
			(new_self, operand)
		})
	}
}
