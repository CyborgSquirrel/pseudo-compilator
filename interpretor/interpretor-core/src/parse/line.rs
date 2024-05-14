use enumflags2::make_bitflags;

use crate::{ast::{
	Instructiune,
	ScrieParam,
	Ident, FloatUnop, FloatBinop, BoolFloatBinop, BoolBoolBinop, InstructiuneNode, FloatRvalue, AtribuireRvalue, ListRvalue, Lvalue}, source::{Offset, Node, Span}};
use itertools::izip;
use unicode_segmentation::UnicodeSegmentation;

use super::{ValueTypeFlags, expression::{self, convert_node, Operand}, Arrow, ValueType, Word, get_grapheme_kind, GraphemeKind, ParserError, ParserErrorKind};

pub type ParserIntermediateResult<'src, T> = Result<(LineCursor<'src>, T), ParserError>;
pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone, Copy)]
pub struct LineCursor<'src> {
	pub code: &'src str,
	pub offset: Offset,
}

impl<'src> LineCursor<'src> {
	pub fn new(code: &'src str, offset: Offset) -> Self {
		Self {
			code,
			offset,
		}
	}

	pub fn make_node<T>(&self, inner: T) -> Node<T> {
		let span = Span(self.offset, self.offset);
		span.node(inner)
	}

	fn code(&self) -> &'src str {
		&self.code[self.offset.bytes() as usize..]
	}

	fn code_until(&self, until: usize) -> &'src str {
		&self.code[self.offset.bytes() as usize..self.offset.bytes() as usize+until]
	}

	fn expect_str(mut self, expected: &'static str) -> ParserIntermediateResult<'src, ()> {
		Ok({
			let offset = self.offset;
			for (grapheme, expected_grapheme) in izip!(
				self.code().graphemes(true),
				expected.graphemes(true),
			) {
				if grapheme != expected_grapheme {
					return Err(offset.make_err(ParserErrorKind::ExpectedStr(expected)))
				}
				self.offset.add_grapheme(grapheme);
			}
			(self, ())
		})
	}

	pub fn expect_str_optional_diacritics(mut self, expected: &'static str) -> ParserIntermediateResult<'src, ()> {
		Ok({
			let offset = self.offset;
			for (grapheme, expected_grapheme) in izip!(
				self.code().graphemes(true),
				expected.graphemes(true),
			) {
				let matches = match (grapheme, expected_grapheme) {
					("a", "ă")|("A", "Ă")|
					("a", "â")|("A", "Â")|
					("i", "î")|("I", "Î")|
					("s", "ș")|("S", "Ș")|
					("t", "ț")|("T", "Ț")
						=> true,
					_
						=> grapheme == expected_grapheme,
				};

				if !matches {
					return Err(offset.make_err(ParserErrorKind::ExpectedStrOptionalDiacritics(expected)))
				}

				self.offset.add_grapheme(grapheme);
			}

			(self, ())
		})
	}

	pub fn skip_spaces(mut self) -> Self {
		for grapheme in self.code().graphemes(true) {
			if grapheme != " " {
				break;
			}
			self.offset.add_grapheme(grapheme);
		}
		self
	}

	pub fn read_while<P: FnMut(&str) -> bool>(mut self, mut predicate: P) -> (Self, &'src str) {
		let old_self = self;
		for grapheme in self.code().graphemes(true) {
			if !predicate(grapheme) {
				break;
			}
			self.offset.add_grapheme(grapheme);
		}
		let result = old_self.code_until((self.offset.bytes() - old_self.offset.bytes()) as usize);
		(self, result)
	}

	pub fn read_one(mut self) -> Option<(Self, &'src str)> {
		let one = self.code().graphemes(true).next();
		if let Some(one) = one {
			self.offset.add_grapheme(one);
			Some((self, one))
		} else {
			None
		}
	}

	fn parse_ident(self) -> ParserIntermediateResult<'src, Ident<'src>> {
		Ok({
			let new_self = self;
			let (new_self, (name, word)) = new_self.parse_word()?;
			if word.is_some() {
				return Err(new_self.offset.make_err(ParserErrorKind::InvalidIdent(String::from(name))))
			}
			if name.is_empty() {
				return Err(new_self.offset.make_err(ParserErrorKind::ExpectedIdent))
			}
			(new_self, Ident(name))
		})
	}

	fn expect_end(self) -> ParserResult<()> {
		Ok({
			match self.read_one() {
				Some((_, "\n")) | None => (),
				_ => return Err(self.offset.make_err(ParserErrorKind::ExpectedEnd)),
			}
		})
	}

	fn parse_second_step_citeste(mut self, old_self: Self) -> ParserIntermediateResult<'src, InstructiuneNode<'src>> {
		Ok({
			let mut lvalues = Vec::new();
			loop {
				self = self.skip_spaces();
				let (new_self, lvalue) = self.parse_lvalue()?;
				self = new_self;
				lvalues.push(lvalue);
				self = self.skip_spaces();
				match self.expect_str(",") {
					Ok((new_self, _)) => self = new_self,
					Err(..) => break,
				}
			}
			(self, old_self.make_node(Instructiune::Citeste(lvalues)))
		})
	}

	fn next_grapheme(self) -> ParserIntermediateResult<'src, &'src str> {
		Ok({
			let one = self.read_one().ok_or(self.offset.make_err(ParserErrorKind::ExpectedAnyGrapheme))?;
			one
		})
	}

	fn parse_scrie_param(self) -> ParserIntermediateResult<'src, ScrieParam<'src>> {
		match self.next_grapheme().map_err(|_| self.offset.make_err(ParserErrorKind::ExpectedScrieParam))? {
			(new_self, "'") => {
				let (new_self, character) = new_self.next_grapheme()?;
				let (new_self, _) = new_self.expect_str("'")?;
				Ok((new_self, ScrieParam::CharacterLiteral(character)))
			}
			(new_self, "\"") => {
				let (new_self, string) = new_self.read_while(|grapheme| grapheme != "\"");
				let (new_self, _) = new_self.expect_str("\"")?;
				Ok((new_self, ScrieParam::StringLiteral(string)))
			}
			_ => {
				let (new_self, rvalue) = self.parse_float_rvalue()?;
				Ok((new_self, ScrieParam::Rvalue(rvalue)))
			}
		}
	}

	fn parse_second_step_scrie(mut self, old_self: Self) -> ParserIntermediateResult<'src, InstructiuneNode<'src>> {
		let mut params = Vec::new();
		let mut done = false;
		while !done {
			let (new_self, param) = self.skip_spaces().parse_scrie_param()?;
			params.push(param);
			self = new_self.skip_spaces();
			match self.expect_str(",") {
				Ok((new_self, _)) => self = new_self,
				Err(..) => done = true,
			}
		}
		Ok((self, old_self.make_node(Instructiune::Scrie(params))))
	}

	fn parse_arrow(self) -> ParserIntermediateResult<'src, Arrow> {
		let new_self = self;
		let (new_self, _) = new_self.expect_str("<-")?;
		if let Ok((new_self, _)) = new_self.expect_str(">") {
			Ok((new_self, Arrow::LeftRight))
		} else {
			Ok((new_self, Arrow::Left))
		}
	}
	
	fn parse_assignment_list_element(self) -> ParserIntermediateResult<'src, Option<FloatRvalue<'src>>> {
		Ok({
			let new_self = self;
			let new_self = new_self.skip_spaces();
			
			if new_self.expect_str(";").is_ok() || new_self.expect_end().is_ok() {
				(self, None)
			} else {
				let (new_self, rvalue) = new_self.parse_float_rvalue()?;
				(new_self, Some(rvalue))
			}
		})
	}

	fn parse_assignment(self, old_self: Self, lvalue: Lvalue<'src>) -> ParserIntermediateResult<'src, InstructiuneNode<'src>> {
		Ok({
			let new_self = self;
			let new_self = new_self.skip_spaces();

			// see if it's an empty list
			if let Ok((new_self, _)) = new_self.expect_str(",") {
				let new_self = new_self.skip_spaces();
				if new_self.expect_str(";").is_ok() || new_self.expect_end().is_ok() {
					let instruction = Instructiune::Atribuire(lvalue, AtribuireRvalue::List(ListRvalue::Literal(Vec::new())));
					return Ok((new_self, old_self.make_node(instruction)));
				} else {
					return Err(new_self.offset.make_err(ParserErrorKind::ExpectedEndOfBlocklessInstruction));
				}
			}

			let (new_self, operand) = new_self.parse_operand()?;
			if let Ok((mut new_self, _)) = new_self.skip_spaces().expect_str(",") {
				let mut list = Vec::new();
				list.push(convert_node(Operand::into_float, operand)?.into_inner()); // TODO: Type check

				loop {
					let result = new_self.parse_assignment_list_element()?;
					new_self = result.0;
					if let Some(rvalue) = result.1 {
						list.push(rvalue);
						let result = new_self.skip_spaces().expect_str(",");
						if let Ok(result) = result {
							new_self = result.0;
						} else {
							break;
						}
					} else {
						break;
					}
				}

				let rvalue = ListRvalue::Literal(list);

				(new_self, old_self.make_node(Instructiune::Atribuire(lvalue, AtribuireRvalue::List(rvalue))))
			} else {
				let Node(span, operand) = operand;
				let instruction = match operand {
					expression::Operand::Ident(value) => {
						Instructiune::Atribuire(lvalue, AtribuireRvalue::Unknown(value))
					}
					expression::Operand::FloatRvalue(value) => {
						Instructiune::Atribuire(lvalue, AtribuireRvalue::Float(value))
					}
					_ => return Err(span.0.make_err(ParserErrorKind::ExpectedValueType(make_bitflags!(ValueType::{Float | List})))),
				};
				
				(new_self, old_self.make_node(instruction))
			}
		})
	}
	
	fn parse_second_step_pentru(self, old_self: Self) -> ParserResult<InstructiuneNode<'src>> {
		let (new_self, lvalue) = self.skip_spaces().parse_ident()?;
		let (new_self, _) = new_self.skip_spaces().expect_str("<-")?;
		let (new_self, start) = new_self.skip_spaces().parse_float_rvalue()?;
		
		let (new_self, _) = new_self.skip_spaces().expect_str(",")?;
		let (new_self, end) = new_self.skip_spaces().parse_float_rvalue()?;
		
		let (new_self, increment) = if let Ok((new_self, _)) = new_self.skip_spaces().expect_str(",") {
			new_self.parse_float_rvalue().map(|(new_self, increment)| (new_self, Some(increment)))?
		} else {
			(new_self, None)
		};
		
		let (new_self, _) = new_self.skip_spaces().expect_str_optional_diacritics("execută")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(old_self.make_node(Instructiune::PentruExecuta(lvalue, start, end, increment, Vec::new())))
	}

	/// Checks if the rest of the statement looks like an assignment (or a swap),
	/// and throws an error if that's the case.
	fn check_invalid_ident(self, name: &'src str) -> ParserResult<()> {
		Ok({
			let new_self = self;
			if let Ok((new_self, _arrow)) = new_self.skip_spaces().parse_arrow() {
				return Err(new_self.offset.make_err(ParserErrorKind::InvalidIdent(String::from(name))));
			}
		})
	}
	
	fn parse_second_step_daca(self, old_self: Self) -> ParserResult<InstructiuneNode<'src>> {
		let new_self = self;
		let (new_self, rvalue) = new_self.skip_spaces().parse_bool_rvalue()?;
		let (new_self, _) = new_self.skip_spaces().expect_str_optional_diacritics("atunci")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(old_self.make_node(Instructiune::DacaAtunciAltfel(rvalue, Vec::new(), None)))
	}
	
	fn parse_second_step_cat_timp(self, old_self: Self) -> ParserResult<InstructiuneNode<'src>> {
		Ok({
			let new_self = self;
			let (new_self, _) = new_self.expect_str(" timp")?;
			let (new_self, _) = new_self.expect_str(" ")?;
			let (new_self, rvalue) = new_self.skip_spaces().parse_bool_rvalue()?;
			let (new_self, _) = new_self.skip_spaces().expect_str_optional_diacritics("execută")?;
			new_self.skip_spaces().expect_end()?;
			old_self.make_node(Instructiune::CatTimpExecuta(rvalue, Vec::new()))
		})
	}
	
	pub fn parse_pana_cand(self, instructions: Vec<InstructiuneNode<'src>>) -> ParserResult<InstructiuneNode<'src>> {
		let old_self = self;

		let (new_self, _) = self.expect_str_optional_diacritics("până când ")?;
		let (new_self, rvalue) = new_self.skip_spaces().parse_bool_rvalue()?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(old_self.make_node(Instructiune::RepetaPanaCand(instructions, rvalue)))
	}
	
	pub fn parse_altfel(self) -> ParserResult<()> {
		let (new_self, _) = self.expect_str_optional_diacritics("altfel")?;
		new_self.skip_spaces().expect_end()?;
		Ok(())
	}
	
	pub fn parse_repeta(self) -> ParserResult<()> {
		let (new_self, _) = self.expect_str_optional_diacritics("repetă")?;
		new_self.skip_spaces().expect_end()?;
		Ok(())
	}

	fn parse_second_step_insereaza(self, old_self: Self) -> ParserIntermediateResult<'src, InstructiuneNode<'src>> {
		Ok({
			let new_self = self;
			let (new_self, list) = new_self.skip_spaces().parse_ident()?;
			let (new_self, _) = new_self.skip_spaces().expect_str(",")?;
			let (new_self, index) = new_self.skip_spaces().parse_float_rvalue()?;
			let (new_self, _) = new_self.skip_spaces().expect_str(",")?;
			let (new_self, value) = new_self.skip_spaces().parse_float_rvalue()?;
			(new_self, old_self.make_node(Instructiune::Insereaza(list, index, value)))
		})
	}

	fn parse_second_step_sterge(self, old_self: Self) -> ParserIntermediateResult<'src, InstructiuneNode<'src>> {
		Ok({
			let new_self = self;
			let (new_self, list) = new_self.skip_spaces().parse_ident()?;
			let (new_self, _) = new_self.skip_spaces().expect_str(",")?;
			let (new_self, index) = new_self.skip_spaces().parse_float_rvalue()?;
			(new_self, old_self.make_node(Instructiune::Sterge(list, index)))
		})
	}
	
	fn parse_blockless_instructions(
		mut self, mut old_self: Self,
		mut name: &'src str, mut word: Option<Word>,
	) -> ParserResult<Vec<InstructiuneNode<'src>>> {
		let mut instructions = Vec::new();
		loop {
			let (new_self, instruction) = match word {
				Some(Word::Scrie) => {
					self.check_invalid_ident(name)?;
					self.parse_second_step_scrie(old_self)?
				}
				Some(Word::Citeste) => {
					self.check_invalid_ident(name)?;
					self.parse_second_step_citeste(old_self)?
				}
				Some(Word::Insereaza) => {
					self.check_invalid_ident(name)?;
					self.parse_second_step_insereaza(old_self)?
				}
				Some(Word::Sterge) => {
					self.check_invalid_ident(name)?;
					self.parse_second_step_sterge(old_self)?
				}
				Some(word) if !word.can_be_ident() => {
					self.check_invalid_ident(name)?;
					return Err(self.offset.make_err(ParserErrorKind::ExpectedBlocklessInstruction));
				}
				_ => {
					old_self.parse_starting_lvalue()?
				}
			};
			instructions.push(instruction);

			self = new_self.skip_spaces();
			let Ok((new_self, _)) = self.expect_str(";") else {
				break;
			};
			self = new_self.skip_spaces();
			old_self = self;

			(self, (name, word)) = self.parse_word()?;
		}
		self.skip_spaces().expect_end()?;
		Ok(instructions)
	}

	fn parse_starting_lvalue(self) -> ParserIntermediateResult<'src, InstructiuneNode<'src>> {
		Ok({
			let old_self = self;
			let new_self = self;
			let (new_self, lvalue) = new_self.parse_lvalue()?;
			
			let (new_self, arrow) = new_self.skip_spaces().parse_arrow()?;
			match arrow {
				Arrow::Left => {
					new_self.parse_assignment(old_self, lvalue)?
				}
				Arrow::LeftRight => {
					let (new_self, other_lvalue) = new_self.skip_spaces().parse_lvalue()?;
					(new_self, old_self.make_node(Instructiune::Interschimbare(lvalue, other_lvalue)))
				}
			}
		})
	}

	fn parse_word(self) -> ParserIntermediateResult<'src, (&'src str, Option<Word>)> {
		Ok({
			let new_self = self;
			let (new_self, name) = new_self.read_while(|x| matches!(get_grapheme_kind(x), GraphemeKind::Other));
			match name.graphemes(true).nth(0).unwrap() {
				"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" => {
					return Err(self.offset.make_err(ParserErrorKind::InvalidWord(String::from(name))));
				}
				_ => (),
			}

			let word = Word::from_name(name);
			(new_self, (name, word))
		})
	}

	pub fn parse(self) -> ParserResult<Vec<InstructiuneNode<'src>>> {
		Ok({
			let old_self = self;

			let new_self = self;
			let (new_self, (value, word)) = new_self.parse_word()?;
			match word {
				Some(Word::Daca) => {
					new_self.check_invalid_ident(value)?;
					vec![new_self.parse_second_step_daca(old_self)?]
				}
				Some(Word::Cat) => {
					if let Ok((_new_self, _arrow)) = new_self.skip_spaces().parse_arrow() {
						new_self.parse_blockless_instructions(old_self, value, word)?
					} else {
						vec![new_self.parse_second_step_cat_timp(old_self)?]
					}
				}
				Some(Word::Pentru) => {
					new_self.check_invalid_ident(value)?;
					vec![new_self.parse_second_step_pentru(old_self)?]
				}
				_ =>
					new_self.parse_blockless_instructions(old_self, value, word)?,
			}
		})
	}
}
