pub mod expression;

use trace::{trace, init_depth_var};
init_depth_var!();

use expression::*;
use crate::syntax::{
	Instructiune,
	ScrieParam,
	Lvalue, FloatUnop, FloatBinop, BoolFloatBinop, BoolBoolBinop};
use itertools::izip;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug)]
pub enum GraphemeKind { Reserved, Ignored, Other }
pub fn get_grapheme_kind(grapheme: &str) -> Option<GraphemeKind> {
	match grapheme {
		"+"|"-"|"*"|"/"|"%"|
		"="|"!"|"<"|">"|"|"|
		"("|")"|"["|"]"
			=> Some(GraphemeKind::Reserved),
		" "
			=> Some(GraphemeKind::Ignored),
		","|";"
			=> None,
		_
			=> Some(GraphemeKind::Other),
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum LineParsingErrorKind {
	ExpectedStr(&'static str),
	ExpectedStrOptionalDiacritics(&'static str),
	ExpectedGrapheme(&'static str),
	ExpectedAnyGrapheme,
	ExpectedEnd,
	ExpectedLvalue,
	ExpectedScrieParam,
	ExpectedFloatRvalue,
	ExpectedBoolRvalue,
	ExpectedNonrecursiveInstruction,
	
	ExpectedMegatron(expression::ExpectingFlags),
	MismatchedParens,
	UnclosedLParen,
	UnclosedRParen,
	InvalidLvalueName,
	InvalidFloatLiteral,
	
	InvalidFloatUnopOperands(FloatUnop),
	InvalidFloatBinopOperands(FloatBinop),
	InvalidBoolFloatBinopOperands(BoolFloatBinop),
	InvalidBoolBoolBinopOperands(BoolBoolBinop),
}

#[derive(Debug)]
pub struct LineParsingError(usize, LineParsingErrorKind);
type LineParsingIntermediateResult<'a, T> = Result<(LineCursor<'a>, T), LineParsingError>;
pub type LineParsingResult<T> = Result<T, LineParsingError>;

#[derive(Debug, Clone, Copy)]
struct LineCursor<'a> {
	pub code: &'a str,
	pub index: usize,
	pub grapheme: usize,
}
impl<'a> LineCursor<'a> {
	fn new(code: &'a str) -> Self {
		Self { code, index: 0, grapheme: 0 }
	}
	fn make_error<T: Into<LineParsingErrorKind>>(&self, kind: T) -> LineParsingError {
		LineParsingError(self.grapheme, kind.into())
	}
	fn code(&self) -> &'a str {
		&self.code[self.index..]
	}
	fn code_until(&self, until: usize) -> &'a str {
		&self.code[self.index..self.index+until]
	}
	fn advance_by(&mut self, amount: usize) {
		self.grapheme += self.code_until(amount).graphemes(true).count();
		self.index += amount;
	}
	fn expect_str(mut self, expected: &'static str) -> LineParsingIntermediateResult<'a, ()> {
		if self.code().starts_with(expected) {
			self.advance_by(expected.len());
			Ok((self, ()))
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedStr(expected))) }
	}
	fn expect_str_optional_diacritics(mut self, expected: &'static str) -> LineParsingIntermediateResult<'a, ()> {
		let mut graphemes = self.code().graphemes(true);
		let mut expected_graphemes = expected.graphemes(true);
		for (grapheme, expected_grapheme) in izip!(&mut graphemes, &mut expected_graphemes) {
			self.grapheme += 1;
			
			// NOTE: It is crucial that grapheme.len() is added here, and not
			// expected_grapheme.len(), because grapheme and expected_grapheme
			// (and their lengths) may be different, and adding the wrong
			// one may ruin the index.
			self.index += grapheme.len();
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
			if !matches { return Err(self.make_error(LineParsingErrorKind::ExpectedStrOptionalDiacritics(expected))) }
		}
		
		if expected_graphemes.next().is_some() {
			return Err(self.make_error(LineParsingErrorKind::ExpectedStrOptionalDiacritics(expected)))
		}
		
		Ok((self, ()))
	}
	fn skip_spaces(mut self) -> Self {
		let (graphemes, offset) =
			self.code().grapheme_indices(true)
				.map(|(i, x)| { (i, Some(x)) })
				.chain(std::iter::once((self.code().len(), None)))
			.enumerate()
				.skip_while(|(_, (_, x))| { x.map_or(false, |x| x == " ") })
				.next()
				.map(|(g, (i, _))| { (g, i) })
			.unwrap(); // this unwrap shouldn't ever fail
		self.grapheme += graphemes;
		self.index += offset;
		self
	}
	fn read_while<P: FnMut(&str) -> bool>(mut self, mut predicate: P) -> (Self, &'a str) {
		let (graphemes, offset) =
			self.code().grapheme_indices(true)
				.map(|(i, x)| { (i, Some(x)) })
				.chain(std::iter::once((self.code().len(), None)))
			.enumerate()
				.skip_while(|(_, (_, x))| { x.map_or(false, &mut predicate) })
				.next()
				.map(|(g, (i, _))| { (g, i) })
			.unwrap(); // this unwrap shouldn't ever fail
		let result = self.code_until(offset);
		self.grapheme += graphemes;
		self.index += offset;
		(self, result)
	}
	fn read_one(mut self) -> Option<(Self, &'a str)> {
		let one = self.code().grapheme_indices(true).next();
		if let Some((_, one)) = one {
			self.grapheme += 1;
			self.index += one.len();
			Some((self, one))
		} else { None }
	}
	fn parse_lvalue(self) -> LineParsingIntermediateResult<'a, Lvalue<'a>> {
		let (new_self, name) = self.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
		if !name.is_empty() {
			Ok((new_self, Lvalue(name)))
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedLvalue)) }
	}
	fn expect_end(self) -> LineParsingResult<()> {
		if self.code.len() == self.index {
			Ok(())
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedEnd)) }
	}
	fn parse_second_step_citeste(mut self) -> LineParsingIntermediateResult<'a, Instructiune<'a>> {
		let mut lvalues = Vec::new();
		let mut done = false;
		while !done {
			self = self.skip_spaces();
			let (new_self, lvalue) = self.parse_lvalue()?;
			self = new_self;
			lvalues.push(lvalue);
			self = self.skip_spaces();
			match self.expect_grapheme(",") {
				Ok((new_self, _)) => self = new_self,
				Err(..) => done = true,
			}
		}
		Ok((self, Instructiune::Citeste(lvalues)))
	}
	fn next_grapheme(mut self) -> LineParsingIntermediateResult<'a, &'a str> {
		let grapheme = self.code()
			.graphemes(true)
			.next();
		if let Some(grapheme) = grapheme {
			self.index += grapheme.len();
			self.grapheme += 1;
			Ok((self, grapheme))
		} else { Err(self.make_error(LineParsingErrorKind::ExpectedAnyGrapheme)) }
	}
	fn expect_grapheme(self, expected_grapheme: &'static str) -> LineParsingIntermediateResult<'a, ()> {
		let err = LineParsingErrorKind::ExpectedGrapheme(expected_grapheme);
		if let Ok((new_self, grapheme)) = self.next_grapheme() {
			if grapheme == expected_grapheme { Ok((new_self, ())) }
			else { Err(self.make_error(err)) }
		} else { Err(self.make_error(err)) }
	}
	fn parse_scrie_param(self) -> LineParsingIntermediateResult<'a, ScrieParam<'a>> {
		match self.next_grapheme().map_err(|_| self.make_error(LineParsingErrorKind::ExpectedScrieParam))? {
			(new_self, "'") => {
				let (new_self, character) = new_self.next_grapheme()?;
				let (new_self, _) = new_self.expect_grapheme("'")?;
				Ok((new_self, ScrieParam::CharacterLiteral(character)))
			}
			(new_self, "\"") => {
				let (new_self, string) = new_self.read_while(|grapheme| grapheme != "\"");
				let (new_self, _) = new_self.expect_grapheme("\"")?;
				Ok((new_self, ScrieParam::StringLiteral(string)))
			}
			_ => {
				let (new_self, rvalue) = self.parse_float_rvalue()?;
				Ok((new_self, ScrieParam::Rvalue(rvalue)))
			}
		}
	}
	fn parse_second_step_scrie(mut self) -> LineParsingIntermediateResult<'a, Instructiune<'a>> {
		let mut params = Vec::new();
		let mut done = false;
		while !done {
			let (new_self, param) = self.skip_spaces().parse_scrie_param()?;
			params.push(param);
			self = new_self.skip_spaces();
			match self.expect_grapheme(",") {
				Ok((new_self, _)) => self = new_self,
				Err(..) => done = true,
			}
		}
		Ok((self, Instructiune::Scrie(params)))
	}
	
	fn parse_second_step_lvalue(self, lvalue: Lvalue<'a>) -> LineParsingIntermediateResult<'a, Instructiune<'a>> {
		let (new_self, _) = self.skip_spaces().expect_str("<-")?;
		if let Ok((new_self, _)) = new_self.expect_grapheme(">") {
			let (new_self, other_lvalue) = new_self.skip_spaces().parse_lvalue()?;
			Ok((new_self, Instructiune::Interschimbare(lvalue, other_lvalue)))
		} else {
			let (new_self, rvalue) = new_self.skip_spaces().parse_float_rvalue()?;
			Ok((new_self, Instructiune::Atribuire(lvalue, rvalue)))
		}
	}
	
	fn parse_second_step_pentru(self) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, lvalue) = self.skip_spaces().parse_lvalue()?;
		let (new_self, _) = new_self.skip_spaces().expect_str("<-")?;
		let (new_self, start) = new_self.skip_spaces().parse_float_rvalue()?;
		
		let (new_self, _) = new_self.skip_spaces().expect_grapheme(",")?;
		let (new_self, end) = new_self.skip_spaces().parse_float_rvalue()?;
		
		let (new_self, increment) = if let Ok((new_self, _)) = new_self.skip_spaces().expect_grapheme(",") {
			new_self.parse_float_rvalue().map(|(new_self, increment)| (new_self, Some(increment)))?
		} else { (new_self, None) };
		
		let (new_self, _) = new_self.skip_spaces().expect_str_optional_diacritics("execută")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::PentruExecuta(lvalue, start, end, increment, Vec::new()))
	}
	
	fn parse_second_step_daca(self) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, rvalue) = self.skip_spaces().parse_bool_rvalue()?;
		let (new_self, _) = new_self.skip_spaces().expect_str_optional_diacritics("atunci")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::DacaAtunciAltfel(rvalue, Vec::new(), None))
	}
	
	fn parse_second_step_cat_timp(self) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, _) = self.expect_str_optional_diacritics(" timp ")?;
		let (new_self, rvalue) = new_self.skip_spaces().parse_bool_rvalue()?;
		let (new_self, _) = new_self.skip_spaces().expect_str_optional_diacritics("execută")?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::CatTimpExecuta(rvalue, Vec::new()))
	}
	
	fn parse_pana_cand(self, instructions: Vec<Instructiune<'a>>) -> LineParsingResult<Instructiune<'a>> {
		let (new_self, _) = self.expect_str_optional_diacritics("până când ")?;
		let (new_self, rvalue) = new_self.skip_spaces().parse_bool_rvalue()?;
		new_self.skip_spaces().expect_end()?;
		
		Ok(Instructiune::RepetaPanaCand(instructions, rvalue))
	}
	
	fn parse_altfel(self) -> LineParsingResult<()> {
		let (new_self, _) = self.expect_str_optional_diacritics("altfel")?;
		new_self.skip_spaces().expect_end()?;
		Ok(())
	}
	
	fn parse_repeta(self) -> LineParsingResult<()> {
		let (new_self, _) = self.expect_str_optional_diacritics("repetă")?;
		new_self.skip_spaces().expect_end()?;
		Ok(())
	}
	
	fn parse_nonrecursive_instructions(mut self, mut name: &'a str) -> LineParsingResult<Vec<Instructiune<'a>>> {
		let mut done = false;
		let mut instructions = Vec::new();
		while !done {
			// dbg!(self.grapheme);
			let (new_self, instruction) = match name {
				"daca"|"dacă"|"cat"|"cât"|"pentru" =>
					Err(self.make_error(LineParsingErrorKind::ExpectedNonrecursiveInstruction)),
				"scrie" =>
					self.parse_second_step_scrie(),
				"citeste"|"citește" =>
					self.parse_second_step_citeste(),
				x =>
					self.parse_second_step_lvalue(Lvalue(x)),
			}?;
			instructions.push(instruction);
			self = new_self.skip_spaces();
			if let Ok((new_self, _)) = self.expect_grapheme(";") {
				self = new_self.skip_spaces();
				let (new_self, new_name) = self.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
				self = new_self;
				name = new_name;
			} else { done = true }
		}
		self.skip_spaces().expect_end()?;
		Ok(instructions)
	}
	
	fn parse(self) -> LineParsingResult<Vec<Instructiune<'a>>> {
		let (new_self, name) = self.read_while(|x| matches!(get_grapheme_kind(x), Some(GraphemeKind::Other)));
		match name {
			"daca"|"dacă" =>
				Ok(vec![new_self.parse_second_step_daca()?]),
			"cat"|"cât" =>
				Ok(vec![new_self.parse_second_step_cat_timp()?]),
			"pentru" =>
				Ok(vec![new_self.parse_second_step_pentru()?]),
			_ =>
				new_self.parse_nonrecursive_instructions(name),
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParsingErrorKind {
	LineParsingError(LineParsingErrorKind),
	DacaAlreadyHasAltfel,
	AltfelWithoutDaca,
	InvalidIndent,
	EmptyBlock,
	RepetaWithoutPanaCand,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsingError(pub usize, pub usize, pub ParsingErrorKind);
type IntermediateParsingResult<'a> = Result<Cursor<'a>, ParsingError>;
pub type ParsingResult<T> = Result<T, ParsingError>;

#[derive(Debug, Clone, Copy)]
struct Cursor<'a> {
	code: &'a str,
	line: usize,
	index: usize,
}

impl<'a> Cursor<'a> {
	fn new(code: &'a str) -> Self {
		Self { code, line: 0, index: 0 }
	}
	fn next_line(mut self) -> Option<(Self, &'a str)> {
		if let Some((offset, _)) = self.code[self.index..].grapheme_indices(true)
			.find(|(_, grapheme)| *grapheme == "\n") {
			let line = &self.code[self.index..self.index+offset];
			self.index += offset + "\n".len();
			self.line += 1;
			Some((self, line))
		} else if self.index < self.code.len() {
			let line = &self.code[self.index..];
			self.index = self.code.len();
			self.line += 1;
			Some((self, line))
		} else { None }
	}
	fn make_error_from_line(&self, line_parsing_error: LineParsingError) -> ParsingError {
		ParsingError(
			self.line,
			line_parsing_error.0,
			ParsingErrorKind::LineParsingError(line_parsing_error.1)
		)
	}
	fn make_error(&self, kind: ParsingErrorKind) -> ParsingError {
		ParsingError(self.line, 0, kind)
	}
	fn parse(mut self, instructions: &mut Vec<Instructiune<'a>>, indent: usize) -> IntermediateParsingResult<'a> {
		enum Expecting<'a> { Anything, PanaCand(Vec<Instructiune<'a>>) }
		let mut expecting = Expecting::Anything;
		while let Some((new_self, line)) = self.next_line() {
			let current_indent = line
				.graphemes(true)
				.take_while(|x| *x == "\t")
				.count();
			match current_indent.cmp(&indent) {
				std::cmp::Ordering::Less => break,
				std::cmp::Ordering::Greater => {
					self = new_self;
					return Err(self.make_error(ParsingErrorKind::InvalidIndent));
				}
				std::cmp::Ordering::Equal => {
					self = new_self;
					let line = &line[current_indent*"\t".len()..];
					let line_cursor = LineCursor::new(line);
					if line != "" {
						expecting = match expecting {
							// pana cand
							Expecting::PanaCand(pana_cand_instructions) => {
								let empty_block = pana_cand_instructions.is_empty();
								
								let instruction = line_cursor.parse_pana_cand(pana_cand_instructions)
									.map_err(|err| self.make_error_from_line(err))?;
								
								if empty_block {
									return Err(self.make_error(ParsingErrorKind::EmptyBlock));
								}
								
								instructions.push(instruction);
								
								Expecting::Anything
							}
							
							Expecting::Anything => {
								// altfel
								if line_cursor.parse_altfel().is_ok() {
									if let Some(Instructiune::DacaAtunciAltfel(_, _, instructions)) = instructions.last_mut() {
										if instructions.is_some() { return Err(self.make_error(ParsingErrorKind::DacaAlreadyHasAltfel)) }
										*instructions = Some({
											let mut instructions = Vec::new();
											self = self.parse(&mut instructions, indent+1)?;
											if instructions.is_empty() {
												return Err(self.make_error(ParsingErrorKind::EmptyBlock));
											}
											instructions
										});
									} else { return Err(self.make_error(ParsingErrorKind::AltfelWithoutDaca)) }
									
									Expecting::Anything
								}
								
								// repeta
								else if line_cursor.parse_repeta().is_ok() {
									let mut instructions = Vec::new();
									self = self.parse(&mut instructions, indent+1)?;
									
									Expecting::PanaCand(instructions)
								}
								
								// everything else
								else {
									let mut parsed_instructions = line_cursor.parse()
										.map_err(|err| self.make_error_from_line(err))?;
									match &mut parsed_instructions[..] {
										[ Instructiune::DacaAtunciAltfel(_, instructions, _) ]
										| [ Instructiune::CatTimpExecuta(_, instructions) ]
										| [ Instructiune::PentruExecuta(_, _, _, _, instructions) ]
										| [ Instructiune::RepetaPanaCand(instructions, _) ] => {
											self = self.parse(instructions, indent+1)?;
											if instructions.is_empty() {
												return Err(self.make_error(ParsingErrorKind::EmptyBlock));
											}
										}
										_ => (), // do nothing 
									}
									instructions.append(&mut parsed_instructions);
									
									Expecting::Anything
								}
							}
						}
					} else { self = new_self }
				}
			}
		}
		
		if let Expecting::PanaCand(..) = expecting {
			return Err(self.make_error(ParsingErrorKind::RepetaWithoutPanaCand));
		}
		
		Ok(self)
	}
}

pub fn parse<'a>(code: &'a str) -> ParsingResult<Vec<Instructiune<'a>>> {
	let mut program = Vec::new();
	let cursor = Cursor::new(&code);
	cursor.parse(&mut program, 0).map(|_| program)
}

#[cfg(test)]
mod tests {
	use enumflags2::make_bitflags;
	use indoc::indoc;
	
	macro_rules! assert_parsing_error {
		( $code:expr, $err:expr ) => {
			assert_eq!(parse(indoc! { $code }).unwrap_err(), $err);
		}
	}
	
	macro_rules! test_parsing_error {
		{ $function:ident, $err:expr, $code:expr } => {
			#[test]
			fn $function() {
				assert_parsing_error!($code, $err);
			}
		}
	}
	
	use super::{
		parse, 
		ParsingError, ParsingErrorKind::*,
		LineParsingErrorKind::*,
		expression::Expecting,
	};
	
	// expressions
	test_parsing_error! {
		missing_operand_or_unop,
		ParsingError(
			1, 15,
			LineParsingError(
				ExpectedMegatron(
					make_bitflags!(Expecting::{PrefixUnop | Rvalue})))),
		r#"scrie   4141+  "#
	}
	
	test_parsing_error! {
		missing_operand,
		ParsingError(
			1, 16,
			LineParsingError(
				ExpectedMegatron(
					make_bitflags!(Expecting::{Rvalue})))),
		r#"scrie   4141+-  "#
	}
	
	test_parsing_error! {
		mismatched_parens,
		ParsingError(
			1, 10,
			LineParsingError(
				MismatchedParens)),
		r#"scrie [1+2)"#
	}
	
	test_parsing_error! {
		too_many_unops,
		ParsingError(
			1, 7,
			LineParsingError(
				ExpectedMegatron(
					make_bitflags!(Expecting::{Rvalue})))),
		r#"scrie ++41+1"#
	}
	
	test_parsing_error! {
		empty_parens,
		ParsingError(
			1, 4,
			LineParsingError(
				ExpectedMegatron(
					make_bitflags!(Expecting::{PrefixUnop | Rvalue})))),
		r#"a<-()"#
	}
	
	test_parsing_error! {
		unclosed_lparen,
		ParsingError(
			1, 7,
			LineParsingError(
				UnclosedLParen)),
		r#"a<-(x+y"#
	}
	
	test_parsing_error! {
		unclosed_rparen,
		ParsingError(
			1, 6,
			LineParsingError(
				UnclosedRParen)),
		r#"a<-x+y)"#
	}
	
	test_parsing_error! {
		invalid_float_unop_operands_fixme,
		ParsingError(
			1, 17,
			LineParsingError(
				ExpectedNonrecursiveInstruction)),
		r#"
			daca +(1=2 sau 3<5 si 4=4) atunci
				scrie "ok"
		"#
	}
	
	test_parsing_error! {
		invalid_float_literal_fixme,
		ParsingError(
			1, 4,
			LineParsingError(
				InvalidFloatLiteral)),
		r#"a<-41yeet41"#
	}
	
	// other stuff
	test_parsing_error! {
		invalid_indent,
		ParsingError(
			2, 0,
			InvalidIndent),
		r#"
			scrie "ok"
				scrie "nope"
		"#
	}
	
	test_parsing_error! {
		altfel_without_daca,
		ParsingError(
			3, 0,
			AltfelWithoutDaca),
		r#"
			scrie 41
			
			altfel
		"#
	}
	
	test_parsing_error! {
		daca_already_has_altfel,
		ParsingError(
			7, 0,
			DacaAlreadyHasAltfel),
		r#"
			scrie 41
			
			daca 41=41 atunci
				scrie "ok"
			altfel
				scrie "catastrofa"
			altfel
				scrie "wut"
		"#
	}
	
	test_parsing_error! {
		empty_block_daca,
		ParsingError(
			2, 0,
			EmptyBlock),
		r#"
			daca 40+1=41 atunci
				
			scrie "bun"
		"#
	}
	
	test_parsing_error! {
		empty_block_altfel,
		ParsingError(
			3, 0,
			EmptyBlock),
		r#"
			daca 40+1=41 atunci
				scrie "bun"
			altfel
			scrie "ne-bun"
		"#
	}
	
	test_parsing_error! {
		empty_block_repeta_pana_cand,
		ParsingError(
			7, 0,
			EmptyBlock),
		r#"
			repeta
				
				
				
				
				
			pana cand 41=42-1
		"#
	}
	
	test_parsing_error! {
		repeta_without_pana_cand,
		ParsingError(
			3, 0,
			RepetaWithoutPanaCand),
		r#"
			repeta
				
				
		"#
	}
	
	test_parsing_error! {
		repeta_without_pana_cand_short,
		ParsingError(
			1, 0,
			RepetaWithoutPanaCand),
		r#"repeta"#
	}
	
	test_parsing_error! {
		recursive_instruction_after_nonrecursive_instruction,
		ParsingError(
			1, 17,
			LineParsingError(ExpectedNonrecursiveInstruction)),
		r#"
			scrie 12+3 ; daca a=12 atunci
		"#
	}
}
