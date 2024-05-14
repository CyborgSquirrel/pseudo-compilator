use enumflags2::{bitflags, BitFlags};

use crate::{ast::{
	Instructiune,
	InstructiuneNode, FloatUnop, FloatBinop, BoolFloatBinop, BoolBoolBinop}, parse::line::LineCursor, source::{Offset, Node}};
use unicode_segmentation::UnicodeSegmentation;

pub mod expression;
pub mod line;

#[derive(Debug)]
pub enum GraphemeKind {
	Reserved,
	Ignored,
	Separator,
	Other,
}

pub fn get_grapheme_kind(grapheme: &str) -> GraphemeKind {
	match grapheme {
		"+"|"-"|"*"|"/"|"%"|
		"="|"!"|"<"|">"|"|"|
		"("|")"|"["|"]"
			=> GraphemeKind::Reserved,
		" "
			=> GraphemeKind::Ignored,
		","|";"|"\n"
			=> GraphemeKind::Separator,
		_
			=> GraphemeKind::Other,
	}
}

#[derive(Debug)]
pub enum Word {
	Daca,
	Cat,
	Pentru,
	Scrie,
	Citeste,
	Insereaza,
	Sterge,
}

impl Word {
	fn from_name(name: &str) -> Option<Self> {
		Some(match name {
			"daca"|"dacă" => Word::Daca,
			"cat"|"cât" => Word::Cat,
			"pentru" => Word::Pentru,
			"scrie" => Word::Scrie,
			"citeste"|"citește" => Word::Citeste,
			"insereaza"|"inserează" => Word::Insereaza,
			"sterge"|"șterge" => Word::Sterge,
			_ => return None,
		})
	}
}

#[derive(Debug)]
pub enum Arrow {
	Left,
	LeftRight,
}

#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
	Float,
	Bool,
	List,
}
pub type ValueTypeFlags = BitFlags<ValueType>;

#[derive(Debug, PartialEq, Eq)]
pub struct ParserError(pub Offset, pub ParserErrorKind);
type ParserIntermediateResult<'src> = Result<Cursor<'src>, ParserError>;
pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, PartialEq, Eq)]
pub enum ParserErrorKind {
	DacaAlreadyHasAltfel,
	AltfelWithoutDaca,
	InvalidIndent,
	EmptyBlock,
	RepetaWithoutPanaCand,

	ExpectedStr(&'static str),
	ExpectedStrOptionalDiacritics(&'static str),
	ExpectedAnyGrapheme,
	ExpectedIdent,
	ExpectedScrieParam,
	ExpectedFloatRvalue,
	ExpectedBoolRvalue,
	ExpectedLvalue,
	ExpectedBlocklessInstruction,
	ExpectedValueType(ValueTypeFlags),

	ExpectedEnd,
	ExpectedEndOfBlocklessInstruction,
	
	ExpectedSomethingElse(expression::ExpectingFlags),
	MismatchedParens,
	UnclosedLParen,
	UnclosedRParen,

	InvalidIdent(String),
	InvalidFunction(String),
	InvalidFloatLiteral,

	InvalidWord(String),
}

#[derive(Debug, Clone, Copy)]
struct Cursor<'src> {
	code: &'src str,
	offset: Offset,
}

impl<'a> Cursor<'a> {
	fn new(code: &'a str) -> Self {
		Self {
			code,
			offset: Offset::zero(),
		}
	}

	fn code(&self) -> &'a str {
		&self.code[self.offset.bytes() as usize..]
	}

	fn parse(mut self, instructions: &mut Vec<InstructiuneNode<'a>>, indent: usize) -> ParserIntermediateResult<'a> {
		enum Expecting<'src> {
			Anything,
			PanaCand(Offset, Vec<InstructiuneNode<'src>>),
		}
		let mut expecting = Expecting::Anything;

		loop {
			// If the code is finished, then stop.
			if self.code().len() == 0 {
				break;
			}
			
			// Compute indent for current line.
			let current_indent = {
				let mut indent = 0;
				for grapheme in self.code().graphemes(true) {
					if grapheme != "\t" {
						break;
					}
					indent += 1;
					self.offset.add_grapheme(grapheme);
				}
				indent
			};

			// Look at the line's indent:
			// - if it's smaller, that means that this block has ended => break
			// - if it's bigger, that means it doesn't belong to any instruction that
			//   would have increased the indent => error
			// - if it's the same, that means that this block is continuing => do nothing
			match current_indent.cmp(&indent) {
				std::cmp::Ordering::Less => break,
				std::cmp::Ordering::Greater => {
					return Err(self.offset.make_err(ParserErrorKind::InvalidIndent));
				}
				std::cmp::Ordering::Equal => (),
			}

			// Create line cursor with *current* offset.
			let line_offset = self.offset;
			let line_cursor = LineCursor::new(self.code, line_offset);

			// Move offset to next line.
			let mut is_empty = true;
			for grapheme in self.code().graphemes(true) {
				self.offset.add_grapheme(grapheme);
				if grapheme == "\n" {
					break;
				}
				if grapheme != " " {
					is_empty = false;
				}
			}

			if is_empty {
				continue;
			}

			// TODO: Make LineCursor output an enum that looks something like:
			//
			// enum Idk {
			// 	Instructions(Vec<Instruction>),
			// 	PanaCand,
			// 	Empty,
			// }
			//
			// This way, you will have just one parse() function, which could
			// conveniently be used outside of this code, for stuff like syntax
			// highlighting (instead of having multiple, context-dependent) parsing
			// functions.

			expecting = match expecting {
				// pana cand
				Expecting::PanaCand(_, pana_cand_instructions) => {
					let empty_block = pana_cand_instructions.is_empty();
				
					let instruction = line_cursor.parse_pana_cand(pana_cand_instructions)?;
				
					if empty_block {
						return Err(line_offset.make_err(ParserErrorKind::EmptyBlock));
					}
				
					instructions.push(instruction);
				
					Expecting::Anything
				}
			
				Expecting::Anything => {
					// altfel
					if line_cursor.parse_altfel().is_ok() {
						if let Some(Instructiune::DacaAtunciAltfel(_, _, altfel)) = instructions.last_mut().map(Node::inner_mut) {
							if altfel.is_some() {
								return Err(line_offset.make_err(ParserErrorKind::DacaAlreadyHasAltfel));
							}

							*altfel = Some({
								let mut instructions = Vec::new();
								self = self.parse(&mut instructions, indent+1)?;
								if instructions.is_empty() {
									return Err(line_offset.make_err(ParserErrorKind::EmptyBlock));
								}
								instructions
							});
						} else {
							return Err(line_offset.make_err(ParserErrorKind::AltfelWithoutDaca))
						}
					
						Expecting::Anything
					}
				
					// repeta
					else if line_cursor.parse_repeta().is_ok() {
						let mut instructions = Vec::new();
						self = self.parse(&mut instructions, indent+1)?;
					
						Expecting::PanaCand(line_offset, instructions)
					}
				
					// everything else
					else {
						let mut parsed_instructions = line_cursor.parse()?;
						match parsed_instructions.get_mut(0).map(Node::inner_mut) {
							Some(Instructiune::DacaAtunciAltfel(_, instructions, _)) 
							| Some(Instructiune::CatTimpExecuta(_, instructions)) 
							| Some(Instructiune::PentruExecuta(_, _, _, _, instructions)) 
							| Some(Instructiune::RepetaPanaCand(instructions, _))  => {
								self = self.parse(instructions, indent+1)?;
								if instructions.is_empty() {
									return Err(line_offset.make_err(ParserErrorKind::EmptyBlock));
								}
							}
							_ => (), // do nothing 
						}
						instructions.append(&mut parsed_instructions);
					
						Expecting::Anything
					}
				}
			}
		}

		if let Expecting::PanaCand(offset, _) = expecting {
			return Err(offset.make_err(ParserErrorKind::RepetaWithoutPanaCand));
		}
		
		Ok(self)
	}
}

pub fn parse<'a>(code: &'a str) -> ParserResult<Vec<InstructiuneNode<'a>>> {
	let mut program = Vec::new();
	let cursor = Cursor::new(&code);
	cursor.parse(&mut program, 0).map(|_| program)
}

#[cfg(test)]
mod tests {
	use enumflags2::make_bitflags;
	use indoc::indoc;
	
	macro_rules! test_parser_err {
		{ $function:ident, ($line:expr , $column:expr , $kind:expr $(,)?), $code:expr } => {
			#[test]
			fn $function() {
				let code = indoc!{ $code };
				print!("===\n{code}\n===\n");
				let err = parse(code).unwrap_err();
				assert_eq!(
					($line, $column, $kind),
					(err.0.line_one(), err.0.column(), err.1),
				)
			}
		}
	}

	macro_rules! test_parser_ok {
		{ $function:ident, $code:expr } => {
			#[test]
			fn $function() {
				parse(indoc! { $code }).unwrap();
			}
		}
	}
	
	use crate::parse::ValueType;

use super::{
		parse, 
		ParserErrorKind::*,
		expression::Expecting,
	};
	
	// expressions
	test_parser_err! {
		missing_operand_or_unop,
		(
			1, 15,
			ExpectedSomethingElse(make_bitflags!(Expecting::{PrefixUnop | Rvalue})),
		),
		r#"scrie   4141+  "#
	}
	
	test_parser_err! {
		missing_operand,
		(
			1, 16,
			(ExpectedSomethingElse(make_bitflags!(Expecting::{Rvalue}))),
		),
		r#"scrie   4141+-  "#
	}
	
	test_parser_err! {
		mismatched_parens,
		(
			1, 10,
			MismatchedParens,
		),
		r#"scrie [1+2)"#
	}
	
	test_parser_err! {
		too_many_unops,
		(
			1, 7,
			ExpectedSomethingElse(make_bitflags!(Expecting::{Rvalue})),
		),
		r#"scrie ++41+1"#
	}
	
	test_parser_err! {
		empty_parens,
		(
			1, 4,
			ExpectedSomethingElse(make_bitflags!(Expecting::{PrefixUnop | Rvalue})),
		),
		r#"a<-()"#
	}
	
	test_parser_err! {
		unclosed_lparen,
		(
			1, 3,
			UnclosedLParen,
		),
		r#"a<-(x+y"#
	}
	
	test_parser_err! {
		unclosed_rparen,
		(
			1, 6,
			UnclosedRParen,
		),
		r#"a<-x+y)"#
	}
	
	test_parser_err! {
		invalid_float_unop_operands_fixme,
		(
			1, 17,
			ExpectedBlocklessInstruction,
		),
		r#"
			daca +(1=2 sau 3<5 si 4=4) atunci
				scrie "ok"
		"#
	}
	
	test_parser_err! {
		invalid_float_literal_fixme,
		(
			1, 4,
			InvalidFloatLiteral,
		),
		r#"a<-41yeet41"#
	}
	
	// other stuff
	test_parser_err! {
		invalid_indent,
		(
			2, 1,
			InvalidIndent,
		),
		r#"
			scrie "ok"
				scrie "nope"
		"#
	}
	
	test_parser_err! {
		altfel_without_daca,
		(
			3, 0,
			AltfelWithoutDaca,
		),
		r#"
			scrie 41
			
			altfel
		"#
	}
	
	test_parser_err! {
		daca_already_has_altfel,
		(
			7, 0,
			DacaAlreadyHasAltfel,
		),
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
	
	test_parser_err! {
		empty_block_daca,
		(
			1, 0,
			EmptyBlock,
		),
		r#"
			daca 40+1=41 atunci
				
			scrie "bun"
		"#
	}
	
	test_parser_err! {
		empty_block_altfel,
		(
			3, 0,
			EmptyBlock,
		),
		r#"
			daca 40+1=41 atunci
				scrie "bun"
			altfel
			scrie "ne-bun"
		"#
	}
	
	test_parser_err! {
		empty_block_repeta_pana_cand,
		(
			7, 0,
			EmptyBlock,
		),
		r#"
			repeta
				
				
				
				
				
			pana cand 41=42-1
		"#
	}
	
	test_parser_err! {
		repeta_without_pana_cand,
		(
			1, 0,
			RepetaWithoutPanaCand,
		),
		r#"
			repeta
				
				
		"#
	}
	
	test_parser_err! {
		repeta_without_pana_cand_short,
		(
			1, 0,
			RepetaWithoutPanaCand,
		),
		r#"repeta"#
	}
	
	test_parser_err! {
		recursive_instruction_after_nonrecursive_instruction,
		(
			1, 17,
			ExpectedBlocklessInstruction,
		),
		r#"
			scrie 12+3 ; daca a=12 atunci
		"#
	}

	test_parser_err! {
		invalid_assignment_daca,
		(
			1, 7,
			InvalidIdent(String::from("daca")),
		),
		r#"
			daca <- 42
		"#
	}

	test_parser_err! {
		invalid_assignment_pentru,
		(
			1, 9,
			InvalidIdent(String::from("pentru")),
		),
		r#"
			pentru <- 42
		"#
	}

	test_parser_err! {
		invalid_assignment_cat,
		(
			1, 6,
			InvalidIdent(String::from("cat")),
		),
		r#"
			cat <- 42
		"#
	}

	test_parser_err! {
		invalid_assignment_daca_second,
		(
			1, 16,
			InvalidIdent(String::from("daca"))
		),
		r#"
			x <- 41; daca <- 42
		"#
	}

	test_parser_err! {
		assignment_type_error,
		(
			1, 5,
			ExpectedValueType(make_bitflags!(ValueType::{Float | List})),
		),
		r#"
			x <- 41 < 12
		"#
	}

	test_parser_err! {
		daca_type_error,
		(
			1, 5,
			ExpectedValueType(make_bitflags!(ValueType::{Bool})),
		),
		r#"
			daca 41 atunci
				scrie "da"
		"#
	}

	test_parser_ok! {
		parse_citeste,
		r#"
			a <- 1,2,3
			citeste a[0], b, c, a[1]
		"#
	}
}
