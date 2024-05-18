use std::rc::Rc;

use enumflags2::{bitflags, BitFlags};

use crate::{ast::{Instructiune, InstructiuneNode}, parse::line::LineCursor, source::{Offset, Node}, LanguageSettings};
use unicode_segmentation::UnicodeSegmentation;

pub mod expression;
pub mod line;

#[cfg(test)]
mod tests;

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
	fn from_name(
		language_settings: &LanguageSettings,
		name: &str,
	) -> Option<Self> {
		Some(match name {
			"daca"|"dacă" => Word::Daca,
			"cat"|"cât" => Word::Cat,
			"pentru" => Word::Pentru,
			"scrie" => Word::Scrie,
			"citeste"|"citește" => Word::Citeste,
			"insereaza"|"inserează" if language_settings.enable_list => Word::Insereaza,
			"sterge"|"șterge" if language_settings.enable_list => Word::Sterge,
			_ => return None,
		})
	}

	fn can_be_ident(&self) -> bool {
		match self {
			Word::Cat => true,
			_ => false,
		}
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
	language_settings: &'src LanguageSettings,
	code: &'src str,
	offset: Offset,
}

impl<'src> Cursor<'src> {
	fn new(
		language_settings: &'src LanguageSettings,
		code: &'src str,
	) -> Self {
		Self {
			language_settings,
			code,
			offset: Offset::zero(),
		}
	}

	fn code(&self) -> &'src str {
		&self.code[self.offset.bytes() as usize..]
	}

	fn parse(mut self, instructions: &mut Vec<InstructiuneNode<'src>>, indent: usize) -> ParserIntermediateResult<'src> {
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
			let line_cursor = LineCursor::new(self.language_settings, self.code, line_offset);

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

pub fn parse<'src>(
	language_settings: &'src LanguageSettings,
	code: &'src str,
) -> ParserResult<Vec<InstructiuneNode<'src>>> {
	let mut program = Vec::new();
	let cursor = Cursor::new(
		language_settings,
		code,
	);
	cursor.parse(&mut program, 0).map(|_| program)
}
