use unicode_segmentation::UnicodeSegmentation;

use crate::parse::{ParserErrorKind, ParserError};


/// Offset in source code string.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Offset {
	line: u32,
	column: u32,
	bytes: u32,
	graphemes: u32,
}

impl Offset {
	pub fn zero() -> Self {
		Self {
			line: 0,
			column: 0,
			bytes: 0,
			graphemes: 0,
		}
	}

  pub fn new(line: u32, column: u32) -> Self {
  	Self {
  		line,
  		column,
  		bytes: 0,
  		graphemes: 0,
  	}
  }

  pub fn add_grapheme(&mut self, grapheme: &str) {
  	// TODO: In debug builds, add check to see if grapheme is actually just one
  	// grapheme.

  	if grapheme == "\n" {
  		self.line += 1;
  		self.column = 0;
  	} else {
  		self.column += 1;
  	}
  	
  	self.graphemes += 1;
  	self.bytes += grapheme.len() as u32;
  }

  pub fn add_string(&mut self, string: &str) {
  	for grapheme in string.graphemes(true) {
  		self.add_grapheme(grapheme);
  	}
  }

	pub fn line(&self) -> u32 { self.line }
	/// 1 indexed line
	pub fn line_one(&self) -> u32 { self.line + 1 }
	pub fn column(&self) -> u32 { self.column }
	pub fn bytes(&self) -> u32 { self.bytes }
	pub fn graphemes(&self) -> u32 { self.graphemes }

	pub fn make_err<T: Into<ParserErrorKind>>(&self, kind: T) -> ParserError {
		ParserError(
			*self,
			kind.into(),
		)
	}

	pub fn span(&self, other: &Offset) -> Span {
		Span(*self, *other)
	}
}

impl std::cmp::PartialOrd for Offset {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.bytes.cmp(&other.bytes))
  }
}

impl std::cmp::Ord for Offset {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.bytes.cmp(&other.bytes)
  }
}

/// Pair of (start_offset, end_offset), delimiting a stretch of source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span(pub Offset, pub Offset);

impl Span {
	pub fn node<T>(&self, inner: T) -> Node<T> {
		Node::new(*self, inner)
	}

	pub fn merge(&self, other: &Span) -> Span {
		Span(
			self.0.min(other.0),
			self.1.max(other.1),
		)
	}
}

#[derive(Debug)]
pub struct Node<T>(pub Span, pub T);

impl<T> Node<T> {
	pub fn new(
		span: Span,
		inner: T,
	) -> Self {
		Node(span, inner)
	}

	pub fn inner_mut(&mut self) -> &mut T {
		&mut self.1
	}

	pub fn inner(&self) -> &T {
		&self.1
	}

	pub fn into_inner(self) -> T {
		self.1
	}

	pub fn span(&self) -> &Span {
		&self.0
	}

	pub fn map<F, U>(self, f: F) -> Node<U>
	where
		F: FnOnce(Span, T) -> U,
	{
		Node(
			self.0,
			f(self.0, self.1)
		)
	}
}
